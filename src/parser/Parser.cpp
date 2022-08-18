#include "Parser.hpp"
#include <utils/Macros.hpp>

using namespace gdml;
using namespace gdml::ast;

// macros :3

#define INVALID_TOKEN() (Token::invalid(m_source))
#define INVALID_LAST_TOKEN() \
    (Token(m_tokens.back().end, m_tokens.back().end, \
    TokenType::Invalid, std::string(), m_source))
#define LAST_TOKEN() \
    (m_tokens.size() ? m_tokens.back() : INVALID_TOKEN())
#define CURRENT_TOKEN() \
    (m_index < m_tokens.size() ? m_tokens.at(m_index) : INVALID_LAST_TOKEN())
#define INIT_TOKEN() \
    auto token = CURRENT_TOKEN();\
    auto next = m_index + 1 < m_tokens.size() ? m_tokens.at(m_index + 1) : INVALID_TOKEN();
#define UPDATE_TOKEN() \
    token = CURRENT_TOKEN();\
    next = m_index + 1 < m_tokens.size() ? m_tokens.at(m_index + 1) : INVALID_TOKEN()
#define NEXT_TOKEN() \
    m_index++;\
    UPDATE_TOKEN()
#define PREV_TOKEN() \
    (m_tokens.size() ? m_tokens.at(m_index - 1) : INVALID_TOKEN())

ExprResult<InterpolatedLiteralExpr> Parser::parseInterpolated() noexcept {
    INIT_TOKEN();

    auto startPos = token.start;

    // consume begin token
    NEXT_TOKEN();

    std::vector<std::string> strings {};
    std::vector<std::string> rawStrings {};
    std::vector<ValueExpr*> values {};

    while (token.type != TokenType::EndInterpolatedString) {
        if (token.type == TokenType::String) {
            strings.push_back(token.data);
            rawStrings.push_back(token.rawData.value());
            NEXT_TOKEN();
        } else if (token.type == TokenType::BeginInterpolatedComponent) {
            m_index++;
            auto value = parseExpression();
            if (!value) return value.unwrapErr();
            values.push_back(value.unwrap());
            UPDATE_TOKEN();
            if (token.type != TokenType::EndInterpolatedComponent) {
                THROW_SYNTAX_ERR(
                    token,
                    "Expected end of component, found " + TOKEN_STR_V(token.type),
                    "",
                    "This is probably a bug in the language. "
                    "Oopsy daisy >w<"
                );
            }
            NEXT_TOKEN();
        } else {
            THROW_SYNTAX_ERR(
                token,
                "Expected end of interpolated string literal, "
                "found " + TOKEN_STR_V(token.type),
                "",
                "This is probably a bug in the language. "
                "Oopsy daisy >w<"
            );
        }
    }
    
    // consume ending token
    m_index++;
    
    return m_ast->make<InterpolatedLiteralExpr>(
        m_source, startPos, token.end,
        strings, rawStrings, values
    );
}

ExprResult<NameExpr> Parser::parseName() noexcept {
    INIT_TOKEN();

    if (token.type != TokenType::Identifier) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(Identifier) + ", found " + 
            TOKEN_STR_V(token.type),
            "",
            ""
        );
    }
    // consume name
    m_index++;

    // is this a scope name?
    if (next.type == TokenType::Scope) {
        // consume scope operator
        m_index++;

        // parse inner scope
        auto innerScope = parseName();
        PROPAGATE_ERROR(innerScope);
        return m_ast->make<ScopeExpr>(
            m_source, token.start, PREV_TOKEN().end,
            token.data, innerScope.unwrap()
        );
    }
    // oh it's just a name
    return m_ast->make<NameExpr>(
        m_source, token.start, token.end, token.data
    );
}

ExprResult<Expr> Parser::parseParenthesis(bool expectType) noexcept {
    INIT_TOKEN();

    // consume opening parenthesis
    m_index++;

    Position start = token.start;
    Expr* expr;
    if (expectType) {
        PROPAGATE_ASSIGN(expr, parseTypeExpression());
    } else {
        PROPAGATE_ASSIGN(expr, parseExpression());
    }

    UPDATE_TOKEN();

    if (token.type != TokenType::RightParen) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(RightParen) + " to match the " +
            TOKEN_STR(LeftParen) + " at " + start.toString(),
            "",
            ""
        );
    }

    // consume closing parenthesis
    m_index++;

    return expr;
}

ExprResult<TypeExpr> Parser::parseTypeExpression() noexcept {
    #define CHECK_CONST() \
        if (token.type == TokenType::Const) {\
            isConst = true;\
            NEXT_TOKEN();\
        }
    
    INIT_TOKEN();

    auto start = token.start;

    // left-side const
    bool isConst = false;
    CHECK_CONST();

    if (token.type != TokenType::Identifier) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(Identifier) + " for type, "
            "found" + TOKEN_STR_V(token.type),
            "",
            ""
        );
    }

    auto typeName = parseName();
    PROPAGATE_ERROR(typeName);
    
    // right-side const
    UPDATE_TOKEN();
    CHECK_CONST();

    TypeExpr* res = m_ast->make<TypeNameExpr>(
        m_source, start, PREV_TOKEN().end,
        typeName.unwrap(), types::TypeQualifiers(isConst)
    );

    UPDATE_TOKEN();

    // there can be no pointers to a reference, 
    // so just check for pointers first and then 
    // for references
    
    // pointers
    size_t ptrs = 0;
    while (ptrs = extractSymbolCount(token.type, '*', true)) {
        isConst = false;
        for (size_t i = 0; i < ptrs; i++) {
            // const applies to outermost
            if (i == ptrs - 1) {
                NEXT_TOKEN();
                CHECK_CONST();
                UPDATE_TOKEN();
            }
            res = m_ast->make<PointerExpr>(
                m_source, start, PREV_TOKEN().end,
                res, types::PointerType::Pointer,
                types::TypeQualifiers(isConst)
            );
        }
    }

    // references
    if (token.type == TokenType::BitAnd) {
        res = m_ast->make<PointerExpr>(
            m_source, start, PREV_TOKEN().end,
            res, types::PointerType::Reference,
            types::TypeQualifiers(isConst)
        );
        NEXT_TOKEN();
    }
    // move thingies
    else if (token.type == TokenType::And) {
        res = m_ast->make<PointerExpr>(
            m_source, start, PREV_TOKEN().end,
            res, types::PointerType::Move,
            types::TypeQualifiers(isConst)
        );
        NEXT_TOKEN();
    }

    // there can be no more references
    if (token.type == TokenType::BitAnd || token.type == TokenType::And) {
        THROW_SYNTAX_ERR(
            token,
            "You can not create references to references",
            "", ""
        );
    }

    return res;
}

ExprResult<VariableDeclExpr> Parser::parseVarDeclaration() noexcept {
    // 'let' or 'auto' has already been consumed since this 
    // function is used for implicit declarations like 
    // function parameters aswell

    INIT_TOKEN();

    auto startPos = token.start;

    if (token.type != TokenType::Identifier) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(Identifier) + " for "
            "variable declaration",
            "Give your variable a good & descriptive name",
            ""
        );
    }

    auto name = token.data;
    Option<TypeExpr*> varType = None;
    
    // consume name
    m_index++;

    // le type
    if (next.type == TokenType::Colon) {
        // consume colon
        m_index++;
        auto type = parseTypeExpression();
        PROPAGATE_ERROR(type);
        varType = Some(type.unwrap());
    }

    UPDATE_TOKEN();

    Option<ValueExpr*> value = None;
    if (token.type == TokenType::Assign) {
        m_index++;
        PROPAGATE_ASSIGN(value, parseExpression());
    }

    // for the end position
    UPDATE_TOKEN();

    return Ok(m_ast->make<VariableDeclExpr>(m_source, startPos, token.end, varType, name, value));
}

ExprResult<CallExpr> Parser::parseCall(ValueExpr* value) noexcept {
    INIT_TOKEN();

    auto start = token.start;

    // consume '('
    NEXT_TOKEN();

    std::vector<ValueExpr*> args;

    // is this just '()'?
    if (token.type == TokenType::RightParen) {
        // consume ')'
        m_index++;
        return m_ast->make<CallExpr>(
            m_source, start, token.end,
            value, args
        );
    }

    while (m_index < m_tokens.size()) {
        auto var = parseExpression();
        PROPAGATE_ERROR(var);
        args.push_back(var.unwrap());

        UPDATE_TOKEN();
        if (token.type == TokenType::Comma) {
            // consume comma
            m_index++;
            continue;
        }
        // if it's not a comma, then it must be the 
        // closing brace or the code is malformed
        break;
    }

    if (token.type != TokenType::RightParen) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(RightParen) + " to conclude "
            "function call argument list",
            "",
            ""
        );
    }
    // consume ')'
    m_index++;

    return m_ast->make<CallExpr>(
        m_source, start, token.end,
        value, args
    );
}

ExprResult<ValueExpr> Parser::parseValue() noexcept {
    INIT_TOKEN();

    if (token.type == TokenType::LeftParen) {
        auto expr = parseParenthesis();
        PROPAGATE_ERROR(expr);
        return static_cast<ValueExpr*>(expr.unwrap());
    }

    if (token.type == TokenType::Identifier) {
        NameExpr* name;
        PROPAGATE_ASSIGN(name, parseName());
        return m_ast->make<VariableExpr>(
            m_source, name->start, name->end, name
        );
    }

    // liberals
    switch (token.type) {
        case TokenType::True: {
            m_index++;
            return m_ast->make<BoolLiteralExpr>(
                m_source, token.start, token.end, true
            );
        } break;

        case TokenType::False: {
            m_index++;
            return m_ast->make<BoolLiteralExpr>(
                m_source, token.start, token.end, false
            );
        } break;

        case TokenType::None: {
            m_index++;
            return m_ast->make<NoneLiteralExpr>(
                m_source, token.start, token.end
            );
        } break;

        case TokenType::Null: {
            m_index++;
            return m_ast->make<NullLiteralExpr>(
                m_source, token.start, token.end
            );
        } break;

        case TokenType::Float: {
            try {
                // consume number
                m_index++;
                types::DataType numType = types::DataType::I32;
                auto end = token.end;
                if (next.type == TokenType::NumeralSuffix) {
                    numType = types::dataTypeFromString(next.data);
                    end = next.end;
                    // consume suffix
                    m_index++;
                }
                return m_ast->make<FloatLiteralExpr>(
                    m_source, token.start, end,
                    std::stod(token.data), numType
                );
            } catch(std::exception& e) {
                THROW_SYNTAX_ERR(
                    token,
                    "Float literal appears not to be valid? "
                    "(Caught exception: " + std::string(e.what()) + ")",
                    "",
                    "This is most likely an error in the AST parser"
                );
            }
        } break;

        case TokenType::Int: {
            try {
                // consume number
                m_index++;
                int base = 10;
                #define CHECK_BASE(iden, num) \
                    if (token.data.substr(0, 2) == iden) { base = num; token.data.erase(0, 2); }
                CHECK_BASE("0b", 2);
                CHECK_BASE("0x", 16);
                
                auto end = token.end;
                types::DataType numType = types::DataType::I32;
                if (next.type == TokenType::NumeralSuffix) {
                    numType = types::dataTypeFromString(next.data);
                    end = next.end;
                    // consume suffix
                    m_index++;
                }
                if (dataTypeIsUnsigned(numType)) {
                    return m_ast->make<UIntLiteralExpr>(
                        m_source, token.start, end,
                        std::stoull(token.data, nullptr, base), numType
                    );
                }
                return m_ast->make<IntLiteralExpr>(
                    m_source, token.start, end,
                    std::stoll(token.data, nullptr, base), numType
                );
            } catch(std::exception& e) {
                THROW_SYNTAX_ERR(
                    token,
                    "Integer literal appears not to be valid? "
                    "(Caught exception: " + std::string(e.what()) + ")",
                    "",
                    "This is most likely an error in the AST parser"
                );
            }
        } break;

        case TokenType::String: {
            m_index++;
            return m_ast->make<StringLiteralExpr>(
                m_source, token.start, token.end, token.data, token.rawData.value()
            );
        } break;

        case TokenType::BeginInterpolatedString: {
            PROPAGATE_VALUE(parseInterpolated());
        } break;
    }

    THROW_SYNTAX_ERR(
        token,
        "Expected identifier or literal, found " + TOKEN_STR_V(token.type),
        "",
        ""
    );
}

ExprResult<ValueExpr> Parser::parseUnary() noexcept {
    INIT_TOKEN();

    auto start = token.start;

    // get prefix if there's one
    TokenType prefixOp = TokenType::Invalid;
    if (isUnaryPrefixOperator(token.type)) {
        prefixOp = token.type;
        m_index++;
    }

    // get the thing being unarified
    auto value = parseValue();
    PROPAGATE_ERROR(value);

    auto res = value.unwrap();

    UPDATE_TOKEN();

    auto prefixEnd = token.end;

    // function calls
    if (token.type == TokenType::LeftParen) {
        PROPAGATE_ASSIGN(res, parseCall(res));
        UPDATE_TOKEN();
    }

    // get suffix if there is one
    TokenType suffixOp = TokenType::Invalid;
    if (isUnarySuffixOperator(token.type)) {
        suffixOp = token.type;
        m_index++;
    }

    if (prefixOp != TokenType::Invalid) {
        res = m_ast->make<UnaryExpr>(
            m_source, start, prefixEnd,
            prefixOp, res, UnaryExpr::Prefix
        );
    }
    if (suffixOp != TokenType::Invalid) {
        res = m_ast->make<UnaryExpr>(
            m_source, start, token.end,
            suffixOp, res, UnaryExpr::Suffix
        );
    }

    UPDATE_TOKEN();

    // casts
    if (token.type == TokenType::As) {
        // consume 'as'
        m_index++;

        TypeExpr* intoType;
        PROPAGATE_ASSIGN(intoType, parseTypeExpression());
        UPDATE_TOKEN();

        res = m_ast->make<CastTypeExpr>(
            m_source, start, PREV_TOKEN().end,
            res, intoType
        );
    }

    return res;
}

ExprResult<ValueExpr> Parser::parseBinary(ValueExpr* lhs, int precedence) noexcept {
    INIT_TOKEN();

    auto start = lhs->start;

    while (m_index < m_tokens.size()) {

        // precedence of current token
        auto currentPrec = precedenceOfOperator(token.type);

        // if the current token is not an operator, 
        // this will always be truthy since -1 < 0
        if (currentPrec < precedence) {
            return Ok(lhs);
        }

        // save operator
        auto op = token.type;
        m_index++;

        // parse rhs
        ValueExpr* rhs;
        PROPAGATE_ASSIGN(rhs, parseUnary());

        // get precedence of next operator
        UPDATE_TOKEN();
        auto nextPrec = precedenceOfOperator(token.type);

        // if the next operator is more important, 
        // let it take the current rhs as lhs
        if (currentPrec < nextPrec) {
            PROPAGATE_ASSIGN(rhs, parseBinary(rhs, currentPrec + 1));
        }

        // merge lhs & rhs into binop expression
        lhs = m_ast->make<BinaryExpr>(
            m_source, start, PREV_TOKEN().end,
            op, lhs, rhs
        );
    }
    THROW_SYNTAX_ERR(
        PREV_TOKEN(),
        "Expected binary operator branch, found end of file",
        "",
        "I don't know how you got here."
    );
}

ExprResult<ValueExpr> Parser::parseExpression() noexcept {
    auto value = parseUnary();
    PROPAGATE_ERROR(value);
    INIT_TOKEN();
    if (isBinaryOperator(token.type)) {
        return parseBinary(value.unwrap());
    }
    return value;
}

ExprResult<Stmt> Parser::parseUsing() noexcept {
    INIT_TOKEN();

    auto start = token.start;

    // consume 'using'
    NEXT_TOKEN();

    switch (token.type) {
        case TokenType::Namespace: {
            // consume 'namespace'
            m_index++;

            NameExpr* name;
            PROPAGATE_ASSIGN(name, parseName());

            return m_ast->make<UsingNameSpaceStmt>(
                m_source, start, PREV_TOKEN().end, name
            );
        } break;

        default: break;
    }

    THROW_SYNTAX_ERR(
        token,
        "Invalid token for an " + TOKEN_STR(Using) + " declaration",
        "",
        ""
    );
}

ExprResult<FunctionDeclStmt> Parser::parseFunDeclaration() noexcept {
    INIT_TOKEN();

    auto start = token.start;

    auto impl = token.type == TokenType::Implement;

    // consume 'fun'
    NEXT_TOKEN();

    auto funName = parseName();
    PROPAGATE_ERROR(funName);

    UPDATE_TOKEN();

    if (token.type != TokenType::LeftParen) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(LeftParen) + " for function "
            "parameters, found " + TOKEN_STR_V(token.type),
            "If the function has no parameters, write '()'",
            ""
        );
    }
    // consume parenthesis
    NEXT_TOKEN();

    std::vector<VariableDeclExpr*> params;

    // is it just '()'
    if (token.type == TokenType::RightParen) {
        // ooooo! goto! evil code! bad code!
        goto no_params;
    }

    while (m_index < m_tokens.size()) {
        auto var = parseVarDeclaration();
        PROPAGATE_ERROR(var);
        params.push_back(var.unwrap());

        UPDATE_TOKEN();
        if (token.type == TokenType::Comma) {
            // consume comma
            m_index++;
            continue;
        }
        // if it's not a comma, then it must be the 
        // closing brace or the code is malformed
        break;
    }

    if (token.type != TokenType::RightParen) {
        THROW_SYNTAX_ERR(
            token,
            "Expected " + TOKEN_STR(RightParen) + " to conclude "
            "function parameter list",
            "",
            ""
        );
    }

no_params:

    // consume parenthesis
    NEXT_TOKEN();

    // return type
    Option<TypeExpr*> retType = None;
    if (token.type == TokenType::Arrow) {
        // consume arrow
        m_index++;
        PROPAGATE_ASSIGN(retType, parseTypeExpression());
    }

    UPDATE_TOKEN();

    // const-qualified function
    bool isConst = false;
    if (token.type == TokenType::Const) {
        isConst = true;
        NEXT_TOKEN();
    }

    auto end = PREV_TOKEN().end;

    // body
    Option<StmtList*> body = None;
    if (token.type == TokenType::LeftBrace) {
        PROPAGATE_ASSIGN(body, parseBlock());
    }
    // short-form syntax `fun () => expr;`
    else if (token.type == TokenType::FatArrow) {
        auto bodyStart = next.start;

        // consume arrow
        m_index++;

        ValueExpr* value;
        PROPAGATE_ASSIGN(value, parseExpression());
        
        UPDATE_TOKEN();

        body = m_ast->make<StmtList>(
            m_source, bodyStart, PREV_TOKEN().end,
            std::vector<Stmt*> {
                m_ast->make<ReturnStmt>(
                    m_source, bodyStart, PREV_TOKEN().end,
                    value
                )
            }
        );
        // short-form syntax must end with semicolon
        if (token.type != TokenType::SemiColon) {
            THROW_SYNTAX_ERR(
                token,
                "Expected " + TOKEN_STR(SemiColon) + 
                "after short-form function body, found " + 
                TOKEN_STR_V(token.type),
                "Add a " + TOKEN_STR(SemiColon) + " at the end",
                ""
            );
        }
    }
    // if no body, semicolon required
    else if (token.type != TokenType::SemiColon) {
        THROW_SYNTAX_ERR(
            token,
            "Expected function body or " + TOKEN_STR(SemiColon) + 
            "after function declaration, found " + TOKEN_STR_V(token.type),
            "",
            ""
        );
    }

    return m_ast->make<FunctionDeclStmt>(
        m_source, start, end,
        m_ast->make<FunctionTypeExpr>(
            m_source, start, end,
            params, retType,
            types::TypeQualifiers(isConst)
        ),
        funName.unwrap(), body, impl
    );
}

ExprResult<IfStmt> Parser::parseIfChain() noexcept {
    INIT_TOKEN();

    Option<ValueExpr*> condition = None;
    auto ifStart = token.start;

    // if this is not hit, then we are 
    // in a final 'else' block
    if (token.type == TokenType::If) {
        NEXT_TOKEN();

        bool usingParen = false;
        auto leftParen = token.start;
        // skip parenthesis if they are there; 
        // otherwise optional
        if (token.type == TokenType::LeftParen) {
            usingParen = true;
            m_index++;
        }

        PROPAGATE_ASSIGN(condition, parseExpression());

        UPDATE_TOKEN();

        // found closing parenthesis?
        if (token.type == TokenType::RightParen) {
            if (usingParen) {
                // skip it
                NEXT_TOKEN();
            } else {
                THROW_SYNTAX_ERR(
                    token,
                    "Found lonely " + TOKEN_STR(RightParen),
                    "Remove the " + TOKEN_STR(RightParen) + ", or add " +
                    TOKEN_STR(LeftParen) + leftParen.toString(),
                    ""
                );
            }
        }
        // need one?
        else if (usingParen) {
            THROW_SYNTAX_ERR(
                token,
                "Expected matching " + TOKEN_STR(RightParen) + ", "
                "but found none",
                "Add the " + TOKEN_STR(RightParen) + " here",
                ""
            );
        }
    }

    if (token.type != TokenType::LeftBrace) {
        THROW_SYNTAX_ERR(
            token,
            "Expected block (" + TOKEN_STR(LeftBrace) + ") after " + 
            TOKEN_STR(If) + " condition, found " + TOKEN_STR_V(token.type),
            "",
            ""
        );
    }

    auto block = parseBlock();
    PROPAGATE_ERROR(block);

    UPDATE_TOKEN();

    Option<IfStmt*> elseBranch = None;
    if (token.type == TokenType::Else) {
        if (!condition.has_value()) {
            THROW_SYNTAX_ERR(
                token,
                "Found another " + TOKEN_STR(Else) + " branch after "
                "concluding" + TOKEN_STR(Else) + " branch " + ifStart.toString(),
                "Remove this branch",
                ""
            );
        }
        // skip 'else'
        m_index++;
        PROPAGATE_ASSIGN(elseBranch, parseIfChain());
    }

    UPDATE_TOKEN();

    return m_ast->make<IfStmt>(
        m_source, ifStart, token.end,
        condition, block.unwrap(), elseBranch
    );
}

ExprResult<StmtList> Parser::parseBlock(bool topLevel) noexcept {
    std::vector<Stmt*> statements;

    INIT_TOKEN();
    auto start = token.start;

    // if this is just '{}'
    if (next.type == TokenType::RightBrace) {
        m_index += 2;
        return m_ast->make<StmtList>(
            m_source, start, next.end, statements
        );
    }

    // consume opening brace
    m_index++;

    bool foundClosingBrace = false;
    while (m_index < m_tokens.size()) {
        auto s = parseStatement(topLevel);
        PROPAGATE_ERROR(s);
        if (s.unwrap()) {
            statements.push_back(s.unwrap());
        }
        UPDATE_TOKEN();
        if (token.type == TokenType::RightBrace) {
            foundClosingBrace = true;
            break;
        }
    }
    if (!foundClosingBrace) {
        THROW_SYNTAX_ERR(
            PREV_TOKEN(),
            "Expected " + TOKEN_STR(RightBrace) + " "
            "to match the " + TOKEN_STR(LeftBrace) + " at " +
            start.toString() + ", found end of file",
            "Add the " + TOKEN_STR(RightBrace),
            ""
        );
    }

    UPDATE_TOKEN();
    
    // consume closing brace
    m_index++;

    return m_ast->make<StmtList>(
        m_source, start, token.end, statements
    );
}

ExprResult<Stmt> Parser::parseStatement(bool topLevel) noexcept {
    INIT_TOKEN();

    #define CHECK_SEMICOLON() \
        UPDATE_TOKEN();\
        if (token.type != TokenType::SemiColon) {\
            THROW_SYNTAX_ERR(\
                token,\
                "Expected " + TOKEN_STR(SemiColon) + " for end of "\
                "statement, found " + TOKEN_STR_V(token.type),\
                "Add a " + TOKEN_STR(SemiColon) + " before the next statement",\
                ""\
            );\
        }\
        m_index++;

    switch (token.type) {
        case TokenType::SemiColon: {
            // just skip 'em
            m_index++;
            return nullptr;
        } break;

        case TokenType::Import: {
            if (!topLevel) {
                THROW_SYNTAX_ERR(
                    token,
                    "Found a non-top-level import statement",
                    "",
                    "Import statements are only allowed at global scope"
                );
            }
            auto start = token.start;
            bool isRelative = true;
            if (next.type == TokenType::At) {
                isRelative = false;
                NEXT_TOKEN();
            }
            if (next.type != TokenType::String) {
                THROW_SYNTAX_ERR(
                    next,
                    "Expected " + TOKEN_STR(String) + " after " + 
                    TOKEN_STR(Import) + ", found " + TOKEN_STR_V(next.type),
                    "",
                    ""
                );
            }
            auto stmt = m_ast->make<ImportStmt>(
                m_source, start, next.end, next.data, isRelative
            );
            // skip 'import' and literal
            m_index += 2;
            // CHECK_SEMICOLON();
            return stmt;
        } break;

        case TokenType::Let: {
            m_index++;
            PROPAGATE_VALUE(parseVarDeclaration());
        } break;

        case TokenType::EmbedLanguageIdentifier: {
            if (next.type != TokenType::EmbeddedCode) {
                THROW_SYNTAX_ERR(
                    next,
                    "Expected " + TOKEN_STR(EmbeddedCode) + " after " +
                    TOKEN_STR(EmbedLanguageIdentifier) + ", found " +
                    TOKEN_STR_V(next.type),
                    "",
                    "This should not be possible. Something has gone "
                    "wrong in the compiler."
                );
            }
            // consume both
            m_index += 2;
            return m_ast->make<EmbedCodeStmt>(
                m_source, token.start, next.end,
                token.data, next.data
            );
        } break;

        case TokenType::Namespace: {
            if (!topLevel) {
                THROW_SYNTAX_ERR(
                    token,
                    "Namespaces are only permitted at top level",
                    "",
                    ""
                );
            }

            auto start = token.start;

            // consume token
            m_index++;
            if (next.type != TokenType::Identifier) {
                THROW_SYNTAX_ERR(
                    next,
                    "Expected " + TOKEN_STR(Identifier) + " for " + 
                    "namespace name, found " + TOKEN_STR_V(next.type),
                    "",
                    "Anonymous namespaces are currently not "
                    "supported, sorry!"
                );
            }
            auto name = parseName();
            PROPAGATE_ERROR(name);

            StmtList* list;
            PROPAGATE_ASSIGN(list, parseBlock(topLevel));

            UPDATE_TOKEN();

            return m_ast->make<NameSpaceStmt>(
                m_source, start, PREV_TOKEN().end,
                name.unwrap(), list
            );
        } break;

        case TokenType::Using: {
            Stmt* stmt;
            PROPAGATE_ASSIGN(stmt, parseUsing());
            CHECK_SEMICOLON();
            return stmt;
        } break;

        case TokenType::Implement:
        case TokenType::Function: {
            PROPAGATE_VALUE(parseFunDeclaration());
        } break;

        case TokenType::Return: {
            if (topLevel) {
                THROW_SYNTAX_ERR(
                    token,
                    "Found a top-level return statement",
                    "", ""
                );
            }
            auto start = token.start;
            m_index++;
            auto expr = parseExpression();
            PROPAGATE_ERROR(expr);
            auto stmt = m_ast->make<ReturnStmt>(
                m_source, start, PREV_TOKEN().end, expr.unwrap()
            );
            CHECK_SEMICOLON();
            return stmt;
        } break;

        case TokenType::LeftBrace: {
            StmtList* body;
            PROPAGATE_ASSIGN(body, parseBlock(topLevel));

            return m_ast->make<BlockStmt>(
                m_source, body->start, body->end, body
            );
            // no need to conclude blocks with semicolons
        } break;

        case TokenType::If: {
            if (topLevel) {
                THROW_SYNTAX_ERR(
                    token,
                    "Found a top-level if statement",
                    "", ""
                );
            }
            // no need to conclude ifs with semicolons
            PROPAGATE_VALUE(parseIfChain());
        } break;

        default: {
            if (topLevel) {
                THROW_SYNTAX_ERR(
                    token,
                    "Found an invalid token or a top-level expression",
                    "",
                    "Only declarations and definitions are allowed "
                    "at top-level"
                );
            }

            // must be an expression then
            ValueExpr* res;
            PROPAGATE_ASSIGN(res, parseExpression());

            UPDATE_TOKEN();
            // implicit return
            if (!topLevel && token.type == TokenType::RightBrace) {
                return m_ast->make<ReturnStmt>(
                    res->source, res->start, res->end, res
                );
            } else {
                // check that the statement was ended properly
                CHECK_SEMICOLON();
            }

            return res;
        }
    }
}

ParseResult Parser::parse() noexcept {
    auto ast = new AST();

    m_index = 0;
    m_source = m_tokens.front().source;

    m_ast = ast;
    while (m_index < m_tokens.size()) {
        auto stmt = parseStatement(true);
        if (!stmt) {
            m_ast = nullptr;
            delete ast;
            return stmt.unwrapErr();
        }
        if (stmt.unwrap()) {
            ast->tree().push_back(stmt.unwrap());
        }
    }
    m_ast = nullptr;
    m_source = nullptr;
    
    return ast;
}

Parser::Parser(Instance& inst, Tokens const& tokens) noexcept
 : m_instance(inst), m_tokens(tokens) {}

