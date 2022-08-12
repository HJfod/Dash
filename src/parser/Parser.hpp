#pragma once

#include "AST.hpp"

namespace gdml {
    class GDML;

    using ParseResult = LineResult<ast::AST*>;
    template<class Stmt>
    using ExprResult = LineResult<Stmt*>;

    class Parser {
    protected:
        GDML& m_shared;
        SourceFile const* m_source;
        size_t m_index = 0;
        Tokens m_tokens;
        ast::AST* m_ast = nullptr;

        ExprResult<ast::TypeExpr> parseTypeExpression() noexcept;

        ExprResult<ast::CallExpr> parseCall(ast::ValueExpr* value) noexcept;
        ExprResult<ast::NameExpr> parseName() noexcept;
        ExprResult<ast::InterpolatedLiteralExpr> parseInterpolated() noexcept;
        ExprResult<ast::Expr> parseParenthesis(bool expectType = false) noexcept;
        ExprResult<ast::VariableDeclExpr> parseVarDeclaration() noexcept;
        ExprResult<ast::ValueExpr> parseValue() noexcept;
        ExprResult<ast::ValueExpr> parseBinary(
            ast::ValueExpr* lhs, int precedence = 0
        ) noexcept;
        ExprResult<ast::ValueExpr> parseUnary() noexcept;
        ExprResult<ast::ValueExpr> parseExpression() noexcept;

        ExprResult<ast::FunctionDeclStmt> parseFunDeclaration() noexcept;
        ExprResult<ast::IfStmt> parseIfChain() noexcept;
        ExprResult<ast::BlockStmt> parseBlock(bool topLevel = false) noexcept;
        ExprResult<ast::Stmt> parseStatement(bool topLevel = false) noexcept;
    
    public:
        Parser(GDML& shared, Tokens const& tokens) noexcept;

        ParseResult parse() noexcept;

        static ParseResult parseWhole(GDML& compiler, Tokens const& tokens) noexcept;
    };

}