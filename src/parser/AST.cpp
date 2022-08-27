#include "AST.hpp"
#include <compiler/GDML.hpp>
#include <compiler/Compiler.hpp>
#include <compiler/Instance.hpp>
#include <utils/Macros.hpp>
#include <compiler/Value.hpp>
#include <compiler/Entity.hpp>

using namespace gdml;
using namespace gdml::ast;

#define PUSH_INDENT() \
    instance.getCompiler().getFormatter().pushIndent(); \
    instance.getCompiler().getFormatter().newLine(stream)

#define POP_INDENT() \
    instance.getCompiler().getFormatter().popIndent(); \
    instance.getCompiler().getFormatter().newLine(stream)

#define NEW_LINE() \
    instance.getCompiler().getFormatter().newLine(stream)

#define PUSH_SCOPE() \
    instance.getCompiler().pushScope()

#define POP_SCOPE() \
    instance.getCompiler().popScope()

#define PUSH_NAMESPACE(name) \
    instance.getCompiler().getScope().pushNamespace(name)

#define POP_NAMESPACE() \
    instance.getCompiler().getScope().popNamespace()

#define STR(str) #str
#define STRING(str) STR(str)

#define EXPECT_TYPE(from) \
    if (!from->evalType.type) {\
        THROW_COMPILE_ERR(\
            "Internal compiler error: Expected " #from " to have a type, "\
            "but it was undefined",\
            "It was probably VariableDeclExpr. Make sure to "\
            "give your variable declaration a type",\
            "In " __FUNCTION__ ":" STRING(__LINE__)\
        );\
    }\
    if (typeIsVoid(from->evalType.type)) {\
        THROW_TYPE_ERR(\
            "Expected " #from " to have a type, but it was `void`",\
            "",\
            ""\
        );\
    }

#define EXPECT_VARIABLE(var) \
    if (!var) {\
        THROW_COMPILE_ERR(\
            "Internal compiler error: Expected " #var " to not be null, "\
            "but it was",\
            "",\
            "In " __FUNCTION__ ":" STRING(__LINE__)\
        );\
    }

#define EXPECT_VARIABLE_CG(var) \
    if (!var) {\
        stream << "static_assert(false, \"Internal GDML transpiler error: Expected " #var \
            " not to be null in " __FUNCTION__ ":" STRING(__LINE__) "\")";\
        return;\
    }

#define DEBUG_LOG_TYPE() \
    instance.getShared().logDebug(__FUNCTION__ " -> " + evalType.toString())

#define DEBUG_LOG_TYPE_S() \
    instance.getShared().logDebug(__FUNCTION__ " -> Stmt (`void`)")

#define DATA_TYPE_CHECK(typeName) \
    case types::DataType::typeName:\
        evalType = QualifiedType {\
            instance.getCompiler().getBuiltInType(types::DataType::typeName),\
            types::LITERAL_QUALIFIED\
        };\
        break

#define DATA_EVAL_VALUE(typeName) \
    case types::DataType::typeName:\
    return instance.getCompiler().makeValue<NumeralValue<types::typeName>>(\
        static_cast<types::typeName>(value)\
    );\
    break

static bool matchBranchTypes(Option<QualifiedType> const& a, Option<QualifiedType> const& b) {
    // do both branches actually return something?
    if (a.has_value() && b.has_value()) {
        return a.value().convertibleTo(b.value(), true);
    }
    return true;
}

// BoolLiteralExpr

TypeCheckResult BoolLiteralExpr::compile(Instance& instance) noexcept {
    evalType = QualifiedType {
        instance.getCompiler().getBuiltInType(types::DataType::Bool),
        types::LITERAL_QUALIFIED
    };
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> BoolLiteralExpr::eval(Instance& instance) {
    return instance.getCompiler().getConstValue(
        value ? ConstValue::True : ConstValue::False
    );
}

// IntLiteralExpr

TypeCheckResult IntLiteralExpr::compile(Instance& instance) noexcept {
    switch (type) {
        DATA_TYPE_CHECK(I8);
        DATA_TYPE_CHECK(I16);
        DATA_TYPE_CHECK(I32);
        DATA_TYPE_CHECK(I64);
        default: THROW_TYPE_ERR(
            "Integer literal type is somehow not "
            "valid.",
            "",
            "This is an error in the compiler."
        );
    }
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> IntLiteralExpr::eval(Instance& instance) {
    switch (type) {
        DATA_EVAL_VALUE(I8);
        DATA_EVAL_VALUE(I16);
        DATA_EVAL_VALUE(I32);
        DATA_EVAL_VALUE(I64);
        default: return nullptr;
    }
}

// UIntLiteralExpr

TypeCheckResult UIntLiteralExpr::compile(Instance& instance) noexcept {
    switch (type) {
        DATA_TYPE_CHECK(U8);
        DATA_TYPE_CHECK(U16);
        DATA_TYPE_CHECK(U32);
        DATA_TYPE_CHECK(U64);
        default: THROW_TYPE_ERR(
            "Unsigned integer literal type is somehow not "
            "valid.",
            "",
            "This is an error in the compiler."
        );
    }
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> UIntLiteralExpr::eval(Instance& instance) {
    switch (type) {
        DATA_EVAL_VALUE(U8);
        DATA_EVAL_VALUE(U16);
        DATA_EVAL_VALUE(U32);
        DATA_EVAL_VALUE(U64);
        default: return nullptr;
    }
}

// FloatLiteralExpr

TypeCheckResult FloatLiteralExpr::compile(Instance& instance) noexcept {
    switch (type) {
        DATA_TYPE_CHECK(F32);
        DATA_TYPE_CHECK(F64);
        default: THROW_TYPE_ERR(
            "Unsigned integer literal type is somehow not "
            "valid.",
            "",
            "This is an error in the compiler."
        );
    }
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> FloatLiteralExpr::eval(Instance& instance) {
    switch (type) {
        DATA_EVAL_VALUE(F32);
        DATA_EVAL_VALUE(F64);
        default: return nullptr;
    }
}

// StringLiteralExpr

TypeCheckResult StringLiteralExpr::compile(Instance& instance) noexcept {
    evalType = QualifiedType {
        instance.getCompiler().getBuiltInType(types::DataType::String),
        types::LITERAL_QUALIFIED
    };
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> StringLiteralExpr::eval(Instance& instance) {
    if (!value.size()) {
        return instance.getCompiler().getConstValue(ConstValue::EmptyString);
    }
    return instance.getCompiler().makeValue<StringValue>(value);
}

// InterpolatedLiteralExpr

TypeCheckResult InterpolatedLiteralExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILDREN(components);
    // todo: evalValue
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> InterpolatedLiteralExpr::eval(Instance& instance) {
    // todo
    return nullptr;
}

void InterpolatedLiteralExpr::codegen(Instance& com, std::ostream& stream) const noexcept {
    for (size_t ix = 0; ix < rawStrings.size() + components.size(); ix++) {
        if (ix) {
            stream << " + ";
        }
        if (ix % 2) {
            // todo: convert codegenned result to string or smth
            components.at(ix / 2)->codegen(com, stream);
        } else {
            stream << "\"" << rawStrings.at(ix / 2) << "\"";
        }
    }
}

// NullLiteralExpr

TypeCheckResult NullLiteralExpr::compile(Instance& instance) noexcept {
    evalType = QualifiedType {
        instance.getCompiler().makeType<PointerType>(
            QualifiedType(
                instance.getCompiler().getBuiltInType(types::DataType::Void)
            )
        ),
        types::LITERAL_QUALIFIED
    };
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> NullLiteralExpr::eval(Instance& instance) {
    return instance.getCompiler().getConstValue(ConstValue::Null);
}

// UnaryExpr

TypeCheckResult UnaryExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(value);

    if (isLValueOperator(op) && value->evalType.qualifiers.isConst) {
        THROW_TYPE_ERR(
            "Invalid operands for binary expression: left-hand-side "
            "is const-qualified `" + value->evalType.toString() + "`, but "
            "the operator used " + TOKEN_STR_V(op) + " requires a "
            "modifiable value",
            "",
            ""
        );
    }

    evalType = value->evalType;

    DEBUG_LOG_TYPE();
    return Ok();
}

void UnaryExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (!value->evalType.type->codegenUnary(this, stream)) {
        if (type == Prefix) {
            stream << tokenTypeToString(op);
        }
        value->codegen(instance, stream);
        if (type == Suffix) {
            stream << tokenTypeToString(op);
        }
    }
}

// BinaryExpr

TypeCheckResult BinaryExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(LHS);
    GDML_TYPECHECK_CHILD(RHS);

    EXPECT_TYPE(LHS);
    EXPECT_TYPE(RHS);

    auto foundImpl = LHS->evalType.type->implementsBinaryOperator(op, RHS->evalType.type);

    if (foundImpl == ImplStatus::None) {
        THROW_TYPE_ERR(
            "Type `" + LHS->evalType.type->toString() + "` does not "
            "implement operator " + TOKEN_STR_V(op) + " for type `" +
            RHS->evalType.type->toString() + "`",
            "",
            "There are no implicit conversions in GDML. All types "
            "must match exactly!"
        );
    }

    if (isLValueOperator(op) && LHS->evalType.qualifiers.isConst) {
        THROW_TYPE_ERR(
            "Invalid operands for binary expression: left-hand-side "
            "is const-qualified `" + LHS->evalType.toString() + "`, but "
            "the operator used " + TOKEN_STR_V(op) + " requires a "
            "modifiable value",
            "",
            ""
        );
    }

    evalType = LHS->evalType;

    DEBUG_LOG_TYPE();

    // todo: compile-time

    // special operators
    if (op == TokenType::QuestionAssign) {
        // turn into ```
        // +  if (!LHS) LHS = RHS;
        // -> LHS
        // ```

        if (!parent->insertStatement(ast->make<IfStmt>(
            source, start, end,
            ast->make<UnaryExpr>(
                source, start, end,
                TokenType::Not, LHS, UnaryExpr::Prefix
            ),
            ast->make<StmtList>(
                source, start, end,
                std::vector<Stmt*> {
                    ast->make<BinaryExpr>(
                        source, start, end,
                        TokenType::Assign, LHS, RHS
                    ),
                }
            ),
            None
        ), this, false)) {
            THROW_COMPILE_ERR(
                "Unable to unwrap question assignment operator",
                "",
                "This is probably a compiler error"
            );
        }

        if (usedAsStmt) {
            if (!parent->removeStatement(this)) {
                THROW_COMPILE_ERR(
                    "Unable to remove question assignment operator "
                    "statement",
                    "",
                    "This is probably a compiler error"
                );
            }
        } else {
            if (!parent->swap(this, LHS)) {
                THROW_COMPILE_ERR(
                    "Unable to swap out question assignment operator "
                    "statement",
                    "",
                    "This is probably a compiler error"
                );
            }
        }
    }

    else if (op == TokenType::DoubleQuestion) {
        // turn into ```
        // +  auto _ = LHS;
        // -> _ ? _ : RHS
        // ```

        auto placeholder = generateIdentifierName();

        if (!parent->insertStatement(
            ast->make<VariableDeclExpr>(
                source, start, end,
                None, placeholder, LHS
            ),
            this, false
        )) {
            THROW_COMPILE_ERR(
                "Unable to unwrap nullish coalescing operator",
                "",
                "This is probably a compiler error"
            );
        }

        if (!parent->swap(this, ast->make<TernaryExpr>(
            source, start, end,
            ast->make<VariableExpr>(
                source, start, end,
                ast->make<NameExpr>(
                    source, start, end,
                    placeholder
                )
            ),
            ast->make<VariableExpr>(
                source, start, end,
                ast->make<NameExpr>(
                    source, start, end,
                    placeholder
                )
            ),
            RHS
        ))) {
            THROW_COMPILE_ERR(
                "Unable to swap out nullish coalescing operator",
                "",
                "This is probably a compiler error"
            );
        }
    }

    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> BinaryExpr::eval(Instance& instance) {
    if (status != ImplStatus::Constexpr) {
        return nullptr;
    }

    auto lhsValue = LHS->eval(instance);
    if (!lhsValue) return nullptr;

    auto rhsValue = RHS->eval(instance);
    if (!rhsValue) return nullptr;

    return evalType.type->evalBinary(op, lhsValue, rhsValue);
}

void BinaryExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (!LHS->evalType.type->codegenBinary(this, stream)) {
        LHS->codegen(instance, stream);
        stream << tokenTypeToString(op);
        RHS->codegen(instance, stream);
    }
}

// TernaryExpr

void TernaryExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    // add brackets and shit to make sure meaning 
    // doesn't change (C++ presedence might differ 
    // from Instance)
    stream << "((";
    condition->codegen(instance, stream);
    stream << ")";

    instance.getCompiler().getFormatter().newLine(stream);

    stream << " ? (";
    truthy->codegen(instance, stream);
    stream << ")";

    instance.getCompiler().getFormatter().newLine(stream);

    stream << " : (";
    falsy->codegen(instance, stream);
    stream << "))";
}

// PointerExpr

TypeCheckResult PointerExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(to);

    evalType = QualifiedType(
        instance.getCompiler().makeType<PointerType>(
            to->evalType
        )
    );

    DEBUG_LOG_TYPE();
    return Ok();
}

void PointerExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << evalType.codegenName();
}

// VariableExpr

TypeCheckResult VariableExpr::compile(Instance& instance) noexcept {
    return compileWithParams(instance, None);
}

TypeCheckResult VariableExpr::compileWithParams(
    Instance& instance, 
    Option<std::vector<Parameter>> const& args
) noexcept {
    auto var = instance.getCompiler().getEntity(name->fullName(), None, args);
    if (!var) {
        auto overloads = 
            instance.getCompiler().getEntities(
                name->fullName(), EntityType::Function
            );

        if (args && overloads.size()) {

            // mfw readable code
            THROW_COMPILE_ERR(
                "No matching overload for \"" + name->fullName() + "\" found",
                "Possible overloads: \n\t" +
                    join(
                        overloads,
                        "\n\t",
                        +[](std::shared_ptr<Entity> const& ent) {
                            return "(" + join<&Parameter::toString>(
                                std::static_pointer_cast<FunctionEntity>(
                                    ent
                                )->type.type->getParameters(),
                                ", "
                            ) + ")";
                        }
                    ),
                "Tried to match overload " + name->fullName() +
                "(" + join<&Parameter::toString>(args.value(), ", ") + ")"
            );
        }
        THROW_COMPILE_ERR(
            var.unwrapErr(),
            "",
            ""
        );
    }
    // variables and functions
    if (var.unwrap()->isValue()) {
        evalType = std::static_pointer_cast<ValueEntity>(
            var.unwrap()
        )->getValueType();
    }
    // otherwise bad identifier for variable
    else {
        THROW_COMPILE_ERR(
           "Identifier \"" + name->fullName() + "\" is not a variable, "
           "function, or class constructor",
            "",
            ""
        );
    }

    entity = var.unwrap();

    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> VariableExpr::eval(Instance& instance) {
    return std::static_pointer_cast<ValueEntity>(entity)->eval(instance);
}

void VariableExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << (entity.get() ? entity->getFullName() : name->fullName());
}

// NameExpr

void NameExpr::codegen(Instance&, std::ostream& stream) const noexcept {
    stream << name;
}

// ScopeExpr

TypeCheckResult ScopeExpr::compile(Instance& instance) noexcept {
    PUSH_NAMESPACE(name);
    GDML_TYPECHECK_CHILD(item);
    POP_NAMESPACE();

    DEBUG_LOG_TYPE();
    return Ok();
}

void ScopeExpr::codegen(Instance& com, std::ostream& stream) const noexcept {
    stream << name << "::";
    item->codegen(com, stream);
}

// TypeNameExpr

TypeCheckResult TypeNameExpr::compile(Instance& instance) noexcept {
    auto entity = instance.getCompiler().getEntity(name->fullName(), None, None);
    if (!entity || !entity.unwrap()->isType()) {
        THROW_TYPE_ERR(
            "Unknown type \"" + name->fullName() + "\"",
            "Not all C++ types are supported yet, sorry!",
            ""
        );
    }

    evalType = QualifiedType {
        entity.unwrap()->getValueType().type,
        qualifiers
    };
    DEBUG_LOG_TYPE();

    DEBUG_LOG_TYPE();
    return Ok();
}

void TypeNameExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << evalType.codegenName();
}

// MemberExpr

MemberExpr::Kind MemberExpr::tokenTypeToKind(TokenType op) {
    switch (op) {
        default:
        case TokenType::Dot: return Kind::Object;
        case TokenType::Arrow: return Kind::Pointer;
        case TokenType::OptionalArrow: return Kind::Optional;
    }
}

TypeCheckResult MemberExpr::compile(Instance& instance) noexcept {
    return compileWithParams(instance, None);
}

TypeCheckResult MemberExpr::compileWithParams(
    Instance& instance, 
    Option<std::vector<Parameter>> const& args
) noexcept {
    GDML_TYPECHECK_CHILD(object);
    GDML_TYPECHECK_CHILD(member);

    if (!object->evalType.type) {
        THROW_COMPILE_ERR(
            "Member operator used on an object without a type",
            "",
            "Probably a compiler error, sorry!"
        );
    }

    // applied on a pointer?
    if (object->evalType.type->getTypeClass() == types::TypeClass::Pointer) {
        auto asPtr = std::static_pointer_cast<PointerType>(object->evalType.type);
        auto innerType = asPtr->getInnerType().type;
        if (!innerType) {
            THROW_COMPILE_ERR(
                "Member deference operator used on a void pointer "
                "type `" + object->evalType.type->toString() + "`",
                "Cast the object into a class pointer type with `as ClassName`",
                ""
            );
        }
        if (!innerType->hasDefinition()) {
            THROW_COMPILE_ERR(
                "Member deference operator used on type `" +
                object->evalType.type->toString() + "`, which lacks "
                "a full definition",
                "This probably means your class's definition is not in "
                "scope. Make sure to include it!",
                ""
            );
        }
        if (!innerType->implementsMemberOperator()) {
            THROW_COMPILE_ERR(
                "Member dereference operator used on type `" +
                object->evalType.type->toString() + "` that does not "
                "accept member operators",
                "",
                ""
            );
        }

        objectType = object->evalType;

        evalType = innerType->typeOfMember(member->fullName(), args);
        
        if (!evalType.type) {
            THROW_TYPE_ERR(
                "Matching member for \"" + member->fullName() + "\" not found",
                "",
                ""
            );
        }
    }
    // otherwise 
    else {
        if (kind != Kind::Object) {
            THROW_COMPILE_ERR(
                "Member deference operator used on a non-pointer "
                "type `" + object->evalType.type->toString() + "`",
                "Change the " + TOKEN_STR(Arrow) + " to " + TOKEN_STR(Dot),
                ""
            );
        }
        if (!object->evalType.type->hasDefinition()) {
            THROW_COMPILE_ERR(
                "Member access operator used on type `" +
                object->evalType.type->toString() + "`, which lacks "
                "a full definition",
                "This probably means your class's definition is not in "
                "scope. Make sure to include it!",
                ""
            );
        }
        if (!object->evalType.type->implementsMemberOperator()) {
            THROW_COMPILE_ERR(
                "Member access operator used on type `" +
                object->evalType.type->toString() + "` that does not "
                "accept member operators",
                "",
                ""
            );
        }
        
        objectType = QualifiedType(
            instance.getCompiler().makeType<PointerType>(
                object->evalType
            )
        );

        evalType = object->evalType.type->typeOfMember(member->fullName(), args);

        if (!evalType.type) {
            THROW_TYPE_ERR(
                "Matching member for \"" + member->fullName() + "\" not found",
                "",
                ""
            );
        }
    }

    // unwrap ?->
    if (kind == Kind::Optional) {
        // turn into ```
        // +  auto _ = object;
        // -> _ ? _->member : nullptr
        // ```

        auto placeholder = generateIdentifierName();

        if (!parent->insertStatement(
            ast->make<VariableDeclExpr>(
                source, start, end,
                None, placeholder, member
            ),
            this, false
        )) {
            THROW_COMPILE_ERR(
                "Unable to unwrap nullish coalescing operator",
                "",
                "This is probably a compiler error"
            );
        }

        if (!parent->swap(this, ast->make<TernaryExpr>(
            source, start, end,
            ast->make<VariableExpr>(
                source, start, end,
                ast->make<NameExpr>(
                    source, start, end,
                    placeholder
                )
            ),
            ast->make<MemberExpr>(
                source, start, end,
                Kind::Pointer,
                ast->make<VariableExpr>(
                    source, start, end,
                    ast->make<NameExpr>(
                        source, start, end,
                        placeholder
                    )
                ),
                member
            ),
            ast->make<NullLiteralExpr>(
                source, start, end
            )
        ))) {
            THROW_COMPILE_ERR(
                "Unable to swap out nullish coalescing operator",
                "",
                "This is probably a compiler error"
            );
        }
    }

    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> MemberExpr::eval(Instance& instance) {
    // todo
    return nullptr;
}

void MemberExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << "(";
    object->codegen(instance, stream);
    stream << ")";
    switch (kind) {
        case Kind::Object:  stream << ".";  break;
        case Kind::Pointer: stream << "->"; break;
        case Kind::Optional:stream << "->"; break;
    }
    member->codegen(instance, stream);
}

// PointerToExpr

TypeCheckResult PointerToExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(value);

    EXPECT_TYPE(value);

    // is this dereference?
    if (deref) {
        if (value->evalType.type->getTypeClass() != types::TypeClass::Pointer) {
            THROW_TYPE_ERR(
                "Dereference operator used on a non-pointer type `" + 
                value->evalType.toString() + "`",
                "",
                ""
            );
        }
        evalType = value->evalType.into<PointerType>().type->getInnerType();
    }
    // otherwise take address of type
    else {
        evalType = QualifiedType(
            instance.getCompiler().makeType<PointerType>(value->evalType)
        );
    }

    return Ok();
}

void PointerToExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << (deref ? "*" : "&");
    stream << "(";
    value->codegen(instance, stream);
    stream << ")";
}

// NameSpaceStmt

TypeCheckResult NameSpaceStmt::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD_O(name);

    if (name) {
        instance.getCompiler().getScope().makeEntity<Namespace>(
            name.value()->fullName(),
            instance.getCompiler().getASTParser().isExtern()
        );
    }

    if (name) {
        for (auto& ns : name.value()->fullNameList()) {
            PUSH_NAMESPACE(ns);
        }
    } else {
        PUSH_NAMESPACE("");
    }
    GDML_TYPECHECK_CHILD(content);
    if (name) {
        for (auto& _ : name.value()->fullNameList()) {
            POP_NAMESPACE();
        }
    }
    
    DEBUG_LOG_TYPE_S();
    return Ok();
}

void NameSpaceStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << "namespace " << (name ? name.value()->fullName() : "") << " {";
    PUSH_INDENT();
    content->codegen(instance, stream);
    POP_INDENT();
    stream << "}";
    instance.getCompiler().getFormatter().skipSemiColon();
}

// UsingNameSpaceStmt

TypeCheckResult UsingNameSpaceStmt::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(name);

    if (!instance.getCompiler().getScope().hasEntity(
        name->fullName(), EntityType::Namespace, None, true
    )) {
        THROW_COMPILE_ERR(
            "Unknown namespace \"" + name->fullName() + "\"",
            "",
            ""
        );
    }

    instance.getCompiler().getScope().useNamespace(name->fullNameList());

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void UsingNameSpaceStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (instance.getShared().getRule(LanguageRule::KeepUsingStatements)) {
        stream << "using namespace ";
        name->codegen(instance, stream);
    }
}

// VariableDeclExpr

TypeCheckResult VariableDeclExpr::inferType(Instance& instance, bool isMember) noexcept {
    // does variable have an explicit type
    if (type.has_value()) {
        evalType = type.value()->evalType;

        // does value match
        if (value.has_value()) {
            bool doMatch = true;
            // handle constructors specially
            if (auto asCtor = dynamic_cast<ConstructExpr*>(value.value())) {
                // if no name defined, check constructor
                if (!asCtor->name) {
                    doMatch = false;

                    // type must be a class
                    if (evalType.type->getTypeClass() != types::TypeClass::Class) {
                        THROW_TYPE_ERR(
                            "Type `" + evalType.type->toString() + "` is not a "
                            "class",
                            "",
                            ""
                        );
                    }

                    // check initializer list
                    PROPAGATE_ERROR(asCtor->checkInitializer(
                        evalType.into<ClassType>().type->getEntity(),
                        instance
                    ));
                }
            }
            // match type
            if (doMatch && !evalType.convertibleTo(value.value()->evalType)) {
                THROW_TYPE_ERR(
                    "Declared type `" + evalType.toString() + 
                    "` does not match inferred type `" + 
                    value.value()->evalType.toString() + 
                    "` of value",
                    "",
                    ""
                );
            }
        }
    }
    // otherwise type is just inferred from value
    else {
        if (value.has_value()) {
            // initializer list must have a type
            if (auto asCtor = dynamic_cast<ConstructExpr*>(value.value())) {
                if (!asCtor->name) {
                    THROW_TYPE_ERR(
                        "No type for initializer list provided or "
                        "inferrable",
                        "Annotate which class you are instantiating "
                        "by adding the class name before the initializer "
                        "list",
                        ""
                    );
                }
            }
            evalType = value.value()->evalType;
        }
        // class members must have types
        else if (isMember) {
            THROW_TYPE_ERR(
                "Member variable must have a type",
                "Either add a default value to infer type from "
                "or explicitly delcare a type",
                ""
            );
        }
    }
    // no void >:(
    if (typeIsVoid(evalType.type)) {
        THROW_TYPE_ERR(
            "Attempted to initialize a variable with `void` type, "
            "which is not possible",
            "",
            ""
        );
    }
    DEBUG_LOG_TYPE();
    return Ok();
}

TypeCheckResult VariableDeclExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD_O(type);
    GDML_TYPECHECK_CHILD_O(value);

    PROPAGATE_ERROR(inferType(instance, false));

    if (instance.getCompiler().hasEntity<Variable>(name, None, false, false)) {
        THROW_COMPILE_ERR(
            "Variable named \"" + name + "\" already exists "
            "in this scope",
            "",
            ""
        );
    }
    variable = instance.getCompiler().getScope().makeEntity<Variable>(
        name, evalType, nullptr, this,
        instance.getCompiler().getASTParser().isExtern()
    );
    
    DEBUG_LOG_TYPE();
    return Ok();
}

TypeCheckResult VariableDeclExpr::compile(
    std::shared_ptr<Class> entity,
    Instance& instance
) noexcept {
    GDML_TYPECHECK_CHILD_O(type);
    GDML_TYPECHECK_CHILD_O(value);

    // this
    if (name == tokenTypeToString(TokenType::This)) {
        evalType = QualifiedType(
            instance.getCompiler().makeType<PointerType>(
                QualifiedType(
                    entity->getClassType(),
                    // todo: figure out const-qualification
                    types::CONST_QUALIFIED
                )
            )
        );
    }

    PROPAGATE_ERROR(inferType(instance, false));

    if (instance.getCompiler().hasEntity<Variable>(name, None, false, false)) {
        THROW_COMPILE_ERR(
            "Variable named \"" + name + "\" already exists "
            "in this scope",
            "",
            ""
        );
    }
    variable = instance.getCompiler().getScope().makeEntity<Variable>(
        name, evalType, nullptr, this,
        instance.getCompiler().getASTParser().isExtern()
    );
    
    DEBUG_LOG_TYPE();
    return Ok();
}

TypeCheckResult VariableDeclExpr::compileAsMember(
    std::shared_ptr<Class> classEntity,
    Instance& instance
) noexcept {
    GDML_TYPECHECK_CHILD_O(type);
    GDML_TYPECHECK_CHILD_O(value);

    PROPAGATE_ERROR(inferType(instance, true));

    if (classEntity->hasMember(name, false)) {
        THROW_COMPILE_ERR(
            "Class already has a member named \"" + name + "\"",
            "",
            ""
        );
    }
    variable = classEntity->makeMember<Variable>(
        name, evalType, nullptr, this,
        instance.getCompiler().getASTParser().isExtern()
    );

    
    DEBUG_LOG_TYPE();
    return Ok();
}

std::shared_ptr<Value> VariableDeclExpr::eval(Instance& instance) {    
    if (value.value()) {
        variable->value = value.value()->eval(instance);
    }
    return variable->value;
}

void VariableDeclExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (evalType.type) {
        stream << evalType.codegenName() << " ";
    } else {
        stream << "auto ";
    }
    stream << name;
    if (value.has_value()) {
        stream << " = ";
        if (evalType.type) {
            evalType.codegenConvert(
                value.value()->evalType, value.value(), stream
            );
        } else {
            value.value()->codegen(instance, stream);
        }
    }
}

// CastTypeExpr

TypeCheckResult CastTypeExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(target);
    GDML_TYPECHECK_CHILD(intoType);

    if (!target->evalType.castableTo(intoType->evalType)) {
        THROW_TYPE_ERR(
            "Type `" + target->evalType.toString() + "` does "
            "not have an implemented cast operator to type `" + 
            intoType->evalType.toString() + "`",

            "Implement a cast operator: `impl " + 
            target->evalType.type->toString() + " as " +
            intoType->evalType.type->toString() + " { /* ... */ }`",

            "Const-qualified values can not be casted to non-const-"
            "qualified values"
        );
    }

    evalType = intoType->evalType;

    DEBUG_LOG_TYPE();
    return Ok();
}

void CastTypeExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    target->evalType.type->codegenCast(
        intoType->evalType.type,
        target,
        stream
    );
}

// FunctionTypeExpr

TypeCheckResult FunctionTypeExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILDREN(parameters);
    GDML_TYPECHECK_CHILD_O(returnType);

    QualifiedType evalRetType {};
    if (returnType.has_value()) {
        evalRetType = returnType.value()->evalType;
    }

    std::vector<Parameter> evalParamTypes {};
    for (auto& param : parameters) {
        evalParamTypes.emplace_back(param);
    }

    evalType = QualifiedType {
        instance.getCompiler().makeType<FunctionType>(
            evalRetType, evalParamTypes
        ),
        qualifiers
    };

    DEBUG_LOG_TYPE();

    DEBUG_LOG_TYPE();
    return Ok();
}

TypeCheckResult FunctionTypeExpr::compileAsMember(
    std::shared_ptr<Class> classEntity,
    Instance& instance
) noexcept {
    for (auto& param : parameters) {
        PROPAGATE_ERROR(param->compile(classEntity, instance));
    }

    GDML_TYPECHECK_CHILD_O(returnType);

    QualifiedType evalRetType {};
    if (returnType.has_value()) {
        evalRetType = returnType.value()->evalType;
    }

    auto funType = FunctionType::Normal;
    std::vector<Parameter> evalParamTypes {};
    for (auto& param : parameters) {
        evalParamTypes.push_back(param);
        if (param->name == "this") {
            funType = FunctionType::Member;
        }
    }

    evalType = QualifiedType {
        instance.getCompiler().makeType<FunctionType>(
            evalRetType, evalParamTypes, funType
        ),
        qualifiers
    };

    DEBUG_LOG_TYPE();

    DEBUG_LOG_TYPE();
    return Ok();
}

// FunctionDeclStmt

TypeCheckResult FunctionDeclStmt::inferReturnType(Instance& instance) {
    // todo: better return type check and make sure all branches return
    // return type inference & typecheck
    if (body.has_value()) {
        auto infer = body.value()->inferBranchReturnType(instance);
        if (!infer) return infer.unwrapErr();
        
        auto explicitType = entity->type.type->getReturnType();

        auto inferredType = infer.unwrap();
        if (inferredType.has_value()) {
            // check if inferred matches explicit type
            if (
                explicitType.type &&
                !inferredType.value().convertibleTo(explicitType)
            ) {
                THROW_TYPE_ERR(
                    "Inferred return type `" + inferredType.value().toString() + "` "
                    "does not match explicitly specified return type `" + explicitType.toString() + 
                    "`",
                    "",
                    ""
                );
            }
            // only replace if explicit type is not provided
            if (!explicitType.type) {
                entity->type.type->setReturnType(inferredType.value());
            }
        } else {
            if (!explicitType.type) {
                entity->type.type->setReturnType(QualifiedType(
                    instance.getCompiler().getBuiltInType(types::DataType::Void)
                ));
            }
        }
    }
    DEBUG_LOG_TYPE_S();
    return Ok();
}

TypeCheckResult FunctionDeclStmt::compile(Instance& instance) noexcept {
    PUSH_SCOPE();

    GDML_TYPECHECK_CHILD(type);
    GDML_TYPECHECK_CHILD(name);

    auto funType = type->evalType.into<FunctionType>();

    if (instance.getCompiler().getScope(1).hasEntity(
        name->fullName(),
        EntityType::Function,
        funType.type->getParameters(),
        false
    )) {
        THROW_COMPILE_ERR(
            "Function named \"" + name->fullName() + "\" with "
            "these parameters already exists in this scope",
            "",
            ""
        );
    }

    entity = instance.getCompiler().getScope(1).makeEntity<FunctionEntity>(
        name->fullName(), funType, this,
        instance.getCompiler().getASTParser().isExtern()
    );

    GDML_TYPECHECK_CHILD_O(body);
    POP_SCOPE();

    PROPAGATE_ERROR(inferReturnType(instance));
    
    DEBUG_LOG_TYPE_S();
    return Ok();
}

TypeCheckResult FunctionDeclStmt::compileAsMember(
    std::shared_ptr<Class> classEntity,
    Instance& instance
) noexcept {
    PUSH_SCOPE();

    parentClass = classEntity;

    PROPAGATE_ERROR(type->compileAsMember(
        classEntity,
        instance
    ));
    GDML_TYPECHECK_CHILD(name);

    auto funType = type->evalType.into<FunctionType>();

    if (classEntity->hasMemberFunction(
        name->fullName(),
        funType.type->getParameters(),
        false
    )) {
        THROW_COMPILE_ERR(
            "Member function named \"" + name->fullName() + "\" with "
            "these parameters already exists in this class",
            "",
            ""
        );
    }
    entity = classEntity->makeMember<FunctionEntity>(
        name->fullName(), funType, this,
        instance.getCompiler().getASTParser().isExtern()
    );

    GDML_TYPECHECK_CHILD_O(body);
    POP_SCOPE();

    PROPAGATE_ERROR(inferReturnType(instance));
    
    DEBUG_LOG_TYPE_S();
    return Ok();
}

void FunctionDeclStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    // get evaluated funtion type
    auto funType = static_cast<FunctionType*>(type->evalType.type.get());

    // 'static' specifier
    if (parentClass.get()) {
        // todo: private functions and stuff
        stream << "public: ";
        // static member functions don't 
        // have a 'this' parameter
        bool isStatic = true;
        for (auto& param : type->parameters) {
            if (param->name == tokenTypeToString(TokenType::This)) {
                isStatic = false;
            }            
        }
        if (isStatic) {
            stream << "static ";
        }
    } else {
        // default static for file functions
        if (
            instance.getShared().getRule(LanguageRule::DefaultStaticFunctions) &&
            !name->isScoped()
        ) {
            stream << "static ";
        }
    }

    // otherwise return type as normal
    if (funType->getReturnType().type) {
        stream << funType->getReturnType().codegenName() << " ";
    } else {
        stream << "auto ";
    }
    name->codegen(instance, stream);

    stream << "(";
    bool firstArg = true;
    for (auto& param : type->parameters) {
        // do not codegen explicit this
        if (param->name == "this") continue;
        if (!firstArg) {
            stream << ", ";
        }
        firstArg = false;
        param->codegen(instance, stream);
    }
    stream << ")";
    if (body.has_value()) {
        stream << " {";
        PUSH_INDENT();
        body.value()->codegen(instance, stream);
        POP_INDENT();
        stream << "}";
        instance.getCompiler().getFormatter().skipSemiColon();
    }
}

// CallExpr

TypeCheckResult CallExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILDREN(args);

    Option<MemberExpr*> memberContext = None;

    // if the target is a VariableExpr, handle it specially to 
    // account for overloads
    if (auto asVar = dynamic_cast<VariableExpr*>(target)) {

        std::vector<Parameter> argTypes;
        for (auto& arg : args) {
            argTypes.push_back(arg->evalType);
        }
        PROPAGATE_ERROR(asVar->compileWithParams(instance, argTypes));

    }
    // if the target is a MemberExpr, handle it specially to 
    // pass in 'this' context
    else if (auto asMem = dynamic_cast<MemberExpr*>(target)) {

        std::vector<Parameter> argTypes;
        for (auto& arg : args) {
            argTypes.push_back(arg->evalType);
        }
        PROPAGATE_ERROR(asMem->compileWithParams(instance, argTypes));

        memberContext = asMem;

    }
    // otherwise handle normally
    else {
        GDML_TYPECHECK_CHILD(target);
    }

    std::shared_ptr<FunctionType> targetType = nullptr;

    // todo: support operator()

    EXPECT_TYPE(target);

    // functions
    if (target->evalType.type->getTypeClass() == types::TypeClass::Function) {
        targetType = std::static_pointer_cast<FunctionType>(target->evalType.type);
    }
    // otherwise not callable
    else {
        THROW_TYPE_ERR(
            "Attempted to call an expression that did not "
            "evaluate to a function type",
            "",
            ""
        );
    }

    // normal functions
    if (targetType->getFunType() == FunctionType::Normal) {
        if (targetType->getParameters().size() != args.size()) {
            THROW_TYPE_ERR(
                "Function requires " + std::to_string(targetType->getParameters().size()) + 
                " parameters, but only " + std::to_string(args.size()) + " were provided",
                "",
                ""
            );
        }
    }
    // member functions have 'this' passed differently
    else {
        if (targetType->getParameters().size() - 1 != args.size()) {
            THROW_TYPE_ERR(
                "Function requires " + std::to_string(targetType->getParameters().size() - 1) + 
                " parameters, but only " + std::to_string(args.size()) + " were provided",
                "",
                ""
            );
        }
        if (!memberContext) {
            THROW_TYPE_ERR(
                "Non-static member function called without a 'this' parameter",
                "Apply this function call to some class instance, i.e. `inst.fun()`",
                ""
            );
        }
        args.insert(args.begin(), memberContext.value()->object);
    }

    size_t i = 0;
    for (auto& arg : args) {
        auto argType = arg->evalType;
        // dumb way to coerce 'this' arg type to always be a pointer
        if (targetType->getFunType() == FunctionType::Member && !i) {
            argType = memberContext.value()->objectType;
        }
        auto targetParam = targetType->getParameters().at(i).type;
        if (!argType.convertibleTo(targetParam)) {
            THROW_TYPE_ERR_AT(
                "Argument of type `" + argType.toString() + "` cannot "
                "be passed to parameter of type `" + targetParam.toString() + "`",

                "Add an explicit type conversion on the argument: "
                "`as " + targetParam.toString() + "`",

                "There are no implicit conversions in GDML. All types "
                "must match exactly!",

                arg->start, arg->end
            );
        }
        i++;
    }

    evalType = targetType->getReturnType();

    DEBUG_LOG_TYPE();

    DEBUG_LOG_TYPE();
    return Ok();
}

void CallExpr::codegen(Instance& com, std::ostream& stream) const noexcept {
    stream << "(";
    target->codegen(com, stream);
    stream << ")(";
    bool firstArg = true;

    auto targetType = std::static_pointer_cast<FunctionType>(target->evalType.type);

    size_t i = 0;
    for (auto const& arg : args) {
        // skip 'this' parameter
        if (targetType->getFunType() == FunctionType::Member && !i) {
            i++;
            continue;
        }
        if (!firstArg) {
            stream << ", ";
        }
        firstArg = false;
        arg->evalType.codegenConvert(
            targetType->getParameters().at(i).type, arg, stream
        );
        i++;
    }
    stream << ")";
}

// ConstructorDeclStmt

TypeCheckResult ConstructorDeclStmt::compile(Instance&) noexcept {
    THROW_COMPILE_ERR(
        "Internal compiler error: " __FUNCTION__ " called, but "
        "the specific overload should've been used instead.",
        "",
        ""
    );
}

TypeCheckResult ConstructorDeclStmt::compile(
    std::shared_ptr<Class> classEntity, 
    Instance& instance
) noexcept {
    PUSH_SCOPE();

    parentClass = classEntity;

    GDML_TYPECHECK_CHILDREN(parameters);
    std::vector<Parameter> evalParamTypes {};
    for (auto& param : parameters) {
        evalParamTypes.emplace_back(param);
    }

    auto funName = isDestructor ? DESTRUCTOR_FUN_NAME : CONSTRUCTOR_FUN_NAME;
    if (classEntity->hasMemberFunction(funName, evalParamTypes, false)) {
        THROW_TYPE_ERR(
            "Class already has a constructor with these parameters",
            "",
            ""
        );
    }

    auto funType = instance.getCompiler().makeType<FunctionType>(
        QualifiedType(classEntity->getClassTypePointer()), evalParamTypes
    );
    entity = classEntity->makeMember<FunctionEntity>(
        funName, funType, this,
        instance.getCompiler().getASTParser().isExtern()
    );

    instance.getCompiler().getScope().makeEntity<Variable>(
        "this", QualifiedType(classEntity->getClassTypePointer()),
        nullptr, nullptr,
        instance.getCompiler().getASTParser().isExtern()
    );

    GDML_TYPECHECK_CHILD_O(body);
    POP_SCOPE();

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void ConstructorDeclStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << "public: ";
    if (isDestructor) {
        stream << "~";
    }
    stream << parentClass->getName() << "(";
    bool firstArg = true;
    for (auto& param : parameters) {
        if (!firstArg) {
            stream << ", ";
        }
        firstArg = false;
        param->codegen(instance, stream);
    }
    stream << ")";
    if (body) {
        stream << " {";
        PUSH_INDENT();
        body.value()->codegen(instance, stream);
        POP_INDENT();
        stream << "}";
        instance.getCompiler().getFormatter().skipSemiColon();
    }
}

// ConstructExpr

TypeCheckResult ConstructExpr::checkInitializer(
    std::shared_ptr<Class> entity,
    Instance& instance
) noexcept {
    classEntity = entity;

    std::vector<Parameter> params;
    for (auto& [pName, pValue] : namedValues) {
        params.emplace_back(pName, pValue->evalType);
    }
    funEntity = entity->getMemberFunction(CONSTRUCTOR_FUN_NAME, params, true);
    if (!funEntity) {
        THROW_TYPE_ERR(
            "No matching constructor for `" + entity->getFullName() + "` "
            "found",
            "",
            ""
        );
    }
    return Ok();
}

TypeCheckResult ConstructExpr::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD_O(name);
    GDML_TYPECHECK_CHILDREN(values);
    GDML_TYPECHECK_CHILDREN_M(namedValues);

    if (name) {
        auto cls = instance.getCompiler().getEntity<Class>(name.value()->fullName());
        if (!cls) {
            THROW_TYPE_ERR(
                "Unknown class \"" + name.value()->fullName() + "\"",
                "",
                ""
            );
        }
        // check initializer list
        PROPAGATE_ERROR(checkInitializer(cls.unwrap(), instance));
        evalType = isNew ?
            QualifiedType(cls.unwrap()->getClassTypePointer()) :
            QualifiedType(cls.unwrap()->getClassType());
    }

    DEBUG_LOG_TYPE();
    return Ok();
}

void ConstructExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    std::vector<ValueExpr*> paramsOrdered = values;

    EXPECT_VARIABLE_CG(classEntity);
    EXPECT_VARIABLE_CG(funEntity);

    // order parameters for C++
    for (auto& param : funEntity->type.type->getParameters()) {
        // this should not happen
        if (!param.name) {
            stream << "something_has_gone_very_wrong_inside_the_gdml_compiler";
            continue;
        }
        for (auto& [argName, arg] : namedValues) {
            if (argName == param.name.value()) {
                paramsOrdered.push_back(arg);
            }
        }
    }

    stream << classEntity->getFullName() << "(";
    PUSH_INDENT();

    bool firstArg = true;
    for (auto& param : paramsOrdered) {
        if (!firstArg) {
            stream << ",";
            NEW_LINE();
        }
        firstArg = false;
        param->codegen(instance, stream);
    }

    POP_INDENT();
    stream << ")";
}

// BlockStmt

BranchInferResult BlockStmt::inferBranchReturnType(Instance& instance) {
    return body->inferBranchReturnType(instance);
}

TypeCheckResult BlockStmt::compile(Instance& instance) noexcept {
    PUSH_SCOPE();
    GDML_TYPECHECK_CHILD(body);
    POP_SCOPE();

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void BlockStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << "{";
    PUSH_INDENT();
    body->codegen(instance, stream);
    POP_INDENT();
    stream << "}";
}

// ImportStmt

void ImportStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (path.extension() != ".gdml") {
        if (!isRelative) {
            stream << "#include <" << path.string() << ">";
            NEW_LINE();
        }
        else if (path.is_absolute()) {
            stream << "#include \"" << path.string() << "\"";
            NEW_LINE();
        }
        else {
            stream << "#include \""
                << instance.getSource()->path.parent_path().string()
                << "/" << path.string() << "\"";
            NEW_LINE();
        }
    }
    instance.getCompiler().getFormatter().skipSemiColon();
}

// IfStmt

BranchInferResult IfStmt::inferBranchReturnType(Instance& instance) {
    Option<QualifiedType> ifInfer = None;
    PROPAGATE_ASSIGN(ifInfer, branch->inferBranchReturnType(instance));

    if (elseBranch.has_value()) {
        Option<QualifiedType> elseInfer = None;
        PROPAGATE_ASSIGN(elseInfer, elseBranch.value()->inferBranchReturnType(instance));

        if (!matchBranchTypes(ifInfer, elseInfer)) {
            // matchBranchTypes only fails if both branches 
            // actually have a return type
            THROW_TYPE_ERR(
                "Branches have incompatible return types; "
                "If branch returns `" + ifInfer.value().toString() + 
                "` but else branch returns `" + elseInfer.value().toString() + "`",
                "",
                ""
            );
        }
    }

    return ifInfer;
}

TypeCheckResult IfStmt::compile(Instance& instance) noexcept {
    PUSH_SCOPE();
    GDML_TYPECHECK_CHILD_O(condition);
    POP_SCOPE();

    PUSH_SCOPE();
    GDML_TYPECHECK_CHILD_O(elseBranch);
    POP_SCOPE();

    if (condition && !condition.value()->evalType.convertibleTo(
        QualifiedType {
            instance.getCompiler().getBuiltInType(types::DataType::Bool)
        }
    )) {
        THROW_TYPE_ERR_AT(
            "Condition for if statement does not evaluate to `bool`, "
            "but instead evaluates to `" + condition.value()->evalType.toString() +
            "`",
            "",
            "",
            condition.value()->start, condition.value()->end
        );
    }

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void IfStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (condition.has_value()) {
        stream << "if (";
        if (!condition.value()->evalType.codegenConvert(
            QualifiedType {
                instance.getCompiler().getBuiltInType(types::DataType::Bool)
            }, condition.value(), stream
        )) {
            condition.value()->codegen(instance, stream);
        }
        stream << ") ";
    }
    stream << "{";
    PUSH_INDENT();
    branch->codegen(instance, stream);
    POP_INDENT();
    stream << "}";
    if (elseBranch.has_value()) {
        stream << " else ";
        elseBranch.value()->codegen(instance, stream);
    }
    instance.getCompiler().getFormatter().skipSemiColon();
}

// StmtList

BranchInferResult StmtList::inferBranchReturnType(Instance& instance) {
    Option<QualifiedType> value = None;
    for (auto& stmt : statements) {
        PROPAGATE_ASSIGN(value, stmt->inferBranchReturnType(instance));
    }
    return value;
}

void StmtList::codegen(Instance& instance, std::ostream& stream) const noexcept {
    bool first = true;
    for (auto const& stmt : statements) {
        if (!first) {
            NEW_LINE();
            NEW_LINE();
        }
        first = false;

        stmt->codegen(instance, stream);
        instance.getCompiler().getFormatter().semiColon(stream);
    }
}

Stmt* StmtList::getParentStatement(Stmt* current) {
    return current;
}

bool StmtList::insertStatement(
    Stmt* toAdd,
    Option<Stmt*> const& relative,
    bool after
) {
    if (relative) {
        bool added = false;
        for (auto it = statements.begin(); it != statements.end(); it++) {
            if (*it == relative.value()) {
                if (after) {
                    statements.insert(it + 1, toAdd);
                } else {
                    statements.insert(it, toAdd);
                }
                added = true;
                break;
            }
        }
        if (!added) {
            statements.push_back(toAdd);
        }
    } else {
        statements.push_back(toAdd);
    }
    toAdd->parent = this;
    return true;
}

bool StmtList::removeStatement(Stmt* stmt) {
    auto rem = std::ranges::remove(statements, stmt);
    statements.erase(rem.begin(), rem.end());
    stmt->parent = nullptr;
    return true;
}

bool StmtList::swap(Stmt* stmt, Stmt* to) {
    GDML_SWAP_CHILDREN(statements);
    return false;
}

// ClassFwdDeclStmt

TypeCheckResult ClassFwdDeclStmt::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(name);
    
    if (!instance.getCompiler().getScope().hasEntity(
        name->fullName(), EntityType::Class, None, false
    )) {
        instance.getCompiler().getScope().makeEntity<Class>(
            name->fullName(), 
            instance.getCompiler().makeType<ClassType>(name->fullName()),
            instance.getCompiler().getASTParser().isExtern()
        );
    }

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void ClassFwdDeclStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (isStruct) {
        stream << "struct ";
    } else {
        stream << "class ";
    }
    name->codegen(instance, stream);
}

// ClassDeclStmt

TypeCheckResult ClassDeclStmt::compile(Instance& instance) noexcept {
    GDML_TYPECHECK_CHILD(name);

    auto findEntity = instance.getCompiler().getEntity<Class>(name->fullName(), None);
    if (findEntity) {
        entity = findEntity.unwrap();
        if (entity->isComplete()) {
            THROW_COMPILE_ERR(
                "Class \"" + name->fullName() + "\" is already "
                "fully defined",
                "",
                ""
            );
        }
        entity->markComplete();
    } else {
        entity = instance.getCompiler().getScope().makeEntity<Class>(
            name->fullName(), 
            instance.getCompiler().makeType<ClassType>(name->fullName()),
            instance.getCompiler().getASTParser().isExtern()
        );
    }

    for (auto& member : members) {
        PROPAGATE_ERROR(member->compileAsMember(entity, instance));
    }

    for (auto& function : functions) {
        PROPAGATE_ERROR(function->compileAsMember(entity, instance));
    }

    for (auto& constructor : constructors) {
        PROPAGATE_ERROR(constructor->compile(entity, instance));
    }

    DEBUG_LOG_TYPE_S();
    return Ok();
}

void ClassDeclStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (isStruct) {
        stream << "struct ";
    } else {
        stream << "class ";
    }
    name->codegen(instance, stream);
    stream << " {";
    PUSH_INDENT();
    for (auto const& member : members) {
        member->codegen(instance, stream);
        instance.getCompiler().getFormatter().semiColon(stream);
        NEW_LINE();
    }
    for (auto const& constructor : constructors) {
        NEW_LINE();
        constructor->codegen(instance, stream);
        instance.getCompiler().getFormatter().semiColon(stream);
    }
    for (auto const& function : functions) {
        NEW_LINE();
        function->codegen(instance, stream);
        instance.getCompiler().getFormatter().semiColon(stream);
    }
    POP_INDENT();
    stream << "}";
}

// ReturnStmt

BranchInferResult ReturnStmt::inferBranchReturnType(Instance& instance) {
    return Option<QualifiedType>(value->evalType);
}

// ExternStmt

TypeCheckResult ExternStmt::compile(Instance& instance) noexcept {
    instance.getCompiler().getASTParser().pushExtern();
    GDML_TYPECHECK_CHILD(content);
    instance.getCompiler().getASTParser().popExtern();

    return Ok();
}

void ExternStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {}

// EmbedCodeStmt

void EmbedCodeStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << data;
}

// DebugStmt

TypeCheckResult DebugStmt::compile(Instance& instance) noexcept {
    instance.getCompiler().dumpScopes();
    return Ok();
}

void DebugStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    instance.getCompiler().getFormatter().skipSemiColon();
}

// AST

AST::AST(
    SourceFile const* src,
    Position const& start,
    Position const& end
) : StmtList(src, start, end, {}) {}

AST::~AST() {
    for (auto& g : m_garbage) {
        delete g;
    }
}


