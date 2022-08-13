#include "AST.hpp"
#include <compiler/GDML.hpp>
#include <compiler/Compiler.hpp>
#include <compiler/Instance.hpp>

using namespace gdml;
using namespace gdml::ast;

#define THROW_TYPE_ERR(msg, hint, note) \
    return LineError { Error::TypeError,\
        msg, hint, note, start, end, source }

#define PUSH_INDENT() \
    instance.getCompiler().getFormatter().pushIndent(); \
    instance.getCompiler().getFormatter().newline(stream)

#define POP_INDENT() \
    instance.getCompiler().getFormatter().popIndent(); \
    instance.getCompiler().getFormatter().newline(stream)

#define NEW_LINE() \
    instance.getCompiler().getFormatter().newline(stream)

void BinaryExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    LHS->codegen(instance, stream);
    if (instance.getShared().getFlag(Flags::PrettifyOutput)) stream << " ";
    stream << tokenTypeToString(op);
    if (instance.getShared().getFlag(Flags::PrettifyOutput)) stream << " ";
    RHS->codegen(instance, stream);
}


void TernaryExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    // add brackets and shit to make sure meaning 
    // doesn't change (C++ presedence might differ 
    // from Instance)
    stream << "((";
    condition->codegen(instance, stream);
    stream << ")";

    instance.getCompiler().getFormatter().newline(stream);

    stream << " ? (";
    truthy->codegen(instance, stream);
    stream << ")";

    instance.getCompiler().getFormatter().newline(stream);

    stream << " : (";
    falsy->codegen(instance, stream);
    stream << "))";
}


void PointerExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    to->codegen(instance, stream);
    if (isReference) {
        stream << "&";
    } else {
        stream << "*";
    }
    if (qualifiers.isConst) {
        if (instance.getShared().getFlag(Flags::PrettifyOutput)) stream << " ";
        stream << "const";
    }
}

TypeCheckResult PointerExpr::compile(Instance& instance) const noexcept {
    GDML_TYPECHECK_CHILD(to);
    return Ok();
}


TypeCheckResult ScopeExpr::compile(Instance& instance) const noexcept {
    instance.getCompiler().pushScope(name);
    GDML_TYPECHECK_CHILD(item);
    instance.getCompiler().popScope(name);
    return Ok();
}


TypeCheckResult TypeNameExpr::compile(Instance& instance) const noexcept {
    if (!instance.getCompiler().typeExists(name->fullName())) {
        THROW_TYPE_ERR(
            "Unknown type \"" + name->fullName() + "\"",
            "Not all C++ types are supported yet, sorry!",
            ""
        );
    }
    return Ok();
}

void TypeNameExpr::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << instance.getCompiler().getType(name->fullName())->getCodegenName();
    if (qualifiers.isConst) {
        stream << " const";
    }
}


TypeCheckResult FunctionDeclStmt::compile(Instance& instance) const noexcept {
    GDML_TYPECHECK_CHILD(type);
    GDML_TYPECHECK_CHILD(name);
    GDML_TYPECHECK_CHILD_O(body);
    // todo: figure out if function implementation exists
    return Ok();
}

void FunctionDeclStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    // FunctionTypeExpr::codegen returns a function pointer
    // type declaration, so can't use that
    
    // default static for file functions
    if (
        instance.getShared().getRule(LanguageRule::DefaultStaticFunctions) &&
        !name->isScoped()
    ) {
        stream << "static ";
    }

    if (type->returnType.has_value()) {
        type->returnType.value()->codegen(instance, stream);
        stream << " ";
    } else {
        stream << "auto ";
    }
    name->codegen(instance, stream);
    stream << "(";
    bool firstArg = true;
    for (auto& param : type->parameters) {
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
    }
}

void BlockStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    stream << "{";
    PUSH_INDENT();
    body->codegen(instance, stream);
    POP_INDENT();
    stream << "}";
}

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
}

void IfStmt::codegen(Instance& instance, std::ostream& stream) const noexcept {
    if (condition.has_value()) {
        stream << "if (";
        condition.value()->codegen(instance, stream);
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
        stream << ";";
    }
}

void AST::codegen(Instance& instance, std::ostream& stream) const noexcept {
    bool first = true;
    for (auto& stmt : m_tree) {
        if (!first) {
            NEW_LINE();
            NEW_LINE();
        }
        first = false;

        stmt->codegen(instance, stream);
        stream << ";";
    }
    NEW_LINE();
}
