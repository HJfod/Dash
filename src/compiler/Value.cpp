#include "Compiler.hpp"
#include "Value.hpp"
#include <parser/AST.hpp>

using namespace gdml;

Value::Value(Compiler& compiler) : m_compiler(compiler) {}

Variable::Variable(
    QualifiedType const& type,
    Value* value,
    ast::VariableDeclExpr* decl
) : type(type), value(value), declaration(decl) {}

Value* FunctionEntity::eval(Instance& instance) {
    if (!declaration->body.has_value()) {
        return nullptr;
    }
    return declaration->body.value()->eval(instance);
}

FunctionEntity::FunctionEntity(
    QualifiedFunType const& type,
    ast::FunctionDeclStmt* decl
) : type(type), declaration(decl) {}

