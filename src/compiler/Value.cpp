#include "Compiler.hpp"
#include "Value.hpp"
#include <parser/AST.hpp>

using namespace gdml;

Value::Value(Compiler& compiler) : m_compiler(compiler) {}

ABuiltInValue::ABuiltInValue(Compiler& compiler) : Value(compiler) {}

StringValue::StringValue(
    Compiler& compiler,
    std::string const& value
) : ABuiltInValue(compiler), m_value(value) {}

std::shared_ptr<Value> StringValue::copy() {
    return m_compiler.makeValue<StringValue>(m_value);
}

std::string const& StringValue::getValue() const {
    return m_value;
}

void StringValue::setValue(std::string const& value) {
    m_value = value;
}

std::shared_ptr<Value> StringValue::applyUnary(
    TokenType op,
    bool prefix
) {
    // todo
    return nullptr;
}

std::shared_ptr<Value> StringValue::applyBinary(
    TokenType op, 
    std::shared_ptr<ABuiltInValue> other
) {
    // todo
    return nullptr;
}
