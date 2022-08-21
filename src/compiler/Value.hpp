#pragma once

#include <utils/Types.hpp>
#include "Type.hpp"

namespace gdml {
    class Value {
    protected:
        Compiler& m_compiler;

    public:
        Value(Compiler& compiler);

        virtual std::shared_ptr<Value> copy() = 0;
        virtual ~Value() = default;
    };

    class PointerValue : public Value {
    protected:
        std::shared_ptr<Value> m_value;
    
    public:
        PointerValue(Compiler& compiler, std::shared_ptr<Value> value);

        std::shared_ptr<Value> copy() override;

        std::shared_ptr<Value> getValue() const;
        void setValue(std::shared_ptr<Value> value);
    };

    class ABuiltInValue : public Value {
    public:
        ABuiltInValue(Compiler& compiler);

        virtual std::shared_ptr<Value> applyUnary(
            TokenType op,
            bool prefix
        ) = 0;
        virtual std::shared_ptr<Value> applyBinary(
            TokenType op, 
            std::shared_ptr<ABuiltInValue> other
        ) = 0;
    };

    class StringValue :
        public ABuiltInValue,
        public std::enable_shared_from_this<StringValue>
    {
    protected:
        types::String m_value;

    public:
        StringValue(
            Compiler& compiler,
            std::string const& value
        );

        std::shared_ptr<Value> copy() override;

        std::string const& getValue() const;
        void setValue(std::string const& value);

        std::shared_ptr<Value> applyUnary(
            TokenType op,
            bool prefix
        ) override;
        std::shared_ptr<Value> applyBinary(
            TokenType op, 
            std::shared_ptr<ABuiltInValue> other
        ) override;
    };

    template<class T>
    class NumeralValue :
        public ABuiltInValue,
        public std::enable_shared_from_this<NumeralValue<T>>
    {
    protected:
        T m_value;
    
    public:
        NumeralValue(
            Compiler& compiler,
            T const& value
        ) : ABuiltInValue(compiler),
            m_value(value) {}

        std::shared_ptr<Value> copy() override {
            return m_compiler.makeValue<NumeralValue<T>>(m_value);
        }
        std::shared_ptr<Value> makeCopy(T value) {
            return m_compiler.makeValue<NumeralValue<T>>(value);
        }
        T getValue() const {
            return m_value;
        }
        void setValue(T const& value) {
            m_value = value;
        }

        std::shared_ptr<Value> applyUnary(TokenType op, bool prefix) override {
            switch (op) {
                case TokenType::Add: {
                    return makeCopy(+m_value);
                } break;
                
                case TokenType::Sub: {
                    if constexpr (
                        !std::is_same_v<T, types::Bool> &&
                        !std::is_unsigned_v<T>
                    ) {
                        return makeCopy(-m_value);
                    } else {
                        return copy();
                    }
                } break;
                
                case TokenType::Increment: {
                    if constexpr (!std::is_same_v<T, types::Bool>) {
                        if (prefix) {
                            return makeCopy(++m_value);
                        } else {
                            m_value++;
                            return copy();
                        }
                    } else {
                        return copy();
                    }
                } break;
                
                case TokenType::Decrement: {
                    if constexpr (!std::is_same_v<T, types::Bool>) {
                        if (prefix) {
                            return makeCopy(--m_value);
                        } else {
                            m_value--;
                            return copy();
                        }
                    } else {
                        return copy();
                    }
                } break;

                case TokenType::Not: {
                    return m_compiler.makeValue<NumeralValue<types::Bool>>(!m_value);
                } break;

                case TokenType::BitNot: {
                    if constexpr (
                        !std::is_same_v<T, types::Bool> &&
                        !std::is_floating_point_v<T>
                    ) {
                        return makeCopy(~m_value);
                    } else {
                        return copy();
                    }
                } break;

                default: break;
            }
            return nullptr;
        }

        std::shared_ptr<Value> applyBinary(
            TokenType op, 
            std::shared_ptr<ABuiltInValue> other
        ) override {
            // strict type system requires other value 
            // to be the same type
            auto otherValue = std::static_pointer_cast<NumeralValue<T>>(other)->getValue();
            
            #define GDML_DEF_BV_MODIFY(token, op) \
                case TokenType::token: {\
                    m_value op otherValue;\
                    return this->shared_from_this();\
                } break
            
            #define GDML_DEF_BV_MODIFY_E(token, op, ...) \
                case TokenType::token: {\
                    if constexpr (__VA_ARGS__) {\
                        m_value op otherValue;\
                    }\
                    return this->shared_from_this();\
                } break

            #define GDML_DEF_BV_COPY(token, op) \
                case TokenType::token: {\
                    return makeCopy(m_value op otherValue);\
                } break

            #define GDML_DEF_BV_COPY_E(token, op, ...) \
                case TokenType::token: {\
                    if constexpr (__VA_ARGS__) {\
                        return makeCopy(m_value op otherValue);\
                    } else {\
                        return copy();\
                    }\
                } break

            #define GDML_DEF_BV_BOOL(token, op) \
                case TokenType::token: {\
                    return m_compiler.makeValue<NumeralValue<types::Bool>>(\
                        m_value op otherValue\
                    );\
                } break
            
            constexpr auto NO_BOOL = !std::is_same_v<T, types::Bool>;
            constexpr auto NO_FLOAT = !std::is_floating_point_v<T>;

            switch (op) {
                GDML_DEF_BV_MODIFY(Assign, =);

                GDML_DEF_BV_MODIFY_E(AddAssign, +=, NO_BOOL);
                GDML_DEF_BV_MODIFY_E(SubAssign, -=, NO_BOOL);
                GDML_DEF_BV_MODIFY_E(MulAssign, *=, NO_BOOL);
                GDML_DEF_BV_MODIFY_E(DivAssign, /=, NO_BOOL);
                GDML_DEF_BV_MODIFY_E(ModAssign, *=, NO_BOOL);

                GDML_DEF_BV_COPY(Add, +);
                GDML_DEF_BV_COPY(Sub, -);
                GDML_DEF_BV_COPY(Mul, *);
                GDML_DEF_BV_COPY_E(Div, /, NO_BOOL);
                GDML_DEF_BV_COPY_E(Mod, %, NO_BOOL && NO_FLOAT);

                GDML_DEF_BV_BOOL(And,               &&);
                GDML_DEF_BV_BOOL(Or,                ||);
                GDML_DEF_BV_BOOL(Equal,             ==);
                GDML_DEF_BV_BOOL(NotEqual,          !=);
                GDML_DEF_BV_BOOL(LessThan,          <);
                GDML_DEF_BV_BOOL(GreaterThan,       >);
                GDML_DEF_BV_BOOL(LessThanOrEqual,   <=);
                GDML_DEF_BV_BOOL(GreaterThanOrEqual,>=);

                GDML_DEF_BV_COPY_E(BitAnd, &, NO_FLOAT);
                GDML_DEF_BV_COPY_E(BitOr,  |, NO_FLOAT);
                GDML_DEF_BV_COPY_E(BitXor, ^, NO_FLOAT);

                GDML_DEF_BV_MODIFY_E(BitAndAssign,  &=, NO_FLOAT);
                GDML_DEF_BV_MODIFY_E(BitOrAssign,   |=, NO_FLOAT);
                GDML_DEF_BV_MODIFY_E(BitXorAssign,  ^=, NO_FLOAT);

                GDML_DEF_BV_COPY_E(BitShiftLeft,  <<, NO_BOOL && NO_FLOAT);
                GDML_DEF_BV_COPY_E(BitShiftRight, >>, NO_BOOL && NO_FLOAT);

                case TokenType::DoubleQuestion: {
                    return m_value ? this->shared_from_this() : other;
                } break;

                case TokenType::QuestionAssign: {
                    if (!m_value) {
                        m_value = otherValue;
                    }
                    return this->shared_from_this();
                } break;

                case TokenType::Pow: {
                    m_value = static_cast<T>(pow(m_value, otherValue));
                    return this->shared_from_this();
                } break;

                default: break;
            }
            return nullptr;
        }
    };
}
