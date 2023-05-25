#include <lang/State.hpp>
#include <lang/Expr.hpp>
#include "../Debug.hpp"

using namespace geode::prelude;
using namespace gdml::lang;
using namespace gdml;

ExprResult<VarDeclExpr> VarDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    bool isExtern = Token::draw(Keyword::Extern, stream).has_value();
    GEODE_UNWRAP(Token::pull(Keyword::Let, stream));
    GEODE_UNWRAP_INTO(auto ident, IdentExpr::pull(stream));

    Option<Rc<TypeExpr>> type;
    if (Token::draw(':', stream)) {
        GEODE_UNWRAP_INTO(type, TypeExpr::pull(stream));
    }

    Option<Rc<Expr>> value;
    if (Token::draw(Op::Seq, stream)) {
        GEODE_UNWRAP_INTO(value, Expr::pull(stream));
    }

    return rb.commit<VarDeclExpr>(ident, type, value, isExtern);
}

Type VarDeclExpr::typecheck(UnitParser& state) const {
    Type varty;
    if (!type && !value) {
        state.error(range, "Variable needs an explicit type or value");
        varty = Primitive::Unk;
    }
    if (type) {
        auto ty = type.value()->typecheck(state);
        if (value) {
            auto vt = value.value()->typecheck(state);
            if (ty.convertible(vt)) {
                // todo: hint what the types are
                state.error(range, "Value does not match explicit type");
            }
        }
        varty = ty;
    }
    if (value) {
        if (isExtern) {
            state.error(value.value()->range, "Extern variables may not have a value");
        }
        varty = value.value()->typecheck(state);
    }
    state.verifyCanPush(ident);
    state.push(Var {
        .name = ident->path,
        .type = varty,
        .isExtern = isExtern,
        .decl = shared_from_this(),
    });
    return varty;
}

std::string VarDeclExpr::debug(size_t indent) const {
    return DebugPrint("VarDeclExpr", indent)
        .member("ident", ident)
        .member("type", type)
        .member("value", value);
}

Option<Entity> VarDeclExpr::typecheckEntity(UnitParser& state) const {
    return Var {
        .name = ident->path,
        .type = this->typecheck(state),
        .isExtern = isExtern,
        .decl = shared_from_this(),
    };
}

ParseResult<Option<Op>> FunDeclExpr::pullParams(Vec<Param>& target, Stream& stream, bool requireTypes) {
    // allow empty parameter list
    if (Token::peek(')', stream)) {
        return Ok(None);
    }
    Option<Op> op;
    while (true) {
        auto start = stream.location();
        stream.debugTick();
        Param param;
        GEODE_UNWRAP_INTO(param.name, Token::pull<Ident>(stream));
        if (requireTypes && param.name != "this") {
            GEODE_UNWRAP(Token::pull(':', stream));
            GEODE_UNWRAP_INTO(param.type, TypeExpr::pull(stream));
        }
        else if (Token::draw(':', stream)) {
            GEODE_UNWRAP_INTO(param.type, TypeExpr::pull(stream));
        }
        if (Token::draw(Op::Eq, stream)) {
            GEODE_UNWRAP_INTO(param.value, Expr::pull(stream));
        }
        param.range = Range(start, stream.location());
        target.push_back(param);
        if (auto o = Token::draw<Op>(stream)) {
            Rollback rb(stream);
            if (op) {
                return rb.error("Only one operator may be specified in function args");
            }
            op = o;
            rb.commit();
            continue;
        }
        GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', ')', stream));
        if (brk) {
            break;
        }
    }
    return Ok(op);
}

ExprResult<FunDeclExpr> FunDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    bool isExtern = Token::draw(Keyword::Extern, stream).has_value();
    GEODE_UNWRAP(Token::pull(Keyword::Function, stream));
    Name name = None;
    Vec<Param> params;
    Option<Rc<TypeExpr>> ret;
    if (auto ident = IdentExpr::pull(stream)) {
        name = ident.unwrap();
    }
    else if (auto op = Token::pull<Op>(stream)) {
        name = op.unwrap();
    }
    else if (Token::draw(Keyword::As, stream)) {
        name = AsName();
    }
    rb.clearMessages();
    GEODE_UNWRAP(Token::pull('(', stream));
    GEODE_UNWRAP_INTO(auto maybeOp, FunDeclExpr::pullParams(params, stream, true));
    GEODE_UNWRAP(Token::pull(')', stream));
    if (maybeOp) {
        name = maybeOp.value();
    }
    if (Token::draw(Keyword::As, stream)) {
        name = AsName();
        GEODE_UNWRAP_INTO(ret, TypeIdentExpr::pull(stream));
    }
    else if (Token::draw(Op::Arrow, stream)) {
        GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
    }
    Option<Rc<Expr>> body;
    if (Token::draw('{', stream)) {
        GEODE_UNWRAP_INTO(body, ListExpr::pull(stream));
        GEODE_UNWRAP(Token::pull('}', stream));
    }
    return rb.commit<FunDeclExpr>(name, params, body, ret, isExtern);
}

ExprResult<FunDeclExpr> FunDeclExpr::pullArrow(Stream& stream) {
    Rollback rb(stream);
    Vec<Param> params;
    Option<Rc<TypeExpr>> ret;
    if (!Token::draw(Keyword::Get, stream)) {
        if (Token::draw(Keyword::Set, stream)) {
            GEODE_UNWRAP(Token::pull('(', stream));
            params.push_back(Param {
                .name = "this",
                .type = None,
                .value = None,
                .range = stream.location(),
            });
            GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, false));
            GEODE_UNWRAP(Token::pull(')', stream));
        }
        else if (Token::draw('(', stream)) {
            GEODE_UNWRAP(FunDeclExpr::pullParams(params, stream, false));
            GEODE_UNWRAP(Token::pull(')', stream));
        }
        else {
            Param param;
            GEODE_UNWRAP_INTO(param.name, Token::pull<Ident>(stream));
            params.push_back(param);
        }
    }
    else {
        params.push_back(Param {
            .name = "this",
            .type = None,
            .value = None,
            .range = stream.location(),
        });
    }
    if (Token::draw(Op::Arrow, stream)) {
        GEODE_UNWRAP_INTO(ret, TypeExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull(Op::Farrow, stream));
    Rc<Expr> body;
    GEODE_UNWRAP_INTO(body, Expr::pull(stream));
    return rb.commit<FunDeclExpr>(None, params, body, ret, false);
}

Type FunDeclExpr::typecheck(UnitParser& state) const {
    FunType fun;
    fun.isExtern = isExtern;

    if (std::holds_alternative<Rc<IdentExpr>>(name)) {
        state.verifyCanPush(std::get<Rc<IdentExpr>>(name));
    }

    state.pushScope(true);
    for (auto& param : params) {
        Type pty = Primitive::Unk;
        if (param.type) {
            pty = param.type.value()->typecheck(state);
        }
        if (param.value) {
            auto vty = param.value.value()->typecheck(state);
            if (!param.type) {
                pty = vty;
            }
            else if (pty.convertible(vty)) {
                state.error(param.range, "Default value type does not match parameter type");
            }
        }
        state.push(Var {
            .name = IdentPath(param.name),
            .type = pty,
            .decl = shared_from_this(),
        });
        fun.params.push_back(ParamType {
            .name = param.name,
            .type = pty,
        });
    }
    if (retType) {
        fun.retType = retType.value()->typecheck(state);
    }
    
    std::visit(makeVisitor {
        [&](Rc<IdentExpr> const& expr) {
            fun.name = expr->path;
        },
        [&](Op const& op) {
            if (isUnOp(op)) {
                if (fun.params.size() == 1) {
                    fun.name = IdentPath(opFunName(
                        op, TRY_FUN(fun.params.at(0).type)
                    ));
                }
                else {
                    state.error(range, "Unary operators must have one parameter");
                }
            }
            else {
                if (fun.params.size() == 2) {
                    fun.name = IdentPath(opFunName(
                        op,
                        TRY_FUN(fun.params.at(0).type),
                        TRY_FUN(fun.params.at(1).type)
                    ));
                }
                else {
                    state.error(range, "Binary operators must have two parameters");
                }
            }
        },
        [&](AsName const&) {
            if (fun.params.size() == 1) {
                if (fun.retType) {
                    fun.name = IdentPath(asFunName(
                        TRY_FUN(fun.params.at(0).type), fun.retType.value()
                    ));
                }
                else {
                    state.error(range, "As operator must have an explicit return type");
                }
            }
            else {
                state.error(range, "As operator must have one parameter");
            }
        },
        [&](std::nullopt_t const&) {},
    }, this->name);

    auto ret = Type(fun, shared_from_this());

    // allow recursion by pushing fun type before body check
    state.scope(1).push(ret);

    if (body) {
        if (isExtern) {
            state.error(body.value()->range, "Extern functions may not have a body");
        }
        auto ret = body.value()->typecheck(state);
        if (fun.retType && !ret.convertible(fun.retType.value())) {
            state.error(body.value()->range, "Function body does not match return type");
        }
    }
    state.popScope();

    return ret;
}

std::string FunDeclExpr::debug(size_t indent) const {
    return DebugPrint("FunDeclExpr", indent)
        .member("name", std::visit(makeVisitor {
            [](std::nullopt_t const&) {
                return std::string("none");
            },
            [](Rc<IdentExpr> const& expr) {
                return expr->path.toString();
            },
            [](Op const& op) {
                return tokenToString(op, true);
            },
            [](AsName const&) {
                return std::string("as");
            },
        }, name))
        .member("params", params)
        .member("body", body)
        .member("retType", retType)
        .member("isExtern", isExtern);
}

Option<Entity> FunDeclExpr::typecheckEntity(UnitParser& state) const {
    return this->typecheck(state);
}

ExprResult<MemberDeclExpr> MemberDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto required = Token::draw(Keyword::Required, stream).has_value();
    GEODE_UNWRAP_INTO(auto ident, Token::pull<Ident>(stream));
    GEODE_UNWRAP(Token::pull(':', stream));
    GEODE_UNWRAP_INTO(auto type, TypeExpr::pull(stream));
    Vec<Ident> dependencies;
    Option<Rc<FunDeclExpr>> getter;
    Option<Rc<FunDeclExpr>> setter;
    Option<Rc<Expr>> defaultValue;
    if (Token::draw('{', stream)) {
        while (true) {
            stream.debugTick();
            if (Token::draw(Keyword::Depends, stream)) {
                GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
                dependencies.push_back(dep);
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            if (Token::peek(Keyword::Get, stream)) {
                GEODE_UNWRAP_INTO(getter, FunDeclExpr::pullArrow(stream));
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            if (Token::draw(Keyword::Set, stream)) {
                GEODE_UNWRAP_INTO(setter, FunDeclExpr::pullArrow(stream));
                GEODE_UNWRAP(Token::pullSemicolons(stream));
                continue;
            }
            break;
        }
        if (getter.has_value() ^ setter.has_value()) {
            return rb.errorNextToken("A getter and setter need to both be defined");
        }
        GEODE_UNWRAP(Token::pull('}', stream));
    }
    else if (Token::draw(Op::Bind, stream)) {
        // multi-bind
        if (Token::draw('{', stream)) {
            Vec<Rc<PropExpr>> props;
            Vec<Rc<Expr>> exprs;
            Map<Ident, Ident> bindedDeps;
            while (true) {
                stream.debugTick();
                GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
                if (Token::draw(Op::Bind, stream)) {
                    GEODE_UNWRAP_INTO(auto bindTo, Token::pull<Ident>(stream));
                    bindedDeps[dep] = bindTo;
                    dependencies.push_back(bindTo);
                }
                else {
                    bindedDeps[dep] = dep;
                    dependencies.push_back(dep);
                }
                GEODE_UNWRAP_INTO(auto brk, Token::pullSeparator(',', '}', stream));
                if (brk) {
                    break;
                }
            }
            GEODE_UNWRAP(Token::pull('}', stream));
            for (auto& [bind, dep] : bindedDeps) {
                props.push_back(stream.make<PropExpr>(
                    bind, stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep)
                ));
                exprs.push_back(stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("value")), bind),
                    Op::Seq
                ));
            }
            getter = stream.make<FunDeclExpr>(
                None, Vec<Param> {
                    Param {
                        .name = "this",
                    },
                },
                stream.make<NodeExpr>(None, props, Vec<Rc<NodeExpr>>()),
                type,
                false
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> {
                    Param {
                        .name = "this",
                    },
                    Param {
                        .name = "value",
                        .type = type,
                    },
                },
                stream.make<ListExpr>(exprs),
                None,
                false
            );
        }
        else {
            GEODE_UNWRAP_INTO(auto dep, Token::pull<Ident>(stream));
            dependencies.push_back(dep);
            getter = stream.make<FunDeclExpr>(
                None, Vec<Param> {
                    Param {
                        .name = "this",
                    },
                },
                stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                type,
                false
            );
            setter = stream.make<FunDeclExpr>(
                None, Vec<Param> {
                    Param {
                        .name = "this",
                    },
                    Param {
                        .name = "value",
                        .type = type,
                    },
                },
                stream.make<BinOpExpr>(
                    stream.make<MemberExpr>(stream.make<IdentExpr>(IdentPath("this")), dep),
                    stream.make<IdentExpr>(IdentPath("value")),
                    Op::Seq
                ),
                None, false
            );
        } 
    }
    if (Token::draw(Op::Eq, stream)) {
        GEODE_UNWRAP_INTO(defaultValue, Expr::pull(stream));
    }
    return rb.commit<MemberDeclExpr>(ident, type, defaultValue, dependencies, getter, setter, required);
}

Type MemberDeclExpr::typecheck(UnitParser& state) const {
    if (defaultValue && (getter || setter)) {
        state.error(range, "Properties may not have both a setter / getter and default value");
    }
    auto ty = type->typecheck(state);
    if (defaultValue) {
        auto valty = defaultValue.value()->typecheck(state);
        if (ty.convertible(valty)) {
            // todo: hint types
            state.error(range, "Value and member type do not match");
        }
    }
    if (getter) {
        auto ret = getter.value()->typecheck(state).getReturnType();
        if (ret && !ret.value().convertible(ty)) {
            state.error(getter.value()->range, "Getter return type does not match member type")
                .note("Member type is {}", ty.toString())
                .note("Return type is {}", ret.value().toString());
        }
    }
    if (setter) {
        // todo: verify setter parameter type
        setter.value()->typecheck(state);
    }
    return ty;
}

std::string MemberDeclExpr::debug(size_t indent) const {
    return DebugPrint("MemberDeclExpr", indent)
        .member("name", name)
        .member("type", type)
        .member("defaultValue", defaultValue)
        .member("dependencies", dependencies)
        .member("getter", getter)
        .member("setter", setter)
        .member("required", required);
}

ExprResult<NodeDeclExpr> NodeDeclExpr::pull(Stream& stream, bool implicitStruct) {
    Rollback rb(stream);
    bool isExtern = false;
    bool isStruct;
    Option<Rc<IdentExpr>> ident;
    if (implicitStruct) {
        isStruct = true;
        GEODE_UNWRAP_INTO(ident, IdentExpr::pull(stream));
    }
    else {
        isExtern = Token::draw(Keyword::Extern, stream).has_value();
        isStruct = Token::draw(Keyword::Struct, stream).has_value();
        if (!isStruct) {
            GEODE_UNWRAP(Token::pull(Keyword::Decl, stream));
        }
        ident = IdentExpr::pull(stream).ok();
        rb.clearMessages();
    }
    Option<Rc<IdentExpr>> extends;
    if (Token::draw(Keyword::Extends, stream)) {
        GEODE_UNWRAP_INTO(extends, IdentExpr::pull(stream));
    }
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<MemberDeclExpr>> members;
    while (!Token::peek('}', stream)) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto mem, MemberDeclExpr::pull(stream));
        members.push_back(mem);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<NodeDeclExpr>(ident, members, extends, isStruct, isExtern);
}

Type NodeDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        state.verifyCanPush(ident.value());
    }
    if (isStruct) {
        StructType sty;
        if (ident) {
            sty.name = ident.value()->path;
        }
        sty.isExtern = isExtern;
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.members[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = mem->defaultValue || mem->getter,
            };
        }
        auto ret = Type(sty, shared_from_this());
        if (ident) {
            state.push(ret);
        }
        return ret;
    }
    else {
        if (!ident) {
            state.error(range, "Anonymous nodes are not allowed");
            return Primitive::Unk;
        }
        NodeType sty;
        sty.name = ident.value()->path;
        for (auto& mem : members) {
            auto mty = mem->typecheck(state);
            sty.props[mem->name] = PropType {
                .type = mty,
                .dependencies = mem->dependencies,
                .required = false,
            };
        }
        auto ret = Type(sty, shared_from_this());
        state.push(ret);
        return ret;
    }
}

Option<Entity> NodeDeclExpr::typecheckEntity(UnitParser& state) const {
    return this->typecheck(state);
}

std::string NodeDeclExpr::debug(size_t indent) const {
    return DebugPrint("NodeDeclExpr", indent)
        .member("ident", ident)
        .member("members", members)
        .member("extends", extends)
        .member("isStruct", isStruct)
        .member("isExtern", isExtern);
}

ExprResult<EnumDeclExpr> EnumDeclExpr::pull(Stream& stream) {
    Rollback rb(stream);
    auto isExtern = Token::draw(Keyword::Extern, stream).has_value();
    GEODE_UNWRAP(Token::pull(Keyword::Enum, stream));
    auto ident = IdentExpr::pull(stream).ok();
    rb.clearMessages();
    GEODE_UNWRAP(Token::pull('{', stream));
    Vec<Rc<NodeDeclExpr>> variants;
    while (true) {
        stream.debugTick();
        GEODE_UNWRAP_INTO(auto var, NodeDeclExpr::pull(stream, true));
        variants.push_back(var);
        GEODE_UNWRAP(Token::pullSemicolons(stream));
        if (Token::peek('}', stream)) {
            break;
        }
    }
    GEODE_UNWRAP(Token::pull('}', stream));
    return rb.commit<EnumDeclExpr>(ident, variants, isExtern);
}

Type EnumDeclExpr::typecheck(UnitParser& state) const {
    if (ident) {
        state.verifyCanPush(ident.value());
    }
    EnumType ty;
    if (ident) {
        ty.name = ident.value()->path;
    }
    ty.isExtern = isExtern;
    for (auto& var : variants) {
        if (!var->ident) {
            throw std::runtime_error("Anonymous enum variant encountered");
        }
        auto name = var->ident.value();
        if (!name->path.isSingle()) {
            state.error(name->range, "Variant name may not be a full path");
        }
        else {
            if (ty.variants.contains(name->path.name)) {
                state.error(var->range, "Variant \"{}\" already defined", name->path);
            }
            else {
                ty.variants.insert({ name->path.name, var->typecheck(state) });
            }
        }
    }
    auto ret = Type(ty, shared_from_this());
    if (ident) {
        state.push(ret);
    }
    return ret;
}

Option<Entity> EnumDeclExpr::typecheckEntity(UnitParser& state) const {
    return this->typecheck(state);
}

std::string EnumDeclExpr::debug(size_t indent) const {
    return DebugPrint("EnumDeclExpr", indent)
        .member("ident", ident)
        .member("variants", variants)
        .member("isExtern", isExtern);
}
