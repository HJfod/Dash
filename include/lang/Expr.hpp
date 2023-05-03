#include "Main.hpp"
#include "Token.hpp"

namespace gdml::lang {
    struct Expr {
        Range range;
        Expr(Range const& range) : range(range) {}
        virtual ~Expr() = default;
        
        virtual std::string debug(size_t indent = 0) const = 0;

        static ExprResult<Expr> pull(Stream& stream);
        static ExprResult<Expr> pullPrimary(Stream& stream);
        static ExprResult<Expr> pullPrimaryNonCall(Stream& stream);
    };

    template <class T>
    struct AExpr : public Expr, public std::enable_shared_from_this<T> {
        AExpr(Range const& range) : Expr(range) {}
    };

    struct LitExpr : public AExpr<LitExpr> {
        Lit value;
        LitExpr(Lit const& value, Range const& range)
            : AExpr(range), value(value) {}
        static ExprResult<LitExpr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct IdentExpr : public AExpr<IdentExpr> {
        Ident ident;
        IdentExpr(Ident const& ident, Range const& range)
            : AExpr(range), ident(ident) {}
        static ExprResult<IdentExpr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct BinOpExpr : public AExpr<BinOpExpr> {
        Rc<Expr> lhs;
        Rc<Expr> rhs;
        Op op;
        BinOpExpr(Rc<Expr> lhs, Rc<Expr> rhs, Op op, Range const& range)
            : AExpr(range), lhs(lhs), rhs(rhs), op(op) {}
        static ExprResult<Expr> pull(Stream& stream, size_t prec, Rc<Expr> lhs);
        static ExprResult<Expr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct MemberExpr : public AExpr<MemberExpr> {
        Rc<Expr> target;
        Ident member;
        MemberExpr(Rc<Expr> target, Ident const& member, Range const& range)
            : AExpr(range), target(target), member(member) {}
        static ExprResult<MemberExpr> pull(Rc<Expr> target, Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct CallExpr : public AExpr<CallExpr> {
        Rc<Expr> target;
        Vec<Rc<Expr>> args;
        CallExpr(Rc<Expr> target, Vec<Rc<Expr>> args, Range const& range)
            : AExpr(range), target(target), args(args) {}
        static ExprResult<CallExpr> pull(Rc<Expr> target, Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct PropExpr : public AExpr<PropExpr> {
        Ident prop;
        Rc<Expr> value;
        PropExpr(Ident prop, Rc<Expr> value, Range const& range)
            : AExpr(range), prop(prop), value(value) {}
        static ExprResult<PropExpr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct NodeExpr : public AExpr<NodeExpr> {
        Ident ident;
        Vec<Rc<PropExpr>> props;
        Vec<Rc<NodeExpr>> children;
        NodeExpr(Ident ident, Vec<Rc<PropExpr>> props, Vec<Rc<NodeExpr>> children, Range const& range)
            : AExpr(range), ident(ident), props(props), children(children) {}
        static ExprResult<NodeExpr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct ListExpr : public AExpr<ListExpr> {
        Vec<Rc<Expr>> exprs;
        ListExpr(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}
        static ExprResult<ListExpr> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };

    struct AST : public AExpr<AST> {
        Vec<Rc<Expr>> exprs;
        AST(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}
        static ExprResult<AST> pull(Stream& stream);
        std::string debug(size_t indent = 0) const override;
    };
}
