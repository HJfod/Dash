#include "Main.hpp"
#include "Parser.hpp"

template <class T>
using ExprResult = ParseResult<Rc<T>>;

struct Expr {
    Range range;
    Expr(Range const& range) : range(range) {}
    static ExprResult<Expr> pull(Stream& stream);
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
};

struct IdentExpr : public AExpr<IdentExpr> {
    Ident ident;
    IdentExpr(Ident const& ident, Range const& range)
      : AExpr(range), ident(ident) {}
    static ExprResult<IdentExpr> pull(Stream& stream);
};

struct ListExpr : public AExpr<ListExpr> {
    Vec<Rc<Expr>> exprs;
    ListExpr(Vec<Rc<Expr>> const& exprs, Range const& range)
      : AExpr(range), exprs(exprs) {}
    static ExprResult<ListExpr> pull(Stream& stream);
};
