#ifndef PARSER_HPP
#define PARSER_HPP

#include "arena.hpp"
#include "lexer.hpp"
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <variant>

enum class Type { undefined, _int, _bool, _void };

struct NodeExprIInt {
    Token i_int; // immediate int
};

struct NodeExprIdent {
    Token ident; // var name
};

struct NodeExprBool {
    Token boolean; // boolean value
};

// Forward declaration
struct NodeTermParens;

struct NodeBinTerm {
    std::variant<NodeExprIdent*, NodeExprIInt*, NodeTermParens*> val;
};

struct NodeLogicTerm {
    std::variant<NodeExprIdent*, NodeExprBool*, NodeTermParens*> val;
};

// Forward declarations
struct NodeBinExpr;
struct NodeLogicExpr;

struct NodeExpr {
    std::variant<NodeBinTerm*, NodeBinExpr*, NodeLogicTerm*, NodeLogicExpr*> var;
    std::optional<Type> type;
};

struct NodeLogicExprAnd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeLogicExprOr {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeLogicExpr {
    std::variant<NodeLogicExprAnd*, NodeLogicExprOr*> val;
};

struct NodeTermParens {
    NodeExpr* expr;
};

struct NodeBinExprAdd {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprMult {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprSub {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExprDiv {
    NodeExpr* lhs;
    NodeExpr* rhs;
};

struct NodeBinExpr {
    std::variant<NodeBinExprAdd*, NodeBinExprDiv*, NodeBinExprSub*, NodeBinExprMult*> val;
};

struct NodeInternalRet {
    NodeExpr* ret; // return val
};

struct NodeInternalPrintf {
    NodeExpr* print;
};

// internal 'functions' such as return and possibly print etc.
struct NodeInternal {
    std::variant<NodeInternalRet*, NodeInternalPrintf*> ret; // return
};

// variable declaration with a type.
struct NodeStmtVar {
    Token ident; // var name
    Type type;
    NodeExpr* expr; // var value
};

struct NodeScope;

struct NodeStmt {
    std::variant<NodeInternal*, NodeStmtVar*, NodeScope*> var; // Internal stuff or variable
};

struct NodeScope {
    std::vector<NodeStmt*> stmts; // All statements in a scope
};

struct NodeIf {
    NodeExpr* cond;
    NodeScope* scope;
};

struct NodeProg {
    std::vector<NodeStmt*> stmts; // All statements outside scopes
};

inline std::string type_string(Type type)
{
    switch(type)
    {
    case Type::_int:
        return "int";
    default:
        return "undefined";
    }
}

inline std::optional<int> get_prec(TokenType type)
{
    if(type == TokenType::_add || type == TokenType::_minus)
    {
        return 0;
    }
    else if(type == TokenType::_star || type == TokenType::_fslash)
    {
        return 1;
    }
    else if(type == TokenType::_and) {
        return 1;
    }
    else if(type == TokenType::_or) {
        return 0;
    }

    return {};
}

class Parser {
  public:
    inline explicit Parser(std::vector<Token> tokens) : tokens(tokens), arena(1024 * 1024 * 4) // 4 mb
    {
    }

    inline std::optional<NodeExpr*> parse_expr(Type type)
    {
        if(type == Type::_int)
            return parse_binary_expr();
        else if(type == Type::_bool)
            return parse_logic_expr();
        else if(type == Type::_void) {}
        return {};
    }

    inline std::optional<NodeExpr*> parse_logic_expr(int min_prec = 0)
    {
        NodeLogicTerm* term_lhs = parse_logic_term();

        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs);

        while(true) {
            std::optional<Token> op = peek();
            std::optional<int> prec;
            if(op.has_value())
            {
                prec = get_prec(op.value().type);
                if(!prec.has_value() || prec.value() < min_prec)
                    break;
            }
            else
                break;

            const auto [type, line, val] = take();
            const int next_prec = prec.value() + 1;

            auto rhs = parse_logic_expr(next_prec);
            if(!rhs.has_value())
            {
                std::cerr << "SyntaxError: Expected expression on line " << line << std::endl;
                exit(EXIT_FAILURE);
            }

            auto expr = arena.emplace<NodeLogicExpr>();
            auto expr_lhs2 = arena.emplace<NodeExpr>();
            expr_lhs2->var = expr_lhs->var;
            if(type == TokenType::_and)
            {
                auto and_ = arena.emplace<NodeLogicExprAnd>(expr_lhs2, rhs.value());
                expr->val = and_;
            }
            else if(type == TokenType::_or)
            {
                auto or_ = arena.emplace<NodeLogicExprOr>(expr_lhs2, rhs.value());
                expr->val = or_;
            }
            else
            {
                assert(false);
            }
            expr_lhs->var = expr;
        }

        return expr_lhs;
    }

    inline std::optional<NodeExpr*> parse_binary_expr(int min_prec = 0)
    {
        NodeBinTerm* term_lhs = parse_binary_term();

        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs);

        while(true)
        {
            std::optional<Token> op = peek();
            std::optional<int> prec;
            if(op.has_value())
            {
                prec = get_prec(op.value().type);
                if(!prec.has_value() || prec.value() < min_prec)
                    break;
            }
            else
                break;

            const auto [type, line, val] = take();
            const int next_prec = prec.value() + 1;

            auto rhs = parse_binary_expr(next_prec);
            if(!rhs.has_value())
            {
                std::cerr << "SyntaxError: Expected expression on line " << line << std::endl;
                exit(EXIT_FAILURE);
            }

            auto expr = arena.emplace<NodeBinExpr>();
            auto expr_lhs2 = arena.emplace<NodeExpr>();
            expr_lhs2->var = expr_lhs->var;
            if(type == TokenType::_add)
            {
                auto add = arena.emplace<NodeBinExprAdd>(expr_lhs2, rhs.value());
                expr->val = add;
            }
            else if(type == TokenType::_minus)
            {
                auto sub = arena.emplace<NodeBinExprSub>(expr_lhs2, rhs.value());
                expr->val = sub;
            }
            else if(type == TokenType::_star)
            {
                auto mult = arena.emplace<NodeBinExprMult>(expr_lhs2, rhs.value());
                expr->val = mult;
            }
            else if(type == TokenType::_fslash)
            {
                auto div = arena.emplace<NodeBinExprDiv>(expr_lhs2, rhs.value());
                expr->val = div;
            }
            else
            {
                assert(false);
            }
            expr_lhs->var = expr;
        }

        return expr_lhs;
    }

    inline NodeScope* parse_scope()
    {
        NodeScope* scope = arena.emplace<NodeScope>();
        bool ended = false;
        while(peek().has_value() && !ended)
        {
            if(auto end = try_take(TokenType::_close_b))
            {
                ended = true;
                break;
            }

            if(auto stmt = parse_stmt())
            {
                scope->stmts.push_back(stmt.value());
            }
            else
            {
                std::cerr << "Invalid statement" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        if(!ended)
        {
            std::cerr << "SyntaxError: Expected '}' on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
        return scope;
    }

    inline NodeLogicTerm* parse_logic_term()
    {
        NodeLogicTerm* term = arena.emplace<NodeLogicTerm>();
        if(peek().has_value())
        {
            auto token = take();
            if(token.type == TokenType::_true || token.type == TokenType::_false)
            {
                auto boolean = arena.emplace<NodeExprBool>(token);
                term->val = boolean;
                return term;
            }
            else if(token.type == TokenType::_ident)
            {
                auto ident = arena.emplace<NodeExprIdent>(token);
                term->val = ident;
                return term;
            }
            else if(token.type == TokenType::_open_p)
            {
                auto expr = parse_logic_expr();
                if(!expr.has_value())
                {
                    std::cerr << "SyntaxError: Expected expression on line " << token.line << std::endl;
                }
                if(auto close = try_take(TokenType::_close_p))
                {
                    auto term_parens = arena.emplace<NodeTermParens>(expr.value());
                    auto term = arena.emplace<NodeLogicTerm>(term_parens);
                    return term;
                }
                else
                {
                    std::cerr << "SyntaxError: Expected ')' on line " << token.line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            else
            {
                std::cerr << "Error: Expected logic term on line " << peek(-1).value().line << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        else
        {
            std::cerr << "Error: Expected logic term on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    inline NodeBinTerm* parse_binary_term()
    {
        NodeBinTerm* term = arena.emplace<NodeBinTerm>();
        if(peek().has_value())
        {
            auto token = take();
            if(token.type == TokenType::_i_int)
            {
                auto i_int = arena.emplace<NodeExprIInt>(token);
                term->val = i_int;
                return term;
            }
            else if(token.type == TokenType::_ident)
            {
                auto ident = arena.emplace<NodeExprIdent>(token);
                term->val = ident;
                return term;
            }
            else if(token.type == TokenType::_open_p)
            {
                auto expr = parse_binary_expr();
                if(!expr.has_value())
                {
                    std::cerr << "SyntaxError: Expected expression on line " << token.line << std::endl;
                }
                if(auto close = try_take(TokenType::_close_p))
                {
                    auto term_parens = arena.emplace<NodeTermParens>(expr.value());
                    auto term = arena.emplace<NodeBinTerm>(term_parens);
                    return term;
                }
                else
                {
                    std::cerr << "SyntaxError: Expected ')' on line " << token.line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            else
            {
                std::cerr << "Error: Expected binary term on line " << peek(-1).value().line << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        else
        {
            std::cerr << "Error: Expected binary term on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    inline std::optional<NodeStmt*> parse_stmt()
    {
        if(peek().has_value())
        {
            if(peek().value().type == TokenType::_ret)
            {
                take();
                auto ret = arena.emplace<NodeInternalRet>();
                if(auto expr = parse_expr(Type::_int))
                {
                    ret->ret = expr.value();
                }
                if(peek().has_value() && peek().value().type == TokenType::_semi)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek(-1).value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                auto internal_expr = arena.emplace<NodeInternal>(ret);
                return arena.emplace<NodeStmt>(internal_expr);
            }
            else if(peek().value().type == TokenType::_type && peek(1).has_value() && peek(1).value().type == TokenType::_ident && peek(2).has_value() && peek(2).value().type == TokenType::_assign)
            {
                Type type = get_type(take());
                auto stmt_var = arena.emplace<NodeStmtVar>(take(), type);
                take(); // take '=' operator
                if(auto expr = parse_expr(type))
                    stmt_var->expr = expr.value();
                else
                {
                    std::cerr << "SyntaxError: Expected a value of type '" << type_string(stmt_var->type) << "' but got nothing" << std::endl;
                    exit(EXIT_FAILURE);
                }
                if(!semicolon())
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << " ~267" << std::endl;

                    std::visit(
                        [](auto&& arg)
                        {
                            if constexpr(std::is_same_v<decltype(arg), NodeBinTerm*>)
                            {
                                std::cout << arg.val.line << std::endl;
                            }
                            else if constexpr(std::is_same_v<decltype(arg), NodeBinExpr*>)
                            {
                                std::cout << arg->val.value().line << std::endl;
                            }
                        },
                        stmt_var->expr->var);

                    exit(EXIT_FAILURE);
                }
                return arena.emplace<NodeStmt>(stmt_var);
            }
            else if(peek().value().type == TokenType::_print && peek(1).value().type == TokenType::_open_p)
            {
                take();
                take();
                auto stmt_prt = arena.emplace<NodeInternalPrintf>();
                if(auto expr = parse_expr(Type::undefined))
                    stmt_prt->print = expr.value();
                if(peek().has_value() && peek().value().type == TokenType::_close_p)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ')' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                if(peek().has_value() && peek().value().type == TokenType::_semi)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                auto internal_expr = arena.emplace<NodeInternal>(stmt_prt);
                return arena.emplace<NodeStmt>(internal_expr);
            }
            else if(auto bracket = try_take(TokenType::_open_b))
            {
                auto scope = parse_scope();
                auto stmt = arena.emplace<NodeStmt>(scope);
                return stmt;
            }
        }

        return {};
    }

    inline std::optional<NodeProg*> parse()
    {
        NodeProg* root = arena.emplace<NodeProg>();
        while(peek().has_value())
        {
            if(auto stmt = parse_stmt())
            {
                root->stmts.push_back(stmt.value());
            }
            else
            {
                std::cerr << "Invalid statement" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        return root;
    }

    inline bool check_bexpr()
    {
        if(peek().has_value() && peek().value().type == TokenType::_i_int)
            return true;
        if(peek().has_value() && peek().value().type == TokenType::_ident)
            return true;
        if(peek().has_value() && peek().value().type == TokenType::_open_p)
            return true;
        return false;
    }

  private:
    std::vector<Token> tokens;
    ArenaAllocator arena;
    size_t index = 0;

    inline std::optional<Token> peek(const int offset = 0) const
    {
        if(index + offset >= tokens.size())
        {
            return {};
        }

        return tokens.at(index + offset);
    }

    inline Token take() { return tokens.at(index++); }

    inline Type get_type(Token token)
    {
        if(token.val.has_value() && token.val.value() == "int")
        {
            return Type::_int;
        }
        else if(token.val.has_value() && token.val.value() == "bool")
        {
            return Type::_bool;
        }
        return Type::undefined;
    }

    inline bool semicolon()
    {
        if(peek().has_value() && peek().value().type == TokenType::_semi)
        {
            take();
            return true;
        }
        return false;
    }

    inline std::optional<Token> try_take(const TokenType token)
    {
        if(peek().has_value() && peek().value().type == token)
            return take();
        return {};
    }
};

#endif // PARSER_HPP
