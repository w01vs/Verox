#ifndef PARSER_HPP
#define PARSER_HPP

#include "arena.hpp"
#include "lexer.hpp"
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <unordered_map>
#include <variant>

enum class Type { undefined, _int };

std::unordered_map<TokenType, int> op_prec = {
    {TokenType::add, 0},
    {TokenType::minus, 0},
    {TokenType::star, 1},
    {TokenType::fslash, 1},
};

struct NodeExprIInt {
    Token i_int; // immediate int
};

struct NodeExprIdent {
    Token ident; // var name
};

struct NodeTermParens;

struct NodeTerm {
    std::variant<NodeExprIdent*, NodeExprIInt*, NodeTermParens*> val;
};

struct NodeBinExpr;

struct NodeExpr {
    std::variant<NodeTerm*, NodeBinExpr*> var;
    std::optional<Type> type;
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

struct NodeStmt {
    std::variant<NodeInternal*, NodeStmtVar*> var; // Internal stuff or variable
};

struct NodeProg {
    std::vector<NodeStmt*> stmts; // All statements
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
    if(op_prec.find(type) == op_prec.end())
    {
        return {};
    }

    return op_prec[type];
}

class Parser {
  public:
    inline explicit Parser(std::vector<Token> tokens) : tokens(tokens), arena(1024 * 1024 * 4) // 4 mb
    {
    }

    inline std::optional<NodeExpr*> parse_expr(int min_prec = 0)
    {
        NodeTerm* term_lhs = parse_term();

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
            {
                break;
            }

            const auto [type, line, val] = take();
            const int next_prec = prec.value() + 1;

            auto rhs = parse_expr(next_prec);
            if(!rhs.has_value())
            {
                std::cerr << "SyntaxError: Expected expression on line " << line << std::endl;
                exit(EXIT_FAILURE);
            }

            auto expr = arena.emplace<NodeBinExpr>();
            auto expr_lhs2 = arena.emplace<NodeExpr>();
            expr_lhs2->var = expr_lhs->var;
            if(type == TokenType::add)
            {
                auto add = arena.emplace<NodeBinExprAdd>(expr_lhs2, rhs.value());
                expr->val = add;
            }
            else if(type == TokenType::minus)
            {
                auto sub = arena.emplace<NodeBinExprSub>(expr_lhs2, rhs.value());
                expr->val = sub;
            }
            else if(type == TokenType::star)
            {
                auto mult = arena.emplace<NodeBinExprMult>(expr_lhs2, rhs.value());
                expr->val = mult;
            }
            else if(type == TokenType::fslash)
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

    inline NodeTerm* parse_term()
    {
        NodeTerm* term = arena.emplace<NodeTerm>();
        if(peek().has_value())
        {
            auto token = take();
            if(token.type == TokenType::i_int)
            {
                auto i_int = arena.emplace<NodeExprIInt>(token);
                term->val = i_int;
                return term;
            }
            else if(token.type == TokenType::ident)
            {
                auto ident = arena.emplace<NodeExprIdent>(token);
                term->val = ident;
                return term;
            }
            else if(token.type == TokenType::open_p) {
                auto expr = parse_expr();
                if(!expr.has_value()) {
                    std::cerr << "SyntaxError: Expected expression on line " << token.line << std::endl;
                }
                if(auto close = try_take(TokenType::close_p)) {
                    auto term_parens = arena.emplace<NodeTermParens>(expr.value());
                    auto term = arena.emplace<NodeTerm>(term_parens);
                    return term;
                }
                else {
                    std::cerr << "SyntaxError: Expected ')' on line " << token.line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            else
            {
                std::cerr << "Error: Expected term on line " << peek(-1).value().line << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        else
        {
            std::cerr << "Error: Expected term on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    inline std::optional<NodeStmt*> parse_stmt()
    {
        if(peek().has_value())
        {
            if(peek().value().type == TokenType::ret)
            {
                take();
                auto ret = arena.emplace<NodeInternalRet>();
                if(auto expr = parse_expr())
                {
                    ret->ret = expr.value();
                }
                if(peek().has_value() && peek().value().type == TokenType::semi)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek(-1).value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                auto internal_expr = arena.emplace<NodeInternal>(ret);
                return arena.emplace<NodeStmt>(internal_expr);
            }
            else if(peek().value().type == TokenType::type && peek(1).has_value() && peek(1).value().type == TokenType::ident && peek(2).has_value() && peek(2).value().type == TokenType::assign)
            {
                Type type = get_type(take());
                auto stmt_var = arena.emplace<NodeStmtVar>(take(), type);
                take(); // take '=' operator
                if(auto expr = parse_expr())
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
                            if constexpr(std::is_same_v<decltype(arg), NodeTerm*>)
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
            else if(peek().value().type == TokenType::print && peek(1).value().type == TokenType::open_p)
            {
                take();
                take();
                auto stmt_prt = arena.emplace<NodeInternalPrintf>();
                if(auto expr = parse_expr())
                    stmt_prt->print = expr.value();
                if(peek().has_value() && peek().value().type == TokenType::close_p)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ')' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                if(peek().has_value() && peek().value().type == TokenType::semi)
                    take();
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
                auto internal_expr = arena.emplace<NodeInternal>(stmt_prt);
                return arena.emplace<NodeStmt>(internal_expr);
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
        return Type::undefined;
    }

    inline bool semicolon()
    {
        if(peek().has_value() && peek().value().type == TokenType::semi)
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
