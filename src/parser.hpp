#ifndef PARSER_HPP
#define PARSER_HPP

#include "arena.hpp"
#include "lexer.hpp"
#include <cstdlib>
#include <iostream>
#include <unordered_map>
#include <variant>

enum class Type { undefined, _int };

std::unordered_map<TokenType, int> op_prec = {
    {TokenType::add, 0},
};

struct NodeExprIInt {
    Token i_int; // immediate int
};

struct NodeExprIdent {
    Token ident; // var name
};

struct Term {
    std::variant<NodeExprIdent*, NodeExprIInt*> val;
};

struct BinExpr;

struct NodeExpr {
    std::variant<NodeExprIdent*, NodeExprIInt*, BinExpr*> var;
    std::optional<Type> type;
};

struct BinOp {
    Token op;
    int prec;
};

struct BinExpr {
    std::optional<BinExpr*> rhs{};
    std::variant<BinOp*, Term*> val;
    std::optional<BinExpr*> lhs{};
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

class Parser {
  public:
    inline explicit Parser(std::vector<Token> tokens) : tokens(tokens), arena(1024 * 1024 * 4) // 4 mb
    {
    }

    inline std::optional<NodeExpr*> parse_expr()
    {
        if(peek().has_value() && peek().value().type == TokenType::semi)
        {
            std::cerr << "Error: Expected expression on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
        std::vector<Token> expr = {};
        while(peek().has_value())
        {
            if(auto ident = try_take(TokenType::ident))
                expr.push_back(ident.value());
            else if(auto i_int = try_take(TokenType::i_int))
                expr.push_back(i_int.value());
            else if(auto add = try_take(TokenType::add))
                expr.push_back(add.value());
            else
                break;
        }

        if(peek().has_value() && peek().value().type == TokenType::semi)
        {
            // last element is an operator, missing term
            if(expr[expr.size() - 1].type == TokenType::add)
            {
                std::cerr << "Error: Expected term on line " << peek(-1).value().line << " ~117" << std::endl;
                exit(EXIT_FAILURE);
            }

            if(expr.size() == 1)
            {
                auto token = expr.at(0);
                if(token.type == TokenType::i_int)
                {
                    auto i_int = arena.emplace<NodeExprIInt>(token);
                    auto node_expr = arena.emplace<NodeExpr>(i_int);
                    return node_expr;
                }
                else if(token.type == TokenType::ident)
                {
                    auto ident = arena.emplace<NodeExprIdent>(token);
                    auto node_expr = arena.emplace<NodeExpr>(ident);
                    return node_expr;
                }
                else
                {
                    std::cerr << "Error: Expected term on line " << peek(-1).value().line << " ~138" << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            auto binexpr = parse_binexpr_v(expr);
            auto expr = arena.emplace<NodeExpr>(binexpr);
            return expr;
        }
        else
        {
            std::cerr << "SyntaxError: Expected ';' on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }

        return {};
    }

    inline BinExpr* parse_binexpr_v(const std::vector<Token>& expr, int op = 0, bool rhs = false)
    {
        if(op % 2 == 0 && op != 0)
        {
            std::cerr << "SyntaxError: Expected term on line " << expr.at(op).line << " ~163" << std::endl;
            exit(EXIT_FAILURE);
        }
        if(expr.size() == 1 || op == 1 || op == expr.size() - 2)
        {
            auto term = arena.emplace<Term>();
            auto bexpr = arena.emplace<BinExpr>();
            bexpr->val = term;
            return bexpr;
        }
        if(op == 0)
        {
            std::vector<std::pair<int, int>> ops = {};
            auto lowest_prec = 100000;
            for(int i = 0; i < expr.size(); i++)
            {
                if(expr[i].type == TokenType::add)
                {
                    
                    if(op_prec[TokenType::add] < lowest_prec)
                    {
                        lowest_prec = op_prec[TokenType::add];
                        ops.emplace_back(i, op_prec[TokenType::add]);
                        break;
                    }
                }
            }
            std::vector<std::pair<int, int>> lowest_ops = {};
            for(int i = 0; i < ops.size(); i++)
            {
                if(ops[i].second == lowest_prec){
                    lowest_ops.push_back(ops[i]);
                }
            }

            int top_op = lowest_ops.size() > 1 ? lowest_ops.size() / 2 - 1 : 0;
            Token t = expr[lowest_ops[top_op].first];
            auto bin_op = arena.emplace<BinOp>(t, lowest_ops[top_op].second);
            auto bin_expr = arena.emplace<BinExpr>();
            bin_expr->val = bin_op;
            bin_expr->lhs = {parse_binexpr_v(expr, lowest_ops[top_op].first)};
            bin_expr->rhs = {parse_binexpr_v(expr, lowest_ops[top_op].first, true)};

            return bin_expr;
        }
        std::cout << "again" << std::endl;
        if(rhs)
        {
            std::vector<std::pair<int, int>> ops = {};
            auto lowest_prec = 100000;
            for(int i = op; i < expr.size(); i++)
            {
                if(expr[i].type == TokenType::add)
                {
                    if(op_prec[TokenType::add] < lowest_prec)
                    {
                        lowest_prec = op_prec[TokenType::add];
                        ops.emplace_back(i, op_prec[TokenType::add]);
                        break;
                    }
                }
            }
            std::vector<std::pair<int, int>> lowest_ops = {};
            for(int i = 0; i < ops.size(); i++)
            {
                if(ops[i].second == lowest_prec)
                    lowest_ops.emplace_back(ops[i]);
            }
            int top_op = lowest_ops.size() / 2 - 1;
            auto bin_op = arena.emplace<BinOp>(expr.at(lowest_ops[top_op].first), lowest_ops[top_op].second);
            auto bin_expr = arena.emplace<BinExpr>();
            bin_expr->val = bin_op;
            bin_expr->lhs = {parse_binexpr_v(expr, lowest_ops[top_op].first)};
            bin_expr->rhs = {parse_binexpr_v(expr, lowest_ops[top_op].first, true)};
            return bin_expr;
        }

        std::vector<std::pair<int, int>> ops = {};
        auto lowest_prec = 100000;
        for(int i = 0; i < op; i++)
        {
            if(expr[i].type == TokenType::add)
            {
                if(op_prec[TokenType::add] < lowest_prec)
                {
                    lowest_prec = op_prec[TokenType::add];
                    ops.emplace_back(i, op_prec[TokenType::add]);
                    break;
                }
            }
        }
        std::vector<std::pair<int, int>> lowest_ops = {};
        for(int i = 0; i < ops.size(); i++)
        {
            if(ops[i].second == lowest_prec)
                lowest_ops.emplace_back(ops[i]);
        }
        int top_op = lowest_ops.size() / 2 - 1;
        auto bin_op = arena.emplace<BinOp>(expr.at(lowest_ops[top_op].first), lowest_ops[top_op].second);
        auto bin_expr = arena.emplace<BinExpr>();
        bin_expr->val = bin_op;
        bin_expr->lhs = {parse_binexpr_v(expr, lowest_ops[top_op].first)};
        bin_expr->rhs = {parse_binexpr_v(expr, lowest_ops[top_op].first, true)};
        return bin_expr;
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
                    std::cerr << "SyntaxError: Expected ';' on line " << std::endl;

                    std::visit(
                        [](auto&& arg)
                        {
                            if constexpr(std::is_same_v<decltype(arg), NodeExprIdent*>)
                            {
                                std::cout << arg.ident.line << std::endl;
                            }
                            else if constexpr(std::is_same_v<decltype(arg), NodeExprIInt*>)
                            {
                                std::cout << arg.i_int.line << std::endl;
                            }
                            else if constexpr(std::is_same_v<decltype(arg), BinExpr*>)
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
        int i = 1;
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
            i++;
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
