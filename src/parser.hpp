#ifndef PARSER_HPP
#define PARSER_HPP

#include "arena.hpp"
#include "nodes.hpp"
#include "token.hpp"
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <variant>

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
    else if(type == TokenType::_and)
    {
        return 1;
    }
    else if(type == TokenType::_or)
    {
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
        if(type == Type::_int) // Parse integer stuff
            return parse_binary_expr();
        else if(type == Type::_bool) // Parse boolean stuff
            return parse_logic_expr();
        else if(type == Type::_void) {} // not here yet
        return {};
    }

    inline std::optional<NodeExpr*> parse_logic_expr(int min_prec = 0)
    {
        // Parse for prefixed not operator
        if(auto not_ = try_take(TokenType::_not))
        {
            if(auto expr = parse_logic_expr()) {
                auto not_expr = arena.emplace<NodeLogicExprNot>(expr.value());
                auto logic_expr = arena.emplace<NodeLogicExpr>(not_expr);
                auto end_expr = arena.emplace<NodeExpr>(logic_expr);
                return end_expr;
            }
            else
            {
                std::cerr << "SyntaxError: Expected expression on line " << not_.value().line << std::endl;
                exit(EXIT_FAILURE);
            
            }
        }
        // Parse left hand side
        NodeLogicTerm* term_lhs = parse_logic_term();
        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs);

        // Parse the remainder
        while(true)
        {
            // Check what operator is next and take it
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

            // Parse the next expression
            auto rhs = parse_logic_expr(next_prec);
            if(!rhs.has_value())
            {
                std::cerr << "SyntaxError: Expected expression on line " << line << std::endl;
                exit(EXIT_FAILURE);
            }
            // Specific parsing for the operators
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
            else if(type == TokenType::_not)
            {
                auto not_ = arena.emplace<NodeLogicExprNot>(rhs.value());
                expr->val = not_;
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
        // Parse left hand side
        NodeBinTerm* term_lhs = parse_binary_term();
        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs);

        // Parse the remainder
        while(true)
        {
            // Check what operator is next and take it
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

            // Parse the next expression recursively
            auto rhs = parse_binary_expr(next_prec);
            if(!rhs.has_value())
            {
                std::cerr << "SyntaxError: Expected expression on line " << line << std::endl;
                exit(EXIT_FAILURE);
            }

            // Specific parsing for the operators
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

            // Try to parse the next statement
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
        if(!ended) // Check if the scope was closed
        {
            std::cerr << "SyntaxError: Expected '}' on line " << peek(-1).value().line << std::endl;
            exit(EXIT_FAILURE);
        }
        return scope;
    }

    inline NodeCompExpr* parse_comp_expr(Token left)
    {
        NodeCompExpr* comp = arena.emplace<NodeCompExpr>();
        if(peek().has_value() && peek(1).has_value())
        {
            Token op = take();
            Token right = take();
            NodeBinTerm* term_l = nullptr;
            if(left.type == TokenType::_int_lit)
            {
                auto term = arena.emplace<NodeIntLit>(left);
                term_l = arena.emplace<NodeBinTerm>(term);
            }
            else if(left.type == TokenType::_ident)
            {
                auto term = arena.emplace<NodeIdent>(left);
                term_l = arena.emplace<NodeBinTerm>(term);
            }
            else
            {
                std::cerr << "SyntaxError: Expected integer literal or identifier on line " << left.line << std::endl;
                exit(EXIT_FAILURE);
            }

            NodeBinTerm* term_r = nullptr;
            if(right.type == TokenType::_int_lit)
            {
                auto term = arena.emplace<NodeIntLit>(right);
                term_r = arena.emplace<NodeBinTerm>(term);
            }
            else if(right.type == TokenType::_ident)
            {
                auto term = arena.emplace<NodeIdent>(right);
                term_r = arena.emplace<NodeBinTerm>(term);
            }
            else
            {
                std::cerr << "SyntaxError: Expected integer literal or identifier on line " << right.line << std::endl;
                exit(EXIT_FAILURE);
            }

            if(op.type == TokenType::_eq)
            {
                auto eq = arena.emplace<NodeCompExprEq>(term_l, term_r);
                comp->val = eq;
            }
            else if(op.type == TokenType::_greater)
            {
                auto greater = arena.emplace<NodeCompExprGreater>(term_l, term_r);
                comp->val = greater;
            }
            else if(op.type == TokenType::_less)
            {
                auto less = arena.emplace<NodeCompExprLess>(term_l, term_r);
                comp->val = less;
            }
            else if(op.type == TokenType::_greater_eq)
            {
                auto greater_eq = arena.emplace<NodeCompExprGreaterEq>(term_l, term_r);
                comp->val = greater_eq;
            }
            else if(op.type == TokenType::_less_eq)
            {
                auto less_eq = arena.emplace<NodeCompExprLessEq>(term_l, term_r);
                comp->val = less_eq;
            }
            else if(op.type == TokenType::_neq)
            {
                auto neq = arena.emplace<NodeCompExprNeq>(term_l, term_r);
                comp->val = neq;
            }
            else
            {
                std::cerr << "SyntaxError: Expected comparison operator on line " << op.line << std::endl;
                exit(EXIT_FAILURE);
            }

            return comp;
        }
        else
        {
            std::cerr << "SyntaxError: Expected comparison expression on line " << left.line << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    inline NodeLogicTerm* parse_logic_term()
    {
        NodeLogicTerm* term = arena.emplace<NodeLogicTerm>();
        if(peek().has_value()) // Make sure there is a term
        {
            auto token = take();
            if(token.type == TokenType::_true || token.type == TokenType::_false) // Case boolean literal
            {
                auto boolean = arena.emplace<NodeBool>(token);
                term->val = boolean;
                return term;
            }
            else if(token.type == TokenType::_ident) // Case identifier
            {
                // First check if its a comparison expression
                if(auto next = peek())
                {
                    Token t = next.value();
                    if(t.type == TokenType::_eq || t.type == TokenType::_greater || t.type == TokenType::_less || t.type == TokenType::_neq || t.type == TokenType::_greater_eq || t.type == TokenType::_less_eq)
                    {
                        // Parse the comparison expression
                        auto comp = parse_comp_expr(token);
                        term->val = comp;
                        return term;
                    }
                }
                // Otherwise it's just an identifier
                auto ident = arena.emplace<NodeIdent>(token);
                term->val = ident;
                return term;
            }
            // Parse parenthesised expression
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
            else if(token.type == TokenType::_int_lit) // In this case it has to be a comparison => will change this to a 'can be compared' requirement in 3 years when I get to it
            {
                if(auto next = peek())
                {
                    Token t = next.value();
                    if(t.type == TokenType::_eq || t.type == TokenType::_greater || t.type == TokenType::_less || t.type == TokenType::_neq || t.type == TokenType::_greater_eq || t.type == TokenType::_less_eq)
                    {
                        // Parse the comparison expression
                        auto comp = parse_comp_expr(token);
                        term->val = comp;
                        return term;
                    }
                }
            }
        }

        std::cerr << "Error: Expected logic term on line " << peek(-1).value().line << std::endl;
        exit(EXIT_FAILURE);
    }

    inline NodeBinTerm* parse_binary_term()
    {
        NodeBinTerm* term = arena.emplace<NodeBinTerm>();
        if(peek().has_value()) // Make sure there is a term
        {
            auto token = take();
            if(token.type == TokenType::_int_lit) // Case int literal
            {
                auto int_lit = arena.emplace<NodeIntLit>(token);
                term->val = int_lit;
                return term;
            }
            else if(token.type == TokenType::_ident) // Case identifier
            {
                auto ident = arena.emplace<NodeIdent>(token);
                term->val = ident;
                return term;
            }
            else if(token.type == TokenType::_open_p) // Case parenthesised expression
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
            else if(auto _if = try_take(TokenType::_if))
            {
                if(auto open_p = try_take(TokenType::_open_p))
                {
                    if(auto expr = parse_logic_expr())
                    {
                        if(auto close_p = try_take(TokenType::_close_p))
                        {
                            if(auto bracket = try_take(TokenType::_open_b))
                            {
                                if(auto scope = parse_scope())
                                {
                                    auto if_ = arena.emplace<NodeIf>(expr.value(), scope);
                                    auto stmt = arena.emplace<NodeStmt>(if_);
                                    return stmt;
                                }
                                else
                                {
                                    std::cerr << "SyntaxError: Expected scope on line " << peek().value().line << std::endl;
                                    exit(EXIT_FAILURE);
                                }
                            }
                            else
                            {
                                std::cerr << "SyntaxError: Expected '{' on line " << peek().value().line << std::endl;
                                exit(EXIT_FAILURE);
                            }
                        }
                        else
                        {
                            std::cerr << "SyntaxError: Expected ')' on line " << peek().value().line << std::endl;
                            exit(EXIT_FAILURE);
                        }
                    }
                    else
                    {
                        std::cerr << "SyntaxError: Expected expression on line " << peek().value().line << std::endl;
                        exit(EXIT_FAILURE);
                    }
                }
                else
                {
                    std::cerr << "SyntaxError: Expected '(' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
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
        if(peek().has_value() && peek().value().type == TokenType::_int_lit)
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
