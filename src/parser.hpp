#ifndef PARSER_HPP
#define PARSER_HPP

#pragma once
#include "arena.hpp"
#include "nodes.hpp"
#include "tokens.hpp"
#include "type.hpp"
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <variant>

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

    inline std::optional<NodeExpr*> parse_expr(GeneralType type)
    {
        if(type == TypeControl::_int) // Parse integer stuff
            return parse_binary_expr();
        else if(type == TypeControl::_bool) // Parse boolean stuff
            return parse_logic_expr();
        else if(type == TypeControl::_void) {} // nothing here yet
        return {};
    }

    inline std::optional<NodeExpr*> parse_logic_expr(int min_prec = 0)
    {
        // Parse for prefixed not operator
        if(auto not_ = try_take(TokenType::_not))
        {
            if(auto expr = parse_logic_expr())
            {
                auto not_expr = arena.emplace<NodeLogicExprNot>(expr.value());
                auto logic_expr = arena.emplace<NodeLogicExpr>(not_expr);
                auto end_expr = arena.emplace<NodeExpr>(logic_expr);
                return end_expr;
            }
            else
            {
                std::cerr << "SyntaxError: Expected logic expression on line " << not_.value().line << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        // Parse left hand side
        auto term_lhs = parse_logic_term();
        if(!term_lhs.has_value())
        {
            std::cerr << "SyntaxError: Expected logic expression on line " << peek().value().line << std::endl;
            exit(EXIT_FAILURE);
        }
        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs.value());

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
                std::cerr << "SyntaxError: Expected logic expression on line " << line << std::endl;
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
        auto term_lhs = parse_binary_term();
        if(!term_lhs.has_value())
        {
            std::cerr << "SyntaxError: Expected binary expression on line " << peek().value().line << std::endl;
            exit(EXIT_FAILURE);
        }
        NodeExpr* expr_lhs = arena.emplace<NodeExpr>(term_lhs.value());

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
                std::cerr << "SyntaxError: Expected binary expression on line " << line << std::endl;
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
                std::cerr << "Invalid statement on line " << peek().value().line << std::endl;
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
    inline std::string parse_member(std::string mems = "", bool can_end = false)
    {
        if(auto id = try_take(TokenType::_ident))
        {
            return id->val.value() + parse_member("", true);
        }
        else if(auto dot = try_take(TokenType::_dot))
        {
            return "." + parse_member("");
        }
        if(can_end)
        {
            return "";
        }
        std::cerr << "SyntaxError: Expected identifier or '.' on line " << peek(-1)->line << std::endl;
        exit(EXIT_FAILURE);
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

    inline std::optional<NodeLogicTerm*> parse_logic_term()
    {
        NodeLogicTerm* term = arena.emplace<NodeLogicTerm>();
        if(peek().has_value()) // Make sure there is a term
        {
            auto token = take();
            if(token.type == TokenType::_true || token.type == TokenType::_false) // Case boolean literal
            {
                take();
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
                    if(t.type == TokenType::_dot)
                    {
                        take();
                        token.val.value() += "." + parse_member();
                        if(auto n = peek())
                            t = n.value();
                    }
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
                    std::cerr << "SyntaxError: Expected logic expression on line " << token.line << std::endl;
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

        return {};
    }

    inline std::optional<NodeBinTerm*> parse_binary_term()
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
                if(peek().has_value() && peek().value().type == TokenType::_dot)
                {
                    take();
                    token.val.value() += "." + parse_member();
                }
                auto ident = arena.emplace<NodeIdent>(token);
                term->val = ident;
                return term;
            }
            else if(token.type == TokenType::_open_p) // Case parenthesised expression
            {
                auto expr = parse_binary_expr();
                if(!expr.has_value())
                {
                    std::cerr << "SyntaxError: Expected binary expression on line " << token.line << std::endl;
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
        return {};
    }

    inline std::optional<NodeStmtStructMove*> parse_inline_ud_decl(UDType type)
    {
        if(auto open = try_take(TokenType::_open_b))
        {
            std::vector<std::variant<NodeExpr*, NodeStmtStructMove*>> var;
            int members = type.members.size();
            var.reserve(members);
            for(int i = 0; i < members; i++)
            {
                std::string member_name = type.member_names.at(i);
                GeneralType* t = type.members.at(member_name);
                UDType* member_type = dynamic_cast<UDType*>(t);
                if(member_type == nullptr)
                {
                    if(auto expr = parse_expr(*t))
                    {
                        var.emplace_back(expr.value());
                    }
                }
                else
                {
                    if(auto expr = parse_inline_ud_decl(*member_type))
                    {
                        var.emplace_back(expr.value());
                    }
                }

                auto p = try_take(TokenType::_comma);
                if(!p && i != members - 1)
                {
                    std::cerr << "SyntaxError: Expected ',' on line " << open->line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }

            if(auto close = try_take(TokenType::_close_b))
            {
                auto move_struct = arena.emplace<NodeStmtStructMove>(type, std::move(var));
                return move_struct;
            }
            else
            {
                std::cerr << "SyntaxError: Expected '}'" << std::endl;
                exit(EXIT_FAILURE);
            }
        }

        return {};
    }

    inline std::optional<NodeStmtStructDecl*> parse_ud_declaration()
    {
        if((peek().value().type == TokenType::_ident) && peek(1).has_value() && peek(1).value().type == TokenType::_ident && peek(2).has_value() && peek(2).value().type == TokenType::_open_b)
        {
            const UDType* type = dynamic_cast<const UDType*>(TypeControl::GetInstance()->FindType(take().val.value()));
            if(type == nullptr)
            {
                std::cerr << "TypeError: Type '" << type->name << "' does not exist" << std::endl;
                exit(EXIT_FAILURE);
            }
            Token name = take(); // take name
            vars.insert({name.val.value(), type});
            if(auto str = parse_inline_ud_decl(*type))
            {
                if(!semicolon())
                {
                    std::cerr << "SyntaxError: Expected ';' on line" << std::endl;
                    exit(EXIT_FAILURE);
                }
                auto struct_ = arena.emplace<NodeStmtStructDecl>(name, str.value());
                return struct_;
            }
            std::cerr << "SyntaxError: Expected struct initialisation on line " << peek(-1)->line << std::endl;
            exit(EXIT_FAILURE);
        }

        return {};
    }

    inline std::optional<NodeStmtVarDecl*> parse_declaration()
    {
        if((peek().value().type == TokenType::_type) && peek(1).has_value() && peek(1).value().type == TokenType::_ident && peek(2).has_value() && peek(2).value().type == TokenType::_assign)
        {
            const GeneralType* type = TypeControl::GetInstance()->FindType(take().val.value());
            if(type == nullptr)
            {
                std::cerr << "TypeError: Type '" << type->name << "' does not exist" << std::endl;
                exit(EXIT_FAILURE);
            }

            auto stmt_var = arena.emplace<NodeStmtVarDecl>(take(), type);
            take(); // take '=' operator
            if(auto expr = parse_expr(*type))
            {
                stmt_var->expr = expr.value();
                vars.insert({stmt_var->ident.val.value(), type});
            }
            else
            {
                std::cerr << "SyntaxError: Expected a value of type '" << type->name << "' but got nothing" << std::endl;
                exit(EXIT_FAILURE);
            }
            if(!semicolon())
            {
                std::cerr << "SyntaxError: Expected ';' on line ";

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
            return stmt_var;
        }
        return {};
    }

    inline std::optional<NodeStmt*> parse_stmt()
    {
        if(peek().has_value())
        {
            if(peek().value().type == TokenType::_ret)
            {
                take();
                auto ret = arena.emplace<NodeInternalRet>();
                if(auto expr = parse_expr(*TypeControl::GetInstance()->FindType("int")))
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
            else if(parse_struct())
            {
                return arena.emplace<NodeStmt>(true);
            }
            else if(auto stmt_var = parse_declaration())
            {
                return arena.emplace<NodeStmt>(stmt_var.value());
            }
            else if(peek().value().type == TokenType::_print && peek(1).value().type == TokenType::_open_p)
            {
                take();
                take();
                auto stmt_prt = arena.emplace<NodeInternalPrintf>();
                if(auto expr = parse_expr(TypeControl::_void))
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
            else if(auto _if = parse_if())
            {
                auto stmt = arena.emplace<NodeStmt>(_if.value());
                return stmt;
            }
            else if(auto assign = parse_assign())
            {
                auto stmt = arena.emplace<NodeStmt>(assign.value());
                return stmt;
            }
            else if(auto cont = try_take(TokenType::_continue))
            {
                auto flow = arena.emplace<NodeLoopFlow>(NodeLoopFlow::CONTINUE);
                auto internal = arena.emplace<NodeInternal>(flow);
                auto stmt = arena.emplace<NodeStmt>(internal);
                if(semicolon())
                {
                    return stmt;
                }
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            else if(auto brk = try_take(TokenType::_break))
            {
                auto flow = arena.emplace<NodeLoopFlow>(NodeLoopFlow::BREAK);
                auto internal = arena.emplace<NodeInternal>(flow);
                auto stmt = arena.emplace<NodeStmt>(internal);
                if(semicolon())
                {
                    return stmt;
                }
                else
                {
                    std::cerr << "SyntaxError: Expected ';' on line " << peek().value().line << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            else if(auto _while = parse_while())
            {
                auto stmt = arena.emplace<NodeStmt>(_while.value());
                return stmt;
            }
            else if(auto _ud_decl = parse_ud_declaration())
            {
                auto stmt = arena.emplace<NodeStmt>(_ud_decl.value());
                return stmt;
            }
        }

        return {};
    }

    inline std::optional<NodeStmtAssign*> parse_assign()
    {
        if(peek().value().type == TokenType::_ident)
        {
            Token ident;
            std::string t = "";
            if(peek(1).has_value() && peek(1).value().type == TokenType::_dot)
            {
                ident = take();
                t = parse_member();
            }
            else
            {
                if(peek(1).has_value() && peek(1).value().type != TokenType::_assign)
                {
                    return {};
                }
                else
                {
                    ident = take();
                }
            }

            ident.val.value() += t;
            std::vector<std::string> ss = split(ident.val.value(), ".");
            const GeneralType* type = vars.at(ss.at(0));
            if(type == nullptr)
            {
                std::cerr << "Type does not exist on line " << ident.line << std::endl;
                exit(EXIT_FAILURE);
            }
            const UDType* stype = dynamic_cast<const UDType*>(type);
            if(peek().has_value() && peek().value().type == TokenType::_assign)
            {
                take(); // take '='
                if(stype == nullptr)
                {
                    auto assign = arena.emplace<NodeStmtAssign>();
                    assign->ident = ident;
                    if(vars.find(ident.val.value()) == vars.end())
                    {
                        std::cerr << "SyntaxError: Variable '" << ident.val.value() << "' is not defined" << std::endl;
                        exit(EXIT_FAILURE);
                    }
                    const GeneralType* type = vars.at(ident.val.value());
                    if(auto parse = parse_expr(*type))
                    {
                        assign->expr = parse.value();
                        switch(assign->expr.index())
                        {
                        case 0: // NodeExpr*
                            {
                                std::cerr << "parser 780\n";
                                NodeExpr* expr = std::get<NodeExpr*>(assign->expr);
                                expr->type = type;
                            }
                            // other case doesn't exist since it will be a UDType
                        }
                    }
                    else
                    {
                        std::cerr << "SyntaxError: Expected expression on line " << peek().value().line << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    if(!semicolon())
                    {
                        std::cerr << "SyntaxError: Expected ';' on line " << peek().value().line << std::endl;
                        exit(EXIT_FAILURE);
                    }
                    return assign;
                }
                else
                {
                    auto node = arena.emplace<NodeStmtAssign>();
                    node->ident = ident;
                    UDType* t = nullptr;
                    for(int i = 1; i < ss.size(); i++)
                    {
                        GeneralType* x = stype->members.at(ss[i]);
                        t = dynamic_cast<UDType*>(x);
                        if(t == nullptr)
                        {
                            if(auto expr = parse_expr(*x))
                            {
                                node->expr = expr.value();
                                node->members = ident.val.value();
                                break;
                            }
                            std::cerr << "Failed parsing expression of type " << x->name << std::endl;
                            exit(EXIT_FAILURE);
                        }
                        stype = t;
                    }

                    if(!semicolon())
                    {
                        std::cerr << "SyntaxError: Expected ';' on line " << peek(-1).value().line << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    return node;
                }
            }
            else
            {
                std::cerr << "SyntaxError: Expected '=' on line " << peek().value().line << std::endl;
                exit(EXIT_FAILURE);
            }
        }

        return {};
    }

    inline std::optional<NodeWhile*> parse_while()
    {
        if(peek().has_value() && peek().value().type == TokenType::_while)
        {
            take();
            if(auto open_p = try_take(TokenType::_open_p))
            {
                if(auto expr = parse_expr(TypeControl::_bool))
                {
                    if(auto close_p = try_take(TokenType::_close_p))
                    {
                        if(auto bracket = try_take(TokenType::_open_b))
                        {
                            if(auto scope = parse_scope())
                            {
                                auto node_while = arena.emplace<NodeWhile>();
                                node_while->cond = expr.value();
                                node_while->scope = scope;
                                return node_while;
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

        return {};
    }

    bool parse_struct()
    {
        if(peek().value().type == TokenType::_struct)
        {
            take();
            // Name of the struct
            auto i = take();
            std::string name;
            if(i.type == TokenType::_ident)
            {
                name = i.val.value();
            }
            else
            {
                std::cerr << "SyntaxError: Expected identifier for struct on line " << i.line << std::endl;
                exit(EXIT_FAILURE);
            }
            int size = 0;
            if(auto token = try_take(TokenType::_open_b))
            {
                std::map<std::string, GeneralType*> members;
                GeneralType const* type = &TypeControl::GetInstance()->_void;
                // get variables in the struct
                while(peek().has_value() && peek().value().type != TokenType::_close_b)
                {
                    auto tok = take();
                    if(tok.type == TokenType::_type)
                    {
                        type = TypeControl::GetInstance()->FindType(tok.val.value());
                    }
                    else if(tok.type == TokenType::_ident)
                    {
                        auto t = TypeControl::GetInstance()->FindType(tok.val.value());
                        if(t != nullptr)
                        {
                            type = t;
                        }
                    }
                    else
                    {
                        std::cerr << "SyntaxError: Expected a type identifier on line " << tok.line << std::endl;
                        exit(EXIT_FAILURE);
                    }
                    std::string varname;
                    int lc;
                    size += type->bytes;
                    if(auto mname = try_take(TokenType::_ident))
                    {
                        varname = mname.value().val.value();

                        lc = mname.value().line;
                    }
                    else
                    {
                        std::cerr << "SyntaxError: Expected identifier" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    if(!semicolon())
                    {
                        std::cerr << "SyntaxError: Expected ';' on line " << lc;
                        exit(EXIT_FAILURE);
                    }

                    members.insert({varname, (GeneralType*)(type)});
                }
                if(auto close_b = try_take(TokenType::_close_b))
                {
                    for(auto& t : members) {}
                    UDType* t = new UDType{name, size, members};
                    if(!semicolon())
                    {
                        std::cerr << "SyntaxError: Expected ';' on line " << close_b.value().line;
                        exit(EXIT_FAILURE);
                    }

                    TypeControl::GetInstance()->RegisterType(*t);
                    return true;
                }

                std::cerr << "SyntaxError: failed parsing struct" << std::endl;
                exit(EXIT_FAILURE);
            }
        }

        return false;
    }

    // Refactor this to be more readable and probably split if, else and else if into seperate functions
    inline std::optional<NodeIf*> parse_if(bool chained = false)
    {

        if(auto _if = try_take(TokenType::_if))
        {
            std::vector<NodeIf*> nested_ifs;
            auto node_if = arena.emplace<NodeIf>();
            if(auto open_p = try_take(TokenType::_open_p))
            {
                if(auto expr = parse_logic_expr())
                {
                    node_if->cond = expr.value();
                    if(auto close_p = try_take(TokenType::_close_p))
                    {
                        if(auto bracket = try_take(TokenType::_open_b))
                        {
                            if(auto scope = parse_scope())
                            {
                                node_if->scope = scope;
                                while(peek().has_value() && peek().value().type == TokenType::_else && !chained)
                                {
                                    take();
                                    if(auto _nested = parse_if(true))
                                    {
                                        nested_ifs.emplace_back(_nested.value());
                                    }
                                    else
                                    {
                                        if(auto bracket_ = try_take(TokenType::_open_b))
                                        {
                                            if(auto scope_ = parse_scope())
                                            {
                                                node_if->else_stmts = scope_;
                                            }
                                            else
                                            {
                                                std::cerr << "SyntaxError: Expected '}' on line " << bracket_.value().line << std::endl;
                                                exit(EXIT_FAILURE);
                                            }
                                        }
                                        else
                                        {
                                            std::cerr << "SyntaxError: Expected '{' on line " << peek().value().line << std::endl;
                                            exit(EXIT_FAILURE);
                                        }
                                    }
                                }

                                node_if->elseif_stmts = nested_ifs;
                                return node_if;
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
                std::cerr << "Invalid statement on line " << peek().value().line << std::endl;
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
    std::map<std::string, const GeneralType*> vars;

    inline std::optional<Token> peek(const int offset = 0) const
    {
        if(index + offset >= tokens.size())
        {
            return {};
        }

        return tokens.at(index + offset);
    }

    inline Token take() { return tokens.at(index++); }

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

    inline std::vector<std::string> split(std::string s, const std::string& delimiter)
    {
        std::vector<std::string> tokens;
        size_t pos = 0;
        std::string token;
        while((pos = s.find(delimiter)) != std::string::npos)
        {
            token = s.substr(0, pos);
            tokens.push_back(token);
            s.erase(0, pos + delimiter.length());
        }
        tokens.push_back(s);

        return tokens;
    }
};

#endif // PARSER_HPP
