#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "nodes.hpp"
#include "type.hpp"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>

class Generator {
  public:
    explicit Generator(const NodeProg* root) : root(root) {}

    inline std::string gen_prog()
    {
        ext << "global main\nsection .text\n";
        code << "main:\n";
        push("rbp");
        code << "    mov rbp, rsp\n\n";
        data << "default rel\n";
        data << "section .data\n";
        for(int i = 0; i < root->stmts.size(); i++) { gen_stmt(root->stmts.at(i)); }
        data << "\n";
        data << ext.str() << init.str() << code.str();
        return data.str();
    }

    inline void gen_stmt(const NodeStmt* stmt, size_t loop_id = 0)
    {

        switch(stmt->var.index())
        {
        case 0: // Internal things
            {
                std::cerr << "38\n";
                NodeInternal* var = std::get<NodeInternal*>(stmt->var);
                gen_internal(var, loop_id);
                break;
            }
        case 1: // Declaration
            {
                std::cerr << "45\n";
                NodeStmtVarDecl* var = std::get<NodeStmtVarDecl*>(stmt->var);
                if(is_declared(var->ident.val.value()))
                {
                    std::cerr << "Error: Identifier '" << var->ident.val.value() << "' has already been declared on line " << get_var(var->ident.val.value()).line << ", but was declared again on line " << var->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                code << "    ;; Declaring variable " << '\'' << var->ident.val.value() << '\'' << " of type " << var->type->name << '\n';
                vars.push_back({var->ident.val.value(), sp, var->type, var->ident.line});
                gen_expr(var->expr);
                break;
            }
        case 2: // Scope
            {
                std::cerr << "59\n";
                NodeScope* scope = std::get<NodeScope*>(stmt->var);
                gen_scope(scope);
                break;
            }
        case 3: // If
            {
                std::cerr << "66\n";
                NodeIf* ifstmt = std::get<NodeIf*>(stmt->var);
                gen_if(ifstmt);
                break;
            }
        case 4: // Assignment/Reassignment
            {
                std::cerr << "73\n";
                NodeStmtAssign* assign = std::get<NodeStmtAssign*>(stmt->var);
                if(!is_declared(assign->ident.val.value()))
                {
                    std::cerr << "Error: Identifier '" << assign->ident.val.value() << "' has not been declared on line " << assign->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }

                switch(assign->expr.index())
                {
                case 0: // NodeExpr*
                    {
                        std::cerr << "85\n";
                        NodeExpr* expr = std::get<NodeExpr*>(assign->expr);
                        if(!assign->members)
                        {
                            const Var& var = get_var(assign->ident.val.value());
                            if(var.type != expr->type.value())
                            {
                                std::cerr << "Error: Identifier '" << assign->ident.val.value() << "' is not of type '" << expr->type.value()->name << "' on line " << assign->ident.line << std::endl;
                                exit(EXIT_FAILURE);
                            }

                            code << "    ;; Reassigning variable\n";
                            gen_expr(expr);
                            pop("rax");
                            code << "    mov [rsp + " << (sp - var.stackl - 1) * 8 << "], rax\n\n";
                        }
                        else
                        {
                            // this is a member variable
                            auto member_names = split(assign->members.value(), ".");
                            const Chunk& chunk = get_struct(assign->ident.val.value());
                            int offset = 0;
                            for(std::string& s : member_names)
                            {
                                const GeneralType* t = TypeControl::GetInstance()->FindType(s);
                                const UDType* udt = dynamic_cast<const UDType*>(t);
                                if(udt != nullptr)
                                {
                                    offset += udt->offsets.at(s);
                                }
                            }
                            code << "    ;; Reassigning variable\n";
                            gen_expr(expr);
                            pop("rax");
                            code << "    mov [rsp + " << (sp - chunk.start_stackl + offset - 1) * 8 << "], rax\n\n";
                        }
                        break;
                    }
                case 1: // NodeStmtStructMove
                    {
                        const Chunk& chunk = get_struct(assign->ident.val.value());
                        std::cerr << "126\n";
                        NodeStmtStructMove* move = std::get<NodeStmtStructMove*>(assign->expr);
                        if(*chunk.type != move->type)
                        {
                            std::cerr << "Error: Identifier '" << assign->ident.val.value() << "' is not of type '" << move->type.name << "' on line " << assign->ident.line << std::endl;
                            exit(EXIT_FAILURE);
                        }
                        int _sp = sp;
                        gen_struct_move(move);
                        _sp = sp - _sp;

                        break;
                    }
                default:
                    break;
                }
                break;
            }
        case 5: // While loop
            {
                std::cerr << "143\n";
                NodeWhile* _while = std::get<NodeWhile*>(stmt->var);
                gen_while(_while);
                break;
            }
        case 6: // NodeStmtStructDecl*
            {
                std::cerr << "151\n";
                NodeStmtStructDecl* s_assign = std::get<NodeStmtStructDecl*>(stmt->var);
                gen_struct_expr(s_assign);
                break;
            }
        default:
            break;
        }
    }

    inline void gen_struct_expr(const NodeStmtStructDecl* decl)
    {
        struct_vars.emplace_back(decl->ident.val.value(), sp, (size_t)decl->init->type.bytes, &decl->init->type, decl->ident.line);
        gen_struct_move(decl->init);
    }

    inline void gen_struct_move(const NodeStmtStructMove* smove)
    {
        for(int i = 0; i < smove->exprs.size(); i++)
        {
            auto item = smove->exprs.at(i);

            switch(item.index())
            {
            case 0: // NodeExpr*
                {
                    std::cerr << "173\n";
                    NodeExpr* expr = std::get<NodeExpr*>(item);
                    gen_expr(expr);
                    break;
                }
            case 1: // NodeStmtStructMove*
                {
                    std::cerr << "180\n";
                    NodeStmtStructMove* move = std::get<NodeStmtStructMove*>(item);
                    gen_struct_move(move);
                    break;
                }
            }
        }
    }

    inline void gen_if(const NodeIf* ifstmt)
    {
        code << "    ;; If statement\n";
        gen_expr(ifstmt->cond);
        pop("rax");
        code << "    cmp rax, 0\n";
        std::string label_end = "if_" + std::to_string(if_labels) + "_end";
        std::string label_else = "if_" + std::to_string(if_labels) + "_else";
        if(ifstmt->elseif_stmts.size() == 0)
        {
            code << "    ;; Else statement\n";
            code << "    je " << label_else << "\n";
            gen_scope(ifstmt->scope);
            code << "    jmp " << label_end << "\n";
            if(ifstmt->else_stmts.has_value())
            {
                code << label_else << ":\n\n";
                gen_scope(ifstmt->else_stmts.value());
            }
            code << "    ;; End if statement\n";
            code << label_end << ":\n";
            return;
        }

        for(int i = 0; i < ifstmt->elseif_stmts.size(); i++)
        {

            auto iff = ifstmt->elseif_stmts.at(i);
            std::string label_elseif = "if_" + std::to_string(if_labels) + "_elseif_" + std::to_string(i);
            std::string label_next = "if_" + std::to_string(if_labels) + "_elseif_" + std::to_string(i + 1);
            if(i == 0)
            {
                code << "    je " << label_elseif << "\n";
                code << "    ;; First if statement\n";
                gen_scope(ifstmt->scope);
                code << "    jmp " << label_end << "\n";
                code << label_elseif << ":\n";
            }
            else
            {
                code << label_elseif << ":\n";
            }
            code << "    ;; Begin elseif condition\n";
            gen_expr(iff->cond);
            pop("rax");
            code << "    cmp rax, 0\n";
            if(i < ifstmt->elseif_stmts.size() - 1)
            {
                code << "    je " << label_next << "\n";
            }
            else
            {
                if(ifstmt->else_stmts.has_value())
                {
                    code << "    je " << label_else << "\n";
                }
                else
                {
                    code << "    je " << label_end << "\n";
                }
            }

            code << "    ;; End elseif condition\n";

            gen_scope(iff->scope);
            code << "    jmp " << label_end << "\n";
        }

        if(ifstmt->else_stmts.has_value())
        {
            code << "    ;; Else statement\n";
            code << label_else << ":\n\n";
            gen_scope(ifstmt->else_stmts.value());
        }

        code << "    ;; End if statement\n";
        code << label_end << ":\n";
    }

    inline void gen_while(const NodeWhile* _while)
    {
        code << "    ;; While statement\n";
        std::string label_begin = "while_" + std::to_string(loop_labels) + "_begin";
        std::string label_end = "while_" + std::to_string(loop_labels) + "_end";
        code << label_begin << ":\n";
        gen_expr(_while->cond);
        pop("rax");
        code << "    cmp rax, 0\n";
        code << "    jz " << label_end << "\n";
        gen_scope(_while->scope, loop_labels++);
        code << "    jmp " << label_begin << "\n";
        code << label_end << ":\n";
    }

    inline void gen_scope(const NodeScope* scope, size_t loop_id = 0)
    {
        code << "    ;; Entering scope\n";
        size_t vars_size = vars.size();
        for(int i = 0; i < scope->stmts.size(); i++) { gen_stmt(scope->stmts.at(i), loop_id); }
        size_t pop = vars.size() - vars_size;
        if(pop > 0)
        {
            code << "    ;; Exiting scope\n";
            code << "    add rsp, " << pop * 8 << "\n\n";
            sp -= pop;
        }
        for(int i = 0; i < pop; i++) { vars.pop_back(); }
        code << "    ;; Exiting scope\n";
    }

    inline void gen_expr(const NodeExpr* expr)
    {
        if(expr->var.index() == 0) // BinTerm
        {
            std::cerr << "303\n";
            NodeBinTerm* term = std::get<NodeBinTerm*>(expr->var);
            gen_binary_term(term);
        }
        else if(expr->var.index() == 1) // BinExpr
        {
            std::cerr << "309\n";
            NodeBinExpr* binexpr = std::get<NodeBinExpr*>(expr->var);
            gen_binary_expression(binexpr);
        }
        else if(expr->var.index() == 2) // LogicTerm
        {
            std::cerr << "315\n";
            NodeLogicTerm* term = std::get<NodeLogicTerm*>(expr->var);
            gen_logic_term(term);
        }
        else if(expr->var.index() == 3) // LogicExpr
        {
            std::cerr << "321\n";
            NodeLogicExpr* logic = std::get<NodeLogicExpr*>(expr->var);
            gen_logic_expression(logic);
        }
        else if(expr->var.index() == 4) // CompExpr
        {
            std::cerr << "327\n";
            NodeCompExpr* comp = std::get<NodeCompExpr*>(expr->var);
            gen_comp_expression(comp);
        }
    }

    inline void gen_binary_term(const NodeBinTerm* term)
    {
        if(term->val.index() == 0) // Identifier
        {
            std::cerr << "338\n";
            NodeIdent* ident = std::get<NodeIdent*>(term->val);

            if(ident->ident.val.value().find(".") != std::string::npos)
            {
                std::vector<std::string> ss = split(ident->ident.val.value(), ".");
                if(!is_declared(ss.at(0)))
                {
                    std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const Chunk& ch = get_struct(ss.at(0));
                int offset = 0;
                const UDType* t = nullptr;

                const UDType* type = dynamic_cast<const UDType*>(ch.type);
                for(int i = 1; i < ss.size(); i++)
                {
                    const GeneralType* x = type->members.at(ss[i]);
                    t = dynamic_cast<const UDType*>(x);
                    if(t == nullptr)
                    {
                        if(*x != TypeControl::_int && *x != TypeControl::_bool)
                        {
                            std::cerr << "Error: Member variable '" << ident->ident.val.value() << "' is not of type 'int' or 'bool' on line " << ident->ident.line << std::endl;
                            exit(EXIT_FAILURE);
                        }
                    }

                    offset += type->offsets.at(ss[i]);

                    if(t != nullptr)
                        type = t;
                }
                std::stringstream offs;
                code << "    ;; Loading variable '" << ch.name << "'\n";
                offs << "QWORD [rsp + " << (sp - (sp - ch.start_stackl + (offset / 8)) - 1) * 8 << "]";
                push(offs.str());
                code << "\n";
            }
            else
            {
                if(!is_declared(ident->ident.val.value()))
                {
                    std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const Var& var = get_var(ident->ident.val.value());
                if(*var.type != TypeControl::_int && *var.type != TypeControl::_bool)
                {
                    std::cerr << "Error: Identifier '" << ident->ident.val.value() << "' is not of type 'int' or 'bool' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                std::stringstream offset;
                code << "    ;; Loading variable '" << var.name << "'\n";
                offset << "QWORD [rsp + " << (sp - var.stackl - 1) * 8 << "]";
                push(offset.str());
                code << "\n";
            }
        }
        else if(term->val.index() == 1) // Immediate Int
        {
            std::cerr << "389\n";
            NodeIntLit* i_int = std::get<NodeIntLit*>(term->val);
            code << "    mov rax, " << i_int->i_int.val.value() << "\n";
            push("rax");
            code << "\n";
        }
        else if(term->val.index() == 2) // Parentheses
        {
            std::cerr << "397\n";
            NodeTermParens* paren = std::get<NodeTermParens*>(term->val);
            gen_expr(paren->expr);
        }
    }

    inline void gen_internal(const NodeInternal* internal, size_t loop_id)
    {
        size_t val = internal->internal.index();
        switch(val)
        {
        case 0: // Return
            {
                std::cerr << "410\n";
                NodeInternalRet* ret = std::get<NodeInternalRet*>(internal->internal);
                code << "    ;; Return\n";
                gen_expr(ret->ret);
                pop("rdi");
                if(sp % 2 != 0)
                    code << "    add rsp, " << (sp) * 8 << "\n";
                else
                    code << "    add rsp, " << (sp - 1) * 8 << "\n";
                code << "    mov rsp, rbp\n";
                pop("rbp");
                code << "    mov rax, 60\n";
                code << "    syscall\n";
                code << "\n";
                break;
            }
        case 1: // Printf
            {
                std::cerr << "428\n";
                NodeInternalPrintf* print = std::get<NodeInternalPrintf*>(internal->internal);
                code << "    ;; Printf\n";
                break;
            }
        case 2: // Loop Flow
            {
                std::cerr << "435\n";
                NodeLoopFlow* flow = std::get<NodeLoopFlow*>(internal->internal);
                switch(*flow)
                {
                case NodeLoopFlow::CONTINUE:
                    code << "   jmp loop_" << loop_id << "_begin\n";
                    break;
                case NodeLoopFlow::BREAK:
                    code << "    jmp loop_" << loop_id << "_end\n";
                    break;
                default:
                    std::cerr << "Error: Unknown loop flow" << std::endl;
                    exit(EXIT_FAILURE);
                }
                break;
            }
        default:
            {
                std::cerr << "Error: Unknown internal" << std::endl;
                exit(EXIT_FAILURE);
                break;
            }
        }
    }

    inline void gen_binary_expression(const NodeBinExpr* binexpr)
    {
        if(binexpr->val.index() == 0) // Add
        {
            std::cerr << "464\n";
            NodeBinExprAdd* add = std::get<NodeBinExprAdd*>(binexpr->val);
            gen_expr(add->lhs);
            gen_expr(add->rhs);
            pop("rdi");
            pop("rax");
            code << "    add rax, rdi\n";
            push("rax");
            code << "\n";
        }
        else if(binexpr->val.index() == 1) // Division
        {
            std::cerr << "476\n";
            NodeBinExprDiv* div = std::get<NodeBinExprDiv*>(binexpr->val);
            gen_expr(div->lhs);
            gen_expr(div->rhs);
            pop("rdi");
            pop("rax");
            code << "    cqo\n";
            code << "    idiv rdi\n";
            push("rax");
            code << "\n";
        }
        else if(binexpr->val.index() == 2) // Subtraction
        {
            std::cerr << "489\n";
            NodeBinExprSub* sub = std::get<NodeBinExprSub*>(binexpr->val);
            gen_expr(sub->lhs);
            gen_expr(sub->rhs);
            pop("rdi");
            pop("rax");
            code << "    sub rax, rdi\n";
            push("rax");
            code << "\n";
        }
        else if(binexpr->val.index() == 3) // Multiplication
        {
            std::cerr << "501\n";
            NodeBinExprMult* mul = std::get<NodeBinExprMult*>(binexpr->val);
            gen_expr(mul->lhs);
            gen_expr(mul->rhs);
            pop("rdi");
            pop("rax");
            code << "    imul rax, rdi\n";
            push("rax");
            code << "\n";
        }
        else
        {
            std::cerr << "Error: Unknown binary expression" << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    inline void gen_comp_expression(const NodeCompExpr* comp)
    {
        if(comp->val.index() == 0) // Equal
        {
            std::cerr << "522\n";
            NodeCompExprEq* eq = std::get<NodeCompExprEq*>(comp->val);
            gen_binary_term(eq->lhs);
            gen_binary_term(eq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    sete al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 1) // Greater
        {
            std::cerr << "536\n";
            NodeCompExprGreater* greater = std::get<NodeCompExprGreater*>(comp->val);
            gen_binary_term(greater->lhs);
            gen_binary_term(greater->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    setg al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 2) // Less
        {
            std::cerr << "550\n";
            NodeCompExprLess* less = std::get<NodeCompExprLess*>(comp->val);
            gen_binary_term(less->lhs);
            gen_binary_term(less->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    setl al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 3) // Greater or Equal
        {
            std::cerr << "564\n";
            NodeCompExprGreaterEq* greater_eq = std::get<NodeCompExprGreaterEq*>(comp->val);
            gen_binary_term(greater_eq->lhs);
            gen_binary_term(greater_eq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    setge al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 4) // Less or Equal
        {
            std::cerr << "578\n";
            NodeCompExprLessEq* less_eq = std::get<NodeCompExprLessEq*>(comp->val);
            gen_binary_term(less_eq->lhs);
            gen_binary_term(less_eq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    setle al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 5) // Not Equal
        {
            std::cerr << "592\n";
            NodeCompExprNeq* neq = std::get<NodeCompExprNeq*>(comp->val);
            gen_binary_term(neq->lhs);
            gen_binary_term(neq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rax, rdi\n";
            code << "    setne al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
    }

    inline void gen_logic_term(const NodeLogicTerm* term)
    {
        if(term->val.index() == 0) // Identifier
        {
            std::cerr << "610\n";
            NodeIdent* ident = std::get<NodeIdent*>(term->val);

            if(ident->ident.val.value().find(".") != std::string::npos)
            {
                std::vector<std::string> ss = split(ident->ident.val.value(), ".");
                if(!is_declared(ss.at(0)))
                {
                    std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const Chunk& ch = get_struct(ss.at(0));
                int offset = 0;
                UDType* t = nullptr;
                for(int i = 0; i < ss.size(); i++)
                {
                    GeneralType* x = ch.type->members.at(ss[i]);
                    t = dynamic_cast<UDType*>(x);
                    if(t == nullptr)
                    {
                        if(*x != TypeControl::_int && *x != TypeControl::_bool)
                        {
                            std::cerr << "Error: Member variable '" << ident->ident.val.value() << "' is not of type 'int' or 'bool' on line " << ident->ident.line << std::endl;
                            exit(EXIT_FAILURE);
                        }
                    }

                    offset += ch.type->offsets.at(ss[i]);
                }
                std::stringstream offs;
                code << "    ;; Loading variable '" << ch.name << "'\n";
                offs << "QWORD [rsp + " << (sp - (sp - ch.start_stackl + (offset / 8)) - 1) * 8 << "]";
                push(offs.str());
                code << "\n";
            }
            else
            {
                if(!is_declared(ident->ident.val.value()))
                {
                    std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const Var& var = get_var(ident->ident.val.value());
                if(*var.type != TypeControl::_bool)
                {
                    std::cerr << "Error: Identifier '" << ident->ident.val.value() << "' is not of type 'bool' on line " << ident->ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                std::stringstream offset;
                code << "    ;; Loading variable '" << var.name << "'\n";
                offset << "QWORD [rsp + " << (sp - var.stackl - 1) * 8 << "]";
                push(offset.str());
                code << "\n";
            }
        }
        else if(term->val.index() == 1) // Boolean
        {
            std::cerr << "661\n";
            NodeBool* i_int = std::get<NodeBool*>(term->val);
            code << "    xor rax, rax\n";
            if(i_int->boolean.type == TokenType::_true)
                code << "    mov rax, 1\n";
            push("rax");
            code << "\n";
        }
        else if(term->val.index() == 2) // Parentheses
        {
            std::cerr << "671\n";
            NodeTermParens* paren = std::get<NodeTermParens*>(term->val);
            gen_expr(paren->expr);
        }
        else if(term->val.index() == 3) // CompExpr
        {
            std::cerr << "677\n";
            NodeCompExpr* comp = std::get<NodeCompExpr*>(term->val);
            gen_comp_expression(comp);
        }
    }

    // ONLY boolean logic
    inline void gen_logic_expression(const NodeLogicExpr* expr)
    {
        if(expr->val.index() == 0) // And
        {
            std::cerr << "688\n";
            NodeLogicExprAnd* and_expr = std::get<NodeLogicExprAnd*>(expr->val);
            gen_expr(and_expr->lhs);
            gen_expr(and_expr->rhs);
            pop("rdi");
            pop("rax");
            code << "    and rax, rdi\n";
            push("rax");
            code << "\n";
        }
        else if(expr->val.index() == 1) // Or
        {
            std::cerr << "700\n";
            NodeLogicExprOr* or_expr = std::get<NodeLogicExprOr*>(expr->val);
            gen_expr(or_expr->lhs);
            gen_expr(or_expr->rhs);
            pop("rdi");
            pop("rax");
            code << "    or rax, rdi\n";
            push("rax");
            code << "\n";
        }
        else if(expr->val.index() == 2) // Not
        {
            std::cerr << "712\n";
            NodeLogicExprNot* not_expr = std::get<NodeLogicExprNot*>(expr->val);
            gen_expr(not_expr->expr);
            pop("rax");
            code << "    xor rax, 1\n";
            push("rax");
            code << "\n";
        }
    }

  private:
    struct Var {
        std::string name;
        size_t stackl;
        const GeneralType* type;
        int line;
    };

    struct Chunk {
        std::string name;
        size_t start_stackl;
        size_t size;
        const UDType* type;
        int line;

        Chunk(std::string name, size_t start_stackl, size_t size, const UDType* type, int line) : name(name), start_stackl(start_stackl), size(size), type(type), line(line) {}
    };

    const NodeProg* root;
    std::stringstream code;
    std::stringstream ext;
    std::stringstream data;
    std::stringstream init;
    size_t sp = 0;
    size_t str_count = 0;
    size_t if_labels = 0;
    size_t loop_labels = 0;

    std::vector<Var> vars;
    std::vector<Chunk> struct_vars;
    std::unordered_map<std::string, bool> internals_called;

    bool is_declared(std::string identifier) { return (std::ranges::find(vars, identifier, &Var::name) != vars.end() || std::ranges::find(struct_vars, identifier, &Chunk::name) != struct_vars.end()); }

    Var get_var(std::string identifier) { return *std::ranges::find(vars, identifier, &Var::name); }

    Chunk get_struct(std::string identifier) { return *std::ranges::find(struct_vars, identifier, &Chunk::name); }

    void gen_ret()
    {
        code << "    add rsp, " << (sp - 1) * 8 << "\n";
        code << "    mov rsp, rbp\n";
        pop("rbp");
        code << "    mov rax, 60\n";
        code << "    xor rdi, rdi\n";
        code << "    syscall\n";
    }

    void pop(const std::string& reg)
    {
        code << "    pop " << reg << "\n";
        sp--;
    }

    void push(const std::string& reg)
    {
        code << "    push " << reg << "\n";
        sp++;
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

#endif // CODEGEN_HPP
