#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "nodes.hpp"
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

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

    inline void gen_stmt(const NodeStmt* stmt)
    {
        if(stmt->var.index() == 0) // Internal
        {
            NodeInternal* var = std::get<NodeInternal*>(stmt->var);
            gen_internal(var);
        }
        else if(stmt->var.index() == 1) // Variable
        {

            NodeStmtVar* var = std::get<NodeStmtVar*>(stmt->var);
            if(var_declared(var->ident.val.value()))
            {
                std::cerr << "Error: Identifier '" << var->ident.val.value() << "' has already been declared on line " << get_var(var->ident.val.value()).line << ", but was declared again on line " << var->ident.line << std::endl;
                exit(EXIT_FAILURE);
            }
            code << "    ;; Declaring variable " << '\'' << var->ident.val.value() << '\'' << " of type " << print_type(var->type) << '\n';
            vars.push_back({var->ident.val.value(), sp, var->type, var->ident.line});
            gen_expr(var->expr);
        }
        else if(stmt->var.index() == 2) // Scope
        {
            NodeScope* scope = std::get<NodeScope*>(stmt->var);
            gen_scope(scope);
        }
        else if(stmt->var.index() == 3) // If statement
        {
            NodeIf* ifstmt = std::get<NodeIf*>(stmt->var);
            gen_if(ifstmt);
        }
    }

    inline void gen_if(const NodeIf* ifstmt) {
        int _sp = sp;
        code << "    ;; If statement\n";
        gen_expr(ifstmt->cond);
        pop("rax");
        code << "    cmp rax, 1\n";
        std::string label = "if_" + std::to_string(labels++);
        code << "    jz " << label << "\n";
        gen_scope(ifstmt->scope);
        code << label << ":\n\n";
        sp = _sp;
    }

    inline void gen_scope(const NodeScope* scope)
    {
        code << "    ;; Entering scope\n";
        size_t vars_size = vars.size();
        for(int i = 0; i < scope->stmts.size(); i++) { gen_stmt(scope->stmts.at(i)); }
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
            NodeBinTerm* term = std::get<NodeBinTerm*>(expr->var);
            gen_binary_term(term);
        }
        else if(expr->var.index() == 1) // BinExpr
        {
            NodeBinExpr* binexpr = std::get<NodeBinExpr*>(expr->var);
            gen_binary_expression(binexpr);
        }
        else if(expr->var.index() == 2) // LogicTerm
        {
            NodeLogicTerm* term = std::get<NodeLogicTerm*>(expr->var);
            gen_logic_term(term);
        }
        else if(expr->var.index() == 3) // LogicExpr
        {
            NodeLogicExpr* logic = std::get<NodeLogicExpr*>(expr->var);
            gen_logic_expression(logic);
        }
        else if(expr->var.index() == 4) // CompExpr
        {
            NodeCompExpr* comp = std::get<NodeCompExpr*>(expr->var);
            gen_comp_expression(comp);
        }
    }

    inline void gen_binary_term(const NodeBinTerm* term)
    {
        if(term->val.index() == 0) // Identifier
        {
            NodeIdent* ident = std::get<NodeIdent*>(term->val);
            if(!var_declared(ident->ident.val.value()))
            {
                std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                exit(EXIT_FAILURE);
            }
            const Var& var = get_var(ident->ident.val.value());
            std::stringstream offset;
            code << "    ;; Loading variable '" << var.name << "'\n";
            offset << "QWORD [rsp + " << (sp - var.stackl - 1) * 8 << "]";
            push(offset.str());
            code << "\n";
        }
        else if(term->val.index() == 1) // Immediate Int
        {
            NodeIntLit* i_int = std::get<NodeIntLit*>(term->val);
            code << "    mov rax, " << i_int->i_int.val.value() << "\n";
            push("rax");
            code << "\n";
        }
        else if(term->val.index() == 2) // Parentheses
        {
            NodeTermParens* paren = std::get<NodeTermParens*>(term->val);
            gen_expr(paren->expr);
        }
    }

    inline void gen_internal(const NodeInternal* internal)
    {
        struct InternalVisitor {
            Generator* const gen;
            void operator()(const NodeInternalRet* ret) const
            {
                gen->code <<  "    ;; Return\n";
                gen->gen_expr(ret->ret);
                gen->pop("rdi");
                if(gen->sp % 2 != 0)
                    gen->code << "    add rsp, " << (gen->sp) * 8 << "\n";
                else
                    gen->code << "    add rsp, " << (gen->sp - 1) * 8 << "\n";
                gen->code << "    mov rsp, rbp\n";
                gen->pop("rbp");
                gen->code << "    mov rax, 60\n";
                gen->code << "    syscall\n";
                gen->code << "\n";
            }
            void operator()(const NodeInternalPrintf* print) const
            {
                std::cout << "Printing is currently not supported" << std::endl;
                return;
            }
        };

        InternalVisitor visitor{this};
        std::visit(visitor, internal->ret);
    }

    inline void gen_binary_expression(const NodeBinExpr* binexpr)
    {
        if(binexpr->val.index() == 0) // Add
        {
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
            NodeCompExprEq* eq = std::get<NodeCompExprEq*>(comp->val);
            gen_binary_term(eq->lhs);
            gen_binary_term(eq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rdi, rax\n";
            code << "    sete al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 1) // Greater
        {
            NodeCompExprGreater* greater = std::get<NodeCompExprGreater*>(comp->val);
            gen_binary_term(greater->lhs);
            gen_binary_term(greater->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rdi, rax\n";
            code << "    setg al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 2) // Less
        {
            NodeCompExprLess* less = std::get<NodeCompExprLess*>(comp->val);
            gen_binary_term(less->lhs);
            gen_binary_term(less->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rdi, rax\n";
            code << "    setl al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 3) // Greater or Equal
        {
            NodeCompExprGreaterEq* greater_eq = std::get<NodeCompExprGreaterEq*>(comp->val);
            gen_binary_term(greater_eq->lhs);
            gen_binary_term(greater_eq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rdi, rax\n";
            code << "    setge al\n";
            code << "    movzx rax, al\n";
            push("rax");
            code << "\n";
        }
        else if(comp->val.index() == 4) // Less or Equal
        {
            NodeCompExprLessEq* less_eq = std::get<NodeCompExprLessEq*>(comp->val);
        }
        else if(comp->val.index() == 5) // Not Equal
        {
            NodeCompExprNeq* neq = std::get<NodeCompExprNeq*>(comp->val);
            gen_binary_term(neq->lhs);
            gen_binary_term(neq->rhs);
            pop("rdi");
            pop("rax");
            code << "    cmp rdi, rax\n";
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
            NodeIdent* ident = std::get<NodeIdent*>(term->val);
            if(!var_declared(ident->ident.val.value()))
            {
                std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                exit(EXIT_FAILURE);
            }
            const Var& var = get_var(ident->ident.val.value());
            if(var.type != Type::_bool)
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
        else if(term->val.index() == 1) // Boolean
        {
            NodeBool* i_int = std::get<NodeBool*>(term->val);
            code << "    xor rax, rax\n";
            if(i_int->boolean.type == TokenType::_true)
                code << "    mov rax, 1\n";
            push("rax");
            code << "\n";
        }
        else if(term->val.index() == 2) // Parentheses
        {
            NodeTermParens* paren = std::get<NodeTermParens*>(term->val);
            gen_expr(paren->expr);
        }
        else if(term->val.index() == 3) // CompExpr
        {
            NodeCompExpr* comp = std::get<NodeCompExpr*>(term->val);
            gen_comp_expression(comp);
        }
    }

    // ONLY boolean logic
    inline void gen_logic_expression(const NodeLogicExpr* expr)
    {
        if(expr->val.index() == 0) // And
        {
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
            NodeLogicExprNot* not_expr = std::get<NodeLogicExprNot*>(expr->val);
            gen_expr(not_expr->expr);
            pop("rax");
            code << "    not rax\n";
            push("rax");
            code << "\n";
        }
    }

  private:
    struct Var {
        std::string name;
        size_t stackl;
        Type type;
        int line;
    };

    const NodeProg* root;
    std::stringstream code;
    std::stringstream ext;
    std::stringstream data;
    std::stringstream init;
    size_t sp = 0;
    size_t str_count = 0;
    size_t labels = 0;

    std::vector<Var> vars;
    std::unordered_map<std::string, bool> internals_called;

    bool var_declared(std::string identifier) { return std::ranges::find(vars, identifier, &Var::name) != vars.end(); };

    Var get_var(std::string identifier) { return *std::ranges::find(vars, identifier, &Var::name); };

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
};

#endif // CODEGEN_HPP
