#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "parser.hpp"
#include <sstream>
#include <string>
#include <unordered_map>

std::string print_type(Type type)
{
    switch(type)
    {
    case Type::_int:
        return "int";
        break;
    default:
        return "undefined";
        break;
    }
}

class Generator {
  public:
    explicit Generator(const NodeProg* root) : root(root) {}

    std::string gen_prog()
    {
        ext << "global main\nsection .text\n";
        code << "main:\n";
        push("rbp");
        code << "    mov rbp, rsp\n\n";
        data << "default rel\n";
        data << "section .data\n";
        for(int i = 0; i < root->stmts.size(); i++) { gen_stmt(root->stmts.at(i)); }
        if(!internals_called.at("ret"))
            gen_ret();
        data << "\n";
        data << ext.str() << init.str() << code.str();
        return data.str();
    }

    void gen_stmt(const NodeStmt* stmt)
    {
        if(stmt->var.index() == 0) // Internal
        {
            NodeInternal* var = std::get<NodeInternal*>(stmt->var);
            gen_internal(var);
        }
        else if(stmt->var.index() == 1) // Variable
        {
            NodeStmtVar* var = std::get<NodeStmtVar*>(stmt->var);
            if(vars.contains(var->ident.val.value()))
            {
                std::cerr << "Error: Identifier '" << var->ident.val.value() << "' has already been declared on line " << vars.at(var->ident.val.value()).line << ", but was declared again on line " << var->ident.line << std::endl;
                exit(EXIT_FAILURE);
            }

            vars.insert({var->ident.val.value(), {sp, var->type, var->ident.line}});
            gen_expr(var->expr);
        }
    }

    void gen_expr(const NodeExpr* expr)
    {
        if(expr->var.index() == 0) // Term
        {
            NodeTerm* term = std::get<NodeTerm*>(expr->var);
            gen_term(term);
        }
        if(expr->var.index() == 1) // BinExpr
        {
            NodeBinExpr* binexpr = std::get<NodeBinExpr*>(expr->var);
            gen_bexpr(binexpr);
        }
    }

    void gen_term(const NodeTerm* term)
    {
        if(term->val.index() == 0) // Identifier
        {
            NodeExprIdent* ident = std::get<NodeExprIdent*>(term->val);
            if(!vars.contains(ident->ident.val.value()))
            {
                std::cerr << "Error: Undeclared identifier '" << ident->ident.val.value() << "' on line " << ident->ident.line << std::endl;
                exit(EXIT_FAILURE);
            }
            const Var& var = vars.at(ident->ident.val.value());
            std::stringstream offset;
            offset << "QWORD [rsp + " << (sp - var.stackl - 1) * 8 << "]";
            push(offset.str());
            code << "\n";
        }
        else if(term->val.index() == 1) // Immediate Int
        {
            NodeExprIInt* i_int = std::get<NodeExprIInt*>(term->val);
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

    void gen_internal(const NodeInternal* internal)
    {
        struct InternalVisitor {
            Generator* const gen;
            void operator()(const NodeInternalRet* ret) const
            {
                if(!gen->internals_called.contains("ret"))
                    gen->internals_called.insert({"ret", true});
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

    void gen_bexpr(const NodeBinExpr* binexpr)
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
        else if (binexpr->val.index() == 1) // Division
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
        else if (binexpr->val.index() == 2) // Subtraction
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
        else if (binexpr->val.index() == 3) // Multiplication
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
        else {
            std::cerr << "Error: Unknown binary expression" << std::endl;
            exit(EXIT_FAILURE);
        }
    }

  private:
    const NodeProg* root;
    std::stringstream code;
    std::stringstream ext;
    std::stringstream data;
    std::stringstream init;
    size_t sp = 0;
    size_t str_count = 0;

    struct Var {
        size_t stackl;
        Type type;
        int line;
    };

    std::unordered_map<std::string, Var> vars;
    std::unordered_map<std::string, bool> internals_called;

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
