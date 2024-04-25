#pragma once
#include "parser.hpp"
#include <sstream>
#include <unordered_map>

std::string print_type(Type type)
{
    switch (type)
    {
    case Type::_int:
        return "int";
        break;
    default:
        return "undefined";
        break;
    }
}

class Generator
{
public:
    explicit Generator(NodeProg root) : root(root) {}

    std::string gen_prog()
    {
        ext << "global _start\nsection .text\n";
        code << "_start:\n";
        push("rbp");
        code << "    mov rbp, rsp\n\n";
        data << "section .data\n";
        for (int i = 0; i < root.stmts.size(); i++)
        {
            gen_stmt(root.stmts[i]);
        }
        if (!internals_called.at("ret"))
            gen_ret();
        data << "\n";
        data << ext.str() << code.str();
        return data.str();
    }

    void gen_stmt(const NodeStmt &  stmt)
    {
        struct StmtVisitor
        {
            Generator *const gen;
            void operator()(const NodeInternal &internal) const
            {
                gen->gen_internal(internal);
            }

            void operator()(const NodeStmtVar &var) const
            {
                if (gen->vars.contains(var.ident.val.value()))
                {
                    std::cerr << "Error: Identifier '" << var.ident.val.value() << "' has already been declared on line " << gen->vars.at(var.ident.val.value()).line << ", but was declared again on line " << var.ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }

                struct ExprVisitor
                {
                    Generator *const gen;
                    const NodeStmtVar var;
                    void operator()(const NodeExprIdent &ident) const
                    {
                        const auto temp = ident.ident.val.value();
                        if (!gen->vars.contains(temp))
                        {
                            std::cerr << "Error: Undeclared identifier '" << ident.ident.val.value() << "' on line " << ident.ident.line << std::endl;
                            exit(EXIT_FAILURE);
                        }
                        else
                        {
                            if (gen->vars.at(temp).type == var.type)
                            {
                                gen->vars.insert({var.ident.val.value(), Var{gen->sp, var.type, var.ident.line}});
                                gen->gen_expr(var.expr);
                            }
                            else
                            {
                                std::cerr << "TypeError: expected " << type_string(var.type) << " but got " << type_string(gen->vars.at(temp).type) << std::endl;
                            }
                        }
                    }
                    void operator()(const NodeExprIInt &i_int) const
                    {
                        gen->vars.insert({var.ident.val.value(), Var{gen->sp, var.type, i_int.i_int.line}});
                        gen->gen_expr(var.expr);
                    }
                };

                ExprVisitor visitor{gen, var};
                std::visit(visitor, var.expr.var);
            }
        };

        StmtVisitor visitor{this};
        std::visit(visitor, stmt.var);
    }

    std::string gen_expr(const NodeExpr &expr)
    {
        struct ExprVisitor
        {
            Generator *const gen;
            bool eval;
            std::string operator()(const NodeExprIdent &ident) const
            {
                if (!gen->vars.contains(ident.ident.val.value()))
                {
                    std::cerr << "Error: Undeclared identifier '" << ident.ident.val.value() << "' on line " << ident.ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const auto &var = gen->vars.at(ident.ident.val.value());
                std::stringstream offset;
                offset << "QWORD [rsp + " << (gen->sp - var.stackl - 1) * 8 << "]";
                gen->push(offset.str());
                gen->code << "\n";

                return ident.ident.val.value();
            }
            std::string operator()(const NodeExprIInt &i_int) const
            {
                gen->code << "    mov rax, " << i_int.i_int.val.value() << "\n";
                gen->push("rax");
                gen->code << "\n";
                return i_int.i_int.val.value();
            }
        };

        ExprVisitor visitor{this};
        return std::visit(visitor, expr.var);
    }

    void gen_internal(const NodeInternal &internal)
    {
        struct InternalVisitor

        {
            Generator *const gen;
            void operator()(const NodeInternalRet &ret) const
            {
                if (!gen->internals_called.contains("ret"))
                    gen->internals_called.insert({"ret", true});
                gen->gen_expr(ret.ret);
                gen->pop("rdi");
                if (gen->sp % 2 != 0)
                    gen->code << "    add rsp, " << (gen->sp) * 8 << "\n";
                else
                    gen->code << "    add rsp, " << (gen->sp - 1) * 8 << "\n";
                gen->code << "    mov rsp, rbp\n";
                gen->pop("rbp");
                gen->code << "    mov rax, 60\n";
                gen->code << "    syscall\n";
                gen->code << "\n";
            }
            void operator()(const NodeInternalPrintf &print) const
            {
                if (!gen->internals_called.contains("puts"))
                {
                    gen->internals_called.insert({"puts", true});
                    gen->ext << "extern puts\n";
                    gen->ext << "\n";
                }
                gen->data << "    s" << gen->str_count++ << " db \"" << gen->gen_expr(print.print) << "\",0\n";

                gen->code << "    lea rdi, [s0]\n";
                if (gen->sp % 2 != 0)
                    gen->code << "    sub rsp, 8\n";
                gen->code << "    call puts\n";
                gen->code << "\n";
            }
        };

        InternalVisitor visitor{this};
        std::visit(visitor, internal.ret);
    }

private:
    const NodeProg root;
    std::stringstream code;
    std::stringstream ext;
    std::stringstream data;
    size_t sp = 0;
    size_t str_count = 0;

    struct Var
    {
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

    void pop(const std::string &reg)
    {
        code << "    pop " << reg << "\n";
        sp--;
    }

    void push(const std::string &reg)
    {
        code << "    push " << reg << "\n";
        sp++;
    }
};