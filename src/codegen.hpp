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
        output << "global _start\nsection .text\n_start:\n";
        for (int i = 0; i < root.stmts.size(); i++)
        {
            gen_stmt(root.stmts[i]);
        }
        if (!ret)
            gen_ret();
        return output.str();
    }

    void gen_stmt(const NodeStmt &stmt)
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
                    std::cerr << "Identifier '" << var.ident.val.value() << "' has already been declared" << std::endl;
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
                            std::cerr << "Error: Identifier '" << temp << "' has not been declared" << std::endl;
                            exit(EXIT_FAILURE);
                        }
                        else
                        {
                            if (gen->vars.at(temp).type == var.type)
                            {
                                gen->vars.insert({var.ident.val.value(), Var{gen->sp, var.type}});
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
                        gen->vars.insert({var.ident.val.value(), Var{gen->sp, var.type}});
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

    void gen_expr(const NodeExpr &expr)
    {
        struct ExprVisitor
        {
            Generator *const gen;
            void operator()(const NodeExprIdent &ident) const
            {
                if (!gen->vars.contains(ident.ident.val.value()))
                {
                    std::cerr << "Undeclared identifier '" << ident.ident.val.value() << "' on line " << ident.ident.line << std::endl;
                    exit(EXIT_FAILURE);
                }
                const auto &var = gen->vars.at(ident.ident.val.value());
                std::stringstream offset;
                offset << "QWORD [rsp + " << (gen->sp - var.stackl - 1) * 8 << "]\n";
                gen->push(offset.str());
            }
            void operator()(const NodeExprIInt &i_int) const
            {
                gen->output << "    mov rax, " << i_int.i_int.val.value() << "\n";
                gen->push("rax");
            }
        };

        ExprVisitor visitor{this};
        std::visit(visitor, expr.var);
    }

    void gen_internal(const NodeInternal &internal)
    {
        struct InternalVisitor
        {
            Generator *const gen;
            void operator()(const NodeInternalRet &ret) const
            {
                gen->gen_expr(ret.ret);
                gen->output << "    mov rax, 60\n";
                gen->pop("rdi");
                gen->output << "    syscall\n";
                gen->ret = true;
            }
        };

        InternalVisitor visitor{this};
        std::visit(visitor, internal.ret);
    }

private:
    const NodeProg root;
    std::stringstream output;
    bool ret = false;
    size_t sp = 0;

    struct Var
    {
        size_t stackl;
        Type type;
    };

    std::unordered_map<std::string, Var> vars;

    void gen_ret()
    {
        output << "    mov rax, 60\n";
        output << "    xor rdi, rdi\n";
        output << "    syscall\n";
    }

    void pop(const std::string &reg)
    {
        output << "    pop " << reg << "\n";
        sp--;
    }

    void push(const std::string &reg)
    {
        output << "    push " << reg << "\n";
        sp++;
    }
};