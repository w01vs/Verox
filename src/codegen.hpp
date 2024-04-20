#include "parser.hpp"
#include <sstream>

class Generator
{
public:
    explicit Generator(NodeProg root) : root(root) {}

    [[nodiscard]] std::string generate() const
    {
        std::stringstream output;
        output << "global _start\nsection .text\n_start:\n";
        output << "    mov rax, 60\n";
        output << "    mov rdi, " << root.expr.i_int.val.value() << "\n";
        output << "    syscall"; 
        return output.str();
    }

private:
    const NodeProg root;
};