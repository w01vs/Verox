#include <iostream>
#include <fstream>
#include <optional>
#include <sstream>
#include <vector>

#include  "arena.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "codegen.hpp"

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Error: Requires an input file " << std::endl;
        exit(EXIT_FAILURE);
    }

    std::string filename = argv[1];
    if (!filename.ends_with(".vx"))
    {
        std::cerr << "Error: file is not of type '.vx'" << std::endl;
        exit(EXIT_FAILURE);
    }

    std::string to_compile;

    std::stringstream cstream;
    std::fstream in(argv[1], std::ios::in);
    cstream << in.rdbuf();
    to_compile = cstream.str();
    in.close();

    Lexer lexer(to_compile);
    std::vector<Token> t = lexer.to_tokens();

    Parser parser(t);
    std::optional<NodeProg> tree = parser.parse();

    if (!tree.has_value())
    {
        std::cerr << "Invalid program" << std::endl;
        exit(EXIT_FAILURE);
    }

    Generator gen(tree.value());

    std::fstream out("verox.asm", std::ios::out);
    out << gen.gen_prog();
    out.close();
    // std::string path = "/mnt/c/Programming/compiler/";
    // std::string command = "usr/bin/nasm -felf64 " + path + "out.asm && usr/bin/ld " + path + "out.o -o " + path + "out";
    // std::cout << command << std::endl;
    // system(command.c_str());
    system("nasm -felf64 verox.asm");
    system("ld -o verox verox.o -L/lib/x86_64-linux-gnu -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2");

    return EXIT_SUCCESS;
}