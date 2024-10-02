#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <vector>

#include "codegen.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "tokens.hpp"

std::map<TokenType, Type> token_type_map = { {TokenType::_int_lit, Type::_int}, {TokenType::_string, Type::_string}, {TokenType::_bool, Type::_bool} };

int main(int argc, char* argv[])
{
    if(argc != 2)
    {
        std::cerr << "Error: Requires an input file yeet " << std::endl;
        exit(EXIT_FAILURE);
    }
    
    std::string filename = argv[1];
    if(!filename.ends_with(".vx"))
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
    // print_tokens(t);

    Parser parser(t);
    std::optional<NodeProg*> tree = parser.parse();

    if(!tree.has_value())
    {
        std::cerr << "Invalid program" << std::endl;
        exit(EXIT_FAILURE);
    }

    Generator gen(tree.value());
    std::fstream out("verox.asm", std::ios::out);
    out << gen.gen_prog();
    out.close();
    // std::string path = "/mnt/c/Programming/compiler/";
    // std::string command = "usr/bin/nasm -felf64 " + path + "out.asm &&
    // usr/bin/ld " + path + "out.o -o " + path + "out"; std::cout << command <<
    // std::endl; system(command.c_str());
    system("nasm -felf64 verox.asm");
    system("gcc -o verox verox.o -lc");
    
    return EXIT_SUCCESS;
}
