#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>
#include <optional>
#include <string>
#include <vector>
#include "tokens.hpp"

inline void print_token_type(const TokenType& type)
{
    switch(type)
    {
    case TokenType::_ret:
        std::cout << "return" << std::endl;
        break;
    case TokenType::_int_lit:
        std::cout << "i_int" << std::endl;
        break;
    case TokenType::_semi:
        std::cout << "semicolon" << std::endl;
        break;
    case TokenType::_type:
        std::cout << "type" << std::endl;
        break;
    case TokenType::_ident:
        std::cout << "identifier" << std::endl;
        break;
    case TokenType::_assign:
        std::cout << "assignment" << std::endl;
        break;
    case TokenType::_open_p:
        std::cout << "opening parenthesis" << std::endl;
        break;
    case TokenType::_close_p:
        std::cout << "closing parenthesis" << std::endl;
        break;
    case TokenType::_print:
        std::cout << "print" << std::endl;
        break;
    case TokenType::_add:
        std::cout << "add" << std::endl;
        break;
    case TokenType::_star:
        std::cout << "star" << std::endl;
        break;
    case TokenType::_fslash:
        std::cout << "forward slash" << std::endl;
        break;
    case TokenType::_minus:
        std::cout << "minus" << std::endl;
        break;
    case TokenType::_open_b:
        std::cout << "opening brace" << std::endl;
        break;
    case TokenType::_close_b:
        std::cout << "closing brace" << std::endl;
        break;
    case TokenType::_true:
        std::cout << "true" << std::endl;
        break;
    case TokenType::_false:
        std::cout << "false" << std::endl;
        break;
    case TokenType::_or:
        std::cout << "or" << std::endl;
        break;
    case TokenType::_and:
        std::cout << "and" << std::endl;
        break;
    case TokenType::_not:
        std::cout << "not" << std::endl;
        break;
    case TokenType::_eq:
        std::cout << "equal" << std::endl;
        break;
    case TokenType::_greater:
        std::cout << "greater" << std::endl;
        break;
    case TokenType::_less:
        std::cout << "less" << std::endl;
        break;
    case TokenType::_greater_eq:
        std::cout << "greater or equal" << std::endl;
        break;
    case TokenType::_less_eq:
        std::cout << "less or equal" << std::endl;
        break;
    case TokenType::_neq:
        std::cout << "not equal" << std::endl;
        break;
    case TokenType::_if:
        std::cout << "if" << std::endl;
        break;
    default:
        std::cout << "I forgot or what the fuck is this?" << std::endl;
    }
}

inline void print_tokens(const std::vector<Token>& tokens)
{
    for(int i = 0; i < tokens.size(); i++)
    {
        std::cout << "Next token: " << std::endl;
        print_token_type(tokens[i].type);
    }
}

class Lexer {
  public:
    inline explicit Lexer(std::string src) : src(src) {}

    inline std::vector<Token> to_tokens()
    {
        std::vector<Token> tokens;
        std::string buf;
        int lc = 1;
        while(peek().has_value())
        {
            if(keywords(tokens, buf, lc)) {}
            else if(digits(tokens, buf, lc)) {}
            else if(symbols(tokens, buf, lc)) {}
            else
            {
                std::cerr << "dude what the fuck is that (this means the input file contains things that are not being parsed)" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        index = 0;
        return tokens;
    }

    inline bool keywords(std::vector<Token>& tokens, std::string& buf, int& lc)
    {
    if(std::isalpha(peek().value()) || peek().value() == '_')
        {
            buf.push_back(take());
            while(peek().has_value() && (std::isalnum(peek().value()) || peek().value() == '_')) { buf.push_back(take()); }
            if(buf == "return")
            {
                tokens.emplace_back(TokenType::_ret, lc);
                buf.clear();
                return true;
            }
            else if(buf == "int")
            {
                tokens.emplace_back(TokenType::_type, lc, buf);
                buf.clear();
                return true;
            }
            else if(buf == "print")
            {
                tokens.emplace_back(TokenType::_print, lc);
                buf.clear();
                return true;
            }
            else if(buf == "true")
            {
                tokens.emplace_back(TokenType::_true, lc, buf);
                buf.clear();
                return true;
            }
            else if(buf == "false")
            {
                tokens.emplace_back(TokenType::_false, lc, buf);
                buf.clear();
                return true;
            }
            else if(buf == "bool")
            {
                tokens.emplace_back(TokenType::_type, lc, buf);
                buf.clear();
                return true;
            }
            else if(buf == "if")
            {
                tokens.emplace_back(TokenType::_if, lc);
                buf.clear();
                return true;
            }
            else if(buf == "else") {
                tokens.emplace_back(TokenType::_else, lc);
                buf.clear();
                return true;
            }
            else if(buf == "while") {
                tokens.emplace_back(TokenType::_while, lc);
                buf.clear();
                return true;
            }
            else if(buf == "for") {
                tokens.emplace_back(TokenType::_for, lc);
                buf.clear();
                return true;
            }
            else if(buf == "break") {
                tokens.emplace_back(TokenType::_break, lc);
                buf.clear();
                return true;
            }
            else if(buf == "continue") {
                tokens.emplace_back(TokenType::_continue, lc);
                buf.clear();
                return true;
            }
            else if(buf == "struct") {
                tokens.emplace_back(TokenType::_struct, lc);
                buf.clear();
                return true;
            }
            else
            {
                tokens.emplace_back(TokenType::_ident, lc, buf);
                buf.clear();
                return true;
            }
        }
        return false;
    }

    inline bool digits(std::vector<Token>& tokens, std::string& buf, int& lc)
    {
        if(std::isdigit(peek().value()))
        {
            buf.push_back(take());
            while(peek().has_value() && std::isdigit(peek().value())) { buf.push_back(take()); }
            tokens.emplace_back(TokenType::_int_lit, lc, buf);
            buf.clear();
            return true;
        }
        return false;
    }

    inline bool symbols(std::vector<Token>& tokens, std::string& buf, int& lc)
    {
        if(peek().has_value() && peek().value() == ';')
        {
            take();
            tokens.emplace_back(TokenType::_semi, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '=')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.emplace_back(TokenType::_eq, lc);
                return true;
            }
            tokens.emplace_back(TokenType::_assign, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '\n')
        {
            take();
            lc++;
            return true;
        }
        else if(peek().has_value() && peek().value() == '(')
        {
            take();
            tokens.emplace_back(TokenType::_open_p, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == ')')
        {
            take();
            tokens.emplace_back(TokenType::_close_p, lc);
            return true;
        }
        else if(peek().has_value() && std::isspace(peek().value()))
        {
            take();
            return true;
        }
        else if(peek().has_value() && peek().value() == '+')
        {
            take();
            tokens.emplace_back(TokenType::_add, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '-')
        {
            take();
            tokens.emplace_back(TokenType::_minus, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '*')
        {
            take();
            tokens.emplace_back(TokenType::_star, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '/')
        {
            take();
            tokens.emplace_back(TokenType::_fslash, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '{')
        {
            take();
            tokens.emplace_back(TokenType::_open_b, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '}')
        {
            take();
            tokens.emplace_back(TokenType::_close_b, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '>')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.emplace_back(TokenType::_greater_eq, lc);
                return true;
            }
            tokens.emplace_back(TokenType::_greater, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '<')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.emplace_back(TokenType::_less_eq, lc);
                return true;
            }
            tokens.emplace_back(TokenType::_less, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '&')
        {
            take();
            if(peek().has_value() && peek().value() == '&') {
                take();
                tokens.emplace_back(TokenType::_and, lc);
                return true;
            }
        }
        else if(peek().has_value() && peek().value() == '|')
        {
            take();
            if(peek().has_value() && peek().value() == '|') {
                take();
                tokens.emplace_back(TokenType::_or, lc);
                return true;
            }
        }
        else if(peek().has_value() && peek().value() == '!')
        {
            take();
            tokens.emplace_back(TokenType::_not, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == '.') {
            take();
            tokens.emplace_back(TokenType::_dot, lc);
            return true;
        }
        else if(peek().has_value() && peek().value() == ',') {
            take();
            tokens.emplace_back(TokenType::_comma, lc);
            return true;
        }
        
        return false;
    }

  private:
    inline std::optional<char> peek(const size_t offset = 0) const
    {
        if(index + offset >= src.length())
        {
            return {};
        }
        return src.at(index + offset);
    }

    inline char take() { return src.at(index++); }

    const std::string src;
    size_t index = 0;
};

#endif // LEXER_HPP
