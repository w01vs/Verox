#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>
#include <optional>
#include <string>
#include <vector>

enum class TokenType {
    ret,     // return -> internal
    i_int,   // immediate int
    semi,    // semicolon
    type,    // a type
    ident,   // a variable name
    assign,  // '='
    open_p,  // opening parenthesis
    close_p, // closing parenthesis
    print,   // print -> internal
    add,     // '+'
    star,    // '*'
    fslash,  // '/'
    minus,   // '-'
};

struct Token {
    TokenType type;
    int line;
    std::optional<std::string> val{};
};

inline void print_tokens(const std::vector<Token>& tokens)
{
    for(int i = 0; i < tokens.size(); i++)
    {
        std::cout << "Next token: " << std::endl;
        switch(tokens[i].type)
        {
        case TokenType::i_int:
            std::cout << "i_int: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::ret:
            std::cout << "return" << std::endl;
            break;
        case TokenType::semi:
            std::cout << "semicolon" << std::endl;
            break;
        case TokenType::type:
            std::cout << "type: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::ident:
            std::cout << "identifier: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::assign:
            std::cout << "assignment" << std::endl;
            break;
        case TokenType::close_p:
            std::cout << ")" << std::endl;
            break;
        case TokenType::open_p:
            std::cout << "(" << std::endl;
            break;
        case TokenType::print:
            std::cout << "print" << std::endl;
            break;
        case TokenType::add:
            std::cout << "+" << std::endl;
        default:
            break;
        }
    }
}

inline void print_token_type(const TokenType& type)
{
    switch(type)
    {
    case TokenType::ret:
        std::cout << "return" << std::endl;
        break;
    case TokenType::i_int:
        std::cout << "i_int" << std::endl;
        break;
    case TokenType::semi:
        std::cout << "semicolon" << std::endl;
        break;
    case TokenType::type:
        std::cout << "type" << std::endl;
        break;
    case TokenType::ident:
        std::cout << "identifier" << std::endl;
        break;
    case TokenType::assign:
        std::cout << "assignment" << std::endl;
        break;
    case TokenType::open_p:
        std::cout << "opening parenthesis" << std::endl;
        break;
    case TokenType::close_p:
        std::cout << "closing parenthesis" << std::endl;
        break;
    case TokenType::print:
        std::cout << "print" << std::endl;
        break;
    case TokenType::add:
        std::cout << "add" << std::endl;
        break;
    case TokenType::star:
        std::cout << "star" << std::endl;
        break;
    case TokenType::fslash:
        std::cout << "forward slash" << std::endl;
        break;
    case TokenType::minus:
        std::cout << "minus" << std::endl;
        break;
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
                std::cerr << "dude what the fuck is that" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        index = 0;
        return tokens;
    }

    inline bool keywords(std::vector<Token>& tokens, std::string& buf, int& lc)
    {
        if(std::isalpha(peek().value()))
        {
            buf.push_back(take());
            while(peek().has_value() && std::isalnum(peek().value())) { buf.push_back(take()); }
            if(buf == "return")
            {
                tokens.push_back({TokenType::ret, lc});
                buf.clear();
                return true;
            }
            else if(buf == "int")
            {
                tokens.push_back({TokenType::type, lc, buf});
                buf.clear();
                return true;
            }
            else if(buf == "print")
            {
                tokens.push_back({TokenType::print, lc});
                buf.clear();
                return true;
            }
            else
            {
                tokens.push_back({TokenType::ident, lc, buf});
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
            tokens.push_back({TokenType::i_int, lc, buf});
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
            tokens.push_back({TokenType::semi, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '=')
        {
            take();
            tokens.push_back({TokenType::assign, lc});
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
            tokens.push_back({TokenType::open_p, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == ')')
        {
            take();
            tokens.push_back({TokenType::close_p, lc});
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
            tokens.push_back({TokenType::add, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '-')
        {
            take();
            tokens.push_back({TokenType::minus, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '*')
        {
            take();
            tokens.push_back({TokenType::star, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '/')
        {
            take();
            tokens.push_back({TokenType::fslash, lc});
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
