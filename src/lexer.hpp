#ifndef LEXER_HPP
#define LEXER_HPP

#include <iostream>
#include <optional>
#include <string>
#include <vector>

enum class TokenType {
    _ret,           // return -> internal
    _i_int,         // immediate int
    _true,          // boolean true
    _false,         // boolean false
    _semi,          // ';'
    _type,          // a typename
    _ident,         // a variable name
    _assign,        // '='
    _open_p,        // '('
    _close_p,       // ')'
    _print,         // print -> internal
    _add,           // '+'
    _star,          // '*'
    _fslash,        // '/'
    _minus,         // '-'
    _open_b,        // '{'
    _close_b,       // '{'
    _or,            // '||'
    _and,           // '&&'
    _not,           // '!'
    _eq,            // '=='
    _greater,       // '>'
    _less,          // '<'
    _greater_eq,    // '>='
    _less_eq,       // '<='
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
        case TokenType::_i_int:
            std::cout << "i_int: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::_ret:
            std::cout << "return" << std::endl;
            break;
        case TokenType::_semi:
            std::cout << "semicolon" << std::endl;
            break;
        case TokenType::_type:
            std::cout << "type: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::_ident:
            std::cout << "identifier: " << tokens[i].val.value() << std::endl;
            break;
        case TokenType::_assign:
            std::cout << "assignment" << std::endl;
            break;
        case TokenType::_close_p:
            std::cout << ")" << std::endl;
            break;
        case TokenType::_open_p:
            std::cout << "(" << std::endl;
            break;
        case TokenType::_print:
            std::cout << "print" << std::endl;
            break;
        case TokenType::_add:
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
    case TokenType::_ret:
        std::cout << "return" << std::endl;
        break;
    case TokenType::_i_int:
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
    default:
        std::cout << "I forgot or what the fuck is this?" << std::endl;
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
                tokens.push_back({TokenType::_ret, lc});
                buf.clear();
                return true;
            }
            else if(buf == "int")
            {
                tokens.push_back({TokenType::_type, lc, buf});
                buf.clear();
                return true;
            }
            else if(buf == "print")
            {
                tokens.push_back({TokenType::_print, lc});
                buf.clear();
                return true;
            }
            else if(buf == "true")
            {
                tokens.push_back({TokenType::_true, lc, buf});
                buf.clear();
                return true;
            }
            else if(buf == "false")
            {
                tokens.push_back({TokenType::_true, lc, buf});
                buf.clear();
                return true;
            }
            else if(buf == "bool")
            {
                tokens.push_back({TokenType::_type, lc, buf});
                buf.clear();
                return true;
            }
            else
            {
                tokens.push_back({TokenType::_ident, lc, buf});
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
            tokens.push_back({TokenType::_i_int, lc, buf});
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
            tokens.push_back({TokenType::_semi, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '=')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.push_back({TokenType::_eq, lc});
                return true;
            }
            tokens.push_back({TokenType::_assign, lc});
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
            tokens.push_back({TokenType::_open_p, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == ')')
        {
            take();
            tokens.push_back({TokenType::_close_p, lc});
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
            tokens.push_back({TokenType::_add, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '-')
        {
            take();
            tokens.push_back({TokenType::_minus, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '*')
        {
            take();
            tokens.push_back({TokenType::_star, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '/')
        {
            take();
            tokens.push_back({TokenType::_fslash, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '{')
        {
            take();
            tokens.push_back({TokenType::_open_b, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '}')
        {
            take();
            tokens.push_back({TokenType::_close_b, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '>')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.push_back({TokenType::_greater_eq, lc});
                return true;
            }
            tokens.push_back({TokenType::_greater, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '<')
        {
            take();
            if(peek().has_value() && peek().value() == '=')
            {
                take();
                tokens.push_back({TokenType::_less_eq, lc});
                return true;
            }
            tokens.push_back({TokenType::_less, lc});
            return true;
        }
        else if(peek().has_value() && peek().value() == '&')
        {
            take();
            if(peek().has_value() && peek().value() == '&') {
                take();
                tokens.push_back({TokenType::_and, lc});
                return true;
            }
        }
        else if(peek().has_value() && peek().value() == '|')
        {
            take();
            if(peek().has_value() && peek().value() == '|') {
                take();
                tokens.push_back({TokenType::_or, lc});
                return true;
            }
        }
        else if(peek().has_value() && peek().value() == '!')
        {
            take();
            tokens.push_back({TokenType::_not, lc});
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
