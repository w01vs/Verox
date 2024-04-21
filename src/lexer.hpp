#pragma once

#include <string>
#include <vector>
#include <optional>

enum class TokenType
{
    ret,
    i_int,
    semi,
    type,
    ident,
    assign
};
struct Token
{
    TokenType type;
    int line;
    std::optional<std::string> val{};
};

void print_tokens(std::vector<Token> &tokens)
{
    for (int i = 0; i < tokens.size(); i++)
    {
        std::cout << "Next token: " << std::endl;
        switch (tokens[i].type)
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
        default:
            break;
        }
    }
}

class Lexer
{
public:
    explicit Lexer(std::string src) : src(src) {}

    std::vector<Token> to_tokens()
    {
        std::vector<Token> tokens;
        std::string buf;
        int lc = 1;
        while (peek().has_value())
        {
            if (keywords(tokens, buf, lc))
            {
            }
            else if (digits(tokens, buf, lc))
            {
            }
            else if (symbols(tokens, buf, lc))
            {
            }
            else {
                std::cerr << "dude what the fuck is that" << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        index = 0;
        return tokens;
    }

    bool keywords(std::vector<Token> &tokens, std::string &buf, int &lc)
    {
        if (std::isalpha(peek().value()))
        {
            buf.push_back(take());
            while (peek().has_value() && std::isalnum(peek().value()))
            {
                buf.push_back(take());
            }
            if (buf == "return")
            {
                tokens.push_back({TokenType::ret, lc});
                buf.clear();
                return true;
            }
            else if (buf == "int")
            {
                tokens.push_back({TokenType::type, lc, buf});
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

    bool digits(std::vector<Token> &tokens, std::string &buf, int &lc)
    {
        if (std::isdigit(peek().value()))
        {
            buf.push_back(take());
            while (peek().has_value() && std::isdigit(peek().value()))
            {
                buf.push_back(take());
            }
            tokens.push_back({TokenType::i_int, lc, buf});
            buf.clear();
            return true;
        }
        return false;
    }

    bool symbols(std::vector<Token> &tokens, std::string &buf, int &lc)
    {
        if (peek().has_value() && peek().value() == ';')
        {
            take();
            tokens.push_back({TokenType::semi, lc});
            return true;
        }
        else if (peek().has_value() && peek().value() == '=')
        {
            take();
            tokens.push_back({TokenType::assign, lc});
            return true;
        }
        else if (peek().has_value() && peek().value() == '\n')
        {
            take();
            lc++;
            return true;
        }
        else if (peek().has_value() && std::isspace(peek().value()))
        {
            take();
            return true;
        }
        return false;
    }

private:
    [[nodiscard]] std::optional<char> peek(const size_t offset = 0) const
    {
        if (index + offset >= src.length())
        {
            return {};
        }
        return src.at(index + offset);
    }

    char take()
    {
        return src.at(index++);
    }

    const std::string src;
    size_t index = 0;
};