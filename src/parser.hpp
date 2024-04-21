#pragma once

#include "lexer.hpp"
#include <iostream>

struct NodeExpr
{
    Token i_int;
};

struct NodeRet
{
    Token ret;
};

struct NodeProg
{
    NodeExpr expr;
};

struct NodeStmt
{
    NodeExpr *expr;
    NodeRet *ret;
};

class Parser
{
public:
    explicit Parser(std::vector<Token> tokens) : tokens(tokens) {}

    std::optional<NodeExpr> parse_expr()
    {
        NodeExpr expr;
        if (peek().has_value() && peek().value().type == TokenType::i_int)
        {
            return NodeExpr{.i_int = take()};
        }
        return {};
    }
    /*
        std::optional<NodeRet> parse_ret()
        {
            NodeRet ret;
            if (peek().has_value() && peek().value().type == TokenType::ret)
            {
            }
        }


        std::optional<NodeStmt> parse_stmt()
        {
            std::optional<NodeStmt> stmt;
            if (peek().has_value() && peek().value().type == TokenType::ret)
            {
                NodeRet r = parse_ret().value();
                stmt.ret = &r;
            }
        }
    */
    std::optional<NodeProg> parse()
    {
        std::optional<NodeProg> root;
        while (peek().has_value())
        {
            if (peek().has_value() && peek().value().type == TokenType::ret)
            {
                take();
                if (auto expr = parse_expr())
                {
                    root = NodeProg{.expr = (expr.value())};
                }
            }
            else
            {
                std::cerr << "Invalid expression" << std::endl;
                exit(EXIT_FAILURE);
            }

            if (peek().has_value() && peek().value().type == TokenType::semi)
            {
                take();
            }
            else
            {
                std::cerr << "Expected ';' at line " << std::endl;
                exit(EXIT_FAILURE);
            }
        }
        return root;
    }

private:
    [[nodiscard]] std::optional<Token> peek(const int offset = 0) const
    {
        if (index + offset >= tokens.size())
        {
            return {};
        }

        return tokens.at(index + offset);
    }

    Token take()
    {
        return tokens.at(index++);
    }
    std::vector<Token> tokens;
    size_t index = 0;
};