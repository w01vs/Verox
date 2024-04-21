#pragma once

#include "lexer.hpp"
#include <iostream>
#include <variant>

enum class Type
{
    undefined,
    _int
};

struct NodeExprIInt
{
    Token i_int; // immediate int
};

struct NodeExprIdent
{
    Token ident; // var name
};

struct NodeExpr
{
    std::variant<NodeExprIdent, NodeExprIInt> var;
    std::optional<Type> type;
};

struct NodeInternalRet
{
    NodeExpr ret; // return val
};

// internal 'functions' such as return and possibly print etc.
struct NodeInternal
{
    std::variant<NodeInternalRet> ret; // return
};

// variable declaration with a type.
struct NodeStmtVar
{
    Token ident; // var name
    Type type;
    NodeExpr expr; // var value
};

struct NodeStmt
{
    std::variant<NodeInternal, NodeStmtVar> var; // Internal stuff or variable
};

struct NodeProg
{
    std::vector<NodeStmt> stmts; // All statements
};

std::string type_string(Type type)
{
    switch (type)
    {
    case Type::_int:
        return "int";
    default:
        return "undefined";
    }
}

class Parser
{
public:
    explicit Parser(std::vector<Token> tokens) : tokens(tokens) {}

    std::optional<NodeExpr> parse_expr()
    {
        NodeExpr expr;
        if (peek().has_value() && peek().value().type == TokenType::i_int)
        {
            return NodeExpr{NodeExprIInt{take()}, Type::_int};
        }
        else if (peek().has_value() && peek().value().type == TokenType::ident)
        {
            return NodeExpr{NodeExprIdent{take()}};
        }
        return {};
    }

    std::optional<NodeStmt> parse_stmt()
    {
        if (peek().has_value())
        {
            if (peek().value().type == TokenType::ret)
            {
                take();
                NodeInternalRet ret;
                if (auto expr = parse_expr())
                {
                    ret = {expr.value()};
                }
                if (peek().has_value() && peek().value().type == TokenType::semi)
                {
                    take();
                }
                else
                {
                    std::cerr << "Expected ';'" << std::endl;
                    exit(EXIT_FAILURE);
                }
                return NodeStmt{NodeInternal{ret}};
            }
            else if (peek().value().type == TokenType::type && peek(1).has_value() && peek(1).value().type == TokenType::ident && peek(2).has_value() && peek(2).value().type == TokenType::assign)
            {
                Type type = get_type(take());
                NodeStmtVar stmt_var = {take(), type};
                take(); // take '=' operator
                if (auto expr = parse_expr())
                {
                    stmt_var.expr = expr.value();
                }
                else
                {
                    std::cerr << "SyntaxError: Expected a value of type '" << type_string(stmt_var.type) << "' but got nothing" << std::endl;
                }
                if (!semicolon())
                {
                    std::cerr << "SyntaxError: Expected ';' on line ";
                    
                    std::visit([](auto&& arg)
                    { if constexpr(std::is_same_v<decltype(arg), NodeExprIdent>) {std::cout << arg.ident.line << std::endl;}
                    else if constexpr(std::is_same_v<decltype(arg), NodeExprIInt>) { std::cout << arg.i_int.line << std::endl;}}
                    , stmt_var.expr.var);

                    exit(EXIT_FAILURE);
                }
                return NodeStmt{stmt_var};
            }
        }

        return {};
    }

    std::optional<NodeProg> parse()
    {
        NodeProg root;
        while (peek().has_value())
        {   
            if (auto stmt = parse_stmt())
            {
                root.stmts.push_back(stmt.value());
            }
            else
            {
                std::cerr << "Invalid statement" << std::endl;
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

    Type get_type(Token token)
    {
        if (token.val.has_value() && token.val.value() == "int")
        {
            return Type::_int;
        }
        return Type::undefined;
    }

    bool semicolon()
    {
        if (peek().has_value() && peek().value().type == TokenType::semi)
        {
            take();
            return true;
        }
        return false;
    }
    std::vector<Token> tokens;
    size_t index = 0;
};