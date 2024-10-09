#pragma once

#include <optional>
#include <string>

enum class TokenType {
    // stuff
    _ret,           // return -> internal
    _int_lit,       // immediate int
    _string,        // immediate string (ASCII)
    _semi,          // ';'
    _type,          // a typename
    _ident,         // a variable name
    _assign,        // '='
    _print,         // print -> internal
    _if,            // if
    _else,          // else
    _while,         // while
    _for,           // for
    _break,         // break
    _continue,      // continue
    // Precedence
    _open_p,        // '('
    _close_p,       // ')'
    // Mafs
    _add,           // '+'
    _star,          // '*'
    _fslash,        // '/'
    _minus,         // '-'
    // Scope
    _open_b,        // '{'
    _close_b,       // '}'
    // Logic
    _true,          // boolean true
    _false,         // boolean false
    _or,            // '||'
    _and,           // '&&'
    _not,           // '!'
    // Comparison
    _eq,            // '=='
    _greater,       // '>'
    _less,          // '<'
    _greater_eq,    // '>='
    _less_eq,       // '<='
    _neq,           // '!='
    // struct things
    _struct,        // struct
    _dot,           // member field accessor
};

struct Token {
    TokenType type;
    int line;
    std::optional<std::string> val{};
    Token() {}
    Token(TokenType type, int line) : type(type), line(line) {}
    Token(TokenType type, int line, std::string val) : type(type), line(line), val(val) {}
};