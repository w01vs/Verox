#pragma once

#include <optional>
#include <string>

enum class TokenType {
    // stuff
    _ret,           // return -> internal
    _int_lit,         // immediate int
    _semi,          // ';'
    _type,          // a typename
    _ident,         // a variable name
    _assign,        // '='
    _print,         // print -> internal
    _if,
    _else,
    _while,
    _for,
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
    _close_b,       // '{'
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
};

struct Token {
    TokenType type;
    int line;
    std::optional<std::string> val{};
};