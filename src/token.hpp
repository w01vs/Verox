#pragma once

#include <optional>
#include <string>

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