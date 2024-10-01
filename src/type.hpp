#pragma once

#include <string>
#include <map>
#include "tokens.hpp"

enum class Type { _int = 0, _bool = 1, _void = 2, undefined = 3, _untyped = 4, _string = 5 };

inline std::string print_type(Type type)
{
    switch(type)
    {
    case Type::_int:
        return "int";
        break;
    case Type::_bool:
        return "bool";
        break;
    case Type::_void:
        return "void";
        break;
    default:
        return "undefined";
        break;
    }
}

extern std::map<TokenType, Type> token_type_map;
