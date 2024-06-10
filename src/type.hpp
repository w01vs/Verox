#pragma once

#include <string>

enum class Type { _int = 0, _bool = 1, _void = 2, undefined = 3 };

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