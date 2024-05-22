#pragma once

#include <string>

enum class Type { undefined, _int, _bool, _void };

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