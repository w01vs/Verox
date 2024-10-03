#pragma once

#include "tokens.hpp"
#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <variant>

enum class Type { _int = 0, _bool = 1, _void = 2, undefined = 3, _untyped = 4, _string = 5 };

struct UDType {
    std::string name;
    int bytes;
    std::map<std::string, std::variant<Type, UDType>> members;
    UDType(std::string name, int bytes,  std::map<std::string, std::variant<Type, UDType>> members) : name(name), bytes(bytes), members(members) { id = id_count++;}
    // function stuff later
    int ID() { return id;}
    bool operator==(const UDType& other) const { return id == other.id; }

    private:
    inline static int id_count = 0;
    int id;
};

class TypeControl {
  public:
    static TypeControl& GetInstance()
    {
        static TypeControl instance;
        return instance;
    }
    int RegisterType(UDType type)
    {
        typenames[type.name] = type;
        return 0;
    }
    int UnregisterType(UDType type)
    {
        auto end = typenames.find(type.name);
        if(end == typenames.end())
            return -1;
        typenames.erase(end);
        return 0;
    }
    UDType* FindType(UDType type)
    {
        if(typenames.count(type.name) > 0)
            return &typenames.at(type.name);
        return nullptr;
    }
    UDType* FindType(std::string name)
    {
        if(typenames.count(name) > 0)
            return &typenames.at(name);
        return nullptr;
    }
    TypeControl(const TypeControl& other) = delete;
    TypeControl& operator=(const TypeControl& other) = delete;
    inline static const UDType _int = UDType("int", 8, {{"single", Type::_int}});
    inline static const UDType _bool = UDType("bool", 1, {{"single", Type::_bool}});
    inline static const UDType _void = UDType("void", 0, {{"single", Type::_void}});

  private:
    TypeControl()
    {
        RegisterType(_int);
        RegisterType(_bool);
        RegisterType(_void);
    }
    ~TypeControl();
    std::map<std::string, UDType> typenames;
}; 

inline std::string type_string(std::variant<UDType, Type> type)
{
    if(type.index() == 1)
    {
        UDType t = std::get<UDType>(type);
        return t.name;
    }
    else if(type.index() == 2)
    {
        Type t = std::get<Type>(type);
        switch(t)
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
    std::cerr << "type does not exist" << std::endl;
    exit(EXIT_FAILURE);
}

extern std::map<TokenType, Type> token_type_map;
