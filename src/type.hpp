#pragma once

#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <variant>
#include <vector>

enum class Type { _int = 0, _bool = 1, _void = 2, undefined = 3, _untyped = 4, _string = 5 };
extern std::map<Type, int> type_size;

struct UDType {
    std::string name;
    int bytes;
    std::vector<std::string> member_names;
    std::map<std::string, std::variant<Type, UDType>> members;
    std::map<std::string, int> offsets;
    UDType(std::string name, int bytes, std::map<std::string, std::variant<Type, UDType>> members) : name(name), bytes(bytes), members(members)
    {
        int total_offset = 0;
        id = id_count++;
        for(auto p : members)
        {
            member_names.emplace_back(p.first);
            offsets[p.first] = total_offset;
            switch(p.second.index())
            {
            case 0: {
                Type t = std::get<Type>(p.second);
                total_offset += type_size.at(t);
                break;
            }
            case 1: {
                UDType t = std::get<UDType>(p.second);
                total_offset += t.bytes;
                break;
            }
            }
        }
    }
    // function stuff later
    int ID() { return id; }
    bool operator==(const UDType& other) const { return id == other.id; }

  private:
    inline static int id_count = 0;
    int id;
};

class TypeControl {
  public:
    static TypeControl* GetInstance()
    {
        static TypeControl instance;
        return &instance;
    }
    int RegisterType(UDType type)
    {
        typenames.emplace(type.name, type);
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
    static bool is_internal(UDType type)
    {
        if(type == _int || type == _bool || type == _void)
            return true;
        return false;
    }

  private:
    TypeControl()
    {
        RegisterType(_int);
        RegisterType(_bool);
        RegisterType(_void);
    }
    ~TypeControl() = default;
    std::map<std::string, UDType> typenames;
};

inline std::string type_string(std::variant<UDType, Type> type)
{
    if(type.index() == 0)
    {
        UDType t = std::get<UDType>(type);
        return t.name;
    }
    else if(type.index() == 1)
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

extern std::map<Type, int> type_size;