#pragma once

#include <cstdlib>
#include <map>
#include <string>
#include <vector>

struct GeneralType {
    std::string name;
    int bytes;

    GeneralType(std::string name, int bytes) : name(name), bytes(bytes) { id = id_count++; }
    int ID() { return id; }
    bool operator==(const GeneralType& other) const { return id == other.id; }
    virtual ~GeneralType() = default;
  private:
    int id;
    inline static int id_count = 0;
};

struct UDType : GeneralType {
    std::vector<std::string> member_names;
    std::map<std::string, GeneralType*> members;
    std::map<std::string, int> offsets;
    UDType(std::string name, int bytes, std::map<std::string, GeneralType*> members) : GeneralType(name, bytes), members(members)
    {
        int total_offset = 0;
        for(auto p : members)
        {
            member_names.emplace_back(p.first);
            offsets[p.first] = total_offset;
            total_offset += p.second->bytes;
        }
    }
};

class TypeControl {
  public:
    static TypeControl* GetInstance()
    {
        static TypeControl instance;
        return &instance;
    }
    int RegisterType(const GeneralType& type)
    {
        typenames.emplace(type.name, &type);
        return 0;
    }
    int UnregisterType(const GeneralType& type)
    {
        auto end = typenames.find(type.name);
        if(end == typenames.end())
            return -1;
        typenames.erase(end);
        return 0;
    }
    GeneralType* FindType(const UDType& type)
    {
        if(typenames.count(type.name) > 0)
            return typenames.at(type.name);
        return nullptr;
    }
    GeneralType* FindType(std::string name)
    {
        if(typenames.count(name) > 0)
            return typenames.at(name);
        return nullptr;
    }
    TypeControl(const TypeControl& other) = delete;
    TypeControl& operator=(const TypeControl& other) = delete;
    inline static const GeneralType _int = GeneralType("int", 8);
    inline static const GeneralType _bool = GeneralType("bool", 1);
    inline static const GeneralType _void = GeneralType("void", 0);
    inline static const GeneralType _string = GeneralType("string", 8);
    inline static const GeneralType _undefined = GeneralType("undefined", 0);
    static bool is_internal(const GeneralType& type)
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
        RegisterType(_string);
        RegisterType(_undefined);
    }
    ~TypeControl() = default;
    std::map<std::string, GeneralType*> typenames;
};
