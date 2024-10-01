#pragma once

#ifndef USERDEFINED
#define USERDEFINED

#include "type.hpp"
#include <map>
#include <variant>

struct UserDefinedType {
    int bytes;
    std::map<std::string, std::variant<Type, UserDefinedType>> members;
    // function stuff later
};

class UserDefinedControl {
  public:
    static UserDefinedControl GetInstance() {
        if(instance == nullptr) {
            instance = new UserDefinedControl();
        }

        return *instance;
    }
    int RegisterType(UserDefinedType type) 
    {
        types[type] = true;
        return 0;
    }
    int UnregisterType(UserDefinedType type) {
        auto end = types.find(type);
        if(end == types.end())
            return -1;
        types.erase(end);
        return 0;
    }

  private:
    UserDefinedControl();
    ~UserDefinedControl() {delete instance;};
    inline static UserDefinedControl* instance = nullptr;
    std::map<UserDefinedType, bool> types;
};

#endif // USERDEFINED
