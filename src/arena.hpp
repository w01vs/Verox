#pragma once

#include <memory>
#include <cstddef>
#include <utility>

class ArenaAllocator
{
public:
    inline explicit ArenaAllocator(size_t bytes) : size(bytes)
    {
        buffer = new std::byte[bytes];
        offset = buffer;
    }

    inline ArenaAllocator(const ArenaAllocator& other) = delete;

    inline ArenaAllocator& operator=(const ArenaAllocator& other) = delete;
    
    inline ~ArenaAllocator() {
        delete[] buffer;
    }

    template<typename T>
    inline T* alloc() {
        size_t remainder = size - (size_t) (offset - buffer);
        auto pointer = (void*)offset;
        const auto aligned = std::align(alignof(T), sizeof(T), pointer, remainder);
        if (aligned == nullptr) {
            throw std::bad_alloc {};
        }
        offset = (std::byte*)(aligned) + sizeof(T);
        return (T*)aligned;
    }

    template <typename T, typename... Args>
    [[nodiscard]] inline T* emplace(Args&&... args) {
        const auto allocated = alloc<T>();
        return new (allocated) T { std::forward<Args>(args)... };
    }
private:
    size_t size;
    std::byte* buffer;
    std::byte* offset;
};