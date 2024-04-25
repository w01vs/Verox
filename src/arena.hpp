#pragma once

class ArenaAllocator
{
public:
    inline explicit ArenaAllocator(size_t bytes) : size(bytes)
    {
        buffer = (std::byte*)malloc(size);
        offset = buffer;
    }

    inline ArenaAllocator(const ArenaAllocator& other) = delete;

    inline ArenaAllocator& operator=(const ArenaAllocator& other) = delete;
    
    inline ~ArenaAllocator() {
        free(buffer);
    }

    template<typename T>
    inline T* push() {
        std::byte* alloc_offset = offset;
        offset += sizeof(T);
        return (T*)alloc_offset;
    }
private:
    size_t size;
    std::byte buffer;
    std::byte offset;
};