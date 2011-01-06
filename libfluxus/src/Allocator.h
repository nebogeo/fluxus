// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <memory.h>
#include <limits>
#include <malloc.h>
#include "dada.h"

#ifndef FLUXUS_ALLOCATOR
#define FLUXUS_ALLOCATOR

#define FLX_ALLOC(T) std::allocator<T>
//#define FLX_ALLOC(T) Fluxus::allocator<T>

static long long stats_allocated=0;
static long long stats_highwater=0;

namespace Fluxus
{
    template <class T> class allocator;
    
    template <class T>
    class allocator
    {
    public:
        typedef size_t size_type;
        typedef ptrdiff_t difference_type;
        typedef T* pointer;
        typedef const T* const_pointer;
        typedef T& reference;
        typedef const T& const_reference;
        typedef T value_type;

        template <class U>
        struct rebind
        {
            typedef allocator<U> other;
        };

        allocator() throw()
        {
        }

        template <class U>
        allocator(const allocator<U>& u) throw()
        {
        }

        ~allocator() throw()
        {
        }

        pointer address(reference r) const
        {
            return &r;
        }

        const_pointer address(const_reference r) const
        {
            return &r;
        }

        size_type max_size() const throw()
        {
            return std::numeric_limits<size_t>::max()/sizeof(T);
        }

        pointer allocate(size_type n, allocator<T>::const_pointer hint = 0)
        {
            stats_allocated+=1;
            if (stats_allocated>stats_highwater)
            {
                stats_highwater=stats_allocated;
                std::cerr<<"highwater now "<<stats_highwater<<std::endl;
            }
            return reinterpret_cast<pointer>(malloc(n * sizeof(T)));
        }

        void deallocate(pointer p, size_type n)
        {
            stats_allocated--;
            free(p);
        }

        void construct(pointer p, const_reference val)
        {
            ::new(p) T(val);
        }

        void destroy(pointer p)
        {
            p->~T();
        }
    };

template <class T1, class T2>
inline
bool operator==(const allocator<T1>& a1, const allocator<T2>& a2) throw()
{
    return true;
}

template <class T1, class T2>
inline
bool operator!=(const allocator<T1>& a1, const allocator<T2>& a2) throw()
{
    return false;
}

}

#endif
