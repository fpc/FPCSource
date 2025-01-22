

type
  Ppw_array = ^Tpw_array;
  Tpw_array = record
    data : pointer;
    size : Tsize_t;
    alloc : Tsize_t;
    extend : Tsize_t;
  end;

  {
  
  static inline void pw_array_init(struct pw_array *arr, size_t extend)
  
   arr->data = 
  # 92 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
              ((void *)0)
  # 92 "/usr/include/pipewire-0.3/pipewire/array.h"
                  ;
   arr->size = arr->alloc = 0;
   arr->extend = extend;
  
  
  
  static inline void pw_array_clear(struct pw_array *arr)
  
   free(arr->data);
   pw_array_init(arr, arr->extend);
  
  
  
  static inline void pw_array_reset(struct pw_array *arr)
  
   arr->size = 0;
  
  
  
  static inline int pw_array_ensure_size(struct pw_array *arr, size_t size)
  
   size_t alloc, need;
  
   alloc = arr->alloc;
   need = arr->size + size;
  
   if ((__builtin_expect(!!(alloc < need),0))) 
    void *data;
    alloc = ( __typeof__(alloc) _max_a = (alloc); __typeof__(arr->extend) _max_b = (arr->extend); (__builtin_expect(!!(_max_a > _max_b),1)) ? _max_a : _max_b; );
    do  if ((__builtin_expect(!!(!(alloc != 0)),0)))  fprintf(
  # 121 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
   stderr
  # 121 "/usr/include/pipewire-0.3/pipewire/array.h"
   , "'%s' failed at %s:%u %s()\n", "alloc != 0" , "/usr/include/pipewire-0.3/pipewire/array.h", 121, __func__); abort();   while (
  # 121 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
   0
  # 121 "/usr/include/pipewire-0.3/pipewire/array.h"
   );
    while (alloc < need)
     alloc *= 2;
    if ((__builtin_expect(!!((data = realloc(arr->data, alloc)) == 
  # 124 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
       ((void *)0)
  # 124 "/usr/include/pipewire-0.3/pipewire/array.h"
       ),0)))
     return -
  # 125 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
            (*__errno_location ())
  # 125 "/usr/include/pipewire-0.3/pipewire/array.h"
                 ;
    arr->data = data;
    arr->alloc = alloc;
   
   return 0;
  
  
  
  
  static inline void *pw_array_add(struct pw_array *arr, size_t size)
  
   void *p;
  
   if (pw_array_ensure_size(arr, size) < 0)
    return 
  # 139 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
          ((void *)0)
  # 139 "/usr/include/pipewire-0.3/pipewire/array.h"
              ;
  
   p = ((void*)((uintptr_t)(arr->data) + (ptrdiff_t)(arr->size)));
   arr->size += size;
  
   return p;
  
  
  
  
  static inline void *pw_array_add_fixed(struct pw_array *arr, size_t size)
  
   void *p;
  
   if ((__builtin_expect(!!(arr->alloc < arr->size + size),0))) 
    
  # 154 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
   (*__errno_location ()) 
  # 154 "/usr/include/pipewire-0.3/pipewire/array.h"
         = 
  # 154 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
           28
  # 154 "/usr/include/pipewire-0.3/pipewire/array.h"
                 ;
    return 
  # 155 "/usr/include/pipewire-0.3/pipewire/array.h" 3 4
          ((void *)0)
  # 155 "/usr/include/pipewire-0.3/pipewire/array.h"
              ;
   
  
   p = ((void*)((uintptr_t)(arr->data) + (ptrdiff_t)(arr->size)));
   arr->size += size;
  
   return p;
  
  
   }

