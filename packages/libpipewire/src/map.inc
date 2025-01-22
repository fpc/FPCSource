
type
  Ppw_map = ^Tpw_map;
  Ppw_map_item = ^Tpw_map_item;

  Tuintptr_t = Pointer;
  Tpw_map_item = record
    case longint of
      0: (Next: Tuintptr_t);
      1: (Data: pointer);
  end;

  Tpw_map = record
    items: Tpw_array;
    free_list: Tuint32_t;
  end;

  {
  static inline void pw_map_init(struct pw_map *map, size_t size, size_t extend)
  
   pw_array_init(&map->items, extend * sizeof(union pw_map_item));
   pw_array_ensure_size(&map->items, size * sizeof(union pw_map_item));
   map->free_list = ((uint32_t)0xffffffff);
  
  
  
  
  
  static inline void pw_map_clear(struct pw_map *map)
  
   pw_array_clear(&map->items);
  
  
  
  
  
  static inline void pw_map_reset(struct pw_map *map)
  
   pw_array_reset(&map->items);
   map->free_list = ((uint32_t)0xffffffff);
  
  
  
  
  
  
  
  
  static inline uint32_t pw_map_insert_new(struct pw_map *map, void *data)
  
   union pw_map_item *start, *item;
   uint32_t id;
  
   if (map->free_list != ((uint32_t)0xffffffff)) 
    start = (union pw_map_item *) map->items.data;
    item = &start[map->free_list >> 1];
    map->free_list = item->next;
    else 
    item = (union pw_map_item *) pw_array_add(&map->items, sizeof(union pw_map_item));
    if (item == 
  # 157 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
               ((void *)0)
  # 157 "/usr/include/pipewire-0.3/pipewire/map.h"
                   )
     return ((uint32_t)0xffffffff);
    start = (union pw_map_item *) map->items.data;
   
   item->data = data;
   id = (item - start);
   return id;
  
  # 173 "/usr/include/pipewire-0.3/pipewire/map.h"
  static inline int pw_map_insert_at(struct pw_map *map, uint32_t id, void *data)
  
   size_t size = ((&(map)->items)->size / (sizeof(union pw_map_item)));
   union pw_map_item *item;
  
   if (id > size)
    return -
  # 179 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
           28
  # 179 "/usr/include/pipewire-0.3/pipewire/map.h"
                 ;
   else if (id == size) 
    item = (union pw_map_item *) pw_array_add(&map->items, sizeof(union pw_map_item));
    if (item == 
  # 182 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
               ((void *)0)
  # 182 "/usr/include/pipewire-0.3/pipewire/map.h"
                   )
     return -
  # 183 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
            (*__errno_location ())
  # 183 "/usr/include/pipewire-0.3/pipewire/map.h"
                 ;
    else 
    item = ((union pw_map_item*)((uintptr_t)((&(map)->items)->data) + (ptrdiff_t)((id)*(sizeof(union pw_map_item)))));
    if (((item)->next & 0x1))
     return -
  # 187 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
            22
  # 187 "/usr/include/pipewire-0.3/pipewire/map.h"
                  ;
   
   item->data = data;
   return 0;
  
  
  
  
  
  
  
  static inline void pw_map_remove(struct pw_map *map, uint32_t id)
  
   if ((((((union pw_map_item*)((uintptr_t)((&(map)->items)->data) + (ptrdiff_t)((id)*(sizeof(union pw_map_item))))))->next & 0x1)))
    return;
  
   ((union pw_map_item*)((uintptr_t)((&(map)->items)->data) + (ptrdiff_t)((id)*(sizeof(union pw_map_item)))))->next = map->free_list;
   map->free_list = (id << 1) | 1;
  
  
  
  
  
  
  
  static inline void *pw_map_lookup(struct pw_map *map, uint32_t id)
  
   if ((__builtin_expect(!!(((id) < ((&(map)->items)->size / (sizeof(union pw_map_item))))),1))) 
    union pw_map_item *item = ((union pw_map_item*)((uintptr_t)((&(map)->items)->data) + (ptrdiff_t)((id)*(sizeof(union pw_map_item)))));
    if (!((item)->next & 0x1))
     return item->data;
   
   return 
  # 219 "/usr/include/pipewire-0.3/pipewire/map.h" 3 4
         ((void *)0)
  # 219 "/usr/include/pipewire-0.3/pipewire/map.h"
             ;
  
  # 230 "/usr/include/pipewire-0.3/pipewire/map.h"
  static inline int pw_map_for_each(struct pw_map *map,
         int (*func) (void *item_data, void *data), void *data)
  
   union pw_map_item *item;
   int res = 0;
  
   for (item = (__typeof__(item)) ((&map->items)->data); (((void*)((uintptr_t)(item) + (ptrdiff_t)(sizeof(*item)))) <= ((void*)((uintptr_t)((&map->items)->data) + (ptrdiff_t)((&map->items)->size)))); (item)++) 
    if (!((item)->next & 0x1))
     if ((res = func(item->data, data)) != 0)
      break;
   
   return res;
  
  
   }

