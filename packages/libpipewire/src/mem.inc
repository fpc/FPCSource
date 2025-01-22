
type
  Ppw_map_range = ^Tpw_map_range;
  Ppw_memblock = ^Tpw_memblock;
  Ppw_memblock_flags = ^Tpw_memblock_flags;
  Ppw_memchunk = ^Tpw_memchunk;
  Ppw_memmap = ^Tpw_memmap;
  Ppw_memmap_flags = ^Tpw_memmap_flags;
  Ppw_mempool = ^Tpw_mempool;
  Ppw_mempool_events = ^Tpw_mempool_events;

  Tpw_memblock_flags = (
    PW_MEMBLOCK_FLAG_NONE := 0,
    PW_MEMBLOCK_FLAG_READABLE := 1 shl 0,
    PW_MEMBLOCK_FLAG_WRITABLE := 1 shl 1,
    PW_MEMBLOCK_FLAG_SEAL := 1 shl 2,
    PW_MEMBLOCK_FLAG_MAP := 1 shl 3,
    PW_MEMBLOCK_FLAG_DONT_CLOSE := 1 shl 4,
    PW_MEMBLOCK_FLAG_DONT_NOTIFY := 1 shl 5,
    PW_MEMBLOCK_FLAG_READWRITE := Ord(PW_MEMBLOCK_FLAG_READABLE) or Ord(PW_MEMBLOCK_FLAG_WRITABLE)
    );

  Tpw_memmap_flags = (
    PW_MEMMAP_FLAG_NONE := 0,
    PW_MEMMAP_FLAG_READ := 1 shl 0,
    PW_MEMMAP_FLAG_WRITE := 1 shl 1,
    PW_MEMMAP_FLAG_TWICE := 1 shl 2,
    PW_MEMMAP_FLAG_PRIVATE := 1 shl 3,
    PW_MEMMAP_FLAG_LOCKED := 1 shl 4,
    PW_MEMMAP_FLAG_READWRITE := Ord(PW_MEMMAP_FLAG_READ) or Ord(PW_MEMMAP_FLAG_WRITE)
    );

  Tpw_memchunk = record
    {undefined structure}
  end;

  Tpw_mempool = record
    props: Ppw_properties;
  end;

  Tpw_memblock = record
    pool: Ppw_mempool;
    id: Tuint32_t;
    ref: longint;
    flags: Tuint32_t;
    _type: Tuint32_t;
    fd: longint;
    size: Tuint32_t;
    map: Ppw_memmap;
  end;

  Tpw_memmap = record
    block: Ppw_memblock;
    ptr: pointer;
    flags: Tuint32_t;
    offset: Tuint32_t;
    size: Tuint32_t;
    tag: array[0..4] of Tuint32_t;
  end;

  Tpw_mempool_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
    added: procedure(Data: pointer; block: Ppw_memblock); cdecl;
    removed: procedure(Data: pointer; block: Ppw_memblock); cdecl;
  end;

  Tpw_map_range = record
    start: Tuint32_t;
    offset: Tuint32_t;
    size: Tuint32_t;
  end;


  tpw_memblock_maptagarray = array[0..4] of Tuint32_t;

function pw_mempool_new(props: Ppw_properties) : Ppw_mempool; cdecl; external libpipewirename Name 'pw_mempool_new';

procedure pw_mempool_add_listener(pool: Ppw_mempool; listener: Pspa_hook; events: Ppw_mempool_events; Data: pointer); cdecl; external libpipewirename Name 'pw_mempool_add_listener';

procedure pw_mempool_clear(pool: Ppw_mempool); cdecl; external libpipewirename Name 'pw_mempool_clear';

procedure pw_mempool_destroy(pool: Ppw_mempool); cdecl; external libpipewirename Name 'pw_mempool_destroy';

function pw_mempool_alloc(pool: Ppw_mempool; flags: Tpw_memblock_flags; _type: Tuint32_t; size: Tsize_t) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_alloc';

function pw_mempool_import_block(pool: Ppw_mempool; mem: Ppw_memblock) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_import_block';

function pw_mempool_import(pool: Ppw_mempool; flags: Tpw_memblock_flags; _type: Tuint32_t; fd: longint) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_import';

procedure pw_memblock_free(mem: Ppw_memblock); cdecl; external libpipewirename Name 'pw_memblock_free';

  {
  static inline void pw_memblock_unref(struct pw_memblock *mem)
  
   if (--mem->ref == 0)
    pw_memblock_free(mem);
  
   }
function pw_mempool_remove_id(pool: Ppw_mempool; id: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_mempool_remove_id';

function pw_mempool_find_ptr(pool: Ppw_mempool; ptr: pointer) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_find_ptr';

function pw_mempool_find_id(pool: Ppw_mempool; id: Tuint32_t) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_find_id';

function pw_mempool_find_fd(pool: Ppw_mempool; fd: longint) : Ppw_memblock; cdecl; external libpipewirename Name 'pw_mempool_find_fd';
function pw_memblock_map(block: Ppw_memblock; flags: Tpw_memmap_flags; offset: Tuint32_t; size: Tuint32_t; tag: tpw_memblock_maptagarray) : Ppw_memmap; cdecl; external libpipewirename Name 'pw_memblock_map';

function pw_mempool_map_id(pool: Ppw_mempool; id: Tuint32_t; flags: Tpw_memmap_flags; offset: Tuint32_t; size: Tuint32_t; tag: tpw_memblock_maptagarray) : Ppw_memmap;
  cdecl; external libpipewirename Name 'pw_mempool_map_id';

function pw_mempool_import_map(pool: Ppw_mempool; other: Ppw_mempool; Data: pointer; size: Tuint32_t; tag: tpw_memblock_maptagarray) : Ppw_memmap; cdecl; external libpipewirename Name 'pw_mempool_import_map';

function pw_mempool_find_tag(pool: Ppw_mempool; tag: tpw_memblock_maptagarray; size: Tsize_t) : Ppw_memmap; cdecl; external libpipewirename Name 'pw_mempool_find_tag';

function pw_memmap_free(map: Ppw_memmap) : longint; cdecl; external libpipewirename Name 'pw_memmap_free';


  {
  
  static inline void pw_map_range_init(struct pw_map_range *range,
           uint32_t offset, uint32_t size,
           uint32_t page_size)
  
   range->offset = ((offset) & ~((page_size) - 1));
   range->start = offset - range->offset;
   range->size = (((range->start + size) + ((page_size) - 1)) & ~((page_size) - 1));
  
  
   }

