
{$IFDEF OGC_INTERFACE}
{ $include machine/asm.inc}

const
  HEAP_BLOCK_USED = 1;
  HEAP_BLOCK_FREE = 0;
  HEAP_DUMMY_FLAG = ( 0 + HEAP_BLOCK_USED );

{$define HEAP_OVERHEAD := (sizeof(cuint32) * 2)}
{$define HEAP_BLOCK_USED_OVERHEAD := (sizeof(pointer) * 2)}
{$define HEAP_MIN_SIZE := (HEAP_OVERHEAD + sizeof(heap_block))}


type
  pheap_block = ^_heap_block_st;
  _heap_block_st = record
    back_flag : cuint32;
    front_flag : cuint32;
    next : Pheap_block;
    prev : Pheap_block;
  end;
  heap_block = _heap_block_st;

  _heap_iblock_st = record
    free_blocks : cuint32;
    free_size : cuint32;
    used_blocks : cuint32;
    used_size : cuint32;
  end;
  heap_iblock = _heap_iblock_st;
  pheap_iblock = ^heap_iblock;

  _heap_cntrl_st = record
    start : Pheap_block;
    final : Pheap_block;
    first : Pheap_block;
    perm_null : Pheap_block;
    last : Pheap_block;
    pg_size : cuint32;
    reserved : cuint32;
  end;
  heap_cntrl = _heap_cntrl_st;
  pheap_cntrl = ^heap_cntrl;


function __lwp_heap_init(theheap: Pheap_cntrl; start_addr: pointer;
  size, pg_size: cuint32): cuint32; cdecl; external;

function __lwp_heap_allocate(theheap: Pheap_cntrl; size: cuint32): pointer; cdecl; external;

function __lwp_heap_free(theheap: Pheap_cntrl; ptr: pointer): cbool; cdecl; external;

function __lwp_heap_getinfo(theheap: Pheap_cntrl; theinfo: Pheap_iblock)
  : cuint32; cdecl; external;


//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_heap.inl}
//{$endif}
{$ENDIF}


