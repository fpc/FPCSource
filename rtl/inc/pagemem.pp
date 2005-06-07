{
    $Id: $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Daniel Mantione
      member of the Free Pascal development team

    Implements a memory manager that makes use of the fact that
    a program is running in a virtual address space where pages
    can be allocated at random, instead of a more traditional
    growing heap.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit pagemem;

{*****************************************************************************}
                                   interface
{*****************************************************************************}

{*****************************************************************************}
                                implementation
{*****************************************************************************}

{$packrecords 1}
{$packenum 1}

type    Tpage_type=(pt_8byte_with_bitmap,pt_suballocation,pt_direct_page);
        Ppage_type=^Tpage_type;

        Pcriterium=^Tcriterium;
        Tcriterium=record
            criterium1,criterium2:cardinal;
        end;

        Ptree_struct=^Ttree_struct;
        Ttree_struct=record
            left,right:ptruint;
        end;

        {This page layout is targeted at very short strings and linked lists
         with very low payload. It uses fixed memory sizes of 8 byte. Memory
         overhead should be avoided at all here. An allocation bitmap does this
         very well, only 1 bit per memory block.}
        Ppage_8byte_with_bitmap=^Tpage_8byte_with_bitmap;
        Tpage_8byte_with_bitmap=record
            page_type:Tpage_type;
            search_index:byte;
            free_count:word;
            page_birthyear:cardinal;
            freelist_prev,freelist_next:Ppage_8byte_with_bitmap;
            block_allocation_map:array[0..15] of cardinal;
        end;

        Ppage_suballocation=^Tpage_suballocation;
        Tpage_suballocation=record
            page_type:Tpage_type;
            reserved:array[1..3] of byte;
            page_birthyear:cardinal;
        end;

        {This page layout is targeted at large memory blocks. We allocate
         pages directly from the OS for such blocks.}
        Ppage_direct=^Tpage_direct;
        Tpage_direct=record
            page_type:Tpage_type;
            reserved:array[1..3] of byte;
            size:cardinal;
        end;

        Pfree_block=^Tfree_block;
        Tfree_block=record
            size:cardinal;
            tree_sizememloc:Ttree_struct;
            tree_memlocation:Ttree_struct;
        end;

        Tsplay_status=(ts_not_found,ts_found_on_left,
                       ts_found_on_p,ts_found_on_right);

        Psuballoc_header=^Tsuballoc_header;
        Tsuballoc_header=record
            alloc_size:ptruint;
        end;

const   tree_sizememloc_offset=4;
        tree_memlocation_offset=12;

        page_size=4096;
        page_shift=12;
        page_mask=$00000fff;
        page_8byte_with_bitmap_maxspace=
                       (page_size-sizeof(Tpage_8byte_with_bitmap)) div 8;

        memblock_align=4;
        memblock_alignround=memblock_align-1;

        min_suballoc_size=sizeof(Tfree_block);

const   freelist_8byte_with_bitmap:Ppage_8byte_with_bitmap=nil;
        page_8byte_with_bitmap_init:Tpage_8byte_with_bitmap=
           (
            page_type:pt_8byte_with_bitmap;
            search_index:0;
            free_count:page_8byte_with_bitmap_maxspace;
            page_birthyear:0;
            freelist_prev:nil;
            freelist_next:nil;
            block_allocation_map:($ffffffff,$ffffffff,$ffffffff,$ffffffff,
                                  $ffffffff,$ffffffff,$ffffffff,$ffffffff,
                                  $ffffffff,$ffffffff,$ffffffff,$ffffffff,
                                  $ffffffff,$ffffffff,$ffffffff,$ffffffff)
           );

var tree_sizememloc,tree_memlocation:Pfree_block;

{****************************************************************************
                          Page allocation/deallocation
 ****************************************************************************} 

function fpmmap(adr:pointer;len,prot,flags,fd,off:sizeint):pointer;external name 'FPC_SYSC_MMAP';
function fpmunmap(adr:pointer;len:sizeint):pointer;external name 'FPC_SYSC_MUNMAP';
function geterrno:longint;external name 'FPC_SYS_GETERRNO';

const   PROT_READ  = $1;             { page can be read }
        PROT_WRITE = $2;             { page can be written }
        PROT_EXEC  = $4;             { page can be executed }
        PROT_NONE  = $0;             { page can not be accessed }

        MAP_SHARED    = $1;          { Share changes }
        MAP_PRIVATE   = $2;          { Changes are private }
        MAP_TYPE      = $f;          { Mask for type of mapping }
        MAP_FIXED     = $10;         { Interpret addr exactly }
        MAP_ANONYMOUS = $20;         { don't use a file }

        MAP_GROWSDOWN  = $100;       { stack-like segment }
        MAP_DENYWRITE  = $800;       { ETXTBSY }
        MAP_EXECUTABLE = $1000;      { mark it as an executable }
        MAP_LOCKED     = $2000;      { pages are locked }
        MAP_NORESERVE  = $4000;      { don't check for reservations }

function req_pages(count:cardinal):pointer;

{Requests count consecutive pages from the OS.}

begin
  req_pages:=fpmmap(nil,count shl page_shift,PROT_READ or PROT_WRITE,
                    MAP_PRIVATE or MAP_ANONYMOUS,0,0);
  if geterrno<>0 then
    req_pages:=nil; {This one can fail, so we can handle an out of memory
                     situation.}
end;

procedure sack_pages(p:pointer;count:cardinal);

begin
  fpmunmap(p,count shl page_shift);
  if geterrno<>0 then
    runerror(204); {This one should succees.}
end;

{****************************************************************************
                          8-bit bitmap allocated memory
 ****************************************************************************}

procedure new_page_8byte_with_bitmap;

var page:Ppage_8byte_with_bitmap;

begin
  page:=req_pages(1);
  page^:=page_8byte_with_bitmap_init;
  page^.freelist_next:=freelist_8byte_with_bitmap;
  page^.freelist_prev:=nil;
  if freelist_8byte_with_bitmap<>nil then
    freelist_8byte_with_bitmap^.freelist_prev:=page;
  freelist_8byte_with_bitmap:=page;
end;

function pgetmem_8byte_with_bitmap:pointer;

var page:Ppage_8byte_with_bitmap;
    bit:cardinal;

begin
  if freelist_8byte_with_bitmap=nil then
    new_page_8byte_with_bitmap;
  page:=freelist_8byte_with_bitmap;
  with page^ do
    begin
      {Search a dword in which a bit is set.}
      while block_allocation_map[search_index]=0 do
        search_index:=(search_index+1) and 15;
      ptrint(pgetmem_8byte_with_bitmap):=ptrint(page)+sizeof(page^)+search_index*256;
      {Search for a set bit in the dword.}
      bit:=1;
      while block_allocation_map[search_index] and bit=0 do
        begin
          bit:=bit shl 1;
          inc(ptrint(pgetmem_8byte_with_bitmap),8);
        end;
      {Allocate the block.}
      block_allocation_map[search_index]:=block_allocation_map[search_index] and not bit;
      dec(free_count);
      if free_count=0 then
        begin
          {There is no space left in this page. Remove it from the freelist.}
          if freelist_next<>nil then
            freelist_next^.freelist_prev:=freelist_prev;
          if freelist_prev<>nil then
            freelist_prev^.freelist_next:=freelist_next;
          if freelist_8byte_with_bitmap=page then
            freelist_8byte_with_bitmap:=freelist_next;
          freelist_prev:=nil;
          freelist_next:=nil;
        end;
    end;
end;


function pfreemem_8byte_with_bitmap(page:Ppage_8byte_with_bitmap;p:pointer):ptrint;

var index,bit:cardinal;

begin
  index:=(ptrint(p)-ptrint(page)-sizeof(page^)) div 8;
  bit:=index and 31;
  index:=index shr 5;
  with page^ do
    begin
      if free_count=0 then
        begin
          {Page will get free slots. Must be included in freelist.}
          if freelist_8byte_with_bitmap=nil then
            freelist_8byte_with_bitmap:=page
          else
            begin
              freelist_next:=freelist_8byte_with_bitmap;
              freelist_8byte_with_bitmap^.freelist_prev:=page;
              freelist_8byte_with_bitmap:=page;
            end;
          {Make sure the next allocation finds the slot without much searching.} 
          search_index:=index;
        end;
      block_allocation_map[index]:=block_allocation_map[index] or (1 shl bit);
      inc(free_count);
      if free_count=page_8byte_with_bitmap_maxspace then
        begin
          {The page is completely free. It can be returned to the OS, but
           remove it from the freelist first.}
          if freelist_next<>nil then
            freelist_next^.freelist_prev:=freelist_prev;
          if freelist_prev<>nil then
            freelist_prev^.freelist_next:=freelist_next;
          if freelist_8byte_with_bitmap=page then
            freelist_8byte_with_bitmap:=freelist_next;
          sack_pages(page,1);
        end;
    end;
  pfreemem_8byte_with_bitmap:=8;
end;

{****************************************************************************
                                 Splay tree stuff
 ****************************************************************************}

{ $define debug}
{$ifdef debug}
procedure write_sizememloc_tree(tree:Pfree_block;level:cardinal);

var i:cardinal;

begin
    if tree=nil then
      exit;
    write_sizememloc_tree(Pfree_block(tree^.tree_sizememloc.left),level+1);
    for i:=1 to level do
      write('  ');
    writeln(tree^.size,' ',hexstr(ptruint(tree),8));
    write_sizememloc_tree(Pfree_block(tree^.tree_sizememloc.right),level+1);
end;

procedure write_memlocation_tree(tree:Pfree_block;level:cardinal);

var i:cardinal;

begin
    if tree=nil then
      exit;
    write_memlocation_tree(Pfree_block(tree^.tree_memlocation.left),level+1);
    for i:=1 to level do
      write('  ');
    writeln(hexstr(ptruint(tree),8));
    write_memlocation_tree(Pfree_block(tree^.tree_memlocation.right),level+1);
end;
{$endif}

procedure rotate_l(var p:ptruint;offset:cardinal);

var p1:ptruint;

begin
    p1:=Ptree_struct(p+offset)^.right;
    Ptree_struct(p+offset)^.right:=Ptree_struct(p1+offset)^.left;
    Ptree_struct(p1+offset)^.left:=p;
    p:=p1;
end;

procedure rotate_r(var p:ptruint;offset:cardinal);

var p1:ptruint;

begin
    p1:=Ptree_struct(p+offset)^.left;
    Ptree_struct(p+offset)^.left:=Ptree_struct(p1+offset)^.right;
    Ptree_struct(p1+offset)^.right:=p;
    p:=p1;
end;

procedure zigzig(var p:ptruint;offset:cardinal);inline;

begin
    rotate_r(p,offset);
    rotate_r(p,offset);
end;

procedure zigzag(var p:ptruint;offset:cardinal);inline;

begin
    rotate_l(Ptree_struct(p+offset)^.left,offset);
    rotate_r(p,offset);
end;

procedure zagzig(var p:ptruint;offset:cardinal);inline;

begin
    rotate_r(Ptree_struct(p+offset)^.right,offset);
    rotate_l(p,offset);
end;

procedure zagzag(var p:ptruint;offset:cardinal);inline;

begin
    rotate_l(p,offset);
    rotate_l(p,offset);
end;

procedure delete_from_tree(var p:ptruint;offset:cardinal);

var p1:ptruint;
    pp1:^ptruint;

begin
  if Ptree_struct(p+offset)^.left=0 then
    p:=Ptree_struct(p+offset)^.right
  else
    begin
      if Ptree_struct(p+offset)^.right<>0 then
        begin
          {Both are occupied. Move right to rightmost leaf of left.}
          p1:=Ptree_struct(p+offset)^.left;
          repeat
            pp1:=@Ptree_struct(p1+offset)^.right;
            p1:=pp1^;
          until p1=0;
          pp1^:=Ptree_struct(p+offset)^.right;
        end;
      p:=Ptree_struct(p+offset)^.left;
    end;
end;

function find_sizememloc(size:ptruint;var p:Pfree_block):Tsplay_status;

begin
  find_sizememloc:=ts_found_on_p;
  if p=nil then
    find_sizememloc:=ts_not_found
  else if size<p^.size then {Do nothing if equal...}
    case find_sizememloc(size,Pfree_block(p^.tree_sizememloc.left)) of
      ts_not_found:
        if p^.size<size then
          find_sizememloc:=ts_not_found;
      ts_found_on_left:
        zigzig(ptruint(p),tree_sizememloc_offset);
      ts_found_on_p:
        find_sizememloc:=ts_found_on_left;
      ts_found_on_right:
        zigzag(ptruint(p),tree_sizememloc_offset);
    end
  else if size>p^.size then
    case find_sizememloc(size,Pfree_block(p^.tree_sizememloc.right)) of
      ts_not_found:
        if p^.size<size then
          find_sizememloc:=ts_not_found;
      ts_found_on_left:
        zagzig(ptruint(p),tree_sizememloc_offset);
      ts_found_on_p:
        find_sizememloc:=ts_found_on_right;
      ts_found_on_right:
        zagzag(ptruint(p),tree_sizememloc_offset);
    end;
end;

{$if 0}
function find_sizememloc(size,loc:ptruint;var p:Pfree_block):Tsplay_status;

var on_left:boolean;

begin
  find_sizememloc:=ts_found_on_p;
  if p=nil then
    find_sizememloc:=ts_not_found
  else
    begin
      on_left:=size<p^.size;
      if size=p^.size then
        if loc=ptruint(p) then
          exit
        else
          on_left:=loc<ptruint(p);
      if on_left then
        case find_sizememloc(size,loc,Pfree_block(p^.tree_sizememloc.left)) of
          ts_not_found:
            find_sizememloc:=ts_not_found;
          ts_found_on_left:
            zigzig(ptruint(p),tree_sizememloc_offset);
          ts_found_on_p:
            find_sizememloc:=ts_found_on_left;
          ts_found_on_right:
            zigzag(ptruint(p),tree_sizememloc_offset);
        end
      else
        case find_sizememloc(size,loc,Pfree_block(p^.tree_sizememloc.right)) of
          ts_not_found:
            find_sizememloc:=ts_not_found;
          ts_found_on_left:
            zagzig(ptruint(p),tree_sizememloc_offset);
          ts_found_on_p:
            find_sizememloc:=ts_found_on_right;
          ts_found_on_right:
            zagzag(ptruint(p),tree_sizememloc_offset);
        end;
    end;
end;
{$endif}

function insert_sizememloc(node:Pfree_block;var p:Pfree_block):Tsplay_status;

{Preconditions:

 node^.size is set
 node^.tree_sizememloc.left is set to nil
 node^.tree_sizememloc.right is set to nil}

var on_left:boolean;

begin
  insert_sizememloc:=ts_found_on_p;
  if p=nil then
    p:=node
  else
    begin
      on_left:=node^.size<p^.size;
      if node^.size=p^.size then
        on_left:=ptruint(node)<ptruint(p);
      if on_left then
        case insert_sizememloc(node,Pfree_block(p^.tree_sizememloc.left)) of
          ts_found_on_left:
            zigzig(ptruint(p),tree_sizememloc_offset);
          ts_found_on_p:
            insert_sizememloc:=ts_found_on_left;
          ts_found_on_right:
            zigzag(ptruint(p),tree_sizememloc_offset);
        end
      else
        case insert_sizememloc(node,Pfree_block(p^.tree_sizememloc.right)) of
          ts_found_on_left:
            zagzig(ptruint(p),tree_sizememloc_offset);
          ts_found_on_p:
            insert_sizememloc:=ts_found_on_right;
          ts_found_on_right:
            zagzag(ptruint(p),tree_sizememloc_offset);
        end;
    end;
{$ifdef debug}
  writeln('sizememlocboom na insert');
  write_sizememloc_tree(tree_sizememloc,1);
{$endif}
end;

{$if 0}
function find_memlocation(location:ptruint;var p:Pfree_block;
                          find_smaller:boolean):Tsplay_status;

begin
  find_memlocation:=ts_found_on_p;
  if p=nil then
    find_memlocation:=ts_not_found
  else if location<ptruint(p) then {Do nothing if equal...}
    case find_memlocation(location,Pfree_block(p^.tree_memlocation.left),
                          find_smaller) of
      ts_not_found:
        if (ptruint(p)>location) or not find_smaller then
          find_memlocation:=ts_not_found;
      ts_found_on_left:
        zigzig(ptruint(p),tree_memlocation_offset);
      ts_found_on_p:
        find_memlocation:=ts_found_on_left;
      ts_found_on_right:
        zigzag(ptruint(p),tree_memlocation_offset);
    end
  else if location>ptruint(p) then
    case find_memlocation(location,Pfree_block(p^.tree_memlocation.right),
                          find_smaller) of
      ts_not_found:
        if (ptruint(p)>location) or not find_smaller then
          find_memlocation:=ts_not_found;
      ts_found_on_left:
        zagzig(ptruint(p),tree_memlocation_offset);
      ts_found_on_p:
        find_memlocation:=ts_found_on_right;
      ts_found_on_right:
        zagzag(ptruint(p),tree_memlocation_offset);
    end;
end;
{$endif}

function insert_memlocation(node:Pfree_block;var p:Pfree_block):Tsplay_status;

{Preconditions:

 node^.size is set
 node^.tree_sizememloc.left is set to nil
 node^.tree_sizememloc.right is set to nil}

begin
  insert_memlocation:=ts_found_on_p;
  if p=nil then
    p:=node
  else if ptruint(node)<=ptruint(p) then {Equal? Insert on left.}
    case insert_memlocation(node,Pfree_block(p^.tree_memlocation.left)) of
      ts_found_on_left:
        zigzig(ptruint(p),tree_memlocation_offset);
      ts_found_on_p:
        insert_memlocation:=ts_found_on_left;
      ts_found_on_right:        zigzag(ptruint(p),tree_memlocation_offset);
    end
  else if ptruint(node)>ptruint(p) then
    case insert_memlocation(node,Pfree_block(p^.tree_memlocation.right)) of
      ts_found_on_left:
        zagzig(ptruint(p),tree_memlocation_offset);
      ts_found_on_p:
        insert_memlocation:=ts_found_on_right;
      ts_found_on_right:
        zagzag(ptruint(p),tree_memlocation_offset);
    end;
{$ifdef debug}
    writeln('memlocationboom na insert');
    write_memlocation_tree(tree_memlocation,1);
{$endif}
end;

function get_memlocation(node:Pfree_block):Pfree_block;

{Iteratively delete node from tree without splaying.}

var p:^Pfree_block;

begin
  p:=@tree_memlocation;
  while (p^<>nil) and (p^<>node) do
    if ptruint(node)<ptruint(p^) then
      p:=@p^^.tree_memlocation.left
    else
      p:=@p^^.tree_memlocation.right;
  get_memlocation:=p^;
  if p^<>nil then
    delete_from_tree(ptruint(p^),tree_memlocation_offset);
end;

function get_sizememloc(node:Pfree_block):Pfree_block;

{Iteratively delete node from tree without splaying.}

var p:^Pfree_block;
    on_left:boolean;

begin
  p:=@tree_sizememloc;
  while (p^<>nil) and (p^<>node) do
    begin
      on_left:=node^.size<p^^.size;
      if node^.size=p^^.size then
        on_left:=ptruint(node)<ptruint(p^);
      if on_left then
        p:=@p^^.tree_sizememloc.left
      else
        p:=@p^^.tree_sizememloc.right;
    end;
  get_sizememloc:=p^;
  if p^<>nil then
    delete_from_tree(ptruint(p^),tree_sizememloc_offset);
end;

function get_block_by_size(size:cardinal):Pfree_block;

var what:^ptruint;

begin
  case find_sizememloc(size,tree_sizememloc) of
    ts_not_found:
      begin
        get_block_by_size:=nil;
        exit;
      end;
    ts_found_on_left:
      what:=@tree_sizememloc^.tree_sizememloc.left;
    ts_found_on_p:
      what:=@tree_sizememloc;
    ts_found_on_right:
      what:=@tree_sizememloc^.tree_sizememloc.right;
  end;
  get_block_by_size:=Pfree_block(what^);
  delete_from_tree(what^,tree_sizememloc_offset);
  if get_memlocation(get_block_by_size)=nil then
    runerror(204);
end;

function get_block_by_memlocation(location:ptruint):Pfree_block;

var what:^ptruint;

begin
  get_block_by_memlocation:=get_memlocation(Pfree_block(location));
  if get_block_by_memlocation<>nil then
    begin
      get_sizememloc(get_block_by_memlocation);
{      case find_sizememloc(get_block_by_memlocation^.size,
                           ptruint(get_block_by_memlocation),tree_sizememloc) of
        ts_not_found:
          runerror(204);
        ts_found_on_left:
          what:=@tree_sizememloc^.tree_sizememloc.left;
        ts_found_on_p:
          what:=@tree_sizememloc;
        ts_found_on_right:
          what:=@tree_sizememloc^.tree_sizememloc.right;
      end;
      delete_from_tree(what^,tree_sizememloc_offset);}
    end;
end;

function get_smaller_neighbour(location:ptruint):Pfree_block;

var p,what:^ptruint;

begin
  {Find a smaller block. Don't splay as it will be deleted.}
  p:=@tree_memlocation;
  what:=nil;
  while (p^<>0) do
    if location<=p^ then
      p:=@Pfree_block(p^)^.tree_memlocation.left
    else
      begin
        what:=p;
        p:=@Pfree_block(p^)^.tree_memlocation.right;
      end;
  if (what=nil) or (ptruint(what^)+Pfree_block(what^)^.size<>location) then
    begin
      get_smaller_neighbour:=nil;
      exit;
    end;
  get_smaller_neighbour:=Pfree_block(what^);
  delete_from_tree(ptruint(what^),tree_memlocation_offset);
  get_sizememloc(get_smaller_neighbour);
end;

{function pgetmem_directpage(memsize:ptrint):pointer;

var npages:ptrint;

begin
  npages:=(memsize+sizeof(Tpage_direct)+page_size-1) div page_size;
  pgetmem_directpage:=req_pages(npages);
  with Ppage_direct(pgetmem_directpage)^ do
    begin
      page_type:=pt_direct_page;
      size:=memsize;
    end;
end;
}

function pgetmem_suballocpage(memsize:ptrint):pointer;

var free_block:Pfree_block;
    page:pointer;
    needsize,remaining,block_start:ptruint;

begin
{$ifdef debug}
  writeln('-------Getmem------- ',memsize);
{$endif}
  {Constant parts on left because of constant evaluation.}
  needsize:=(sizeof(Tsuballoc_header)+memblock_alignround+memsize) and not memblock_alignround;
  if needsize<min_suballoc_size then
    needsize:=min_suballoc_size;
{$ifdef debug}
  writeln('sizememlocboom voor get:');
  write_sizememloc_tree(tree_sizememloc,2);
{$endif}
  free_block:=get_block_by_size(needsize);
  if free_block=nil then
    begin
      page:=req_pages(1);
      Ppage_suballocation(page)^.page_type:=pt_suballocation;
      {Allocate at the end of the page, a free block at the start.}
      free_block:=Pfree_block(ptruint(page)+sizeof(Tpage_suballocation));
      remaining:=page_size-needsize-sizeof(Tpage_suballocation);
      block_start:=ptruint(page)+page_size-needsize;
      Psuballoc_header(block_start)^.alloc_size:=needsize;
      pgetmem_suballocpage:=pointer(block_start+sizeof(Tsuballoc_header));
    end
  else
    begin
      block_start:=ptruint(free_block);
      remaining:=free_block^.size-needsize;
      if (remaining<min_suballoc_size) then
        begin
          needsize:=free_block^.size;
          free_block:=nil;
        end
      else
        inc(ptruint(free_block),needsize);
      Psuballoc_header(block_start)^.alloc_size:=needsize;
      pgetmem_suballocpage:=pointer(block_start+sizeof(Tsuballoc_header));
    end;
  if free_block<>nil then
    begin
      with free_block^ do
        begin
          size:=remaining;
          tree_sizememloc.left:=0;
          tree_sizememloc.right:=0;
          tree_memlocation.left:=0;
          tree_memlocation.right:=0;
        end;
      insert_sizememloc(free_block,tree_sizememloc);
      insert_memlocation(free_block,tree_memlocation);
    end;
end;

function pfreemem_suballoc_page(page:Ppage_direct;p:pointer):ptrint;

var free_block,neighbour:Pfree_block;
    headerp:Psuballoc_header;
    asize:ptruint;

begin
{$Ifdef debug}
  write('-------Freemem------- ');
{$endif}
  headerp:=Psuballoc_header(ptrint(p)-sizeof(Tsuballoc_header));
  asize:=headerp^.alloc_size;
{$ifdef debug}
  writeln(hexstr(ptruint(page),8),' ',asize);
{$endif}
  free_block:=Pfree_block(headerp);
  {Search neighbour to coalesce with above block.}
  neighbour:=get_block_by_memlocation(ptruint(free_block)+asize);
  if neighbour<>nil then
    inc(asize,neighbour^.size);
  {Search neighbour to coalesce with below block.}
  neighbour:=get_smaller_neighbour(ptruint(free_block));
  if neighbour<>nil then
    begin
      inc(asize,neighbour^.size);
      free_block:=neighbour;
    end;
  {Page empty??}
  if (ptruint(free_block) and page_mask=sizeof(Tpage_suballocation)) and
     (asize=page_size-sizeof(Tpage_suballocation)) then
    sack_pages(pointer(ptruint(free_block) and not page_mask),1)
  else
    begin
      with free_block^ do
        begin
          size:=asize;
          tree_sizememloc.left:=0;
          tree_sizememloc.right:=0;
          tree_memlocation.left:=0;
          tree_memlocation.right:=0;
        end;
      insert_sizememloc(free_block,tree_sizememloc);
      insert_memlocation(free_block,tree_memlocation);
    end;
end;

function pgetmem(size:ptrint):pointer;

begin
  if size<=8 then
    pgetmem:=pgetmem_8byte_with_bitmap
  else
    pgetmem:=pgetmem_suballocpage(size);
end;

function pallocmem(size:ptrint):pointer;

begin
  if size<=8 then
    begin
      pallocmem:=pgetmem_8byte_with_bitmap;
      fillchar(Pbyte(pallocmem)^,8,0);
    end
  else
    {Freshly allocated pages are allways already cleared.}
{    pgallocmem:=pgallocmem_directpage(size)};
end;


function pfreemem(p:pointer):ptrint;

var page:pointer;

begin
  page:=pointer(ptrint(p) and not page_mask);
  case Ppage_type(page)^ of
    pt_8byte_with_bitmap:
      pfreemem:=pfreemem_8byte_with_bitmap(page,p);
    pt_suballocation:
      pfreemem:=pfreemem_suballoc_page(page,p);
  else
    runerror(204);
  end;
end;

function pfreememsize(p:pointer;size:ptrint):ptrint;

begin
{  runerror(204);}
  pfreemem(p);
end;

function preallocmem(var p:pointer;size:ptrint):pointer;

begin
  runerror(204);
end;

function pmemsize(p:pointer):ptrint;

begin
  runerror(204);
end;

const page_memory_manager:Tmemorymanager=
    (
      needlock:false;
      getmem:@pgetmem;
      freemem:@pfreemem;
      freememsize:@pfreememsize;
      allocmem:@pallocmem;
      reallocmem:@preallocmem;
      memsize:@pmemsize;
{      memavail:@pmemavail;}
{      maxavail:@pmaxavail;}
{      heapsize:@pheapsize;}
    );

var oldmemman:Tmemorymanager;

initialization
  getmemorymanager(oldmemman);
  setmemorymanager(page_memory_manager);
finalization
  setmemorymanager(oldmemman);
end.

