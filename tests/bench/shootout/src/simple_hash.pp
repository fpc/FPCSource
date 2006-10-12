{
  Copyright 2005, Micha Nelissen, converted from C, originally from
  "simple_hash.h":
}

unit simple_hash;

{$mode objfpc}
{$inline on}

interface

const
  ht_num_primes = 28;

  ht_prime_list: array[0 .. ht_num_primes-1] of dword =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

type
  ht_ppnode = ^ht_pnode;
  ht_pnode = ^ht_node;
  ht_node = record
    key: pchar;
    val: integer;
    next: ht_pnode;
  end;

  ht_pht = ^ht_ht;
  ht_ht = record
    size: dword;
    tbl: ht_ppnode;
    iter_index: dword;
    iter_next: ht_pnode;
    items: integer;
{$ifdef HT_DEBUG}
    collisions: integer;
{$endif}
  end;


function  ht_val(node: ht_pnode): integer; {inline;}
function  ht_key(node: ht_pnode): pchar; {inline;}
function  ht_hashcode(ht: ht_pht; key: pchar): integer; {inline;}
function  ht_node_create(key: pchar): ht_pnode;
function  ht_create(size: dword): ht_pht;
procedure ht_destroy(ht: ht_pht);
function  ht_find(ht: ht_pht; key: pchar): ht_pnode; {inline;}
function  ht_find_new(ht: ht_pht; key: pchar): ht_pnode; {inline;}
function  ht_next(ht: ht_pht): ht_pnode; {inline;}
function  ht_first(ht: ht_pht): ht_pnode; {inline;}
function  ht_count(ht: ht_pht): integer; {inline;}

implementation

uses
  strings;

function  ht_val(node: ht_pnode): integer; {inline;}
begin
  result := node^.val;
end;

function  ht_key(node: ht_pnode): pchar; {inline;}
begin
  result := node^.key;
end;

function  ht_hashcode(ht: ht_pht; key: pchar): integer; {inline;}
var
  val: dword;
begin
  val := 0;
  while key^ <> #0 do
  begin
    val := 5 * val + byte(key^);
    inc(key);
  end;
  result := val mod ht^.size;
end;

function  ht_node_create(key: pchar): ht_pnode;
var
  newkey: pchar;
  node: ht_pnode;
begin
  new(node);
  newkey := strnew(key);
  with node^ do
  begin
    key := newkey;
    val := 0;
    next := nil;
  end;
  result := node;
end;

function  ht_create(size: dword): ht_pht;
var
  i: integer;
  ht: ht_pht;
begin
  i := 0;
  new(ht);
  while ht_prime_list[i] < size do inc(i);
  ht^.size := ht_prime_list[i];
  ht^.tbl := allocmem(sizeof(ht_pnode) * ht^.size);
  ht^.iter_index := 0;
  ht^.iter_next := nil;
  ht^.items := 0;
{$ifdef HT_DEBUG}
  ht^.collisions := 0;
{$endif}
  result := ht;
end;

procedure ht_destroy(ht: ht_pht);
var
  cur, next: ht_pnode;
  i: integer;
{$ifdef HT_DEBUG}
  chain_len, max_chain_len, density: integer;
{$endif}
begin
{$ifdef HT_DEBUG}
  max_chain_len := 0;
  density := 0;
  writeln(' HT: size          ', ht^.size);
  writeln(' HT: items         ', ht^.items);
  writeln(' HT: collisions    ', ht^.collisions);
{$endif}
  for i := 0 to ht^.size-1 do
  begin
    next := ht^.tbl[i];
{$ifdef HT_DEBUG}
    if next <> nil then
      inc(density);
    chain_len := 0;
{$endif}
    while next <> nil do
    begin
      cur := next;
      next := next^.next;
      strdispose(cur^.key);
      dispose(cur);
{$ifdef HT_DEBUG}
      inc(chain_len);
{$endif}
    end;
{$ifdef HT_DEBUG}
    if chain_len > max_chain_len then
      max_chain_len := chain_len;
{$endif}
  end;
  freemem(ht^.tbl);
  dispose(ht);
{$ifdef HT_DEBUG}
  writeln(' HT: density       ', density);
  writeln(' HT: max chain len ', max_chain_len);
{$endif}
end;

function  ht_find(ht: ht_pht; key: pchar): ht_pnode; {inline;}
var
  hash_code: integer;
  node: ht_pnode;
begin
  hash_code := ht_hashcode(ht, key);
  node := ht^.tbl[hash_code];
  while node <> nil do
  begin
    if strcomp(key, node^.key) = 0 then
    begin
      result := node;
      exit;
    end;
    node := node^.next;
  end;
  result := nil;
end;

function  ht_find_new(ht: ht_pht; key: pchar): ht_pnode; {inline;}
var
  hash_code: integer;
  prev, node: ht_pnode;
begin
  hash_code := ht_hashcode(ht, key);
  prev := nil;
  node := ht^.tbl[hash_code];
  while node <> nil do
  begin
    if strcomp(key, node^.key) = 0 then
    begin
      result := node;
      exit;
    end;
    prev := node;
    node := node^.next;
{$ifdef HT_DEBUG}
    inc(ht^.collisions);
{$endif}
  end;
  inc(ht^.items);
  result := ht_node_create(key);
  if prev <> nil then
  begin
    prev^.next := result;
  end else begin
    ht^.tbl[hash_code] := result;
  end;
end;

{
  Hash Table iterator data / functions
}

function  ht_next(ht: ht_pht): ht_pnode; {inline;}
var
  index: dword;
  node: ht_pnode;
begin
  node := ht^.iter_next;
  if node <> nil then
  begin
    ht^.iter_next := node^.next;
    result := node;
    exit;
  end else begin
    while ht^.iter_index < ht^.size do
    begin
      index := ht^.iter_index;
      inc(ht^.iter_index);
      if ht^.tbl[index] <> nil then
      begin
        ht^.iter_next := ht^.tbl[index]^.next;
        result := ht^.tbl[index];
        exit;
      end;
    end;
  end;
  result := nil;
end;

function  ht_first(ht: ht_pht): ht_pnode; {inline;}
begin
  ht^.iter_index := 0;
  ht^.iter_next := nil;
  result := ht_next(ht);
end;

function  ht_count(ht: ht_pht): integer; {inline;}
begin
  result := ht^.items;
end;

end.