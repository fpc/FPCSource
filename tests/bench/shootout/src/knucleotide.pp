(* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Josh Goldfoot
   modified by Vincent Snijders
*)

{$mode objfpc}

program knucleotide;

(* simple_hash available from CVS *)

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
  { TNonFreePooledMemManager - a memory manager for records without freeing }

  PMemChunk = ^TMemChunk;
  TMemChunk = record
    data: pointer;
    next: PMemChunk;
  end;

  TNonFreePooledMemManager = class
  private
    FItemSize: integer;
    FItems: PMemChunk;
    FCurItem: Pointer;
    FEndItem: Pointer;
    FCurSize: integer;
    procedure Grow;
  public
    property ItemSize: integer read FItemSize;
    constructor Create(TheItemSize: integer);
    destructor Destroy; override;
    function NewItem: Pointer; inline;
  end;

  { THashTable }

  ht_ppnode = ^ht_pnode;
  ht_pnode = ^ht_node;
  ht_node = record
    val: integer;
    next: ht_pnode;
    keydata: array[0..0] of char;
  end;

  THashTable=class
  private
    FSize: dword;
    FKeysize: dword;
    FTbl: ht_ppnode;
    FIter_index: dword;
    FIter_next: ht_pnode;
    FNodeMemManager: TNonFreePooledMemManager;
  public
    constructor Create(size: dword; keysize: dword);
    destructor Destroy; override;
    function Find(key: pchar): ht_pnode;
    function FindNew(key: pchar): ht_pnode;
    function First: ht_pnode;
    function Next: ht_pnode;
  end;

{ TNonFreePooledMemManager }

procedure TNonFreePooledMemManager.Grow;
var
  memchunk: PMemChunk;
begin
  if FCurSize<256*1024 then
  // each item has double the size of its predecessor
    inc(FCurSize, FCurSize);
  GetMem(FCurItem,FCurSize);
  FillChar(FCurItem^, FCurSize, 0);
  new(MemChunk);
  MemChunk^.next := FItems;
  MemChunk^.Data := FCurItem;
  FItems := MemChunk;
  FEndItem := FCurItem;
  Inc(FEndItem, FCurSize);
end;

constructor TNonFreePooledMemManager.Create(TheItemSize: integer);
begin
  FItemSize:=TheItemSize;
  FCurSize:=FItemSize*4; // 4 items => the first item has 8 entries
end;

destructor TNonFreePooledMemManager.Destroy;
var
  p: PMemChunk;
begin
  while FItems<>nil do begin
    p := FItems;
    FItems := Fitems^.next;
    FreeMem(p^.Data);
    Dispose(p);
  end;
  inherited Destroy;
end;

function TNonFreePooledMemManager.NewItem: Pointer; inline;
begin
  if (FCurItem=FEndItem) then
   Grow;
  Result:=FCurItem;
  Inc(FCurItem, FItemSize);
end;

{ THashTable }

constructor THashTable.Create(size: dword; keysize: dword);
var
  i: integer;
begin
  i := 0;
  while (i<high(ht_prime_list)) and (size>ht_prime_list[i]) do
    inc(i);
  FSize := ht_prime_list[i];
  fkeysize := keysize;
  ftbl := allocmem(sizeof(ht_pnode) * FSize);
  fiter_index := 0;
  fiter_next := nil;
  FNodeMemManager := TNonFreePooledMemManager.Create(SizeOf(ht_node)+FKeySize);
end;

destructor THashTable.Destroy;
begin
  FNodeMemManager.Free;
  freemem(Ftbl);
  inherited;
end;

function ht_hashcode(key: pchar; keysize: dword): dword; //inline;
var
  val: dword;
  i: integer;
begin
  val := 0;

  for i := 0 to Keysize -1 do
  begin
    val := val * 4;
    inc(val, dword(byte(key^) and 6) shr 1);
    inc(key);
  end;
  result := val;
end;

function THashTable.Find(key: pchar): ht_pnode;
var
  hash_code: dword;
  node: ht_pnode;
begin
  hash_code := ht_hashcode(key, FKeySize) mod FSize;
  node := FTbl[hash_code];
  while node <> nil do
  begin
    if comparebyte(key^, node^.keydata, FKeysize) = 0 then
    begin
      result := node;
      exit;
    end;
    node := node^.next;
  end;
  result := nil;
end;

function THashTable.FindNew(key: pchar): ht_pnode;
var
  hash_code: integer;
  prev, node: ht_pnode;
begin
  prev := nil;
  hash_code := ht_hashcode(key, FKeysize) mod FSize;
  node := FTbl[hash_code];
  while node <> nil do
  begin
    if CompareByte(key^, node^.keydata, FKeysize) = 0 then
    begin
      result := node;
      exit;
    end;
    prev := node;
    node := node^.next;
  end;
  result := FNodeMemManager.NewItem;
  move(key^,Result^.keydata,FKeysize);
  if prev <> nil then
  begin
    prev^.next := result;
  end else begin
    FTbl[hash_code] := result;
  end;
end;

{
  Hash Table iterator data / functions
}

function THashTable.First: ht_pnode;
begin
  FIter_index := 0;
  FIter_next := nil;
  result := next;
end;

function THashTable.Next: ht_pnode;
var
  index: dword;
  node: ht_pnode;
begin
  node := FIter_next;
  if node <> nil then
  begin
    FIter_next := node^.next;
    result := node;
    exit;
  end else begin
    while FIter_index < FSize do
    begin
      index := FIter_index;
      inc(FIter_index);
      if FTbl[index] <> nil then
      begin
        FIter_next := FTbl[index]^.next;
        result := FTbl[index];
        exit;
      end;
    end;
  end;
  result := nil;
end;

{==============================================================================}

type
   sorter      = record
		   sequence : ansistring;
		   num	     : longint;
		 end;
   sorterArray = array of sorter;

function hash_table_size (fl : dword): dword;
begin
  if fl<8 then
   hash_table_size := 1 shl (2 * fl)
  else
   hash_table_size := $10000;
end; { hash_table_size }

function generate_frequencies(fl: integer; buffer: PChar; buflen : longint): THashTable;
var
   reader : PChar;
   i : longint;
begin
   if fl <= buflen then
   begin
      result := THashTable.Create(hash_table_size (fl), fl);
      reader := buffer;
      for i := 0 to buflen-fl do
      begin
         inc(Result.FindNew(reader)^.val);
         inc(reader);
      end;
   end else
      result := nil;
end; { generate_frequencies }

procedure sortArray(var s : sorterArray; size:longint);
var
   i,j : longint;
   tmp : sorter;
begin
   for i := 0 to size-2 do
      for j := i+1 to size-1 do
         if s[i].num < s[j].num then
	 begin
	    tmp := s[i];
	    s[i] := s[j];
	    s[j] := tmp;
	 end;
end; { sortArray }

procedure write_frequencies(fl : integer; buffer : PChar; buflen : longint);
var
  ht	   : THashTable;
  i, size : longint;
  total   : real;
  nd	   : ht_pnode;
  s	   : sorterArray;
begin
  ht := generate_frequencies(fl, buffer, buflen);
  total := 0;
  size := 0;
  nd := ht.First;
  while (nd <> nil) do
  begin
    total := total + nd^.val;
    size := size + 1;
    nd := ht.Next;
  end;
  SetLength(s, size);

  nd := ht.First;
  size := 0;
  while (nd <> nil) do
  begin
    s[size].sequence := upcase(pchar(@nd^.keydata));
    s[size].num := nd^.val;
    size := size + 1;
    nd := ht.Next;
  end;

  sortArray(s, size);
  for i := 0 to size - 1 do
    writeln(s[i].sequence,' ', (100 * s[i].num / total):3:3);
  writeln;

  ht.Free;
end; { write_frequencies }

procedure write_count(searchFor : ansistring; buffer : PChar; buflen : longint);
var
   ht : THashTable;
   nd : ht_pnode;
begin
   ht := generate_frequencies (length(searchFor), buffer, buflen);
   nd := ht.Find(pchar(searchFor));
   if (nd <> nil) then
      write(nd^.val)
   else
      write(0);
   searchfor := UpCase(searchFor);
   writeln(#9, searchFor);

   ht.Free;
end; { write_count }

procedure main;
var
   buffer : PChar;
   len, seqlen : longint;
   buffersize, bufferptr: longint;
   s : String;
begin
   seqlen := 0;
   repeat
      readln(s)
   until (s[1] = '>') and (s[2] = 'T') and (s[3] = 'H');
   buffersize:=1024;
   buffer:=getmem(buffersize);
   bufferptr :=0;
   while not eof do begin
     readln(s);
     if (s[1] <> '>') and (s[1] <> ';') then begin
       len:=length(s);
       if (bufferptr+len+1)>buffersize then  begin
         inc(buffersize,buffersize);
         reallocmem(buffer,buffersize);
       end;
       move (s[1],buffer[bufferptr],len);
       inc(bufferptr,len);
     end;
   end;
   buffer[bufferptr] := #0;
   seqlen := strlen(buffer);

   write_frequencies(1, buffer, seqlen);
   write_frequencies(2, buffer, seqlen);
   write_count('ggt', buffer, seqlen);
   write_count('ggta', buffer, seqlen);
   write_count('ggtatt', buffer, seqlen);
   write_count('ggtattttaatt', buffer, seqlen);
   write_count('ggtattttaatttatagt', buffer, seqlen);
   freemem(buffer);
end; { main }


begin
   //SetPrecisionMode(pmDouble);
   main;
end.

