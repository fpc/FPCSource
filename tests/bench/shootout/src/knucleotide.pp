(* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Josh Goldfoot
   modified by Vincent Snijders
*)

program knucleotide;

{$mode objfpc}{$I-}

(* simple_hash available from CVS *)
uses simple_hash, SysUtils, Strings, Math;

type
   sorter      = record
		    sequence : PChar;
		    num	     : longint;
		 end;
   sorterArray = array of sorter;

function hash_table_size (fl : dword; buflen : dword): dword;
var
   maxsize1, maxsize2, r : dword;
begin
   maxsize1 := buflen - fl;
   maxsize2 := 4;
   while (fl > 1) and (maxsize2 < maxsize1) do
   begin
      fl := fl - 1;
      maxsize2 := maxsize2 * 4;
   end;
   if maxsize1 < maxsize2 then
      r := maxsize1
   else
      r := maxsize2;
   hash_table_size := r;
end; { hash_table_size }

function generate_frequencies(fl: integer; buffer: PChar; buflen : longint): ht_pht;
var
   reader : PChar;
   i, bufend : longint;
   nulled : char;
begin
   if fl <= buflen then
   begin
      result := ht_create (hash_table_size (fl, buflen));
      reader := buffer;
      for i := 0 to buflen-fl do
      begin
         nulled := reader[fl];
         reader[fl] := #0;
         inc(ht_find_new (result, reader)^.val);
         reader[fl] := nulled;
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
  ht	   : ht_pht;
  i, size : longint;
  total   : real;
  nd	   : ht_pnode;
  s	   : sorterArray;
begin
  ht := generate_frequencies(fl, buffer, buflen);
  total := 0;
  size := 0;
  nd := ht_first(ht);
  while (nd <> nil) do
  begin
    total := total + nd^.val;
    size := size + 1;
    nd := ht_next(ht);
  end;
  SetLength(s, size);

  nd := ht_first(ht);
  size := 0;
  while (nd <> nil) do
  begin
    s[size].sequence := nd^.key;
    strupper(s[size].sequence);
    s[size].num := nd^.val;
    size := size + 1;
    nd := ht_next(ht);
  end;

  sortArray(s, size);
  for i := 0 to size - 1 do
    writeln(s[i].sequence,' ', (100 * s[i].num / total):3:3);
  writeln;

  ht_destroy(ht);
end; { write_frequencies }

procedure write_count(searchFor : PChar; buffer : PChar; buflen : longint);
var
   ht : ht_pht;
   nd : ht_pnode;
begin
   ht := generate_frequencies (strlen (searchFor), buffer, buflen);
   nd := ht_find(ht, searchFor);
   if (nd <> nil) then
      write(nd^.val)
   else
      write(0);
   strupper(searchFor);
   writeln(#9, searchFor);

   ht_destroy(ht);
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
   SetPrecisionMode(pmDouble);
   main;
end.