{
  This module collects the basic data types and operations used in the TP
  Yacc program, and other basic stuff that does not belong anywhere else:
  - Yacc input and output files and corresponding bookkeeping information
    used by the parser
  - symbolic character constants
  - dynamically allocated strings
  - integer sets
  - generic quicksort and hash table routines
  - utilities for list-generating
  - other tiny utilities


  Copyright (c) 1990-92  Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>
  Copyright (C) 1996     Berend de Boer <berend@pobox.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


$Revision: 1.2 $
$Modtime: 96-07-31 15:18 $

$History: YACCBASE.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccBase;



interface

const

(* symbolic character constants: *)

bs   = #8;      (* backspace character *)
tab  = #9;      (* tab character *)
nl   = #10;     (* newline character *)
cr   = #13;     (* carriage return *)
ff   = #12;     (* form feed character *)

var

(* Filenames: *)

yfilename     : String;
pasfilename   : String;
lstfilename   : String;
codfilename   : String;
codfilepath1,
codfilepath2  : String; { Under Linux,
                          binary and conf file are never in 1 directory.}

(* Yacc input, output, list and code template file: *)

yyin, yyout, yylst, yycod : Text;

(* the following values are initialized and updated by the parser: *)

line      : String;  (* current input line *)
lno, cno  : Integer; (* current input position (line/column) *)
tokleng   : Integer; (* length of current token *)

const

{$IFDEF MsDos}
max_elems  = 50;  (* maximum size of integer sets *)
{$ELSE}
max_elems  = 75; (* maximum size of integer sets *)
{$ENDIF}

type

(* String pointers: *)

StrPtr    = ^String;

(* Sorted integer sets: *)

IntSet    = array [0..max_elems] of Integer;
              (* word 0 is size *)
IntSetPtr = ^IntSet;

(* Operations: *)

(* Strings pointers: *)

function newStr(str : String) : StrPtr;
  (* creates a string pointer (only the space actually needed for the given
     string is allocated) *)

(* Integer sets (set arguments are passed by reference even if they are not
   modified, for greater efficiency): *)

procedure empty(var M : IntSet);
  (* initializes M as empty *)
procedure singleton(var M : IntSet; i : Integer);
  (* initializes M as a singleton set containing the element i *)
procedure include(var M : IntSet; i : Integer);
  (* include i in M *)
procedure exclude(var M : IntSet; i : Integer);
  (* exclude i from M *)
procedure setunion(var M, N : IntSet);
  (* adds N to M *)
procedure setminus(var M, N : IntSet);
  (* removes N from M *)
procedure intersect(var M, N : IntSet);
  (* removes from M all elements NOT in N *)
function size(var M : IntSet) : Integer;
  (* cardinality of set M *)
function member(i : Integer; var M : IntSet) : Boolean;
  (* tests for membership of i in M *)
function isempty(var M : IntSet) : Boolean;
  (* checks whether M is an empty set *)
function equal(var M, N : IntSet) : Boolean;
  (* checks whether M and N are equal *)
function subseteq(var M, N : IntSet) : Boolean;
  (* checks whether M is a subset of N *)
function newEmptyIntSet : IntSetPtr;
  (* creates a pointer to an empty integer set *)
function newIntSet ( var M : IntSet ) : IntSetPtr;
  (* creates a dynamic copy of M (only the space actually needed
     is allocated) *)

(* Quicksort: *)

type

OrderPredicate = function (i, j : Integer) : Boolean;
SwapProc = procedure (i, j : Integer);

procedure quicksort(lo, hi: Integer;
                    less : OrderPredicate;
                    swap : SwapProc);
  (* General inplace sorting procedure based on the quicksort algorithm.
     This procedure can be applied to any sequential data structure;
     only the corresponding routines less which compares, and swap which
     swaps two elements i,j of the target data structure, must be
     supplied as appropriate for the target data structure.
     - lo, hi: the lower and higher indices, indicating the elements to
       be sorted
     - less(i, j): should return true if element no. i `is less than'
       element no. j, and false otherwise; any total quasi-ordering may
       be supplied here (if neither less(i, j) nor less(j, i) then elements
       i and j are assumed to be `equal').
     - swap(i, j): should swap the elements with index i and j *)

(* Generic hash table routines (based on quadratic rehashing; hence the
   table size must be a prime number): *)

type

TableLookupProc = function(k : Integer) : String;
TableEntryProc  = procedure(k : Integer; symbol : String);

function key(symbol : String;
             table_size : Integer;
             lookup : TableLookupProc;
             entry  : TableEntryProc) : Integer;
  (* returns a hash table key for symbol; inserts the symbol into the
     table if necessary
     - table_size is the symbol table size and must be a fixed prime number
     - lookup is the table lookup procedure which should return the string
       at key k in the table ('' if entry is empty)
     - entry is the table entry procedure which is assumed to store the
       given symbol at the given location *)

function definedKey(symbol : String;
                    table_size : Integer;
                    lookup : TableLookupProc) : Boolean;
  (* checks the table to see if symbol is in the table *)

(* Utility routines: *)

function min(i, j : Integer) : Integer;
function max(i, j : Integer) : Integer;
  (* minimum and maximum of two integers *)
function upper(str : String) : String;
  (* returns str converted to uppercase *)
function strip(str : String) : String;
  (* returns str with leading and trailing blanks stripped off *)
function blankStr(str : String) : String;
  (* returns string of same length as str, with all non-whitespace characters
     replaced by blanks *)
function intStr(i : Integer) : String;
  (* returns the string representation of i *)
function isInt(str : String; var i : Integer) : Boolean;
  (* checks whether str represents an integer; if so, returns the
     value of it in i *)
function path(filename : String) : String;
  (* returns the path in filename *)
function root(filename : String) : String;
  (* returns root (i.e. extension stripped from filename) of
     filename *)
function addExt(filename, ext : String) : String;
  (* if filename has no extension and last filename character is not '.',
     add extension ext to filename *)
function file_size(filename : String) : LongInt;
  (* determines file size in bytes *)

(* Utility functions for list generating routines: *)

type CharSet = set of Char;

function charStr(c : char; reserved : CharSet) : String;
  (* returns a print name for character c, using the standard escape
     conventions; reserved is the class of `reserved' special characters
     which should be quoted with \ (\ itself is always quoted) *)
function singleQuoteStr(str : String) : String;
  (* returns print name of str enclosed in single quotes, using the
     standard escape conventions *)
function doubleQuoteStr(str : String) : String;
  (* returns print name of str enclosed in double quotes, using the
     standard escape conventions *)

implementation

uses YaccMsgs;

(* String pointers: *)

function newStr(str : String) : StrPtr;
  var strp : StrPtr;
  begin
    getmem(strp, succ(length(str)));
    move(str, strp^, succ(length(str)));
    newStr := strp;
  end(*newStr*);

(* Integer sets: *)

procedure empty(var M : IntSet);
  begin
    M[0] := 0;
  end(*empty*);

procedure singleton(var M : IntSet; i : Integer);
  begin
    M[0] := 1; M[1] := i;
  end(*singleton*);

procedure include(var M : IntSet; i : Integer);
  var l, r, k : Integer;
  begin
    (* binary search: *)
    l := 1; r := M[0];
    k := l + (r-l) div 2;
    while (l<r) and (M[k]<>i) do
      begin
        if M[k]<i then
          l := succ(k)
        else
          r := pred(k);
        k := l + (r-l) div 2;
      end;
    if (k>M[0]) or (M[k]<>i) then
      begin
        if M[0]>=max_elems then fatal(intset_overflow);
        if (k<=M[0]) and (M[k]<i) then
          begin
            move(M[k+1], M[k+2], (M[0]-k)*sizeOf(Integer));
            M[k+1] := i;
          end
        else
          begin
            move(M[k], M[k+1], (M[0]-k+1)*sizeOf(Integer));
            M[k] := i;
          end;
        inc(M[0]);
      end;
  end(*include*);

procedure exclude(var M : IntSet; i : Integer);
  var l, r, k : Integer;
  begin
    (* binary search: *)
    l := 1; r := M[0];
    k := l + (r-l) div 2;
    while (l<r) and (M[k]<>i) do
      begin
        if M[k]<i then
          l := succ(k)
        else
          r := pred(k);
        k := l + (r-l) div 2;
      end;
    if (k<=M[0]) and (M[k]=i) then
      begin
        move(M[k+1], M[k], (M[0]-k)*sizeOf(Integer));
        dec(M[0]);
      end;
  end(*exclude*);

procedure setunion(var M, N : IntSet);
  var
    K : IntSet;
    i, j, i_M, i_N : Integer;
  begin
    (* merge sort: *)
    i := 0; i_M := 1; i_N := 1;
    while (i_M<=M[0]) and (i_N<=N[0]) do
      begin
        inc(i);
        if i>max_elems then fatal(intset_overflow);
        if M[i_M]<N[i_N] then
          begin
            K[i] := M[i_M]; inc(i_M);
          end
        else if N[i_N]<M[i_M] then
          begin
            K[i] := N[i_N]; inc(i_N);
          end
        else
          begin
            K[i] := M[i_M]; inc(i_M); inc(i_N);
          end
      end;
    for j := i_M to M[0] do
      begin
        inc(i);
        if i>max_elems then fatal(intset_overflow);
        K[i] := M[j];
      end;
    for j := i_N to N[0] do
      begin
        inc(i);
        if i>max_elems then fatal(intset_overflow);
        K[i] := N[j];
      end;
    K[0] := i;
    move(K, M, succ(i)*sizeOf(Integer));
  end(*setunion*);

procedure setminus(var M, N : IntSet);
  var
    K : IntSet;
    i, i_M, i_N : Integer;
  begin
    i := 0; i_N := 1;
    for i_M := 1 to M[0] do
      begin
        while (i_N<=N[0]) and (N[i_N]<M[i_M]) do inc(i_N);
        if (i_N>N[0]) or (N[i_N]>M[i_M]) then
          begin
            inc(i);
            K[i] := M[i_M];
          end
        else
          inc(i_N);
      end;
    K[0] := i;
    move(K, M, succ(i)*sizeOf(Integer));
  end(*setminus*);

procedure intersect(var M, N : IntSet);
  var
    K : IntSet;
    i, i_M, i_N : Integer;
  begin
    i := 0; i_N := 1;
    for i_M := 1 to M[0] do
      begin
        while (i_N<=N[0]) and (N[i_N]<M[i_M]) do inc(i_N);
        if (i_N<=N[0]) and (N[i_N]=M[i_M]) then
          begin
            inc(i);
            K[i] := M[i_M];
            inc(i_N);
          end
      end;
    K[0] := i;
    move(K, M, succ(i)*sizeOf(Integer));
  end(*intersect*);

function size(var M : IntSet) : Integer;
  begin
    size := M[0]
  end(*size*);

function member(i : Integer; var M : IntSet) : Boolean;
  var l, r, k : Integer;
  begin
    (* binary search: *)
    l := 1; r := M[0];
    k := l + (r-l) div 2;
    while (l<r) and (M[k]<>i) do
      begin
        if M[k]<i then
          l := succ(k)
        else
          r := pred(k);
        k := l + (r-l) div 2;
      end;
    member := (k<=M[0]) and (M[k]=i);
  end(*member*);

function isempty(var M : IntSet) : Boolean;
  begin
    isempty := M[0]=0
  end(*isempty*);

function equal(var M, N : IntSet) : Boolean;
  var i : Integer;
  begin
    if M[0]<>N[0] then
      equal := false
    else
      begin
        for i := 1 to M[0] do
          if M[i]<>N[i] then
            begin
              equal := false;
              exit
            end;
        equal := true
      end
  end(*equal*);

function subseteq(var M, N : IntSet) : Boolean;
  var
    i_M, i_N : Integer;
  begin
    if M[0]>N[0] then
      subseteq := false
    else
      begin
        i_N := 1;
        for i_M := 1 to M[0] do
          begin
            while (i_N<=N[0]) and (N[i_N]<M[i_M]) do inc(i_N);
            if (i_N>N[0]) or (N[i_N]>M[i_M]) then
              begin
                subseteq := false;
                exit
              end
            else
              inc(i_N);
          end;
        subseteq := true
      end;
  end(*subseteq*);

function newIntSet ( var M : IntSet ) : IntSetPtr;
  var
    MP : IntSetPtr;
  begin
    getmem(MP, (size(M)+1)*sizeOf(Integer));
    move(M, MP^, (size(M)+1)*sizeOf(Integer));
    newIntSet := MP;
  end(*newIntSet*);

function newEmptyIntSet : IntSetPtr;
  var
    MP : IntSetPtr;
  begin
    getmem(MP, (max_elems+1)*sizeOf(Integer));
    MP^[0] := 0;
    newEmptyIntSet := MP
  end(*newEmptyIntSet*);

(* Quicksort: *)

procedure quicksort(lo, hi: Integer;
                    less : OrderPredicate;
                    swap : SwapProc);
  (* derived from the quicksort routine in QSORT.PAS in the Turbo Pascal
     distribution *)
  procedure sort(l, r: Integer);
    var i, j, k : Integer;
    begin
      i := l; j := r; k := (l+r) DIV 2;
      repeat
        while less(i, k) do inc(i);
        while less(k, j) do dec(j);
        if i<=j then
          begin
            swap(i, j);
            if k=i then k := j (* pivot element swapped! *)
            else if k=j then k := i;
            inc(i); dec(j);
          end;
      until i>j;
      if l<j then sort(l,j);
      if i<r then sort(i,r);
    end(*sort*);
  begin
    if lo<hi then sort(lo,hi);
  end(*quicksort*);

(* Generic hash table routines: *)

function hash(str : String; table_size : Integer) : Integer;
  (* computes a hash key for str *)
  var i, key : Integer;
  begin
    key := 0;
    for i := 1 to length(str) do
      inc(key, ord(str[i]));
    hash := key mod table_size + 1;
  end(*hash*);

procedure newPos(var pos, incr, count : Integer; table_size : Integer);
  (* computes a new position in the table (quadratic collision strategy)
     - pos: current position (+inc)
     - incr: current increment (+2)
     - count: current number of collisions (+1)
     quadratic collision formula for position of str after n collisions:
       pos(str, n) = (hash(str)+n^2) mod table_size +1
     note that n^2-(n-1)^2 = 2n-1 <=> n^2 = (n-1)^2 + (2n-1) for n>0,
     i.e. the increment inc=2n-1 increments by two in each collision *)
  begin
    inc(count);
    inc(pos, incr);
    if pos>table_size then pos := pos mod table_size + 1;
    inc(incr, 2)
  end(*newPos*);

function key(symbol : String;
             table_size : Integer;
             lookup : TableLookupProc;
             entry  : TableEntryProc) : Integer;
  var pos, incr, count : Integer;
  begin
    pos := hash(symbol, table_size);
    incr := 1;
    count := 0;
    while count<=table_size do
      if lookup(pos)='' then
        begin
          entry(pos, symbol);
          key := pos;
          exit
        end
      else if lookup(pos)=symbol then
        begin
          key := pos;
          exit
        end
      else
        newPos(pos, incr, count, table_size);
    fatal(sym_table_overflow)
  end(*key*);

function definedKey(symbol : String;
                    table_size : Integer;
                    lookup : TableLookupProc) : Boolean;
  var pos, incr, count : Integer;
  begin
    pos := hash(symbol, table_size);
    incr := 1;
    count := 0;
    while count<=table_size do
      if lookup(pos)='' then
        begin
          definedKey := false;
          exit
        end
      else if lookup(pos)=symbol then
        begin
          definedKey := true;
          exit
        end
      else
        newPos(pos, incr, count, table_size);
    definedKey := false
  end(*definedKey*);

(* Utility routines: *)

function min(i, j : Integer) : Integer;
  begin
    if i<j then
      min := i
    else
      min := j
  end(*min*);
function max(i, j : Integer) : Integer;
  begin
    if i>j then
      max := i
    else
      max := j
  end(*max*);
function upper(str : String) : String;
  var i : Integer;
  begin
    for i := 1 to length(str) do
      str[i] := upCase(str[i]);
    upper := str
  end(*upper*);
function strip(str : String) : String;
  begin
    while (length(str)>0) and ((str[1]=' ') or (str[1]=tab)) do
      delete(str, 1, 1);
    while (length(str)>0) and
          ((str[length(str)]= ' ') or
           (str[length(str)]=tab)) do
      delete(str, length(str), 1);
    strip := str;
  end(*strip*);
function blankStr(str : String) : String;
  var i : Integer;
  begin
    for i := 1 to length(str) do
      if str[i]<>tab then str[i] := ' ';
    blankStr := str;
  end(*blankStr*);
function intStr(i : Integer) : String;
  var s : String;
  begin
    str(i, s);
    intStr := s
  end(*intStr*);
function isInt(str : String; var i : Integer) : Boolean;
  var res : Integer;
  begin
    val(str, i, res);
    isInt := res = 0;
  end(*isInt*);
function path(filename : String) : String;
  var i : Integer;
  begin
    i := length(filename);
    while (i>0) and (filename[i]<>'\') and (filename[i]<>':') do
      dec(i);
    path := copy(filename, 1, i);
  end(*path*);
function root(filename : String) : String;
  var
    i : Integer;
  begin
    root := filename;
    for i := length(filename) downto 1 do
      case filename[i] of
        '.' :
          begin
            root := copy(filename, 1, i-1);
            exit
          end;
        '\': exit;
        else
      end;
  end(*addExt*);
function addExt(filename, ext : String) : String;
  (* implemented with goto for maximum efficiency *)
  label x;
  var
    i : Integer;
  begin
    addExt := filename;
    for i := length(filename) downto 1 do
      case filename[i] of
        '.' : exit;
        '\': goto x;
        else
      end;
    x : addExt := filename+'.'+ext
  end(*addExt*);
function file_size(filename : String) : LongInt;
  var f : File;
  begin
    assign(f, filename);
    reset(f, 1);
    if ioresult=0 then
      file_size := fileSize(f)
    else
      file_size := 0;
    close(f);
  end(*file_size*);

(* Utility functions for list generating routines: *)

function charStr(c : char; reserved : CharSet) : String;
  function octStr(c : char) : String;
    (* return octal string representation of character c *)
    begin
      octStr := intStr(ord(c) div 64)+intStr((ord(c) mod 64) div 8)+
                intStr(ord(c) mod 8);
    end(*octStr*);
  begin
    case c of
      bs         : charStr := '\b';
      tab        : charStr := '\t';
      nl         : charStr := '\n';
      cr         : charStr := '\c';
      ff         : charStr := '\f';
      '\'        : charStr := '\\';
      #0..#7,      (* nonprintable characters *)
      #11,#14..#31,
      #127..#255 : charStr := '\'+octStr(c);
      else if c in reserved then
        charStr := '\'+c
      else
        charStr := c
    end
  end(*charStr*);

function singleQuoteStr(str : String) : String;
  var
    i : Integer;
    str1 : String;
  begin
    str1 := '';
    for i := 1 to length(str) do
      str1 := str1+charStr(str[i], ['''']);
    singleQuoteStr := ''''+str1+''''
  end(*singleQuoteStr*);

function doubleQuoteStr(str : String) : String;
  var
    i : Integer;
    str1 : String;
  begin
    str1 := '';
    for i := 1 to length(str) do
      str1 := str1+charStr(str[i], ['"']);
    doubleQuoteStr := '"'+str1+'"'
  end(*doubleQuoteStr*);

end(*YaccBase*).
