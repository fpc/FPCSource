{
  This module collects the basic data types and operations used in the TP
  Lex program, and other basic stuff that does not belong anywhere else:
  - Lex input and output files and corresponding bookkeeping information
    used by the parser
  - symbolic character constants
  - dynamically allocated strings and character classes
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
$Modtime: 96-08-01 10:21 $

$History: LEXBASE.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit LexBase;

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

lfilename     : String;
pasfilename   : String;
lstfilename   : String;
codfilename   : String;
codfilepath   : String; { Under linux, binary and conf file
                          are not in the same path}

(* Lex input, output, list and code template file: *)

yyin, yylst, yyout, yycod : Text;

(* the following values are initialized and updated by the parser: *)

line : String;  (* current input line *)
lno  : Integer; (* current line number *)

const

max_elems  = 1000;  (* maximum size of integer sets *)

type

(* String and character class pointers: *)

StrPtr    = ^String;
CClass    = set of Char;
CClassPtr = ^CClass;

(* Sorted integer sets: *)

IntSet    = array [0..max_elems] of Integer;
              (* word 0 is size *)
IntSetPtr = ^IntSet;

(* Regular expressions: *)

RegExpr = ^Node;

NodeType = (mark_node,    (* marker node *)
            char_node,    (* character node *)
            str_node,     (* string node *)
            cclass_node,  (* character class node *)
            star_node,    (* star node *)
            plus_node,    (* plus node *)
            opt_node,     (* option node *)
            cat_node,     (* concatenation node *)
            alt_node);    (* alternatives node (|) *)

Node = record case node_type : NodeType of
         mark_node : (rule, pos : Integer);
         char_node : (c : Char);
         str_node : (str : StrPtr);
         cclass_node : (cc : CClassPtr);
         star_node, plus_node, opt_node : (r : RegExpr);
         cat_node, alt_node : (r1, r2 : RegExpr);
       end;

(* Some standard character classes: *)

const

letters   : CClass = ['A'..'Z','a'..'z','_'];
digits    : CClass = ['0'..'9'];
alphanums : CClass = ['A'..'Z','a'..'z','_','0'..'9'];

(* Operations: *)

(* Strings and character classes: *)

function newStr(str : String) : StrPtr;
  (* creates a string pointer (only the space actually needed for the given
     string is allocated) *)
function newCClass(cc : CClass) : CClassPtr;
  (* creates a CClass pointer *)

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
function newIntSet : IntSetPtr;
  (* creates a pointer to an empty integer set *)

(* Constructors for regular expressions: *)

const epsExpr : RegExpr = nil;
  (* empty regular expression *)
function markExpr(rule, pos : Integer) : RegExpr;
  (* markers are used to denote endmarkers of rules, as well as other
     special positions in rules, e.g. the position of the lookahead
     operator; they are considered nullable; by convention, we use
     the following pos numbers:
     - 0: endmarker position
     - 1: lookahead operator position *)
function charExpr(c : Char) : RegExpr;
  (* character c *)
function strExpr(str : StrPtr) : RegExpr;
  (* "str" *)
function cclassExpr(cc : CClassPtr) : RegExpr;
  (* [str] where str are the literals in cc *)
function starExpr(r : RegExpr) : RegExpr;
  (* r* *)
function plusExpr(r : RegExpr) : RegExpr;
  (* r+ *)
function optExpr(r : RegExpr) : RegExpr;
  (* r? *)
function mnExpr(r : RegExpr; m, n : Integer) : RegExpr;
  (* constructor expanding expression r{m,n} to the corresponding
     alt expression r^m|...|r^n *)
function catExpr(r1, r2 : RegExpr) : RegExpr;
  (* r1r2 *)
function altExpr(r1, r2 : RegExpr) : RegExpr;
  (* r1|r2 *)

(* Unifiers for regular expressions:
   The following predicates check whether the specified regular
   expression r is of the denoted type; if the predicate succeeds,
   the other arguments of the predicate are instantiated to the
   corresponding values. *)

function is_epsExpr(r : RegExpr) : Boolean;
  (* empty regular expression *)
function is_markExpr(r : RegExpr; var rule, pos : Integer) : Boolean;
  (* marker expression *)
function is_charExpr(r : RegExpr; var c : Char) : Boolean;
  (* character c *)
function is_strExpr(r : RegExpr; var str : StrPtr) : Boolean;
  (* "str" *)
function is_cclassExpr(r : RegExpr; var cc : CClassPtr) : Boolean;
  (* [str] where str are the literals in cc *)
function is_starExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  (* r1* *)
function is_plusExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  (* r1+ *)
function is_optExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  (* r1? *)
function is_catExpr(r : RegExpr; var r1, r2 : RegExpr) : Boolean;
  (* r1r2 *)
function is_altExpr(r : RegExpr; var r1, r2 : RegExpr) : Boolean;
  (* r1|r2 *)

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
function nchars(cc : CClass) : Integer;
  (* returns the cardinality (number of characters) of a character class *)
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

function charStr(c : char; reserved : CClass) : String;
  (* returns a print name for character c, using the standard escape
     conventions; reserved is the class of `reserved' special characters
     which should be quoted with \ (\ itself is always quoted) *)
function singleQuoteStr(str : String) : String;
  (* returns print name of str enclosed in single quotes, using the
     standard escape conventions *)
function doubleQuoteStr(str : String) : String;
  (* returns print name of str enclosed in double quotes, using the
     standard escape conventions *)
function cclassStr(cc : CClass) : String;
  (* returns print name of character class cc, using the standard escape
     conventions; if cc contains more than 128 elements, the complement
     notation (^) is used; if cc is the class of all (non-null) characters
     except newline, the period notation is used *)
function cclassOrCharStr(cc : CClass) : String;
  (* returns a print name for character class cc (either cclassStr, or,
     if cc contains only one element, character in single quotes) *)
function regExprStr(r : RegExpr) : String;
  (* unparses a regular expression *)

implementation

uses LexMsgs;

(* String and character class pointers: *)

function newStr(str : String) : StrPtr;
  var strp : StrPtr;
  begin
    getmem(strp, succ(length(str)));
    move(str, strp^, succ(length(str)));
    newStr := strp;
  end(*newStr*);

function newCClass(cc : CClass) : CClassPtr;
  var ccp : CClassPtr;
  begin
    new(ccp);
    ccp^ := cc;
    newCClass := ccp;
  end(*newCClass*);

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

function newIntSet : IntSetPtr;
  var
    MP : IntSetPtr;
  begin
    getmem(MP, (max_elems+1)*sizeOf(Integer));
    MP^[0] := 0;
    newIntSet := MP
  end(*newIntSet*);

(* Constructors for regular expressions: *)

function newExpr(node_type : NodeType; n : Integer) : RegExpr;
  (* returns new RegExpr node (n: number of bytes to allocate) *)
  var x : RegExpr;
  begin
    getmem(x, sizeOf(NodeType)+n);
    x^.node_type := node_type;
    newExpr := x
  end(*newExpr*);
function markExpr(rule, pos : Integer) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(mark_node, 2*sizeOf(Integer));
    x^.rule := rule;
    x^.pos  := pos;
    markExpr := x
  end(*markExpr*);
function charExpr(c : Char) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(char_node, sizeOf(Char));
    x^.c := c;
    charExpr := x
  end(*charExpr*);
function strExpr(str : StrPtr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(str_node, sizeOf(StrPtr));
    x^.str := str;
    strExpr := x
  end(*strExpr*);
function cclassExpr(cc : CClassPtr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(cclass_node, sizeOf(CClassPtr));
    x^.cc := cc;
    cclassExpr := x
  end(*cclassExpr*);
function starExpr(r : RegExpr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(star_node, sizeOf(RegExpr));
    x^.r := r;
    starExpr := x
  end(*starExpr*);
function plusExpr(r : RegExpr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(plus_node, sizeOf(RegExpr));
    x^.r := r;
    plusExpr := x
  end(*plusExpr*);
function optExpr(r : RegExpr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(opt_node, sizeOf(RegExpr));
    x^.r := r;
    optExpr := x
  end(*optExpr*);
function mnExpr(r : RegExpr; m, n : Integer) : RegExpr;
  var
    ri, rmn : RegExpr;
    i : Integer;
  begin
    if (m>n) or (n=0) then
      mnExpr := epsExpr
    else
      begin
        (* construct r^m: *)
        if m=0 then
          ri := epsExpr
        else
          begin
            ri := r;
            for i := 2 to m do
              ri := catExpr(ri, r);
          end;
        (* construct r{m,n}: *)
        rmn := ri;                  (* r{m,n} := r^m *)
        for i := m+1 to n do
          begin
            if is_epsExpr(ri) then
              ri := r
            else
              ri := catExpr(ri, r);
            rmn := altExpr(rmn, ri)  (* r{m,n} := r{m,n} | r^i,
                                        i=m+1,...,n *)
          end;
        mnExpr := rmn
      end
  end(*mnExpr*);
function catExpr(r1, r2 : RegExpr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(cat_node, 2*sizeOf(RegExpr));
    x^.r1 := r1;
    x^.r2 := r2;
    catExpr := x
  end(*catExpr*);
function altExpr(r1, r2 : RegExpr) : RegExpr;
  var x : RegExpr;
  begin
    x := newExpr(alt_node, 2*sizeOf(RegExpr));
    x^.r1 := r1;
    x^.r2 := r2;
    altExpr := x
  end(*altExpr*);

(* Unifiers for regular expressions: *)

function is_epsExpr(r : RegExpr) : Boolean;
  begin
    is_epsExpr := r=epsExpr
  end(*is_epsExpr*);
function is_markExpr(r : RegExpr; var rule, pos : Integer) : Boolean;
  begin
    if r=epsExpr then
      is_markExpr := false
    else if r^.node_type=mark_node then
      begin
        is_markExpr := true;
        rule := r^.rule;
        pos  := r^.pos;
      end
    else
      is_markExpr := false
  end(*is_markExpr*);
function is_charExpr(r : RegExpr; var c : Char) : Boolean;
  begin
    if r=epsExpr then
      is_charExpr := false
    else if r^.node_type=char_node then
      begin
        is_charExpr := true;
        c := r^.c
      end
    else
      is_charExpr := false
  end(*is_charExpr*);
function is_strExpr(r : RegExpr; var str : StrPtr) : Boolean;
  begin
    if r=epsExpr then
      is_strExpr := false
    else if r^.node_type=str_node then
      begin
        is_strExpr := true;
        str := r^.str;
      end
    else
      is_strExpr := false
  end(*is_strExpr*);
function is_cclassExpr(r : RegExpr; var cc : CClassPtr) : Boolean;
  begin
    if r=epsExpr then
      is_cclassExpr := false
    else if r^.node_type=cclass_node then
      begin
        is_cclassExpr := true;
        cc := r^.cc
      end
    else
      is_cclassExpr := false
  end(*is_cclassExpr*);
function is_starExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  begin
    if r=epsExpr then
      is_starExpr := false
    else if r^.node_type=star_node then
      begin
        is_starExpr := true;
        r1 := r^.r
      end
    else
      is_starExpr := false
  end(*is_starExpr*);
function is_plusExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  begin
    if r=epsExpr then
      is_plusExpr := false
    else if r^.node_type=plus_node then
      begin
        is_plusExpr := true;
        r1 := r^.r
      end
    else
      is_plusExpr := false
  end(*is_plusExpr*);
function is_optExpr(r : RegExpr; var r1 : RegExpr) : Boolean;
  begin
    if r=epsExpr then
      is_optExpr := false
    else if r^.node_type=opt_node then
      begin
        is_optExpr := true;
        r1 := r^.r
      end
    else
      is_optExpr := false
  end(*is_optExpr*);
function is_catExpr(r : RegExpr; var r1, r2 : RegExpr) : Boolean;
  begin
    if r=epsExpr then
      is_catExpr := false
    else if r^.node_type=cat_node then
      begin
        is_catExpr := true;
        r1 := r^.r1;
        r2 := r^.r2
      end
    else
      is_catExpr := false
  end(*is_catExpr*);
function is_altExpr(r : RegExpr; var r1, r2 : RegExpr) : Boolean;
  begin
    if r=epsExpr then
      is_altExpr := false
    else if r^.node_type=alt_node then
      begin
        is_altExpr := true;
        r1 := r^.r1;
        r2 := r^.r2
      end
    else
      is_altExpr := false
  end(*is_altExpr*);

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
function nchars(cc : CClass) : Integer;
  var
    c : Char;
    count : Integer;
  begin
    count := 0;
    for c := #0 to #255 do if c in cc then inc(count);
    nchars := count;
  end(*nchars*);
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

function charStr(c : char; reserved : CClass) : String;
  function octStr(c : char) : String;
    (* return octal string representation of character c *)
    begin
      octStr := intStr(ord(c) div 64)+intStr((ord(c) mod 64) div 8)+
                intStr(ord(c) mod 8);
    end(*octStr*);
  begin
    case c of
      #0..#7,      (* nonprintable characters *)
      #11,#14..#31,
      #127..#255 : charStr := '\'+octStr(c);
      bs         : charStr := '\b';
      tab        : charStr := '\t';
      nl         : charStr := '\n';
      cr         : charStr := '\c';
      ff         : charStr := '\f';
      '\'        : charStr := '\\';
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

function cclassStr(cc : CClass) : String;
  const
    reserved : CClass = ['^','-',']'];
    MaxChar = #255;
  var
    c1, c2 : Char;
    str : String;
    Quit: Boolean;
  begin
    if cc=[#1..#255]-[nl] then
      cclassStr := '.'
    else
      begin
        str := '';
        if nchars(cc)>128 then
          begin
            str := '^';
            cc := [#0..#255]-cc;
          end;
        c1 := chr(0);
        Quit := False;
        while not Quit do  begin
          if c1 in cc then  begin
            c2 := c1;
            while (c2<MaxChar) and (succ(c2) in cc) do
              c2 := succ(c2);
            if c1=c2
             then  str := str+charStr(c1, reserved)
             else
               if c2=succ(c1)
                then  str := str+charStr(c1, reserved)+charStr(c2, reserved)
                else  str := str+charStr(c1, reserved)+'-'+charStr(c2, reserved);
              c1 := c2;
          end;
          Quit := c1 = MaxChar;
          if not Quit then
            c1 := Succ(c1);
        end; { of while }
        cclassStr := '['+str+']'
      end
  end(*cclassStr*);

function cclassOrCharStr(cc : CClass) : String;
  var count : Integer;
      c, c1 : Char;
  begin
    count := 0;
    for c := #0 to #255 do
      if c in cc then
        begin
          c1 := c;
          inc(count);
          if count>1 then
            begin
              cclassOrCharStr := cclassStr(cc);
              exit;
            end;
        end;
    if count=1 then
      cclassOrCharStr := singleQuoteStr(c1)
    else
      cclassOrCharStr := '[]';
  end(*cclassOrCharStr*);

function regExprStr(r : RegExpr) : String;
  function unparseExpr(r : RegExpr) : String;
    var rule_no, pos : Integer;
        c : Char;
        str : StrPtr;
        cc : CClassPtr;
        r1, r2 : RegExpr;
    begin
      if is_epsExpr(r) then
        unparseExpr := ''
      else if is_markExpr(r, rule_no, pos) then
        unparseExpr := '#('+intStr(rule_no)+','+intStr(pos)+')'
      else if is_charExpr(r, c) then
        unparseExpr := charStr(c, [ '"','.','^','$','[',']','*','+','?',
                                    '{','}','|','(',')','/','<','>'])
      else if is_strExpr(r, str) then
        unparseExpr := doubleQuoteStr(str^)
      else if is_cclassExpr(r, cc) then
        unparseExpr := cclassStr(cc^)
      else if is_starExpr(r, r1) then
        unparseExpr := unparseExpr(r1)+'*'
      else if is_plusExpr(r, r1) then
        unparseExpr := unparseExpr(r1)+'+'
      else if is_optExpr(r, r1) then
        unparseExpr := unparseExpr(r1)+'?'
      else if is_catExpr(r, r1, r2) then
        unparseExpr := '('+unparseExpr(r1)+unparseExpr(r2)+')'
      else if is_altExpr(r, r1, r2) then
        unparseExpr := '('+unparseExpr(r1)+'|'+unparseExpr(r2)+')'
      else
        fatal('invalid expression');
    end(*unparseExpr*);
  begin
    regExprStr := unparseExpr(r);
  end(*regExprStr*);

end(*LexBase*).
