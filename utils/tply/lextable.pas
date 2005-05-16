{
  This module collects the various tables used by the Lex program:
  - the symbol table
  - the position table
  - the DFA states and transition tables
  Note: All tables are allocated dynamically (at initialization time)
  because of the 64KB static data limit.


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


$Revision: 1.3 $
$Modtime: 96-08-01 10:23 $

$History: LEXTABLE.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit LexTable;

interface

uses LexBase;

const

(* Maximum table sizes: *)

max_keys           =  997;  (* size of hash symbol table (prime number!)   *)
{$IFDEF MsDos}
max_pos            =  600;  (* maximum number of positions                 *)
max_states         =  300;  (* number of DFA states                        *)
max_trans          =  600;  (* number of transitions                       *)
max_start_states   =   50;  (* maximum number of user-defined start states *)
{$ELSE}
max_pos            = 1200;  (* maximum number of positions                 *)
max_states         =  600;  (* number of DFA states                        *)
max_trans          = 1200;  (* number of transitions                       *)
max_start_states   =  100;  (* maximum number of user-defined start states *)
{$ENDIF}

var

(* Actual table sizes: *)

n_pos            : Integer;
n_states         : Integer;
n_trans          : Integer;
n_start_states   : Integer;

type

(* Table data structures: *)

SymTable = array [1..max_keys] of record
             pname  : StrPtr;
               (* print name; empty entries are denoted by pname=nil *)
             case sym_type : ( none, macro_sym, start_state_sym ) of
             macro_sym : ( subst : StrPtr );
               (* macro substitution *)
             start_state_sym : ( start_state : Integer );
               (* start state *)
           end;

PosTableEntry = record
                  follow_pos    : IntSetPtr;
                    (* set of follow positions *)
                  case pos_type : ( char_pos, cclass_pos, mark_pos ) of
                  char_pos      : ( c   : Char );
                    (* character position *)
                  cclass_pos    : ( cc  : CClassPtr );
                    (* character class position *)
                  mark_pos      : ( rule, pos : Integer );
                    (* mark position *)
                end;

PosTable = array [1..max_pos] of PosTableEntry;

FirstPosTable  = array [0..2*max_start_states+1] of IntSetPtr;
                   (* first positions for start states (even states
                      are entered anywhere on the line, odd states only
                      at the beginning of the line; states 0 and 1 denote
                      default, states 2..2*n_start_states+1 user-defined
                      start states) *)

StateTableEntry = record
                    state_pos : IntSetPtr;
                      (* positions covered by state *)
                    final     : Boolean;
                      (* final state? *)
                    trans_lo,
                    trans_hi  : Integer;
                      (* transitions *)
                  end;

StateTable = array [0..max_states-1] of StateTableEntry;

TransTableEntry = record
                    cc              : CClassPtr;
                      (* characters of transition *)
                    follow_pos      : IntSetPtr;
                      (* follow positions (positions of next state) *)
                    next_state      : Integer;
                      (* next state *)
                  end;

TransTable = array [1..max_trans] of TransTableEntry;


var

verbose           : Boolean;          (* status of the verbose option *)
optimize          : Boolean;          (* status of the optimization option *)

sym_table         : ^SymTable;        (* symbol table *)
pos_table         : ^PosTable;        (* position table *)
first_pos_table   : ^FirstPosTable;   (* first positions table *)
state_table       : ^StateTable;      (* DFA state table *)
trans_table       : ^TransTable;      (* DFA transition table *)


(* Operations: *)

(* Hash symbol table:
   The following routines are supplied to be used with the generic hash table
   routines in LexBase. *)

function lookup(k : Integer) : String;
  (* print name of symbol no. k *)
procedure entry(k : Integer; symbol : String);
  (* enter symbol into table *)

(* Routines to build the position table: *)

procedure addCharPos(c : Char);
procedure addCClassPos(cc : CClassPtr);
procedure addMarkPos(rule, pos : Integer);
  (* Positions are allocated in the order of calls to addCharPos, addCClassPos
     and addMarkPos, starting at position 1. These routines also initialize
     the corresponding follow sets. *)

(* Routines to build the state table: *)

var act_state : Integer; (* state currently considered *)

function newState(POS : IntSetPtr) : Integer;
  (* Add a new state with the given position set; initialize the state's
     position set to POS (the offsets into the transition table are
     initialized when the state becomes active, see startStateTrans, below).
     Returns: the new state number *)

function addState(POS : IntSetPtr) : Integer;
  (* add a new state, but only if there is not already a state with the
     same position set *)

procedure startStateTrans;
procedure endStateTrans;
  (* initializes act_state's first and last offsets into the transition
     table *)

function n_state_trans(i : Integer) : Integer;
  (* return number of transitions in state i *)

procedure addTrans(cc : CClass; FOLLOW : IntSetPtr);
  (* adds a transition to the table *)

procedure mergeTrans;
  (* sorts transitions w.r.t. next states and merges transitions for the
     same next state in the active state *)

procedure sortTrans;
  (* sort transitions in act_state lexicographically *)

implementation

uses LexMsgs;

(* Hash table routines: *)

function lookup(k : Integer) : String;
  begin
    with sym_table^[k] do
      if pname=nil then
        lookup := ''
      else
        lookup := copy(pname^, 1, length(pname^))
  end(*lookup*);
procedure entry(k : Integer; symbol : String);
  begin
    with sym_table^[k] do
      begin
        pname    := newStr(symbol);
        sym_type := none;
      end
  end(*entry*);

(* Routines to build the position table: *)

procedure addCharPos(c : Char);
  begin
    inc(n_pos);
    if n_pos>max_pos then fatal(pos_table_overflow);
    pos_table^[n_pos].follow_pos     := newIntSet;
    pos_table^[n_pos].pos_type       := char_pos;
    pos_table^[n_pos].c              := c;
  end(*addCharPos*);

procedure addCClassPos(cc : CClassPtr);
  begin
    inc(n_pos);
    if n_pos>max_pos then fatal(pos_table_overflow);
    pos_table^[n_pos].follow_pos     := newIntSet;
    pos_table^[n_pos].pos_type       := cclass_pos;
    pos_table^[n_pos].cc             := cc;
  end(*addCClassPos*);

procedure addMarkPos(rule, pos : Integer);
  begin
    inc(n_pos);
    if n_pos>max_pos then fatal(pos_table_overflow);
    pos_table^[n_pos].follow_pos     := newIntSet;
    pos_table^[n_pos].pos_type       := mark_pos;
    pos_table^[n_pos].rule           := rule;
    pos_table^[n_pos].pos            := pos;
  end(*addMarkPos*);

(* Routines to build the state table: *)

function newState(POS : IntSetPtr) : Integer;
  begin
    if n_states>=max_states then fatal(state_table_overflow);
    newState := n_states;
    with state_table^[n_states] do
      begin
        state_pos := POS;
        final     := false;
      end;
    inc(n_states);
  end(*newState*);

function addState(POS : IntSetPtr) : Integer;
  var i : Integer;
  begin
    for i := 0 to pred(n_states) do
      if equal(POS^, state_table^[i].state_pos^) then
        begin
          addState := i;
          exit;
        end;
    addState := newState(POS);
  end(*addState*);

procedure startStateTrans;
  begin
    state_table^[act_state].trans_lo := succ(n_trans);
  end(*startStateTrans*);

procedure endStateTrans;
  begin
    state_table^[act_state].trans_hi := n_trans;
  end(*endStateTrans*);

function n_state_trans(i : Integer) : Integer;
  begin
    with state_table^[i] do
      n_state_trans := trans_hi-trans_lo+1
  end(*n_state_trans*);

(* Construction of the transition table:
   This implementation here uses a simple optimization which tries to avoid
   the construction of different transitions for each individual character
   in large character classes by MERGING transitions whenever possible. The
   transitions, at any time, will be partitioned into transitions on disjoint
   character classes. When adding a new transition on character class cc, we
   repartition the transitions as follows:
   1. If the current character class cc equals an existing one, we can
      simply add the new follow set to the existing one.
   2. Otherwise, for some existing transition on some character class
      cc1 with cc*cc1<>[], we replace the existing transition by a new
      transition on cc*cc1 with follow set = cc1's follow set + cc's follow
      set, and, if necessary (i.e. if cc1-cc is nonempty), a transition on
      cc1-cc with follow set = cc1's follow set. We then remove the elements
      of cc1 from cc, and proceed again with step 1.
   We may stop this process as soon as cc becomes empty (then all characters
   in cc have been distributed among the existing partitions). If cc does
   NOT become empty, we have to construct a new transition for the remaining
   character class (which then will be disjoint from all other character
   classes in the transition table). *)

procedure addTrans(cc : CClass; FOLLOW : IntSetPtr);
  var
    i : Integer;
    cc0, cc1, cc2 : CClass;
  begin
    for i := state_table^[act_state].trans_lo to n_trans do
      if trans_table^[i].cc^=cc then
        begin
          setunion(trans_table^[i].follow_pos^, FOLLOW^);
          exit
        end
      else
        begin
          cc0 := cc*trans_table^[i].cc^;
          if cc0<>[] then
            begin
              cc1 := trans_table^[i].cc^-cc;
              cc2 := cc-trans_table^[i].cc^;
              if cc1<>[] then
                begin
                  trans_table^[i].cc^ := cc1;
                  inc(n_trans);
                  if n_trans>max_trans then fatal(trans_table_overflow);
                  trans_table^[n_trans].cc := newCClass(cc0);
                  trans_table^[n_trans].follow_pos := newIntSet;
                  trans_table^[n_trans].follow_pos^ :=
                    trans_table^[i].follow_pos^;
                  setunion(trans_table^[n_trans].follow_pos^, FOLLOW^);
                end
              else
                begin
                  trans_table^[i].cc^ := cc0;
                  setunion(trans_table^[i].follow_pos^, FOLLOW^);
                end;
              cc := cc2;
              if cc=[] then exit;
            end
        end;
    inc(n_trans);
    if n_trans>max_trans then fatal(trans_table_overflow);
    trans_table^[n_trans].cc          := newCClass(cc);
    trans_table^[n_trans].follow_pos  := newIntSet;
    trans_table^[n_trans].follow_pos^ := FOLLOW^;
  end(*addCharTrans*);

(* comparison and swap procedures for sorting transitions: *)
{$ifndef fpc}{$F+}{$endif}
function transLessNextState(i, j : Integer) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  (* compare transitions based on next states (used in mergeCharTrans) *)
  begin
    transLessNextState := trans_table^[i].next_state<
                          trans_table^[j].next_state
  end(*transLessNextState*);
{$ifndef fpc}{$F+}{$endif}
function transLess(i, j : Integer) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  (* lexical order on transitions *)
  var c : Char; xi, xj : Boolean;
  begin
    for c := #0 to #255 do
      begin
        xi := c in trans_table^[i].cc^;
        xj := c in trans_table^[j].cc^;
        if xi<>xj then
          begin
            transLess := ord(xi)>ord(xj);
            exit
          end;
      end;
    transLess := false
  end(*transLess*);
{$ifndef fpc}{$F+}{$endif}
procedure transSwap(i, j : Integer);
{$ifndef fpc}{$F-}{$endif}
  (* swap transitions i and j *)
  var x : TransTableEntry;
  begin
    x := trans_table^[i];
    trans_table^[i] := trans_table^[j];
    trans_table^[j] := x;
  end(*transSwap*);

procedure mergeTrans;
  var
    i, j, n_deleted : Integer;
  begin
    (* sort transitions w.r.t. next states: *)
    quicksort(state_table^[act_state].trans_lo,
              n_trans,
              {$ifdef fpc}@{$endif}transLessNextState,
              {$ifdef fpc}@{$endif}transSwap);
    (* merge transitions for the same next state: *)
    n_deleted := 0;
    for i := state_table^[act_state].trans_lo to n_trans do
    if trans_table^[i].cc<>nil then
      begin
        j := succ(i);
        while (j<=n_trans) and
              (trans_table^[i].next_state =
               trans_table^[j].next_state) do
          begin
            (* merge cclasses of transitions i and j, then mark
               transition j as deleted *)
            trans_table^[i].cc^ := trans_table^[i].cc^+
                                   trans_table^[j].cc^;
            trans_table^[j].cc  := nil;
            inc(n_deleted);
            inc(j);
          end;
      end;
    (* remove deleted transitions: *)
    j := state_table^[act_state].trans_lo;
    for i := state_table^[act_state].trans_lo to n_trans do
      if trans_table^[i].cc<>nil then
        if i<>j then
          begin
            trans_table^[j] := trans_table^[i];
            inc(j);
          end
        else
          inc(j);
    (* update transition count: *)
    dec(n_trans, n_deleted);
  end(*mergeTrans*);

procedure sortTrans;
  begin
    quicksort(state_table^[act_state].trans_lo,
              n_trans,
              {$ifdef fpc}@{$endif}transLess,
              {$ifdef fpc}@{$endif}transSwap);
  end(*sortTrans*);

var i : Integer;

begin

  verbose          := false;
  optimize         := false;

  n_pos            := 0;
  n_states         := 0;
  n_trans          := 0;
  n_start_states   := 0;

  (* allocate tables: *)

  new(sym_table);
  new(pos_table);
  new(first_pos_table);
  new(state_table);
  new(trans_table);

  (* initialize symbol table: *)

  for i := 1 to max_keys do sym_table^[i].pname := nil;

end(*LexTables*).
