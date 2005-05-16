{
  This module collects the various tables used by the Yacc program:
  - the symbol table
  - the rule table
  - the precedence table
  - the closure table
  - the LALR state, item, transition and reduction table
  Note: All tables are allocated dynamically (at initialization time)
  because of the 64KB static data limit. *)


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
$Modtime: 96-07-31 21:15 $

$History: YACCTABL.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccTabl;

interface

uses
  YaccBase;


const

(* Maximum table sizes: *)

max_keys           =  997;  (* size of hash symbol table (prime number!)   *)
{$IFDEF MsDos}
max_nts            =  300;  (* maximum number of nonterminals              *)
max_lits           =  556;  (* number of literals (300+256)                *)
max_rules          =  301;  (* number of rules (300+1)                     *)
max_types          =  100;  (* number of type tags                         *)
max_prec           =   50;  (* maximum precedence level                    *)
max_states         =  600;  (* number of LR(0) states                      *)
max_items          = 2400;  (* number of items                             *)
max_trans          = 2400;  (* number of transitions                       *)
max_redns          = 1200;  (* number of reductions                        *)
{$ELSE}
max_nts            =  900;  (* maximum number of nonterminals              *)
max_lits           =  max_nts+256;  (* number of literals (300+256)                *)
max_rules          =  max_nts+1;  (* number of rules (300+1)                     *)
max_types          =  100;  (* number of type tags                         *)
max_prec           =   50;  (* maximum precedence level                    *)
{$IFDEF Windows}
max_states         =  800;  (* number of LR(0) states                      *)
{$ELSE}
max_states         = 1200;  (* number of LR(0) states                      *)
{$ENDIF}
max_items          = 9600;  (* number of items                             *)
max_trans          = 9600;  (* number of transitions                       *)
max_redns          = 1200;  (* number of reductions                        *)
{$ENDIF}

{$IFDEF MsDos}
max_rule_len       =   64;  (* maximum length of rules                     *)
max_set_items      =   64;  (* maximum number of items in an item set      *)
{$ELSE}
max_rule_len       =   64;  (* maximum length of rules                     *)
max_set_items      =   96;  (* maximum number of items in an item set      *)
{$ENDIF}

var

(* Actual table sizes: *)

n_nts            : Integer;
n_lits           : Integer;
n_rules          : Integer;
n_types          : Integer;
n_prec           : Integer;
n_states         : Integer;
n_items          : Integer;
n_trans          : Integer;
n_redns          : Integer;

type

(* Table data structures: *)

(* Symbol table: The symbol table consists of a hash table which stores
   print names and internal symbol numbers, and a key table which stores,
   for each internal symbol number, the corresponding hash key. *)

SymRec = record
           pname  : StrPtr;
             (* print name; empty entries are denoted by pname=nil *)
           deff   : Boolean;
             (* flag denoting whether symbol is already defined *)
           sym    : Integer;
             (* internal symbol number (0 or positive: literal symbols
                (literal characters have symbol numbers 1 thru 255);
                negative: nonterminal symbols; 0 denotes endmarker,
                -1 augmented start nonterminal, 256 is reserved for
                error token; note that the predefined symbols except
                the error literal are not actually stored in the symbol
                table; the error symbol is entered at initialization
                s.t. it always has number 256) *)
         end;

SymTable = array [1..max_keys] of SymRec;

SymKeyTable = array [-max_nts..max_lits-1] of Integer;
  (* hash keys for nonterminal and literal symbols *)

(* Rule table: the rule table consists of an array storing the rules
   sequentially in the order in which they appear in the source grammar;
   a rule no.s table which is used to sort rules w.r.t. left-hand side
   nonterminals (after the rule table has been constructed and sorted, all
   references to rules are done indirectly via the rule_no array s.t. the
   rules for each nonterminal can be accessed easily); and an offset table
   which stores, for each nonterminal, the corresponding first and last
   index in the rule no.s table. *)

RuleRec = record
            lhs_sym : Integer; (* lhs nonterminal *)
            rhs_len : Integer; (* length of rhs *)
            rhs_sym : array [1..max_rule_len] of Integer;
              (* rhs symbols *)
          end;

RuleRecPtr = ^RuleRec;

RuleTable = array [1..max_rules] of RuleRecPtr;

RuleNoTable = array [1..max_rules] of Integer;

RuleOffsRec = record
                rule_lo, rule_hi : Integer;
              end;

RuleOffsTable = array [1..max_nts] of RuleOffsRec;

(* Symbol type table: The symbol type table stores the types associated
   with the nonterminal and terminal grammar symbols (0 if none). *)

TypeTable    = array [1..max_types] of Integer;
  (* types declared in the definitions section *)

SymTypeTable = array [-max_nts..max_lits-1] of Integer;
  (* symbol types *)

(* Precedence table: The precedence table stores the type of each
   precedence level (left, right, nonassoc) and, for each literal
   symbol and grammar rule, the assigned precedence level (precedence
   level 0 if none). *)

PrecType = ( left, right, nonassoc );

PrecTable = array [1..max_prec] of PrecType;

SymPrecTable = array [0..max_lits-1] of Integer;

RulePrecTable = array [1..max_rules] of Integer;

(* Closure and first symbols table: The closure table stores, for each
   nonterminal X, the set of those nonterminals Y for which there is a
   rightmost derivation X =>+ Y ... . Similarly, the first set table
   stores, for each nonterminal X, the set of literals a for which there
   is a derivation X =>+ a ... . Both tables are of type SymSetTable.

   The nullable table stores, for each nonterminal, a flag denoting whether
   the nonterminal is nullable (i.e. may be derived to the empty string).

   These tables are constructed by the routines in the YaccClosure unit,
   and are used by the LALR parser construction algorithms in YaccLR0 and
   YaccLookaheads. *)

SymSetTable = array [1..max_nts] of IntSetPtr;

NullableTable = array [1..max_nts] of Boolean;

(* State table:

   Each state stores the first and last index of the kernel items,
   transitions and reductions belonging to it, and a hash key determined
   from the kernel items which is used to speed up searches for existing
   states.

   The items table stores the individual kernel items in the LR(0) set.
   Each entry consists of a rule number together with the item position,
   and a "next" field indicating the associated item in the successor state
   (0 if there is none). The ItemSet type is used to retrieve and manipulate
   individual item sets from the item table.

   The transition table stores the shift and goto transitions in each state
   (each transition is denoted by a (symbol, next_state) pair). Similarly,
   the reductions table stores the reductions in each state, where each
   action is denoted by a (symbolset, ruleno) pair. *)

StateRec = record
             item_lo, item_hi : Integer;
             trans_lo, trans_hi : Integer;
             redns_lo, redns_hi : Integer;
             key : Integer;
           end;

StateTable = array [0..max_states-1] of StateRec;

ItemRec = record
            rule_no, pos_no : Integer;
            next : Integer;
          end;

ItemSet = record
            n_items : Integer;
            item    : array [1..max_set_items] of ItemRec;
          end;

ItemTable = array [1..max_items] of ItemRec;

TransRec = record
             sym, next_state : Integer;
           end;

TransTable = array [1..max_trans] of TransRec;

RednRec = record
            symset : IntSetPtr;
            rule_no : Integer;
          end;

RednTable = array [1..max_redns] of RednRec;

(* Lookaheads table: This table stores, for each kernel item, the
   corresponding LALR(1) lookahead symbol sets. *)

LookaheadTable = array [1..max_items] of IntSetPtr;

(* The propagation table is used to keep track of how lookaheads are
   propagated from kernel items to other lookahead sets. *)

PropList = ^PropEntry;

PropEntry = record
              symset : IntSetPtr;
              next : PropList;
            end;

PropTable = array [1..max_items] of PropList;


var

verbose           : Boolean;          (* status of the verbose option *)
debug             : Boolean;          (* status of the debug option *)
startnt           : Integer;          (* start nonterminal of grammar
                                         (0 if undefined) *)
sym_table         : ^SymTable;        (* symbol table *)
sym_key           : ^SymKeyTable;     (* symbol keys *)
rule_table        : ^RuleTable;       (* rule table *)
type_table        : ^TypeTable;       (* type table *)
sym_type          : ^SymTypeTable;    (* symbol types *)
prec_table        : ^PrecTable;       (* precedence table *)
sym_prec          : ^SymPrecTable;    (* literal symbols precedence *)
rule_prec         : ^RulePrecTable;   (* rules precedence *)
rule_no           : ^RuleNoTable;     (* rule no table *)
rule_offs         : ^RuleOffsTable;   (* rule offset table *)
closure_table     : ^SymSetTable;     (* closure table *)
first_set_table   : ^SymSetTable;     (* first set table *)
nullable          : ^NullableTable;   (* nullable flags table *)
state_table       : ^StateTable;      (* LR(0) state table *)
item_table        : ^ItemTable;       (* LR(0) kernel item table *)
trans_table       : ^TransTable;      (* transition table *)
redn_table        : ^RednTable;       (* reduction table *)
lookahead_table   : ^LookaheadTable;  (* LALR lookaheads table *)
prop_table        : ^PropTable;       (* lookahead propagation table *)


(* Operations: *)

(* Symbol table routines: *)

function new_nt : Integer;
  (* returns a new nonterminal number (<-1) *)

function new_lit : Integer;
  (* returns a new literal number above 256 *)

procedure add_lit ( sym : Integer );
  (* this routine allows to add a user-defined literal symbol;
     the current literal symbols count is adjusted accordingly *)

function get_key ( symbol : String ) : Integer;
  (* returns a hash key for symbol *)

procedure def_key ( k : Integer; sym : Integer );
  (* defines k to be a new symbol with internal symbol number sym *)

function is_def_key ( k : Integer; var sym : Integer ) : Boolean;
  (* checks whether symbol denoted by symbol table key k is already
     defined; if so, returns the corresponding symbol number *)

function pname ( sym : Integer ) : String;
  (* returns the print name of an internal symbol (`$end' for
     symbol 0, `$accept' for nonterminal -1, and a single quoted
     character for literals 1..255) *)

(* Rule table routines: *)

function newRuleRec ( r : RuleRec ) : RuleRecPtr;
  (* obtains a dynamic copy of r (only the number of bytes actually
     needed is allocated) *)

procedure add_rule ( r : RuleRecPtr );
  (* add a rule to the rule table *)

procedure sort_rules;
  (* sorts rules w.r.t. left-hand sides into the rule no table *)

procedure rule_offsets;
  (* computes rule offsets after rules have been sorted *)

function n_nt_rules ( sym : Integer ) : Integer;
  (* returns number of rules for nonterminal sym *)

(* Type Table routines: *)

procedure add_type ( k : Integer );
  (* add a type identifier to the table *)

procedure sort_types;
  (* sort the type table alphabetically, eliminate dups *)

function search_type ( symbol : String ) : Boolean;
  (* search the sorted types table for the given type symbol *)

(* Precedence table routines: *)

function new_prec_level ( prec_type : PrecType ) : Integer;
  (* adds a new precedence level of the denoted type; returns: the new
     level *)

(* State table routines: *)

var act_state : Integer; (* state currently considered *)

procedure new_state;
  (* build a new state *)

procedure add_item ( rule_no, pos_no : Integer );
  (* add an item to the new state (initialize its next field to 0) *)

function add_state : Integer;
  (* add the new state to the state table; if an equivalent state is already
     in the table, dispose the new state, and return the existing state
     number, otherwise return the new state number *)

procedure start_trans;
  (* starts building transitions of the active state *)

procedure add_trans ( sym, next_state : Integer );
  (* adds a transition to the active state *)

procedure end_trans;
  (* ends transitions of the active state *)

procedure start_redns;
  (* starts building reduction actions of the active state *)

procedure add_redn ( symset : IntSetPtr; rule_no : Integer );
  (* adds a reduction to the active state *)

procedure end_redns;
  (* ends reduction actions of the active state *)

function n_state_items ( s : Integer ) : Integer;
function n_state_trans ( s : Integer ) : Integer;
function n_state_redns ( s : Integer ) : Integer;
  (* return the number of kernel items, transitions and reductions in state
     s, respectively *)

function find_item( s : Integer; rule_no, pos_no : Integer ) : Integer;
  (* find item (rule_no, pos_no) in state s; returns: the item number *)

(* Item set routines: *)

procedure empty_item_set ( var item_set : ItemSet );
  (* initializes an empty item set *)

procedure include_item_set ( var item_set : ItemSet;
                             rule_no, pos_no : Integer);
  (* add the denoted item to the given item set *)

procedure get_item_set ( s : Integer; var item_set : ItemSet);
  (* obtain the item set of state s from the item table *)

procedure closure ( var item_set : ItemSet );
  (* compute the closure of item_set (using the closure table) *)

procedure sort_item_set ( var item_set : ItemSet );
  (* sorts an item set w.r.t. position and rule numbers (higher positions,
     lower rules first) *)

implementation

uses YaccMsgs;

(* Symbol table routines: *)

function new_nt : Integer;
  begin
    inc(n_nts);
    if n_nts>max_nts then fatal(nt_table_overflow);
    sym_type^[-n_nts] := 0;
    new_nt := -n_nts;
  end(*new_nt*);

function new_lit : Integer;
  begin
    inc(n_lits);
    if n_lits>max_lits then fatal(lit_table_overflow);
    sym_type^[n_lits-1] := 0;
    sym_prec^[n_lits-1] := 0;
    new_lit := n_lits-1;
  end(*new_lit*);

procedure add_lit ( sym : Integer );
  begin
    if sym>n_lits then n_lits := sym;
    if n_lits>max_lits then fatal(lit_table_overflow);
    sym_type^[sym] := 0;
    sym_prec^[sym] := 0;
  end(*add_lit*);

{$ifndef fpc}{$F+}{$endif}
function lookup(k : Integer) : String;
{$ifndef fpc}{$F-}{$endif}
  (* print name of symbol no. k *)
  begin
    with sym_table^[k] do
      if pname=nil then
        lookup := ''
      else
        lookup := pname^
  end(*lookup*);

{$ifndef fpc}{$F+}{$endif}
procedure entry(k : Integer; symbol : String);
{$ifndef fpc}{$F-}{$endif}
  (* enter symbol into table *)
  begin
    sym_table^[k].pname := newStr(symbol);
  end(*entry*);

function get_key ( symbol : String ) : Integer;
  begin
    get_key := key(symbol, max_keys, {$ifdef fpc}@{$endif}lookup,
                   {$ifdef fpc}@{$endif}entry);
  end(*get_key*);

procedure def_key ( k : Integer; sym : Integer );
  begin
    sym_key^[sym] := k;
    sym_table^[k].deff := true;
    sym_table^[k].sym  := sym;
  end(*def_key*);

function is_def_key ( k : Integer; var sym : Integer ) : Boolean;
  begin
    if sym_table^[k].deff then
      begin
        sym := sym_table^[k].sym;
        is_def_key := true;
      end
    else
      is_def_key := false
  end(*is_def_key*);

function pname ( sym : Integer ) : String;
begin
  case sym of
    1..255 : pname := singleQuoteStr(chr(sym));
    0      : pname := '$end';
    -1     : pname := '$accept';
  else  begin
    if sym_table^[sym_key^[sym]].pname^[1]=''''
      then  begin
        pname := singleQuoteStr(
                   copy( sym_table^[sym_key^[sym]].pname^,
                         2,
                         length(sym_table^[sym_key^[sym]].pname^)-2)
                 )
      end
      else  begin
        pname := sym_table^[sym_key^[sym]].pname^;
      end;
  end;
  end;
end(*pname*);

(* Rule table: *)

function newRuleRec ( r : RuleRec ) : RuleRecPtr;
  var rp : RuleRecPtr;
  begin
    getmem(rp, 2*sizeOf(Integer)+r.rhs_len*sizeOf(Integer));
    move(r, rp^, 2*sizeOf(Integer)+r.rhs_len*sizeOf(Integer));
    newRuleRec := rp;
  end(*newRuleRec*);

procedure add_rule ( r : RuleRecPtr );
  begin
    inc(n_rules);
    if n_rules>max_rules then fatal(rule_table_overflow);
    rule_table^[n_rules] := r;
  end(*add_rule*);

{$ifndef fpc}{$F+}{$endif}
function rule_less ( i, j : Integer ) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  begin
    if rule_table^[rule_no^[i]]^.lhs_sym =
       rule_table^[rule_no^[j]]^.lhs_sym then
      rule_less := rule_no^[i] < rule_no^[j]
    else
      rule_less := rule_table^[rule_no^[i]]^.lhs_sym >
                   rule_table^[rule_no^[j]]^.lhs_sym
  end(*rule_less*);

{$ifndef fpc}{$F+}{$endif}
procedure rule_swap ( i, j : Integer );
{$ifndef fpc}{$F-}{$endif}
  var x : Integer;
  begin
    x := rule_no^[i]; rule_no^[i] := rule_no^[j]; rule_no^[j] := x;
  end(*rule_swap*);

procedure sort_rules;
  var i : Integer;
  begin
    for i := 1 to n_rules do rule_no^[i] := i;
    quicksort ( 1, n_rules, {$ifdef fpc}@{$endif}rule_less,
               {$ifdef fpc}@{$endif}rule_swap );
  end(*sort_rules*);

procedure rule_offsets;
  var i, sym : Integer;
  begin
    for sym := 1 to n_nts do with rule_offs^[sym] do
      begin
        rule_lo := 1; rule_hi := 0;
      end;
    i := 1;
    while (i<=n_rules) do
      begin
        sym := rule_table^[rule_no^[i]]^.lhs_sym;
        rule_offs^[-sym].rule_lo := i;
        inc(i);
        while (i<=n_rules) and
              (rule_table^[rule_no^[i]]^.lhs_sym=sym) do
          inc(i);
        rule_offs^[-sym].rule_hi := i-1;
      end;
  end(*rule_offsets*);

function n_nt_rules ( sym : Integer ) : Integer;
  begin
    with rule_offs^[-sym] do
      n_nt_rules := rule_hi-rule_lo+1
  end(*n_nt_rules*);

(* Type Table routines: *)

procedure add_type ( k : Integer );
  begin
    inc(n_types);
    if n_types>max_types then fatal(type_table_overflow);
    type_table^[n_types] := k;
  end(*add_type*);

(* Routines to sort type identifiers alphabetically: *)

{$ifndef fpc}{$F+}{$endif}
function type_less ( i, j : Integer ) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  begin
    type_less := sym_table^[type_table^[i]].pname^<
                 sym_table^[type_table^[j]].pname^
  end(*type_less*);

{$ifndef fpc}{$F+}{$endif}
procedure type_swap ( i, j : Integer );
{$ifndef fpc}{$F-}{$endif}
  var x : Integer;
  begin
    x := type_table^[i];
    type_table^[i] := type_table^[j];
    type_table^[j] := x;
  end(*type_swap*);

procedure sort_types;
  var i, j, count : Integer;
  begin
    (* sort: *)
    quicksort(1, n_types, {$ifdef fpc}@{$endif}type_less,
              {$ifdef fpc}@{$endif}type_swap);
    (* eliminate dups: *)
    i := 1; j := 1; count := 0;
    while i<=n_types do
      begin
        if i<>j then type_table^[j] := type_table^[i];
        while (i<n_types) and (type_table^[i+1]=type_table^[i]) do
          begin
            inc(i); inc(count);
          end;
        inc(i); inc(j);
      end;
    dec(n_types, count);
  end(*sort_types*);

function search_type ( symbol : String ) : Boolean;
  var l, r, k : Integer;
  begin
    (* binary search: *)
    l := 1; r := n_types;
    k := l + (r-l) div 2;
    while (l<r) and (sym_table^[type_table^[k]].pname^<>symbol) do
      begin
        if sym_table^[type_table^[k]].pname^<symbol then
          l := succ(k)
        else
          r := pred(k);
        k := l + (r-l) div 2;
      end;
    search_type := (k<=n_types) and (sym_table^[type_table^[k]].pname^=symbol);
  end(*search_type*);

(* Precedence table routines: *)

function new_prec_level ( prec_type : PrecType ) : Integer;
  begin
    inc(n_prec);
    if n_prec>max_prec then fatal(prec_table_overflow);
    prec_table^[n_prec] := prec_type;
    new_prec_level := n_prec;
  end(*new_prec_level*);

(* State table: *)

procedure new_state;
  begin
    inc(n_states);
    if n_states>max_states then fatal(state_table_overflow);
    state_table^[n_states-1].item_lo := n_items+1;
  end(*new_state*);

procedure add_item ( rule_no, pos_no : Integer );
  begin
    inc(n_items);
    if n_items>max_items then fatal(item_table_overflow);
    item_table^[n_items].rule_no := rule_no;
    item_table^[n_items].pos_no  := pos_no;
    item_table^[n_items].next    := 0;
  end(*add_item*);

function add_state : Integer;
  function state_key ( s : Integer ) : Integer;
    (* determines a hash key for state s *)
    const max_key = 4001;
      (* should be prime number s.t. hash keys are distributed
         evenly *)
    var i, k : Integer;
    begin
      with state_table^[s] do
        begin
          k := 0;
          for i := item_lo to item_hi do
            with item_table^[i] do
              inc(k, rule_no+pos_no);
          state_key := k mod max_key;
        end;
    end(*state_key*);
  function search_state ( s, lo, hi : Integer; var t : Integer ) : Boolean;
    (* searches the range lo..hi in the state table for a state with the
       same kernel items as s; returns true if found, and then the
       corresponding state number in t *)
    function eq_items(s, t : Integer) : Boolean;
      (* compares kernel item sets of states s and t *)
      var i, i_s, i_t : Integer;
      begin
        if n_state_items(s)<>n_state_items(t) then
          eq_items := false
        else
          begin
            i_s := state_table^[s].item_lo;
            i_t := state_table^[t].item_lo;
            for i := 0 to n_state_items(s)-1 do
              if (item_table^[i_s+i].rule_no<>item_table^[i_t+i].rule_no) or
                 (item_table^[i_s+i].pos_no<>item_table^[i_t+i].pos_no) then
                begin
                  eq_items := false;
                  exit;
                end;
            eq_items := true;
          end
      end(*eq_items*);
    var t1 : Integer;
    begin
      with state_table^[s] do
        for t1 := lo to hi do
          if (key=state_table^[t1].key) and
             eq_items(s, t1) then
            begin
              search_state := true;
              t := t1;
              exit;
            end;
      search_state := false;
    end(*search_state*);
  var s : Integer;
  begin
    with state_table^[n_states-1] do
      begin
        item_hi := n_items;
        key := state_key(n_states-1);
        if search_state(n_states-1, 0, n_states-2, s) then
          begin
            n_items := item_lo;
            dec(n_states);
            add_state := s;
          end
        else
          add_state := n_states-1;
      end;
  end(*add_state*);

procedure start_trans;
  begin
    state_table^[act_state].trans_lo := n_trans+1;
  end(*start_trans*);

procedure add_trans ( sym, next_state : Integer );
  begin
    inc(n_trans);
    if n_trans>max_trans then fatal(trans_table_overflow);
    trans_table^[n_trans].sym        := sym;
    trans_table^[n_trans].next_state := next_state;
  end(*add_trans*);

procedure end_trans;
  begin
    state_table^[act_state].trans_hi := n_trans;
  end(*end_trans*);

procedure start_redns;
  begin
    state_table^[act_state].redns_lo := n_redns+1;
  end(*start_redns*);

procedure add_redn ( symset : IntSetPtr; rule_no : Integer );
  begin
    inc(n_redns);
    if n_redns>max_redns then fatal(redn_table_overflow);
    redn_table^[n_redns].symset  := symset;
    redn_table^[n_redns].rule_no := rule_no;
  end(*add_redn*);

procedure end_redns;
  begin
    state_table^[act_state].redns_hi := n_redns;
  end(*end_redns*);

function n_state_items ( s : Integer ) : Integer;
  begin
    with state_table^[s] do
      n_state_items := item_hi-item_lo+1
  end(*n_state_items*);

function n_state_trans ( s : Integer ) : Integer;
  begin
    with state_table^[s] do
      n_state_trans := trans_hi-trans_lo+1
  end(*n_state_trans*);

function n_state_redns ( s : Integer ) : Integer;
  begin
    with state_table^[s] do
      n_state_redns := redns_hi-redns_lo+1
  end(*n_state_redns*);

function find_item( s : Integer; rule_no, pos_no : Integer ) : Integer;
  var i : Integer;
  begin
    with state_table^[s] do
      for i := item_lo to item_hi do
        if (item_table^[i].rule_no=rule_no) and
           (item_table^[i].pos_no=pos_no) then
          begin
            find_item := i;
            exit;
          end;
    find_item := 0;
  end(*find_item*);

(* Item set routines: *)

procedure empty_item_set ( var item_set : ItemSet );
  begin
    item_set.n_items := 0;
  end(*empty_item_set*);

procedure include_item_set ( var item_set : ItemSet;
                             rule_no, pos_no : Integer);
  begin
    with item_set do
      begin
        inc(n_items);
        if n_items>max_set_items then fatal(item_table_overflow);
        item[n_items].rule_no := rule_no;
        item[n_items].pos_no  := pos_no;
      end;
  end(*include_item_set*);

procedure get_item_set ( s : Integer; var item_set : ItemSet);
  begin
    with state_table^[s], item_set do
      begin
        n_items := n_state_items(s);
        move(item_table^[item_lo], item, n_items*sizeOf(ItemRec));
      end
  end(*get_item_set*);

procedure closure ( var item_set : ItemSet );
  var i, j : Integer;
      nt_syms0, nt_syms : IntSet;
  begin
    with item_set do
      begin
        (* get the nonterminals at current positions in items: *)
        empty(nt_syms0);
        for i := 1 to n_items do
          with item[i], rule_table^[rule_no]^ do
            if (pos_no<=rhs_len) and (rhs_sym[pos_no]<0) then
              include(nt_syms0, rhs_sym[pos_no]);
        nt_syms := nt_syms0;
        (* add closure symbols: *)
        for i := 1 to size(nt_syms0) do
          setunion(nt_syms, closure_table^[-nt_syms0[i]]^);
        (* add the nonkernel items for the nonterminal symbols: *)
        for i := 1 to size(nt_syms) do
          with rule_offs^[-nt_syms[i]] do
            for j := rule_lo to rule_hi do
              include_item_set(item_set, rule_no^[j], 1);
      end;
  end(*closure*);

var sort_items : ItemSet;

(* comparison and swap routines for sort_item_set: *)

{$ifndef fpc}{$F+}{$endif}
function items_less ( i, j : Integer ) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  begin
    with sort_items do
      if item[i].pos_no=item[j].pos_no then
        items_less := item[i].rule_no<item[j].rule_no
      else
        items_less := item[i].pos_no>item[j].pos_no
  end(*items_less*);

{$ifndef fpc}{$F+}{$endif}
procedure items_swap ( i, j : Integer );
{$ifndef fpc}{$F-}{$endif}
  var x : ItemRec;
  begin
    with sort_items do
      begin
        x := item[i]; item[i] := item[j]; item[j] := x;
      end
  end(*items_swap*);

procedure sort_item_set ( var item_set : ItemSet );
  begin
    sort_items := item_set;
    quicksort(1, sort_items.n_items, {$ifdef fpc}@{$endif}items_less,
              {$ifdef fpc}@{$endif}items_swap);
    item_set := sort_items;
  end(*sort_item_set*);

var i : Integer;

begin

  verbose          := false;
  debug            := false;
  startnt          := 0;

  n_nts            := 1;
  n_lits           := 257;
  n_rules          := 0;
  n_types          := 0;
  n_prec           := 0;
  n_states         := 0;
  n_items          := 0;
  n_trans          := 0;
  n_redns          := 0;

  (* allocate tables: *)

  new(sym_table);
  new(sym_key);
  new(rule_table);
  new(rule_no);
  new(rule_offs);
  new(type_table);
  new(sym_type);
  new(prec_table);
  new(sym_prec);
  new(rule_prec);
  new(closure_table);
  new(first_set_table);
  new(nullable);
  new(state_table);
  new(item_table);
  new(trans_table);
  new(redn_table);
  new(lookahead_table);
  new(prop_table);

  (* initialize symbol table: *)

  for i := 1 to max_keys do
    with sym_table^[i] do
      begin
        pname := nil;
        deff  := false;
      end;

  (* enter predefined error symbol into symbol table: *)

  def_key(get_key('error'), 256);

  (* initialize type and precedence tables: *)

  for i := -max_nts to max_lits-1 do sym_type^[i] := 0;
  for i := 0 to max_lits-1 do sym_prec^[i] := 0;
  for i := 1 to max_rules do rule_prec^[i] := 0;

end(*YaccTables*).
