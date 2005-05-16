{
  Yacc lookahead computation. This implementation is based on the
  lookahead set algorithm described in Aho/Sethi/Ullman, 1986,
  Section 4.7.


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
$Modtime: 96-07-31 14:09 $

$History: YACCLOOK.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccLook;

interface


procedure lookaheads;
  (* computes the LALR lookahead sets and enters corresponding reductions
     into the redn table (sorted w.r.t. rule numbers) *)

implementation

uses YaccBase, YaccTabl;

(* This implementation is based on algorithms 4.12 and 4.13 in Aho/Sethi/
   Ullman 1986 (with some optimizations added), which avoid the need to
   construct the full LR(1) set, and are able to compute lookaheads from
   LR(0) kernel items only.

   We start off with the LR(0) state set together with corresponding (shift
   and goto) transitions already computed. We compute the LALR(1) lookahead
   sets for kernel items and also record all corresponding reduce actions
   in the reduction table (where we also have to consider nonkernel items
   with empty right-hand side; these items also call for a reduction, but
   never appear in the kernel item table).

   This implementation uses some simple optimizations to speed up the
   algorithm. The lookahead sets are represented by (IntSet) pointers.
   Lookahead sets are associated with each kernel item in the item table,
   and with each reduction in the reduction table. A kernel item
   calling for a reduction shares its lookahead set pointer with the
   corresponding entry in the reduction table. The lookahead set for
   a nonkernel reduction item (item with empty right-hand side) only
   appears in the reduction table.

   The algorithm consists of two phases:

   1. Initialization:

      The initialization phase consists of a single traversal of the LR(0)
      set, where we compute lookahead sets generated spontaneously (lookaheads
      which are passed on from nonkernel items to the corresponding items in
      successor states), initialize lookahead sets and enter them into the
      lookahead and reduction table. Furthermore, during the initialization
      phase we also initialize the links for the propagation of lookaheads
      in the second phase.

      To determine lookaheads and propagation links, we compute the look-
      aheads for the closures of single LR(0) sets "in the small", according
      to the method in Aho/Sethi/Ullman 1986 (with some modifications),
      where we associate with each kernel item i a corresponding endmarker
      symbol #i as its lookahead symbol.

      The initialization phase proceeds as follows:

      1) Initialize all nonkernel item lookahead sets to empty.

      Now we pass over each state s in the LR0 set, repeating steps 2) thru
      5) specified below:

      2) Compute the closure closure(K(s)) of the states's kernel set K(s).

      3) Compute the lookahead sets for closure(K(s)) (described in detail
         below) where each kernel item i is associated with a special
         endmarker symbol #i as lookahead.

      Now the lookahead symbols, reductions and propagation links are entered
      into the corresponding tables as follows:

      4) Process kernel items: Add a propagation link from the kernel item
         to the lookahead set of the linked item in the corresponding
         successor state (as specified by the next field). If there is no
         successor item (kernel item calling for a reduction), add a
         corresponding entry into the reduction table instead.

      5) Process nonkernel items: find the corresponding kernel item in the
         successor state which is generated spontaneously from the nonkernel
         item. Add the spontaneous lookahead symbols (except endmarker
         symbols) of the nonkernel item determined in step 3) to the kernel
         item. If the nonkernel item has an empty right-hand side (nonkernel
         item calling for a reduction), add a corresponding entry into the
         reduction table instead. Furthermore, for each endmarker symbol
         #i in the spontaneous lookahead set of the nonkernel item, add
         a corresponding propagation link from the ith kernel item to the
         lookahead set of the spontaneous kernel item.

      To compute the spontaneous lookaheads (step 3)), we proceed as follows:

      3a) First compute the first sets of tail strings of all items in
          closure(K(s)). The "tail string" of an item [ X -> v.Yw ], where
          Y is a nonterminal, is the symbol sequence w, whose first set
          induces corresponding spontaneous lookaheads in the nonkernel
          items of the state with left-hand side Y; note that the first
          sets of "tail strings" in items [ X -> v.yw ], where y is a
          *terminal* symbol, are not required and hence it is not
          necessary to compute them. We also record for each item whether
          its tail string is "nullable", i.e., may be derived to the empty
          string. In this case, the item also passes on its own lookaheads,
          in addition to the first symbols of its tail string. First sets
          and nullable flags are computed using the information in YaccTable's
          first and nullable tables.

      3b) Now follows an initialization part in which each item [ X -> v.Yw ]
          passes on the first symbols of its tail string to the lookahead
          sets of each corresponding nonkernel item [ Y -> .u ].

      3c) Finally, we repeatedly pass over the item set, passing on
          lookaheads from items with nullable tail strings. Each item
          [ X -> v.Yw ] with nullable w propagates its own lookaheads to
          all corresponding nonkernel items [ Y -> .u]. Step 3c) terminates
          as soon as no lookahead sets have been modified during the previous
          pass.

   2. Propagation:

      The second phase of the lookahead computation algorithm now is quite
      simple. We repeatedly pass over all kernel items, propagating lookaheads
      according to the propagation links determined in the initialization
      phase. The algorithm terminates as soon as no lookahead sets have been
      modified during the previous pass. *)

(* Data structures used in lookahead computation: *)

type

SymSetArray = array [1..max_set_items] of IntSet;
BoolArray   = array [1..max_set_items] of Boolean;

var

item_set       : ItemSet;
lookahead_set  : SymSetArray;
n_kernel_items : Integer;

procedure spontaneous_lookaheads;

  (* compute spontaneous lookaheads for item_set; negative symbols are
     used for endmarkers (-i denotes endmarker #i) *)

  var count, last_count, i : Integer;
      first_syms : SymSetArray;
      nullable : BoolArray;

  function sym_count ( n : Integer ) : Integer;
    (* count lookahead symbols *)
    var count, i : Integer;
    begin
      count := 0;
      for i := 1 to n do
        inc(count, size(lookahead_set[i]));
      sym_count := count;
    end(*sym_count*);

  procedure compute_first_syms ( i : Integer );
    (* compute first set and nullable flag for tail string of item
       number i *)
    var j : Integer;
    begin
      empty(first_syms[i]); nullable[i] := true;
      with item_set, item[i], rule_table^[rule_no]^ do
        if (pos_no<=rhs_len) and (rhs_sym[pos_no]<0) then
          begin
            j := pos_no+1;
            while (j<=rhs_len) and nullable[i] do
              begin
                if rhs_sym[j]<0 then
                  begin
                    setunion(first_syms[i], first_set_table^[-rhs_sym[j]]^);
                    nullable[i] := YaccTabl.nullable^[-rhs_sym[j]];
                  end
                else
                  begin
                    include(first_syms[i], rhs_sym[j]);
                    nullable[i] := false;
                  end;
                inc(j);
              end;
          end;
    end(*compute_first_syms*);

  procedure init_lookaheads ( i : Integer );
    (* compute initial lookaheads induced by first sets of tail string
       of item i *)
    var sym, j : Integer;
    begin
      with item_set, item[i], rule_table^[rule_no]^ do
        if (pos_no<=rhs_len) and (rhs_sym[pos_no]<0) then
          begin
            sym := rhs_sym[pos_no];
            for j := n_kernel_items+1 to n_items do
              with item[j], rule_table^[rule_no]^ do
                if lhs_sym=sym then
                  setunion(lookahead_set[j], first_syms[i]);
          end
    end(*initial_lookaheads*);

  procedure propagate ( i : Integer );
    (* propagate lookahead symbols of item i *)
    var sym, j : Integer;
    begin
      with item_set, item[i], rule_table^[rule_no]^ do
        if (pos_no<=rhs_len) and (rhs_sym[pos_no]<0) and nullable[i] then
          begin
            sym := rhs_sym[pos_no];
            for j := n_kernel_items+1 to n_items do
              with item[j], rule_table^[rule_no]^ do
                if lhs_sym=sym then
                  setunion(lookahead_set[j], lookahead_set[i]);
          end
    end(*propagate*);

  begin(*spontaneous_lookaheads*)
    with item_set do
      begin
        (* initialize kernel lookahead sets: *)
        for i := 1 to n_kernel_items do singleton(lookahead_set[i], -i);
        (* compute first sets and nullable flags: *)
        for i := 1 to n_items do compute_first_syms(i);
        (* initialize nonkernel lookahead sets: *)
        for i := n_kernel_items+1 to n_items do empty(lookahead_set[i]);
        for i := 1 to n_items do init_lookaheads(i);
        (* repeated passes until no more lookaheads have been added
           during the previous pass: *)
        count := sym_count(n_items);
        repeat
          last_count := count;
          for i := 1 to n_items do
            propagate(i);
          count := sym_count(n_items);
        until last_count=count;
      end;
  end(*spontaneous_lookaheads*);

{$ifndef fpc}{$F+}{$endif}
function redns_less ( i, j : Integer ) : Boolean;
{$ifndef fpc}{$F-}{$endif}
  begin
    redns_less := redn_table^[i].rule_no<redn_table^[j].rule_no
  end(*redns_less*);

{$ifndef fpc}{$F+}{$endif}
procedure redns_swap ( i, j : Integer );
{$ifndef fpc}{$F-}{$endif}
  var x : RednRec;
  begin
    x := redn_table^[i];
    redn_table^[i] := redn_table^[j];
    redn_table^[j] := x;
  end(*redns_swap*);

procedure sort_redns;
  (* sort reduction entries in act_state w.r.t. rule numbers *)
  begin
    with state_table^[act_state] do
      quicksort(redns_lo, redns_hi, {$ifdef fpc}@{$endif}redns_less,
                {$ifdef fpc}@{$endif}redns_swap);
  end(*sort_redns*);

procedure initialize;

  (* initialization phase of lookahead computation algorithm *)

  procedure add_prop ( i : Integer; symset : IntSetPtr );
    (* add a propagation link to kernel item i *)
    var prop : PropList;
    begin
      new(prop);
      prop^.symset := symset;
      prop^.next := prop_table^[i];
      prop_table^[i] := prop;
    end(*add_prop*);

  var i, j, k : Integer;
      lookaheads : IntSetPtr;

  begin
    (* initialize lookahead sets and propagation links: *)
    for i := 1 to n_items do lookahead_table^[i] := newEmptyIntSet;
    for i := 1 to n_items do prop_table^[i] := nil;
    act_state := 0;
    repeat
      with state_table^[act_state], item_set do
        begin
          start_redns;
          get_item_set(act_state, item_set);
          n_kernel_items := n_items;
          (* compute LR(0) closure: *)
          closure(item_set);
          (* compute spontaneous lookaheads: *)
          spontaneous_lookaheads;
          (* process kernel items: *)
          for i := 1 to n_kernel_items do with item[i] do
            if next>0 then
              (* add propagation link: *)
              add_prop(item_lo+i-1, lookahead_table^[next])
            else
              (* enter reduce action: *)
              add_redn(lookahead_table^[item_lo+i-1], rule_no);
          (* process nonkernel items: *)
          (* find successor items: *)
          for k := trans_lo to trans_hi do
            with trans_table^[k] do
              for i := n_kernel_items+1 to n_items do
                with item[i], rule_table^[rule_no]^ do
                  if pos_no>rhs_len then
                    next := 0
                  else if rhs_sym[pos_no]=sym then
                    next := find_item(next_state, rule_no, pos_no+1);
          (* add spontaneous lookaheads and propagation links: *)
          for i := n_kernel_items+1 to n_items do with item[i] do
            if next>0 then
              (* lookaheads are generated spontaneously for successor
                 item: *)
              for j := 1 to size(lookahead_set[i]) do
                if lookahead_set[i][j]>=0 then
                  include(lookahead_table^[next]^, lookahead_set[i][j])
                else
                  add_prop(item_lo+(-lookahead_set[i][j])-1,
                           lookahead_table^[next])
            else
              (* nonkernel reduction item: *)
              begin
                lookaheads := newEmptyIntSet;
                for j := 1 to size(lookahead_set[i]) do
                  if lookahead_set[i][j]>=0 then
                    include(lookaheads^, lookahead_set[i][j])
                  else
                    add_prop(item_lo+(-lookahead_set[i][j])-1,
                             lookaheads);
                add_redn(lookaheads, rule_no);
              end;
          end_redns;
          sort_redns;
        end;
      inc(act_state);
    until act_state=n_states;
  end(*initialize*);

procedure propagate;

  (* propagation phase of lookahead computation algorithm *)

  var i, l : Integer;
      done : Boolean;
      prop : PropList;

  begin
    (* repeated passes over the kernel items table until no more lookaheads
       could be added in the previous pass: *)
    repeat
      done := true;
      for i := 1 to n_items do
        begin
          prop := prop_table^[i];
          while prop<>nil do with prop^ do
            begin
              l := size(symset^);
              setunion(symset^, lookahead_table^[i]^);
              if size(symset^)>l then done := false;
              prop := next;
            end;
        end;
    until done;
  end(*propagate*);

procedure lookaheads;
  begin
    initialize;
    propagate;
  end(*lookaheads*);

end(*YaccLookaheads*).
