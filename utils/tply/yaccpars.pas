{
  Yacc parse table construction.


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

$History: YACCPARS.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccPars;

interface


procedure parse_table;

  (* Constructs the parse table from the information in the state,
     transition and reduction table, and writes parse and rule table
     information to the output file.

     Rules never reduced are detected, and parsing conflicts resolved
     according to the usual disambiguting rules:

     - by default, shift/reduce conflicts are resolved in favour of
       shift, and reduce/reduce conflicts are resolved in favour of
       the rule appearing first in the grammar

     - in the presence of precedence information, shift/reduce conflicts
       are resolved as follows:
       - if the rule has higher precedence than the input symbol,
         reduce
       - if the input symbol has higher precedence than the rule,
         shift
       - if rule and input symbol have the same precedence, use
         associativity to resolve the conflict: if the symbol is
         left-associative, reduce; if right-associative, shift;
         if nonassociative, error.

     The default action for any state is error, unless the state
     only has a single reduce action, and no shift (or nonassoc-induced
     error) actions, in which case the default action is the reduction.
     An accept action is generated for the shift-endmarker action.

     If the verbose option is enabled, the parse_table routine also writes
     a readable listing of the generated parser to the .LST file, including
     descriptions of parse conflicts and rules never reduced.

     Parse table actions are encoded as follows:
     - positive: next state (shift or goto action)
     - negative: rule to reduce (reduce action)
     - 0: error (in default action table) or accept (in shift/reduce
          action table)

     The tables are written out as a collection of typed array constants:

     type YYARec = record { action record }
                     sym, act : Integer; { symbol and action }
                   end;
          YYRRec = record { rule record }
                     len, sym : Integer; { length and lhs symbol }
                   end;

     const

     yynacts   = ...; { number of parse table (shift and reduce) actions }
     yyngotos  = ...; { number of goto actions }
     yynstates = ...; { number of states }
     yynrules  = ...; { number of rules }

     yya : array [1..yynacts] of YYARec = ...;
       { shift and reduce actions }
     yyg : array [1..yyngotos] of YYARec = ...;
       { goto actions }
     yyd : array [0..yynstates-1] of Integer = ...;
       { default actions }
     yyal, yyah,
     yygl, yygh : array [0..yynstates-1] of Integer = ...;
       { offsets into action and goto table }

     yyr : array [1..yynrules] of YYRRec = ...;

  *)

var shift_reduce, reduce_reduce, never_reduced : Integer;
  (* number of parsing conflicts and unreduced rules detected during
     parse table generation *)

implementation

uses YaccBase, YaccTabl;

var reduced : array [1..max_rules] of Boolean;

var yynacts, yyngotos, yynstates : Integer;
    yyd : array [0..max_states-1] of Integer;
    yyal, yyah, yygl, yygh : array [0..max_states-1] of Integer;

function ruleStr ( i : Integer ) : String;
  (* returns print representation of rule number i *)
  var str : String; j : Integer;
  begin
    with rule_table^[i]^ do
      begin
        str := pname(lhs_sym)+' :';
        for j := 1 to rhs_len do
          str := str+' '+pname(rhs_sym[j]);
      end;
    ruleStr := str;
  end(*ruleStr*);

function itemStr ( var item_set : ItemSet; i : Integer ) : String;
  (* returns print representation of item number i in item_set *)
  var str : String; j : Integer;
  begin
    with item_set, item[i], rule_table^[rule_no]^ do
      begin
        str := pname(lhs_sym)+' :';
        for j := 1 to pos_no-1 do
          str := str+' '+pname(rhs_sym[j]);
        str := str+' _';
        for j := pos_no to rhs_len do
          str := str+' '+pname(rhs_sym[j]);
      end;
    itemStr := str;
  end(*itemStr*);

procedure build;

  (* build the parse table, resolve conflicts *)

  var

    i, j, k, s,
    n_errors,
    n_shifts,
    n_gotos,
    n_reductions,
    n_conflicts : Integer;

  item_set : ItemSet;

  begin

    (* initialize: *)

    shift_reduce := 0; reduce_reduce := 0; never_reduced := 0;
    for i := 1 to n_rules do reduced[i] := false;

    (* traverse the state table: *)

    for s := 0 to n_states-1 do with state_table^[s] do

      begin

        if verbose then
          begin
            writeln(yylst);
            writeln(yylst, 'state ', s, ':');
          end;

        (* Check shift and reduce actions, resolve conflicts.
           The number of error actions generated by nonassoc's is counted
           in n_errors, the number of conflicts reported in n_conflicts.
           Shift actions ruled out by disambiguating rules are flagged by
           setting the corresponding next_state to -1. *)

        n_errors := 0; n_conflicts := 0;

        for i := trans_lo to trans_hi do with trans_table^[i] do
          if sym>=0 then
            for j := redns_lo to redns_hi do with redn_table^[j] do
              if member(sym, symset^) then
                if (sym_prec^[sym]>0) and (rule_prec^[rule_no]>0) then
                  (* resolve conflict using precedence: *)
                  if rule_prec^[rule_no]=sym_prec^[sym] then
                    case prec_table^[sym_prec^[sym]] of
                      left     : (* reduce *)
                                 next_state := -1;
                      right    : (* shift *)
                                 exclude(symset^, sym);
                      nonassoc : (* error *)
                                 begin
                                   inc(n_errors);
                                   next_state := -1;
                                   exclude(symset^, sym);
                                 end;
                    end
                  else if rule_prec^[rule_no]>sym_prec^[sym] then
                    (* reduce *)
                    next_state := -1
                  else
                    (* shift *)
                    exclude(symset^, sym)
                else
                  (* shift/reduce conflict: *)
                  begin
                    if verbose then
                      begin
                        if n_conflicts=0 then
                          begin
                            writeln(yylst);
                            writeln(yylst, tab, '*** conflicts:');
                            writeln(yylst);
                          end;
                        writeln(yylst, tab,
                                       'shift ', next_state, ', ',
                                       'reduce ', rule_no-1, ' on ',
                                       pname(sym));
                      end;
                    inc(n_conflicts); inc(shift_reduce);
                    exclude(symset^, sym);
                  end;

        for i := redns_lo to redns_hi do
          for j := i+1 to redns_hi do with redn_table^[j] do
            begin
              for k := 1 to size(symset^) do
                if member(symset^[k], redn_table^[i].symset^) then
                  (* reduce/reduce conflict: *)
                  begin
                    if verbose then
                      begin
                        if n_conflicts=0 then
                          begin
                            writeln(yylst);
                            writeln(yylst, tab, '*** conflicts:');
                            writeln(yylst);
                          end;
                        writeln(yylst, tab,
                                       'reduce ',
                                       redn_table^[i].rule_no-1, ', ',
                                       'reduce ', rule_no-1, ' on ',
                                       pname(symset^[k]));
                      end;
                    inc(n_conflicts); inc(reduce_reduce);
                  end;
              setminus(symset^, redn_table^[i].symset^);
            end;

        (* Count goto, shift and reduce actions to generate. *)

        n_gotos := 0; n_shifts := 0; n_reductions := 0;

        for i := trans_lo to trans_hi do with trans_table^[i] do
          if next_state<>-1 then
            if sym<0 then
              inc(n_gotos)
            else
              inc(n_shifts);

        for i := redns_lo to redns_hi do with redn_table^[i] do
          if size(symset^)>0 then
            inc(n_reductions);

        (* Determine default action. *)

        if (n_shifts+n_errors=0) and (n_reductions=1) then
          (* default action is the reduction *)
          with redn_table^[redns_lo] do
            yyd[s] := -(rule_no-1)
        else
          (* default action is error *)
          yyd[s] := 0;

        (* Flag reduced rules. *)

        for i := redns_lo to redns_hi do
          with redn_table^[i] do
            reduced[rule_no] := true;

        if verbose then

          begin

            (* List kernel items. *)

            writeln(yylst);
            get_item_set(s, item_set);
            closure(item_set);
            sort_item_set(item_set);
            with item_set do
              begin
                for i := 1 to n_items do
                  with item[i], rule_table^[rule_no]^ do
                    if (rule_no=1) or (pos_no>1) or (rhs_len=0) then
                      if pos_no>rhs_len then
                        writeln(yylst, tab,
                                       itemStr(item_set, i), tab,
                                       '(', rule_no-1, ')')
                      else
                        writeln(yylst, tab, itemStr(item_set, i));
              end;

            (* List parse actions. *)

            (* shift, reduce and default actions: *)

            if (n_shifts+n_errors=0) and (n_reductions=1) then
              (* default action is the reduction *)
              with redn_table^[redns_lo] do
                begin
                  writeln(yylst);
                  writeln(yylst, tab, '.', tab, 'reduce ', rule_no-1 );
                end
            else
              (* default action is error *)
              begin
                writeln(yylst);
                for i := trans_lo to trans_hi do with trans_table^[i] do
                  if next_state<>-1 then
                    if sym=0 then
                      (* accept action *)
                      writeln(yylst, tab, pname(sym), tab, 'accept')
                    else if sym>0 then
                      (* shift action *)
                      writeln(yylst, tab,
                                     pname(sym), tab, 'shift ', next_state);
                for i := redns_lo to redns_hi do
                  with redn_table^[i] do
                    for j := 1 to size(symset^) do
                      (* reduce action *)
                      writeln(yylst, tab,
                                     pname(symset^[j]), tab, 'reduce ',
                                     rule_no-1);
                (* error action *)
                writeln(yylst, tab, '.', tab, 'error');
              end;

            (* goto actions: *)

            if n_gotos>0 then
              begin
                writeln(yylst);
                for i := trans_lo to trans_hi do with trans_table^[i] do
                  if sym<0 then
                    writeln(yylst, tab,
                                   pname(sym), tab, 'goto ', next_state);
              end;

          end;

      end;

    for i := 2 to n_rules do
      if not reduced[i] then inc(never_reduced);

    if verbose then
      begin
        writeln(yylst);
        if shift_reduce>0 then
          writeln(yylst, shift_reduce, ' shift/reduce conflicts.');
        if reduce_reduce>0 then
          writeln(yylst, reduce_reduce, ' reduce/reduce conflicts.');
        if never_reduced>0 then
          writeln(yylst, never_reduced, ' rules never reduced.');
      end;

    (* report rules never reduced: *)

    if (never_reduced>0) and verbose then
      begin
        writeln(yylst);
        writeln(yylst, '*** rules never reduced:');
        for i := 2 to n_rules do if not reduced[i] then
          begin
            writeln(yylst);
            writeln(yylst, ruleStr(i), tab, '(', i-1, ')');
          end;
      end;

  end(*build*);

procedure counters;

  (* initialize counters and offsets *)

  var s, i : Integer;

  begin

    yynstates := n_states; yynacts := 0; yyngotos := 0;

    for s := 0 to n_states-1 do with state_table^[s] do
      begin
        yyal[s] := yynacts+1; yygl[s] := yyngotos+1;
        if yyd[s]=0 then
          begin
            for i := trans_lo to trans_hi do with trans_table^[i] do
              if (sym>=0) and (next_state<>-1) then
                inc(yynacts);
            for i := redns_lo to redns_hi do with redn_table^[i] do
              inc(yynacts, size(symset^));
          end;
        for i := trans_lo to trans_hi do with trans_table^[i] do
          if sym<0 then
            inc(yyngotos);
        yyah[s] := yynacts; yygh[s] := yyngotos;
      end;

  end(*counters*);

procedure tables;

  (* write tables to output file *)

  var s, i, j, count : Integer;

  begin

    writeln(yyout);
    writeln(yyout, 'type YYARec = record');
    writeln(yyout, '                sym, act : Integer;');
    writeln(yyout, '              end;');
    writeln(yyout, '     YYRRec = record');
    writeln(yyout, '                len, sym : Integer;');
    writeln(yyout, '              end;');
    writeln(yyout);
    writeln(yyout, 'const');

    (* counters: *)

    writeln(yyout);
    writeln(yyout, 'yynacts   = ', yynacts, ';');
    writeln(yyout, 'yyngotos  = ', yyngotos, ';');
    writeln(yyout, 'yynstates = ', yynstates, ';');
    writeln(yyout, 'yynrules  = ', n_rules-1, ';');

    (* shift/reduce table: *)

    writeln(yyout);
    writeln(yyout, 'yya : array [1..yynacts] of YYARec = (');
    count := 0;
    for s := 0 to n_states-1 do with state_table^[s] do
      begin
        writeln(yyout, '{ ', s, ': }');
        if yyd[s]=0 then
          begin
            for i := trans_lo to trans_hi do with trans_table^[i] do
              if (next_state<>-1) and (sym>=0) then
                begin
                  inc(count);
                  if sym=0 then
                    write(yyout, '  ( sym: 0; act: 0 )')
                  else
                    write(yyout, '  ( sym: ', sym, '; act: ',
                                 next_state, ' )');
                  if count<yynacts then write(yyout, ',');
                  writeln(yyout);
                end;
            for i := redns_lo to redns_hi do with redn_table^[i] do
              for j := 1 to size(symset^) do
                begin
                  inc(count);
                  write(yyout, '  ( sym: ', symset^[j], '; act: ',
                               -(rule_no-1), ' )');
                  if count<yynacts then write(yyout, ',');
                  writeln(yyout);
                end;
        end;
      end;
    writeln(yyout, ');');

    (* goto table: *)

    writeln(yyout);
    writeln(yyout, 'yyg : array [1..yyngotos] of YYARec = (');
    count := 0;
    for s := 0 to n_states-1 do with state_table^[s] do
      begin
        writeln(yyout, '{ ', s, ': }');
        for i := trans_lo to trans_hi do with trans_table^[i] do
          if sym<0 then
            begin
              inc(count);
              write(yyout, '  ( sym: ', sym, '; act: ', next_state, ' )');
              if count<yyngotos then write(yyout, ',');
              writeln(yyout);
            end;
      end;
    writeln(yyout, ');');

    (* default action table: *)

    writeln(yyout);
    writeln(yyout, 'yyd : array [0..yynstates-1] of Integer = (');
    for s := 0 to n_states-1 do
      begin
        write(yyout, '{ ', s, ': } ', yyd[s]);
        if s<n_states-1 then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');

    (* offset tables: *)

    writeln(yyout);
    writeln(yyout, 'yyal : array [0..yynstates-1] of Integer = (');
    for s := 0 to n_states-1 do
      begin
        write(yyout, '{ ', s, ': } ', yyal[s]);
        if s<n_states-1 then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');
    writeln(yyout);
    writeln(yyout, 'yyah : array [0..yynstates-1] of Integer = (');
    for s := 0 to n_states-1 do
      begin
        write(yyout, '{ ', s, ': } ', yyah[s]);
        if s<n_states-1 then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');
    writeln(yyout);
    writeln(yyout, 'yygl : array [0..yynstates-1] of Integer = (');
    for s := 0 to n_states-1 do
      begin
        write(yyout, '{ ', s, ': } ', yygl[s]);
        if s<n_states-1 then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');
    writeln(yyout);
    writeln(yyout, 'yygh : array [0..yynstates-1] of Integer = (');
    for s := 0 to n_states-1 do
      begin
        write(yyout, '{ ', s, ': } ', yygh[s]);
        if s<n_states-1 then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');

    (* rule table: *)

    writeln(yyout);
    writeln(yyout, 'yyr : array [1..yynrules] of YYRRec = (');
    for i := 2 to n_rules do with rule_table^[i]^ do
      begin
        write(yyout, '{ ', i-1, ': } ', '( len: ', rhs_len,
                                        '; sym: ', lhs_sym, ' )');
        if i<n_rules then write(yyout, ',');
        writeln(yyout);
      end;
    writeln(yyout, ');');

    writeln(yyout);

  end(*tables*);

procedure parse_table;
  begin
    build; counters; tables;
  end(*parse_table*);

end(*YaccParseTable*).
