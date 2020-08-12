{
  DFA table construction. This code, admittedly, is not the most aesthetic,
  but it's quite efficient (and that's the main goal). For further
  explanation, refer to Aho/Sethi/Ullman 1986, Section 3.9.


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
$Modtime: 96-08-01 6:13 $

$History: LEXDFA.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit LexDFA;

interface



procedure makeDFATable;
  (* construct DFA from position table *)

implementation

uses LexBase, LexTable;

procedure makeDFATable;
  var i : Integer;
  begin
    (* initialize start states: *)
    for i := 2 to 2*n_start_states+1 do
      if not start_excl^[i div 2] then
        setunion(first_pos_table^[i]^, first_pos_table^[i mod 2]^);
    for i := 0 to 2*n_start_states+1 do
      act_state := newState(first_pos_table^[i]);
    act_state := -1;
    while succ(act_state)<n_states do
      begin
        inc(act_state);
        (* add transitions for active state: *)
        startStateTrans;
        for i := 1 to size(state_table^[act_state].state_pos^) do
          with pos_table^[state_table^[act_state].state_pos^[i]] do
            if pos_type=char_pos then
              addTrans([c], follow_pos)
            else if pos_type=cclass_pos then
              addTrans(cc^, follow_pos)
            else if pos=0 then
              state_table^[act_state].final := true;
        (* assign next states: *)
        for i := state_table^[act_state].trans_lo to n_trans do
          with trans_table^[i] do
            next_state := addState(follow_pos);
        (* merge transitions for the same next state: *)
        mergeTrans;
        (* sort transitions: *)
        sortTrans;
        endStateTrans;
      end;
  end(*makeDFATable*);

end(*LexDFA*).
