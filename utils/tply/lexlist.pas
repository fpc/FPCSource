{
  Lex listing routines.

  This module provides some routines to produce a readable representation
  of the generated DFA tables (the routines are only used when Lex is run
  with the verbose option /v).

  If this module is compiled with defined conditional `debug', the list file
  will contain extensive debugging output (position table, state positions,
  etc.).


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
$Modtime: 96-08-01 6:28 $

$History: LEXLIST.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit LexList;

interface

uses
  LexBase;


procedure listDFATable;
  (* list DFA table *)

implementation

uses LexTable;

procedure listTrans(cc : CClassPtr; next_state : Integer);
  (* list a transition in the format
        cc : next_state *)
  begin
    write(yylst, cclassOrCharStr(cc^):30, ' : ', next_state:5);
  end(*listTrans*);

{$ifdef debug}

procedure listPosTable;
  (* lists the position table *)
  var
    p, i : Integer;
  begin
    if n_pos=0 then exit;
    writeln(yylst);
    for p := 1 to n_pos do
      with pos_table^[p] do
        begin
          write(yylst, p:5, '     ');
          if pos_type=char_pos then
            write(yylst, singleQuoteStr(c):20)
          else if pos_type=cclass_pos then
            write(yylst, cclassStr(cc^):20)
          else if pos_type=mark_pos then
            if pos=0 then
              write(yylst, '# (rule '+intStr(rule)+')':20)
            else
              write(yylst, '/ (rule '+intStr(rule)+')':20);
          write(yylst, ' ':5);
          for i := 1 to size(follow_pos^) do
            if follow_pos^[i]>0 then write(yylst, follow_pos^[i]:5, ' ');
          writeln(yylst);
        end;
    writeln(yylst);
  end(*listPosTable*);

{$endif}

procedure listDFATable;
  var k, state : Integer;
  begin
{$ifdef debug}
    (* list position table: *)
    writeln(yylst);
    writeln(yylst, '( positions : )');
    listPosTable;
    (* list state table: *)
    writeln(yylst);
    writeln(yylst, '( states : )');
{$endif}
    writeln(yylst);
    for state := 0 to pred(n_states) do
      begin
        writeln(yylst);
        write(yylst, state);
        with state_table^[state] do
          begin
            if final then
              write(yylst, '* :')
            else
              write(yylst, '  :');
{$ifdef debug}
            for k := 1 to size(state_pos^) do
              write(yylst, ' ', state_pos^[k]:5);
{$else}
            for k := 1 to size(state_pos^) do
              with pos_table^[state_pos^[k]] do
                if (pos_type=mark_pos) and (pos=0) then
                  write(yylst, ' ', rule:5);
{$endif}
            writeln(yylst);
            for k := trans_lo to trans_hi do
              with trans_table^[k] do
                begin
                  listTrans(cc, next_state);
                  writeln(yylst);
                end;
          end;
      end;
  end(*listDFATable*);

end(*LexList*).
