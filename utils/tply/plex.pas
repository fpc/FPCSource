{
  TP Lex - A lexical analyzer generator for Turbo Pascal


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


$Revision: 1.4 $
$Modtime: 96-08-01 10:22 $

$History: LEX.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.


------------------------- Synopsis ------------------------

   Synopsis   lex [options] lex-file[.l] [output-file[.pas]]

   Options

   -v  "Verbose:" Lex generates a readable description of the generated
       lexical analyzer, written to lex-file with new extension .LST.

   -o  "Optimize:" Lex optimizes DFA tables to produce a minimal DFA

   Description

   This is a reimplementation of the popular UNIX lexical analyzer generator
   Lex for MS-DOS and Turbo Pascal.

   Differences from UNIX Lex:

   - Produces output code for Turbo Pascal, rather than for C.

   - Character tables (%T) are not supported; neither are any directives
     to determine internal table sizes (%p, %n, etc.).

------------------------- Synopsis ------------------------

}

{$I-}
program Lex;

uses
  LexBase, LexTable, LexPos, LexDFA, LexOpt, LexList, LexRules, LexMsgs, SysUtils;


procedure get_line;
  (* obtain line from source file *)
  begin
    readln(yyin, line);
    inc(lno);
  end(*get_line*);

procedure next_section;
  (* find next section mark (%%) in code template *)
  var line : String;
  begin
    while not eof(yycod) do
      begin
        readln(yycod, line);
        if line='%%' then exit;
        writeln(yyout, line);
      end;
  end(*next_section*);

(* Semantic routines: *)

var n_rules : Integer; (* current number of rules *)

procedure define_start_state ( symbol : String; pos : Integer );
  (* process start state definition *)
  begin
{$ifdef fpc}
    with sym_table^[key(symbol, max_keys, @lookup, @entry)] do
{$else}
    with sym_table^[key(symbol, max_keys, lookup, entry)] do
{$endif}
      if sym_type=none then
        begin
          inc(n_start_states);
          if n_start_states>max_start_states then
            fatal(state_table_overflow);
          sym_type    := start_state_sym;
          start_state := n_start_states;
          writeln(yyout, 'const ', symbol, ' = ', 2*start_state, ';');
          first_pos_table^[2*start_state] := newIntSet;
          first_pos_table^[2*start_state+1] := newIntSet;
        end
      else
        error(symbol_already_defined, pos)
  end(*define_start_state*);

procedure define_macro ( symbol, replacement : String );
  (* process macro definition *)
  begin
{$ifdef fpc}
    with sym_table^[key('{'+symbol+'}', max_keys, @lookup, @entry)] do
{$else}
    with sym_table^[key('{'+symbol+'}', max_keys, lookup, entry)] do
{$endif}
      if sym_type=none then
        begin
          sym_type := macro_sym;
          subst    := newStr(strip(replacement));
        end
      else
        error(symbol_already_defined, 1)
  end(*define_macro*);

procedure add_rule;
  (* process rule *)
  var i : Integer;
      FIRST : IntSet;
  begin
    addExpr(r, FIRST);
    if n_st=0 then
      if cf then
        setunion(first_pos_table^[1]^, FIRST)
      else
        begin
          setunion(first_pos_table^[0]^, FIRST);
          setunion(first_pos_table^[1]^, FIRST);
        end
    else
      if cf then
        for i := 1 to n_st do
          setunion(first_pos_table^[2*st[i]+1]^, FIRST)
      else
        for i := 1 to n_st do
          begin
            setunion(first_pos_table^[2*st[i]]^, FIRST);
            setunion(first_pos_table^[2*st[i]+1]^, FIRST);
          end
  end(*add_rule*);

procedure generate_table;

  (* write the DFA table to the output file

     Tables are represented as a collection of typed array constants:

     type YYTRec = record
                     cc : set of Char; { characters }
                     s  : Integer;     { next state }
                   end;

     const

     { table sizes: }

     yynmarks   = ...;
     yynmatches = ...;
     yyntrans   = ...;
     yynstates  = ...;

     { rules of mark positions for each state: }

     yyk : array [1..yynmarks] of Integer = ...;

     { rules of matches for each state: }

     yym : array [1..yynmatches] of Integer = ...;

     { transition table: }

     yyt : array [1..yyntrans] of YYTRec = ...;

     { offsets into the marks, matches and transition tables: }

     yykl, yykh,
     yyml, yymh,
     yytl, yyth : array [0..yynstates-1] of Integer = ...;

  *)

  var yynmarks, yynmatches, yyntrans, yynstates : Integer;
      yykl, yykh,
      yyml, yymh,
      yytl, yyth : array [0..max_states-1] of Integer;

  procedure counters;
    (* compute counters and offsets *)
    var s, i : Integer;
    begin
      yynstates := n_states; yyntrans   := n_trans;
      yynmarks  := 0;        yynmatches := 0;
      for s := 0 to n_states-1 do with state_table^[s] do
        begin
          yytl[s] := trans_lo;   yyth[s] := trans_hi;
          yykl[s] := yynmarks+1; yyml[s] := yynmatches+1;
          for i := 1 to size(state_pos^) do
            with pos_table^[state_pos^[i]] do
              if pos_type=mark_pos then
                if pos=0 then
                  inc(yynmatches)
                else if pos=1 then
                  inc(yynmarks);
          yykh[s] := yynmarks; yymh[s] := yynmatches;
        end;
    end(*counters*);

  procedure writecc(var f : Text; cc : CClass);
    (* print the given character class *)
    function charStr(c : Char) : String;
      begin
        case c of
          #0..#31,     (* nonprintable characters *)
          #127..#255 : charStr := '#'+intStr(ord(c));
          ''''       : charStr := '''''''''';
          else         charStr := ''''+c+'''';
        end;
      end(*charStr*);
    const
      MaxChar = #255;
    var
      c1, c2 : Char;
      col : Integer;
      tag : String;
      Quit: Boolean;
    begin
      write(f, '[ ');
      col := 0;
      c1 := chr(0);
      Quit := False;
      while not Quit do begin
        if c1 in cc then  begin
          if col>0 then
            begin
              write(f, ',');
              inc(col);
            end;
          if col>40 then
            { insert line break }
            begin
              writeln(f);
              write(f, ' ':12);
              col := 0;
            end;
          c2 := c1;
          while (c2<MaxChar) and (succ(c2) in cc) do
            c2 := succ(c2);
          if c1=c2 then
            tag := charStr(c1)
          else if c2=succ(c1) then
            tag := charStr(c1)+','+charStr(c2)
          else
            tag := charStr(c1)+'..'+charStr(c2);
          write(f, tag);
          col := col + length(tag);
          c1 := c2;
        end;
        Quit := c1 = MaxChar;
        if not Quit then
          c1 := Succ(c1);
      end; { of while }
      write(f, ' ]');
    end(*writecc*);

  procedure tables;
    (* print tables *)
    var s, i, count : Integer;
    begin
      writeln(yyout);
      writeln(yyout, 'type YYTRec = record');
      writeln(yyout, '                cc : set of Char;');
      writeln(yyout, '                s  : Integer;');
      writeln(yyout, '              end;');
      writeln(yyout);
      writeln(yyout, 'const');
      (* table sizes: *)
      writeln(yyout);
      writeln(yyout, 'yynmarks   = ', yynmarks, ';');
      writeln(yyout, 'yynmatches = ', yynmatches, ';');
      writeln(yyout, 'yyntrans   = ', yyntrans, ';');
      writeln(yyout, 'yynstates  = ', yynstates, ';');
      (* mark table: *)
      writeln(yyout);
      writeln(yyout, 'yyk : array [1..yynmarks] of Integer = (');
      count := 0;
      for s := 0 to n_states-1 do with state_table^[s] do
        begin
          writeln(yyout, '  { ', s, ': }');
          for i := 1 to size(state_pos^) do
            with pos_table^[state_pos^[i]] do
              if (pos_type=mark_pos) and (pos=1) then
                begin
                  write(yyout, '  ', rule); inc(count);
                  if count<yynmarks then write(yyout, ',');
                  writeln(yyout);
                end;
        end;
      writeln(yyout, ');');
      (* match table: *)
      writeln(yyout);
      writeln(yyout, 'yym : array [1..yynmatches] of Integer = (');
      count := 0;
      for s := 0 to n_states-1 do with state_table^[s] do
        begin
          writeln(yyout, '{ ', s, ': }');
          for i := 1 to size(state_pos^) do
            with pos_table^[state_pos^[i]] do
              if (pos_type=mark_pos) and (pos=0) then
                begin
                  write(yyout, '  ', rule); inc(count);
                  if count<yynmatches then write(yyout, ',');
                  writeln(yyout);
                end;
        end;
      writeln(yyout, ');');
      (* transition table: *)
      writeln(yyout);
      writeln(yyout, 'yyt : array [1..yyntrans] of YYTrec = (');
      count := 0;
      for s := 0 to n_states-1 do with state_table^[s] do
        begin
          writeln(yyout, '{ ', s, ': }');
          for i := trans_lo to trans_hi do
            with trans_table^[i] do
              begin
                write(yyout, '  ( cc: ');
                writecc(yyout, cc^);
                write(yyout, '; s: ');
                write(yyout, next_state, ')');
                inc(count);
                if count<yyntrans then write(yyout, ',');
                writeln(yyout);
              end;
        end;
      writeln(yyout, ');');
      (* offset tables: *)
      writeln(yyout);
      writeln(yyout, 'yykl : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yykl[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
      writeln(yyout, 'yykh : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yykh[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
      writeln(yyout, 'yyml : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yyml[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
      writeln(yyout, 'yymh : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yymh[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
      writeln(yyout, 'yytl : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yytl[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
      writeln(yyout, 'yyth : array [0..yynstates-1] of Integer = (');
      for s := 0 to n_states-1 do
        begin
          write(yyout, '{ ', s, ': } ', yyth[s]);
          if s<n_states-1 then write(yyout, ',');
          writeln(yyout);
        end;
      writeln(yyout, ');');
      writeln(yyout);
    end(*tables*);

  begin
    counters; tables;
  end(*generate_table*);

(* Parser: *)

const

max_items = 255;

var

itemstr : String;
itemc   : Integer;
itempos,
itemlen : array [1..max_items] of Integer;

procedure split ( str : String; count : Integer );
  (* split str into at most count whitespace-delimited items
     (result in itemstr, itemc, itempos, itemlen) *)
  procedure scan(var act_pos : Integer);
    (* scan one item *)
    var l : Integer;
    begin
      while (act_pos<=length(itemstr)) and
            ((itemstr[act_pos]=' ') or (itemstr[act_pos]=tab)) do
        inc(act_pos);
      l := 0;
      while (act_pos+l<=length(itemstr)) and
            (itemstr[act_pos+l]<>' ') and (itemstr[act_pos+l]<>tab) do
        inc(l);
      inc(itemc);
      itempos[itemc] := act_pos;
      itemlen[itemc] := l;
      inc(act_pos, l+1);
      while (act_pos<=length(itemstr)) and
            ((itemstr[act_pos]=' ') or (itemstr[act_pos]=tab)) do
        inc(act_pos);
    end(*scan*);
  var act_pos : Integer;
  begin
    itemstr := str; act_pos := 1;
    itemc := 0;
    while (itemc<count-1) and (act_pos<=length(itemstr)) do scan(act_pos);
    if act_pos<=length(itemstr) then
      begin
        inc(itemc);
        itempos[itemc] := act_pos;
        itemlen[itemc] := length(itemstr)-act_pos+1;
      end;
  end(*split*);

function itemv ( i : Integer ) : String;
  (* return ith item in splitted string (whole string for i=0) *)
  begin
    if i=0 then
      itemv := itemstr
    else if (i<0) or (i>itemc) then
      itemv := ''
    else
      itemv := copy(itemstr, itempos[i], itemlen[i])
  end(*itemv*);

procedure code;
  begin
    while not eof(yyin) do
      begin
        get_line;
        if line='%}' then
          exit
        else
          writeln(yyout, line);
      end;
    error(unmatched_lbrace, length(line)+1);
  end(*code*);

procedure definitions;
  procedure definition;
    function check_id ( symbol : String ) : Boolean;
      var i : Integer;
      begin
        if (symbol='') or not (symbol[1] in letters) then
          check_id := false
        else
          begin
            for i := 2 to length(symbol) do
              if not (symbol[i] in alphanums) then
                begin
                  check_id := false;
                  exit;
                end;
            check_id := true
          end
      end(*check_id*);
    var i : Integer;
        com : String;
    begin
      split(line, 2);
      com := upper(itemv(1));
      if (com='%S') or (com='%START') then
        begin
          split(line, max_items);
          for i := 2 to itemc do
            if check_id(itemv(i)) then
              define_start_state(itemv(i), itempos[i])
            else
              error(syntax_error, itempos[i]);
        end
      else if check_id(itemv(1)) then
        define_macro(itemv(1), itemv(2))
      else
        error(syntax_error, 1);
    end(*definition*);
  begin
    while not eof(yyin) do
      begin
        get_line;
        if line='' then
          writeln(yyout)
        else if line='%%' then
          exit
        else if line='%{' then
          code
        else if (line[1]='%') or (line[1] in letters) then
          definition
        else
          writeln(yyout, line)
      end;
  end(*definitions*);

procedure rules;
  begin
    next_section;
    if line='%%' then
      while not eof(yyin) do
        begin
          get_line;
          if line='' then
            writeln(yyout)
          else if line='%%' then
            begin
              next_section;
              exit;
            end
          else if line='%{' then
            code
          else if (line[1]<>' ') and (line[1]<>tab) then
            begin
              if n_rules=0 then next_section;
              inc(n_rules);
              parse_rule(n_rules);
              if errors=0 then
                begin
                  add_rule;
                  write(yyout, '  ', n_rules);
                  if strip(stmt)='|' then
                    writeln(yyout, ',')
                  else
                    begin
                      writeln(yyout, ':');
                      writeln(yyout, blankStr(expr), stmt);
                    end;
                end
            end
          else
            writeln(yyout, line)
        end
    else
      error(unexpected_eof, length(line)+1);
    next_section;
  end(*rules*);

procedure auxiliary_procs;
  begin
    if line='%%' then
      begin
        writeln(yyout);
        while not eof(yyin) do
          begin
            get_line;
            writeln(yyout, line);
          end;
      end;
  end(*auxiliary_procs*);

(* Main program: *)

var i : Integer;

begin
{$ifdef Unix}
  codfilepath1:='/usr/local/lib/fpc/lexyacc/';
  codfilepath2:='/usr/lib/fpc/lexyacc/';
{$else}
  codfilepath1:=path(paramstr(0));
  codfilepath2:='';
{$endif}


  (* sign-on: *)

  writeln(sign_on);

  (* parse command line: *)

  if paramCount=0 then
    begin
      writeln(usage);
      writeln(options);
      halt(0);
    end;

  lfilename := '';
  pasfilename := '';

  for i := 1 to paramCount do
    if copy(paramStr(i), 1, 1)='-' then
      if upper(paramStr(i))='-V' then
        verbose := true
      else if upper(paramStr(i))='-O' then
        optimize := true
      else
        begin
          writeln(invalid_option, paramStr(i));
          halt(1);
        end
    else if lfilename='' then
      lfilename := addExt(paramStr(i), 'l')
    else if pasfilename='' then
      pasfilename := addExt(paramStr(i), 'pas')
    else
      begin
        writeln(illegal_no_args);
        halt(1);
      end;

  if lfilename='' then
    begin
      writeln(illegal_no_args);
      halt(1);
    end;

  if pasfilename='' then pasfilename := root(lfilename)+'.pas';
  lstfilename := root(lfilename)+'.lst';

  (* open files: *)

  assign(yyin, lfilename);
  assign(yyout, pasfilename);
  assign(yylst, lstfilename);

  reset(yyin);    if ioresult<>0 then fatal(cannot_open_file+lfilename);
  rewrite(yyout); if ioresult<>0 then fatal(cannot_open_file+pasfilename);
  rewrite(yylst); if ioresult<>0 then fatal(cannot_open_file+lstfilename);

  (* search code template *)
  codfilename := 'yylex.cod';
  assign(yycod, codfilename);
  reset(yycod);
  if ioresult<>0 then
    begin
      codfilename := IncludeTrailingPathDelimiter(GetEnvironmentVariable('FPCDIR'))+'lexyacc'+DirectorySeparator+'yylex.cod';
      assign(yycod, codfilename);
      reset(yycod);
      if ioresult<>0 then
        begin
          codfilename := codfilepath1+'yylex.cod';
          assign(yycod, codfilename);
          reset(yycod);
          if (codfilepath2<>'') and (ioresult<>0) then 
            begin
              codfilename := codfilepath2+'yylex.cod';
              assign(yycod, codfilename);
              reset(yycod);
              if ioresult<>0 then 
                fatal(cannot_open_file+codfilename);
            end;
        end;
    end;

  (* parse source grammar: *)

  write('parse ... ');
  lno := 0; n_rules := 0; next_section;
  first_pos_table^[0] := newIntSet;
  first_pos_table^[1] := newIntSet;
  definitions;
  rules;
  if n_rules=0 then error(empty_grammar, length(line)+1);
  if errors=0 then
    begin
      (* generate DFA table and listings and write output code: *)
      write('DFA construction ... ');
      makeDFATable;
      if optimize then
        begin
          write('DFA optimization ... ');
          optimizeDFATable;
        end;
      write('code generation ... ');
      if verbose then listDFATable;
      generate_table; next_section;
    end;
  auxiliary_procs;
  if errors=0 then writeln('DONE');

  (* close files: *)

  close(yyin); close(yyout); close(yylst); close(yycod);

  (* print statistics: *)

  if errors>0 then
    writeln( lno, ' lines, ',
             errors, ' errors found.' )
  else
    writeln( lno, ' lines, ',
             n_rules, ' rules, ',
             n_pos, '/', max_pos, ' p, ',
             n_states, '/', max_states, ' s, ',
             n_trans, '/', max_trans, ' t.');

  if warnings>0 then writeln(warnings, ' warnings.');

  (* terminate: *)

  if errors>0 then erase(yyout);
  if file_size(lstfilename)=0 then
    erase(yylst)
  else
    writeln('(see ', lstfilename, ' for more information)');

  halt(errors);

end(*Lex*).
