{
  Semantic routines for the Yacc parser.


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
$Modtime: 96-08-01 6:03 $

$History: YACCSEM.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccSem;

interface


var

act_prec : Integer;
  (* active precedence level in token and precedence declarations (0 in
     %token declaration) *)
act_type : Integer;
  (* active type tag in token, precedence and type declarations *)

procedure yyerror ( msg : String );
  (* YaccLib.yyerror redefined to ignore 'syntax error' message; the parser
     does its own error handling *)

function sym ( k : Integer ) : Integer;
  (* returns internal symbol number for the symbol k; if k is yet undefined,
     a new nonterminal or literal symbol is created, according to the
     appearance of symbol k (nonterminal if an ordinary identifier, literal
     otherwise) *)

function ntsym ( k : Integer ) : Integer;
  (* like sym, but requires symbol k to be a nonterminal symbol; if it
     is already defined a literal, an error message is issued, and a dummy
     nonterminal symbol returned *)

function litsym ( k : Integer; n : Integer ) : Integer;
  (* same for literal symbols; if n>0 it denotes the literal number to be
     assigned to the symbol; when a new literal identifier is defined, a
     corresponding constant definition is also written to the definition
     file *)

procedure next_section;
  (* find next section mark (%%) in code template *)

procedure definitions;
  (* if necessary, write out definition of the semantic value type YYSType *)

procedure copy_code;
  (* copy Turbo Pascal code section ( %{ ... %} ) to output file *)

procedure copy_action;
  (* copy an action to the output file *)

procedure copy_single_action;
  (* like copy_action, but action must be single statement terminated
     with `;' *)

procedure copy_rest_of_file;
  (* copies the rest of the source file to the output file *)

procedure start_rule ( sym : Integer );
  (* start a new rule with lhs nonterminal symbol sym *)

procedure start_body;
  (* start a new rule body (rhs) *)

procedure end_body;
  (* end a rule body *)

procedure add_symbol ( sym : Integer );
  (* add the denoted symbol to the current rule body *)

procedure add_action;
  (* add an action to the current rule body *)

procedure add_rule_prec ( sym : Integer );
  (* add the precedence of terminal symbol sym to the current rule *)

procedure generate_parser;
  (* generate the parse table *)

implementation

uses YaccBase, YaccTabl, YaccClos, YaccLR0, YaccLook,
  YaccPars, YaccMsgs;

procedure yyerror ( msg : String );
  begin
    if msg='syntax error' then
      (* ignore *)
    else
      fatal(msg)
  end(*yyerror*);

function act_char : char;
  begin
    if cno>length(line) then
      if eof(yyin) then
        act_char := #0
      else
        act_char := nl
    else
      act_char := line[cno]
  end(*act_char*);

function lookahead_char : char;
  begin
    if succ(cno)>length(line) then
      if eof(yyin) then
        lookahead_char := #0
      else
        lookahead_char := nl
    else
      lookahead_char := line[succ(cno)]
  end(*lookahead_char*);

procedure next_char;
  begin
    if cno>length(line) then
      if eof(yyin) then
        { nop }
      else
        begin
          readln(yyin, line);
          inc(lno); cno := 1
        end
    else
      inc(cno)
  end(*next_char*);

var

(* Current rule: *)

act_rule      : RuleRec;

(* Actions: *)

n_act : Integer;
p_act : Boolean;

function sym ( k : Integer ) : Integer;
  var s : Integer;
  begin
    if is_def_key(k, s) then
      sym := s
    else if sym_table^[k].pname^[1]='''' then
      begin
        s := new_lit;
        def_key(k, s);
        sym := s;
      end
    else
      begin
        s := new_nt;
        def_key(k, s);
        sym := s;
      end
  end(*sym*);

function ntsym ( k : Integer ) : Integer;
  var s : Integer;
  begin
    if is_def_key(k, s) then
      if s<0 then
        ntsym := s
      else
        begin
          error(nonterm_expected);
          ntsym := -1;
        end
    else if sym_table^[k].pname^[1]='''' then
      begin
        error(nonterm_expected);
        ntsym := -1;
      end
    else
      begin
        s := new_nt;
        def_key(k, s);
        ntsym := s;
      end
  end(*ntsym*);

function litsym ( k : Integer; n : Integer ) : Integer;
  var s : Integer;
  begin
    if is_def_key(k, s) then
      if s>=0 then
        begin
          if n>0 then error(double_tokennum_def);
          litsym := s;
        end
      else
        begin
          error(literal_expected);
          litsym := 1;
        end
    else if sym_table^[k].pname^[1]='''' then
      begin
        if n>0 then
          begin
            add_lit(n);
            s := n;
          end
        else
          s := new_lit;
        def_key(k, s);
        litsym := s;
      end
    else
      begin
        if n>0 then
          begin
            add_lit(n);
            s := n;
          end
        else
          s := new_lit;
        def_key(k, s);
        writeln(yyout, 'const ', pname(s), ' = ', s, ';');
        litsym := s;
      end;
  end(*litsym*);

procedure next_section;
  var line : String;
  begin
    while not eof(yycod) do
      begin
        readln(yycod, line);
        if line='%%' then exit;
        writeln(yyout, line);
      end;
  end(*next_section*);

procedure definitions;
  var i : Integer;
  begin
    if n_types>0 then
      begin
        writeln(yyout);
        writeln(yyout, 'type YYSType = record case Integer of');
        for i := 1 to n_types do
          writeln(yyout, ' ':15, i:3, ' : ( ',
                         'yy', sym_table^[type_table^[i]].pname^, ' : ',
                         sym_table^[type_table^[i]].pname^, ' );');
        writeln(yyout, ' ':15, 'end(*YYSType*);');
      end;
  end(*definitions*);

procedure copy_code;
  var str_state : Boolean;
  begin
    str_state := false;
    while act_char<>#0 do
      if act_char=nl then
        begin
          writeln(yyout);
          next_char;
        end
      else if act_char='''' then
        begin
          write(yyout, '''');
          str_state := not str_state;
          next_char;
        end
      else if not str_state and (act_char='%') and (lookahead_char='}') then
        exit
      else
        begin
          write(yyout, act_char);
          next_char;
        end;
  end(*copy_code*);

procedure scan_val;
  (* process a $ value in an action
     (not very pretty, but it does its job) *)
  var tag, numstr : String; i, code : Integer;
  begin
    tokleng := 0;
    next_char;
    if act_char='<' then
      begin
        (* process type tag: *)
        next_char;
        tag := '';
        while (act_char<>nl) and (act_char<>#0) and (act_char<>'>') do
          begin
            tag := tag+act_char;
            next_char;
          end;
        if act_char='>' then
          begin
            if not search_type(tag) then
              begin
                tokleng := length(tag);
                error(unknown_identifier);
              end;
            next_char;
          end
        else
          error(syntax_error);
      end
    else
      tag := '';
    tokleng := 0;
    if act_char='$' then
      begin
        (* left-hand side value: *)
        write(yyout, 'yyval');
        (* check for value type: *)
        if (tag='') and (n_types>0) then with act_rule do
          if sym_type^[lhs_sym]>0 then
            tag := sym_table^[sym_type^[lhs_sym]].pname^
          else
            begin
              tokleng := 1;
              error(type_error);
            end;
        if tag<>'' then write(yyout, '.yy', tag);
        next_char;
      end
    else
      begin
        (* right-hand side value: *)
        if act_char='-' then
          begin
            numstr := '-';
            next_char;
          end
        else
          numstr := '';
        while ('0'<=act_char) and (act_char<='9') do
          begin
            numstr := numstr+act_char;
            next_char;
          end;
        if numstr<>'' then
          begin
            val(numstr, i, code);
            if code=0 then
              if i<=act_rule.rhs_len then
                begin
                  write(yyout, 'yyv[yysp-', act_rule.rhs_len-i, ']');
                  (* check for value type: *)
                  if (tag='') and (n_types>0) then with act_rule do
                    if i<=0 then
                      begin
                        tokleng := length(numstr)+1;
                        error(type_error);
                      end
                    else if sym_type^[rhs_sym[i]]>0 then
                      tag := sym_table^[sym_type^[rhs_sym[i]]].pname^
                    else
                      begin
                        tokleng := length(numstr)+1;
                        error(type_error);
                      end;
                  if tag<>'' then write(yyout, '.yy', tag);
                end
              else
                begin
                  tokleng := length(numstr);
                  error(range_error);
                end
            else
              error(syntax_error)
          end
        else
          error(syntax_error)
      end
  end(*scan_val*);

procedure copy_action;
  var str_state : Boolean;
  begin
    str_state := false;
    while act_char=' ' do next_char;
    write(yyout, ' ':9);
    while act_char<>#0 do
      if act_char=nl then
        begin
          writeln(yyout);
          next_char;
          while act_char=' ' do next_char;
          write(yyout, ' ':9);
        end
      else if act_char='''' then
        begin
          write(yyout, '''');
          str_state := not str_state;
          next_char;
        end
      else if not str_state and (act_char='}') then
        begin
          writeln(yyout);
          exit;
        end
      else if not str_state and (act_char='$') then
        scan_val
      else
        begin
          write(yyout, act_char);
          next_char;
        end;
  end(*copy_action*);

procedure copy_single_action;
  var str_state : Boolean;
  begin
    str_state := false;
    while act_char=' ' do next_char;
    write(yyout, ' ':9);
    while act_char<>#0 do
      if act_char=nl then
        begin
          writeln(yyout);
          next_char;
          while act_char=' ' do next_char;
          write(yyout, ' ':9);
        end
      else if act_char='''' then
        begin
          write(yyout, '''');
          str_state := not str_state;
          next_char;
        end
      else if not str_state and (act_char=';') then
        begin
          writeln(yyout, ';');
          exit;
        end
      else if not str_state and (act_char='$') then
        scan_val
      else
        begin
          write(yyout, act_char);
          next_char;
        end;
  end(*copy_single_action*);

procedure copy_rest_of_file;
  begin
    while act_char<>#0 do
      if act_char=nl then
        begin
          writeln(yyout);
          next_char;
        end
      else
        begin
          write(yyout, act_char);
          next_char;
        end;
  end(*copy_rest_of_file*);

procedure start_rule ( sym : Integer );
  begin
    if n_rules=0 then
      begin
        (* fix start nonterminal of the grammar: *)
        if startnt=0 then startnt := sym;
        (* add augmented start production: *)
        with act_rule do
          begin
            lhs_sym := -1;
            rhs_len := 2;
            rhs_sym[1] := startnt;
            rhs_sym[2] := 0; (* end marker *)
          end;
        add_rule(newRuleRec(act_rule));
      end;
    act_rule.lhs_sym := sym;
  end(*start_rule*);

procedure start_body;
  begin
    act_rule.rhs_len := 0;
    p_act := false;
    writeln(yyout, n_rules:4, ' : begin');
  end(*start_body*);

procedure end_body;
  begin
    if not p_act and (act_rule.rhs_len>0) then
      (* add default action: *)
      writeln(yyout, ' ':9, 'yyval := yyv[yysp-',
                            act_rule.rhs_len-1, '];');
    add_rule(newRuleRec(act_rule));
    writeln(yyout, ' ':7, 'end;');
  end(*end_body*);

procedure add_rule_action;
  (* process an action inside a rule *)
  var k : Integer; r : RuleRec;
  begin
    writeln(yyout, ' ':7, 'end;');
    inc(n_act);
    k := get_key('$$'+intStr(n_act));
    with r do
      begin
        lhs_sym := new_nt;
        def_key(k, lhs_sym);
        rhs_len := 0;
      end;
    with act_rule do
      begin
        inc(rhs_len);
        if rhs_len>max_rule_len then fatal(rule_table_overflow);
        rhs_sym[rhs_len] := r.lhs_sym;
      end;
    add_rule(newRuleRec(r));
    rule_prec^[n_rules+1] := rule_prec^[n_rules];
    rule_prec^[n_rules] := 0;
    writeln(yyout, n_rules:4, ' : begin');
  end(*add_rule_action*);

procedure add_symbol ( sym : Integer );
  begin
    if p_act then add_rule_action;
    p_act := false;
    with act_rule do
      begin
        inc(rhs_len);
        if rhs_len>max_rule_len then fatal(rule_table_overflow);
        rhs_sym[rhs_len] := sym;
        if sym>=0 then rule_prec^[n_rules+1] := sym_prec^[sym]
      end
  end(*add_symbol*);

procedure add_action;
  begin
    if p_act then add_rule_action;
    p_act := true;
  end(*add_action*);

procedure add_rule_prec ( sym : Integer );
  begin
    rule_prec^[n_rules+1] := sym_prec^[sym];
  end(*add_rule_prec*);

procedure generate_parser;
  begin
    if startnt=0 then error(empty_grammar);
    if errors=0 then
      begin
        write('sort ... ');
        sort_rules; rule_offsets;
        write('closures ... ');
        closures;
        write('first sets ... ');
        first_sets;
        write('LR0 set ... ');
        LR0Set;
        write('lookaheads ... ');
        lookaheads;
        writeln;
        write('code generation ... ');
        parse_table;
      end;
  end(*generate_parser*);

begin
  n_act := 0;
end(*YaccSem*).
