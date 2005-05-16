{
  Parser for Lex grammar rules.

  This module implements a parser for Lex grammar rules. It should
  probably be reimplemented using Lex and Yacc, but the irregular
  lexical structure of the Lex language makes that rather tedious,
  so I decided to use a conventional recursive-descent-parser
  instead.


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
$Modtime: 96-08-01 6:30 $

$History: LEXRULES.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit LexRules;

interface

uses LexBase, LexTable;


procedure parse_rule ( rule_no : Integer );
  (* rule parser (rule_no=number of parsed rule) *)

(* Return values of rule parser: *)

var

expr, stmt : String;
  (* expression and statement part of rule *)
cf   : Boolean;
  (* caret flag *)
n_st : Integer;
  (* number of start states in prefix *)
st   : array [1..max_states] of Integer;
  (* start states *)
r    : RegExpr;
  (* parsed expression *)

implementation

uses LexMsgs;

(* Scanner routines:

   The following routines provide access to the source line and handle
   macro substitutions. To perform macro substitution, an input buffer
   is maintained which contains the rest of the line to be parsed, plus
   any pending macro substitutions. The input buffer is organized as
   a stack onto which null-terminated replacement strings are pushed
   as macro substitutions are processed (the terminating null-character
   is used as an endmarker for macros, in order to keep track of the
   number of pending macro substitutions); characters are popped from the
   stack via calls to the get_char routine.

   In order to perform macro substitution, the scanner also has to
   maintain some state information to be able to determine when it
   is scanning quoted characters, strings or character classes (s.t.
   no macro substitution is performed in such cases).

   The scanner also keeps track of the current source line position in
   variable act_pos; if there are any macro substitutions on the stack,
   act_pos will point to the position of the original macro call in the
   source line. This is needed to give proper error diagnostics. *)

const max_chars = 2048;

var

act_pos, bufptr : Integer;
  (* current position in source line and input stack pointer *)
buf : array [1..max_chars] of Char;
  (* input buffer *)
str_state, cclass_state, quote_state : Boolean;
  (* state information *)
n_macros : Integer;
  (* number of macros currently on stack *)

procedure mark_error ( msg : String; offset : Integer );
  (* mark error position (offset=offset of error position (to the left of
     act_pos) *)
  begin
    if n_macros=0 then
      error(msg, act_pos-offset)
    else
      error(msg+' in regular definition', act_pos)
  end(*mark_error*);

procedure put_str(str : String);
  (* push str onto input stack *)
  var i : Integer;
  begin
    inc(bufptr, length(str));
    if bufptr>max_chars then fatal(macro_stack_overflow);
    for i := 1 to length(str) do
      buf[bufptr-i+1] := str[i];
  end(*put_str*);

procedure init_scanner;
  (* initialize the scanner *)
  begin
    act_pos := 1; bufptr := 0;
    str_state := false; cclass_state := false; quote_state := false;
    n_macros := 0;
    put_str(line);
  end(*init_scanner*);

function act_char : Char;
  (* current character (#0 if none) *)
  function push_macro : Boolean;
    (* check for macro call at current position in input buffer *)
    function scan_macro ( var name : String ) : Boolean;
      var i : Integer;
      begin
        if (bufptr>1) and
           (buf[bufptr]='{') and (buf[bufptr-1] in letters) then
          begin
            name := '{'; i := bufptr-1;
            while (i>0) and (buf[i] in alphanums) do
              begin
                name := name+buf[i];
                dec(i);
              end;
            if (i>0) and (buf[i]='}') then
              begin
                scan_macro := true;
                name := name+'}';
                bufptr := i-1;
              end
            else
              begin
                scan_macro := false;
                mark_error(syntax_error, -length(name));
                bufptr := i;
              end
          end
        else
          scan_macro := false
      end(*scan_macro*);
    var name : String;
    begin
      if scan_macro(name) then
        begin
          push_macro := true;
{$ifdef fpc}
          with sym_table^[key(name, max_keys, @lookup, @entry)] do
{$else}
          with sym_table^[key(name, max_keys, lookup, entry)] do
{$endif}
            if sym_type=macro_sym then
              begin
                put_str(subst^+#0);
                inc(n_macros);
              end
            else
              mark_error(undefined_symbol, -1)
        end
      else
        push_macro := false
    end(*push_macro*);
  function pop_macro : Boolean;
    (* check for macro endmarker *)
    begin
      if (bufptr>0) and (buf[bufptr]=#0) then
        begin
          dec(bufptr);
          dec(n_macros);
          if n_macros=0 then act_pos := length(line)-bufptr+1;
          pop_macro := true;
        end
      else
        pop_macro := false
    end(*pop_macro*);
  begin
    if not (str_state or cclass_state or quote_state) then
      while push_macro do while pop_macro do ;
    if bufptr=0 then
      act_char := #0
    else
      begin
        while pop_macro do ;
        if (bufptr>0) then
          act_char := buf[bufptr]
        else
          act_char:=#0;
      end
  end(*act_char*);

procedure get_char;
  (* get next character *)
  begin
    if bufptr>0 then
      begin
        case buf[bufptr] of
          '\' : quote_state := not quote_state;
          '"' : if quote_state then
                  quote_state := false
                else if not cclass_state then
                  str_state := not str_state;
          '[' : if quote_state then
                  quote_state := false
                else if not str_state then
                  cclass_state := true;
          ']' : if quote_state then
                  quote_state := false
                else if not str_state then
                  cclass_state := false;
          else  quote_state := false;
        end;
        dec(bufptr);
        if n_macros=0 then
          act_pos := length(line)-bufptr+1;
      end
  end(*get_char*);

(* Semantic routines: *)

procedure add_start_state ( symbol : String );
  (* add start state to st array *)
  begin
{$ifdef fpc}
    with sym_table^[key(symbol, max_keys, @lookup, @entry)] do
{$else}
    with sym_table^[key(symbol, max_keys, lookup, entry)] do
{$endif}
      if sym_type=start_state_sym then
        begin
          if n_st>=max_start_states then exit; { this shouldn't happen }
          inc(n_st);
          st[n_st] := start_state;
        end
      else
        mark_error(undefined_symbol, length(symbol))
  end(*add_start_state*);

(* Parser: *)

procedure parse_rule ( rule_no : Integer );

  procedure rule ( var done : Boolean );

    (* parse rule according to syntax:

       rule                     : start_state_prefix caret
                                  expr [ '$' | '/' expr ]
                                ;

       start_state_prefix       : /* empty */
                                | '<' start_state_list '>'
                                ;

       start_state_list         : ident { ',' ident }
                                ;

       caret                    : /* empty */
                                | '^'
                                ;

       expr                     : term { '|' term }
                                ;

       term                     : factor { factor }
                                ;

       factor                   : char
                                | string
                                | cclass
                                | '.'
                                | '(' expr ')'
                                | factor '*'
                                | factor '+'
                                | factor '?'
                                | factor '{' num [ ',' num ] '}'
                                ;
    *)

    procedure start_state_prefix ( var done : Boolean );
      procedure start_state_list ( var done : Boolean );
        procedure ident ( var done : Boolean );
          var idstr : String;
          begin(*ident*)
            done := act_char in letters;   if not done then exit;
            idstr := act_char;
            get_char;
            while act_char in alphanums do
              begin
                idstr := idstr+act_char;
                get_char;
              end;
            add_start_state(idstr);
          end(*ident*);
        begin(*start_state_list*)
          ident(done);                     if not done then exit;
          while act_char=',' do
            begin
              get_char;
              ident(done);                 if not done then exit;
            end;
        end(*start_state_list*);
      begin(*start_state_prefix*)
        n_st := 0;
        if act_char='<' then
          begin
            get_char;
            start_state_list(done);        if not done then exit;
            if act_char='>' then
              begin
                done := true;
                get_char;
              end
            else
              done := false
          end
        else
          done := true
      end(*start_state_prefix*);
    procedure caret( var done : Boolean );
      begin(*caret*)
        done := true;
        cf   := act_char='^';
        if act_char='^' then get_char;
      end(*caret*);

  procedure scan_char ( var done : Boolean; var c : Char );
    var
      oct_val : Byte;
      count : Integer;
    begin
      done := true;
      if act_char='\' then
        begin
          get_char;
          case act_char of
            #0  : done := false;
            'n' : begin
                    c := nl;
                    get_char
                  end;
            'r' : begin
                    c := cr;
                    get_char
                  end;
            't' : begin
                    c := tab;
                    get_char
                  end;
            'b' : begin
                    c := bs;
                    get_char
                  end;
            'f' : begin
                    c := ff;
                    get_char
                  end;
            '0'..'7' : begin
                         oct_val := ord(act_char)-ord('0');
                         get_char;
                         count := 1;
                         while ('0'<=act_char) and
                           (act_char<='7') and
                           (count<3) do
                           begin
                             inc(count);
                             oct_val := oct_val*8+ord(act_char)-ord('0');
                             get_char
                           end;
                         c := chr(oct_val);
                       end
            else  begin
                    c := act_char;
                    get_char
                  end
          end
        end
      else
        begin
          c := act_char;
          get_char
        end
    end(*scan_char*);
  procedure scan_str ( var done : Boolean; var str : String );
    var c : Char;
    begin
      str := '';
      get_char;
      while (act_char<>#0) and (act_char<>'"') do
        begin
          scan_char(done, c);        if not done then exit;
          str := str+c;
        end;
      if act_char=#0 then
        done := false
      else
        begin
          get_char;
          done := true;
        end
    end(*scan_str*);
  procedure scan_cclass( var done : Boolean; var cc : CClass );
    (* scan a character class *)
    var
      caret : boolean;
      c, c1,cl : Char;
    begin
      cc := [];
      get_char;
      if act_char='^' then
        begin
          caret := true;
          get_char;
        end
      else
        caret := false;
      while (act_char<>#0) and (act_char<>']') do
        begin
          scan_char(done, c);              if not done then exit;
          if act_char='-' then
            begin
              get_char;
              if (act_char<>#0) and (act_char<>']') then
                begin
                  scan_char(done, c1);     if not done then exit;
                  for cl:=c to c1 do
                    cc:=cc+[cl];
                   {cc := cc+[c..c1];}
                end
              else
                cc := cc+[c,'-'];
            end
          else
            cc := cc+[c];
        end;
      if act_char=#0 then
        done := false
      else
        begin
          get_char;
          done := true;
        end;
      if caret then cc := [#1..#255]-cc;
    end(*scan_cclass*);
  procedure scan_num( var done : Boolean; var n : Integer );
    var str : String;
    begin
      if act_char in digits then
        begin
          str := act_char;
          get_char;
          while act_char in digits do
            begin
              str := str+act_char;
              get_char;
            end;
          done := isInt(str, n);
        end
      else
        done := false
    end(*scan_num*);

    procedure DoExpr ( var done : Boolean; var r : RegExpr );
      procedure term ( var done : Boolean; var r : RegExpr );
        procedure factor ( var done : Boolean; var r : RegExpr );
          var str  : String;
              cc   : CClass;
              c    : Char;
              n, m : Integer;
          begin(*factor*)
            case act_char of
              '"' : begin
                      scan_str(done, str);         if not done then exit;
                      r := strExpr(newStr(str));
                    end;
              '[' : begin
                      scan_cclass(done, cc);       if not done then exit;
                      r := cclassExpr(newCClass(cc));
                    end;
              '.' : begin
                      get_char;
                      r := cclassExpr(newCClass([#1..#255]-[nl]));
                      done := true;
                    end;
              '(' : begin
                      get_char;
                      DoExpr(done, r);               if not done then exit;
                      if act_char=')' then
                        begin
                          get_char;
                          done := true;
                        end
                      else
                        done := false
                    end;
              else  begin
                      scan_char(done, c);          if not done then exit;
                      r := charExpr(c);
                    end;
            end;
            while done and (act_char in ['*','+','?','{']) do
              case act_char of
                '*' : begin
                        get_char;
                        r := starExpr(r);
                      end;
                '+' : begin
                        get_char;
                        r := plusExpr(r);
                      end;
                '?' : begin
                        get_char;
                        r := optExpr(r);
                      end;
                '{' : begin
                        get_char;
                        scan_num(done, m);         if not done then exit;
                        if act_char=',' then
                          begin
                            get_char;
                            scan_num(done, n);     if not done then exit;
                            r := mnExpr(r, m, n);
                          end
                        else
                          r := mnExpr(r, m, m);
                        if act_char='}' then
                          begin
                            get_char;
                            done := true;
                          end
                        else
                          done := false
                      end;
              end
          end(*factor*);
        const term_delim : CClass = [#0,' ',tab,'$','|',')','/'];
        var r1 : RegExpr;
        begin(*term*)
          if not (act_char in term_delim) then
            begin
              factor(done, r);             if not done then exit;
              while not (act_char in term_delim) do
                begin
                  factor(done, r1);        if not done then exit;
                  r := catExpr(r, r1);
                end
            end
          else
            begin
              r := epsExpr;
              done := true;
            end
        end(*term*);
      var r1 : RegExpr;
      begin(*expr*)
        term(done, r);                     if not done then exit;
        while act_char='|' do
          begin
            get_char;
            term(done, r1);                if not done then exit;
            r := altExpr(r, r1);
          end
      end(*expr*);

    var r1, r2 : RegExpr;

    begin(*rule*)
      start_state_prefix(done);            if not done then exit;
      caret(done);                         if not done then exit;
      DoExpr(done, r1);                      if not done then exit;
      if act_char='$' then
        begin
          r := catExpr(catExpr(r1,
                 markExpr(rule_no, 1)),
                 cclassExpr(newCClass([nl])));
          get_char;
        end
      else if act_char='/' then
        begin
          get_char;
          DoExpr(done, r2);                  if not done then exit;
          r := catExpr(catExpr(r1,
                 markExpr(rule_no, 1)), r2);
        end
      else
        r := catExpr(r1, markExpr(rule_no, 1));
      r := catExpr(r, markExpr(rule_no, 0));
      done := (act_char=#0) or (act_char=' ') or (act_char=tab);
    end(*rule*);

  var done : Boolean;

  begin(*parse_rule*)
    init_scanner;
    rule(done);
    if done then
      begin
        expr := copy(line, 1, act_pos-1);
        stmt := copy(line, act_pos, length(line));
      end
    else
      mark_error(syntax_error, 0)
  end(*parse_rule*);

end(*LexRules*).
