
/* YACC.Y: Yacc grammar for Yacc main program. 2-17-91, 4-30-91 AG
   To bootstrap Yacc, use Yacc iself to compile this grammar, then
   run tpc on the generated program.

   Note:

   This is not entirely the `official' syntax introduced by Johnson, but it
   should be compatible with UNIX Yacc (except for the differences specified
   in the program header, below), as described in the UNIX manual, including
   the language elements entitled as "old features supported but not
   encouraged."

   Bugs:

   - Processes $$'s, $i's, %} and } inside of comments in Turbo Pascal code
     (instead of ignoring them).

   Shift/reduce conflicts:

   This grammar will produce a number of shift/reduce conflicts caused by
   the error productions, since it does not specify unambigiously whether
   errors are to be handled in global structures (definitions and rules)
   or by enclosed syntactic constructs (e.g. symbols). Yacc will resolve
   these conflicts in favour of shift, which is o.k. (it means that
   errors will be caught in the innermost constructs with error handling,
   thus reducing the amount of skipped symbols in resynchronization).

   Error handling is done using the general method of Schreiner/Friedman
   (see Schreiner/Friedman, "Introduction to compiler construction with
   UNIX," 1985).

*/

%{
(*

  TP Yacc - Yet Another Compiler Compiler for Turbo Pascal

  Copyright (C) 1990-92  Albert Graef <ag@muwiinfa.geschichte.uni-mainz.de>
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
$Modtime: 96-08-01 11:24 $


Last changes:

  Version 3.0 as of April 91
  Version 3.0a as of May 92 (bug fixes in precedence and type information
    updates)

$History: YACC.PAS $
 * 
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.



------------------------- Synopsis ------------------------

   Synopsis   yacc [options] yacc-file[.y] [output-file[.pas]]

   Options

   -v  "Verbose:" Yacc generates a readable description of the generated
       parser, written to yacc-file with new extension .lst.

   -d  "Debug:" Yacc generates parser with debugging output.

   Description

   This is a reimplementation of the popular UNIX compiler generator
   Yacc for MS-DOS and Turbo Pascal.

   Differences from UNIX Yacc:

   - Produces output code for Turbo Pascal, rather than for C.

   - Does not support %union definitions. Instead, a value type is declared
     by specifying the type identifier *itself* as the tag of a %token
     or %type definition. Yacc will automatically generate an appropriate
     yylval variable of a variant record type (YYSType) which is capable of
     holding values of any of the types used in %token and %type.

     Type checking is *very* strict. If you use type definitions, then
     any symbol referred to in an action *must* have a type introduced
     in a type definition. Either the symbol must have been assigned a
     type in the definitions section, or the $<type-identifier> notation
     must be used. The syntax of the %type definition has been changed
     slightly to allow definitions of the form
       %type <type-identifier>
     (omitting the nonterminals) which may be used to declare types which
     are not assigned to any grammar symbol, but are used with the
     $<...> construct.

   - The parse tables constructed by this Yacc version are slightly greater
     than those constructed by UNIX Yacc, since a reduce action will only be
     chosen as the default action if it is the *only* action in the state.
     In difference, UNIX Yacc chooses a reduce action as the default action
     whenever it is the only *reduce* action of the state (even if there are
     other shift actions).

     This solves a bug in UNIX Yacc that makes the generated parser start
     error recovery too late with certain types of error productions (see
     also Schreiner/Friedman, "Introduction to compiler construction with
     UNIX," 1985). Also, errors will be caught sooner in most cases where
     standard Yacc would carry out an additional (default) reduction before
     detecting the error.

------------------------- Synopsis ------------------------

*)

{$IFDEF MsDos}
{$M 16384,0,655360}
{$ENDIF}
{$IFDEF DPMI}
{$M 32768}
{$ENDIF}
{$IFDEF Windows}
{$M 32768,0}
{$ENDIF}

{$X+}
{$I-}
program Yacc;

uses
{$IFDEF Debug}
{$IFDEF DPMI}
  YaccChk,
{$ENDIF}
{$ENDIF}
{$IFDEF Windows}
{$IFNDEF Console}
  WinCrt,
{$ENDIF}
{$ENDIF}
  YaccLib, YaccBase, YaccMsgs, YaccSem, YaccTabl, YaccPars;

%}

/* Lexical part of the Yacc language: */

%token
  ID		/* identifiers: {letter}{letter_or_digit}* */
  C_ID		/* identifier which forms left side of rule, i.e. is
		   followed by a colon */
  LITERAL       /* single character literal */
  LITID         /* multiple character literal */
  NUMBER	/* nonnegative integers: {digit}+ */
  PTOKEN PLEFT PRIGHT PNONASSOC PTYPE PSTART PPREC
  		/* reserved words: PTOKEN=%token, etc. */
  PP		/* source sections separator %% */
  LCURL		/* curly braces: %{ and %} */
  RCURL
  ',' ':' ';' '|' '{' '}' '<' '>' '='
		/* literals */
  ILLEGAL	/* illegal input character */

%start grammar

%%

/* Lexical entities, those that may give rise to syntax errors are augmented
   with error productions, and important symbols call yyerrok. */

id		: ID
c_id		: C_ID
literal         : LITERAL
litid           : LITID
number		: NUMBER
ptoken		: PTOKEN        { yyerrok; }
pleft		: PLEFT	        { yyerrok; }
pright		: PRIGHT        { yyerrok; }
pnonassoc	: PNONASSOC	{ yyerrok; }
ptype		: PTYPE	        { yyerrok; }
pstart		: PSTART        { yyerrok; }
pprec		: PPREC
pp		: PP	        { yyerrok; }
lcurl		: LCURL
rcurl		: RCURL
		| error	        { error(rcurl_expected); }
comma		: ','
colon		: ':'	        { yyerrok; }
semicolon	: ';'	        { yyerrok; }
bar		: '|'	        { yyerrok; }
lbrace		: '{'
rbrace		: '}'
		| error	        { error(rbrace_expected); }
langle		: '<'
rangle		: '>'
		| error         { error(rangle_expected); }
eq		: '='

/* Syntax and semantic routines: */

grammar		: defs pp
		  		{ sort_types;
                                  definitions;
                                  next_section; }
		  rules
		  		{ next_section;
                                  generate_parser;
                                  next_section; }
		  aux_procs
		;

aux_procs	: /* empty: aux_procs is optional */

		| pp { copy_rest_of_file; }

		;


defs		: /* empty */
		| defs def	{ yyerrok; }
		| defs error	{ error(error_in_def); }
		;

def		: pstart id
			 	{ startnt := ntsym($2); }
		| pstart error
				{ error(ident_expected); }
		| lcurl { copy_code; } rcurl

		| ptoken
				{ act_prec := 0; }
		  tag token_list

		| pleft
				{ act_prec := new_prec_level(left); }
		  tag token_list

		| pright
				{ act_prec := new_prec_level(right); }
		  tag token_list

		| pnonassoc
				{ act_prec := new_prec_level(nonassoc); }
		  tag token_list

		| ptype tag nonterm_list

                | ptype tag

		;

tag		: /* empty: type tag is optional */
				{ act_type := 0; }
		| langle id rangle
				{ act_type := $2; add_type($2); }
		;

token_list	: token_num

		| token_list token_num
				{ yyerrok; }
		| token_list comma token_num
				{ yyerrok; }
		| error
				{ error(ident_expected); }
		| token_list error
				{ error(error_in_def); }
		| token_list comma error
				{ error(ident_expected); }
		;

token_num	: literal
				{ if act_type<>0 then
                                    sym_type^[$1] := act_type;
                                  if act_prec<>0 then
                                    sym_prec^[$1] := act_prec; }
               	| litid
				{ litsym($1, 0);
                                  if act_type<>0 then
                                    sym_type^[litsym($1, 0)] := act_type;
                                  if act_prec<>0 then
                                    sym_prec^[litsym($1, 0)] := act_prec; }
               	| id
				{ litsym($1, 0);
                                  if act_type<>0 then
                                    sym_type^[litsym($1, 0)] := act_type;
                                  if act_prec<>0 then
                                    sym_prec^[litsym($1, 0)] := act_prec; }
               	| litid number
				{ litsym($1, 0);
                                  if act_type<>0 then
                                    sym_type^[litsym($1, $2)] := act_type;
                                  if act_prec<>0 then
                                    sym_prec^[litsym($1, 0)]  := act_prec; }
               	| id number
				{ litsym($1, 0);
                                  if act_type<>0 then
                                    sym_type^[litsym($1, $2)] := act_type;
                                  if act_prec<>0 then
                                    sym_prec^[litsym($1, 0)]  := act_prec; }
		;

nonterm_list	: nonterm
		| nonterm_list nonterm
				{ yyerrok; }
		| nonterm_list comma nonterm
				{ yyerrok; }
		| error
				{ error(ident_expected); }
		| nonterm_list error
				{ error(error_in_def); }
		| nonterm_list comma error
				{ error(ident_expected); }
		;

nonterm		: id
				{ if act_type<>0 then
                                    sym_type^[ntsym($1)] := act_type; }
		;


rules		: 		{ next_section; }
		  rule1

		| lcurl { copy_code; } rcurl
				{ next_section; }
		  rule1
					/* rules section may be prefixed
					   with `local' Turbo Pascal
					   declarations */
		| rules rule
				{ yyerrok; }
		| error
				{ error(error_in_rule); }
		| rules error
				{ error(error_in_rule); }
		;

rule1		: c_id
				{ start_rule(ntsym($1)); }
		  colon
		  		{ start_body; }
		  body prec
				{ end_body; }
		;

rule		: rule1

		| bar
				{ start_body; }
		  body prec
				{ end_body; }
		;

body		: /* empty */

		| body literal
				{ add_symbol($2); yyerrok; }
		| body litid
				{ add_symbol(sym($2)); yyerrok; }
		| body id
				{ add_symbol(sym($2)); yyerrok; }
                | body action
				{ add_action; yyerrok; }
		| body error
				{ error(error_in_rule); }
		;

action		: lbrace { copy_action; } rbrace

		| eq { copy_single_action; }
                		/* old language feature; code must be
				   single statement ending with `;' */
		;

prec		: /* empty */

		| pprec literal
				{ add_rule_prec($2); }
		  opt_action

		| pprec litid
				{ add_rule_prec(litsym($2, 0)); }
		  opt_action

		| pprec id
				{ add_rule_prec(litsym($2, 0)); }
		  opt_action

		| prec semicolon

		;

opt_action	: /* empty */

		| action
				{ add_action; }
		;


%%

(* Lexical analyzer (implemented in Turbo Pascal for maximum efficiency): *)

function yylex : integer;
  function end_of_input : boolean;
    begin
      end_of_input := (cno>length(line)) and eof(yyin)
    end(*end_of_input*);
  procedure scan;
    (* scan for nonempty character, skip comments *)
    procedure scan_comment;
      var p : integer;
      begin
        p := pos('*/', copy(line, cno, length(line)));
        if p>0 then
          cno := cno+succ(p)
        else
          begin
            while (p=0) and not eof(yyin) do
              begin
                readln(yyin, line);
                inc(lno);
                p := pos('*/', line)
              end;
            if p=0 then
              begin
                cno := succ(length(line));
                error(open_comment_at_eof);
              end
            else
              cno := succ(succ(p))
          end
      end(*scan_comment*);
    begin
      while not end_of_input do
        if cno<=length(line) then
          case line[cno] of
            ' ', tab : inc(cno);
            '/' :
              if (cno<length(line)) and (line[succ(cno)]='*') then
                begin
                  inc(cno, 2);
                  scan_comment
                end
              else
                exit
            else
              exit
          end
        else
          begin
            readln(yyin, line);
            inc(lno); cno := 1;
          end
    end(*scan*);
  function scan_ident : integer;
    (* scan an identifier *)
    var
      idstr : String;
    begin
      idstr := line[cno];
      inc(cno);
      while (cno<=length(line)) and (
            ('A'<=upCase(line[cno])) and (upCase(line[cno])<='Z') or
            ('0'<=line[cno]) and (line[cno]<='9') or
            (line[cno]='_') or
            (line[cno]='.') ) do
	begin
	  idstr := idstr+line[cno];
	  inc(cno)
	end;
      yylval := get_key(idstr);
      scan;
      if not end_of_input and (line[cno]=':') then
        scan_ident := C_ID
      else
        scan_ident := ID
    end(*scan_ident*);
  function scan_literal: integer;
    (* scan a literal, i.e. string *)
    var
      idstr : String;
      oct_val : Byte;
    begin
      idstr := line[cno];
      inc(cno);
      while (cno<=length(line)) and (line[cno]<>idstr[1]) do
        if line[cno]='\' then
          if cno<length(line) then
            begin
              inc(cno);
              case line[cno] of
                'n' :
                  begin
                    idstr := idstr+nl;
                    inc(cno)
                  end;
                'r' :
                  begin
                    idstr := idstr+cr;
                    inc(cno)
                  end;
                't' :
                  begin
                    idstr := idstr+tab;
                    inc(cno)
                  end;
                'b' :
                  begin
                    idstr := idstr+bs;
                    inc(cno)
                  end;
                'f' :
                  begin
                    idstr := idstr+ff;
                    inc(cno)
                  end;
                '0'..'7' :
                  begin
                    oct_val := ord(line[cno])-ord('0');
                    inc(cno);
                    while (cno<=length(line)) and
                          ('0'<=line[cno]) and
                          (line[cno]<='7') do
                      begin
                        oct_val := oct_val*8+ord(line[cno])-ord('0');
                        inc(cno)
                      end;
                    idstr := idstr+chr(oct_val)
                  end
                else
                  begin
                    idstr := idstr+line[cno];
                    inc(cno)
                  end
              end
            end
          else
            inc(cno)
        else
          begin
            idstr := idstr+line[cno];
            inc(cno)
          end;
      if cno>length(line) then
        error(missing_string_terminator)
      else
        inc(cno);
      if length(idstr)=2 then
        begin
          yylval := ord(idstr[2]);
          scan_literal := LITERAL;
        end
      else if length(idstr)>1 then
        begin
          yylval := get_key(''''+copy(idstr, 2, pred(length(idstr)))+'''');
          scan_literal := LITID;
        end
      else
        scan_literal := ILLEGAL;
    end(*scan_literal*);
  function scan_num : integer;
    (* scan an unsigned integer *)
    var
      numstr : String;
      code : integer;
    begin
      numstr := line[cno];
      inc(cno);
      while (cno<=length(line)) and
            ('0'<=line[cno]) and (line[cno]<='9') do
        begin
          numstr := numstr+line[cno];
          inc(cno)
        end;
      val(numstr, yylval, code);
      if code=0 then
        scan_num := NUMBER
      else
        scan_num := ILLEGAL;
    end(*scan_num*);
  function scan_keyword : integer;
    (* scan %xy *)
    function lookup(key : String; var tok : integer) : boolean;
      (* table of Yacc keywords (unstropped): *)
      const
        no_of_entries = 11;
        max_entry_length = 8;
        keys : array [1..no_of_entries] of String[max_entry_length] = (
          '0', '2', 'binary', 'left', 'nonassoc', 'prec', 'right',
          'start', 'term', 'token', 'type');
        toks : array [1..no_of_entries] of integer = (
          PTOKEN, PNONASSOC, PNONASSOC, PLEFT, PNONASSOC, PPREC, PRIGHT,
          PSTART, PTOKEN, PTOKEN, PTYPE);
      var m, n, k : integer;
      begin
        (* binary search: *)
        m := 1; n := no_of_entries;
        lookup := true;
        while m<=n do
          begin
            k := m+(n-m) div 2;
            if key=keys[k] then
              begin
                tok := toks[k];
                exit
              end
            else if key>keys[k] then
              m := k+1
            else
              n := k-1
          end;
        lookup := false
      end(*lookup*);
    var
      keywstr : String;
      tok : integer;
    begin
      inc(cno);
      if cno<=length(line) then
        case line[cno] of
          '<' :
            begin
              scan_keyword := PLEFT;
              inc(cno)
            end;
          '>' :
            begin
              scan_keyword := PRIGHT;
              inc(cno)
            end;
          '=' :
            begin
              scan_keyword := PPREC;
              inc(cno)
            end;
          '%', '\' :
            begin
              scan_keyword := PP;
              inc(cno)
            end;
          '{' :
            begin
              scan_keyword := LCURL;
              inc(cno)
            end;
          '}' :
            begin
              scan_keyword := RCURL;
              inc(cno)
            end;
          'A'..'Z', 'a'..'z', '0'..'9' :
            begin
              keywstr := line[cno];
              inc(cno);
              while (cno<=length(line)) and (
                    ('A'<=upCase(line[cno])) and (upCase(line[cno])<='Z') or
                    ('0'<=line[cno]) and (line[cno]<='Z') ) do
                begin
                  keywstr := keywstr+line[cno];
                  inc(cno)
                end;
              if lookup(keywstr, tok) then
                scan_keyword := tok
              else
                scan_keyword := ILLEGAL
            end;
          else scan_keyword := ILLEGAL
        end
      else
        scan_keyword := ILLEGAL;
    end(*scan_keyword*);
  function scan_char : integer;
    (* scan any single character *)
    begin
      scan_char := ord(line[cno]);
      inc(cno)
    end(*scan_char*);
  var lno0, cno0 : integer;
  begin
    tokleng := 0;
    scan;
    lno0 := lno; cno0 := cno;
    if end_of_input then
      yylex := 0
    else
      case line[cno] of
        'A'..'Z', 'a'..'z', '_' : yylex := scan_ident;
	'''', '"' : yylex := scan_literal;
	'0'..'9' : yylex := scan_num;
	'%', '\' : yylex := scan_keyword;
        '=' :
          if (cno<length(line)) and (line[succ(cno)]='{') then
            begin
              inc(cno);
              yylex := scan_char
            end
          else
            yylex := scan_char;
	else yylex := scan_char;
      end;
    if lno=lno0 then
      tokleng := cno-cno0
  end(*yylex*);

(* Main program: *)

var i : Integer;

begin
{$ifdef Unix}
 {$ifdef BSD}
  codfilepath:='/usr/local/lib/fpc/lexyacc/';
 {$else}
  codfilepath:='/usr/lib/fpc/lexyacc/';
 {$endif}
{$else}
  codfilepath:=path(paramstr(0));
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

  yfilename := '';
  pasfilename := '';

  for i := 1 to paramCount do
    if copy(paramStr(i), 1, 1)='-' then
      if upper(paramStr(i))='-V' then
        verbose := true
      else if upper(paramStr(i))='-D' then
        debug := true
      else
        begin
          writeln(invalid_option, paramStr(i));
          halt(1);
        end
    else if yfilename='' then
      yfilename := addExt(paramStr(i), 'y')
    else if pasfilename='' then
      pasfilename := addExt(paramStr(i), 'pas')
    else
      begin
        writeln(illegal_no_args);
        halt(1);
      end;

  if yfilename='' then
    begin
      writeln(illegal_no_args);
      halt(1);
    end;

  if pasfilename='' then pasfilename := root(yfilename)+'.pas';
  lstfilename := root(yfilename)+'.lst';

  (* open files: *)

  assign(yyin, yfilename);
  assign(yyout, pasfilename);
  assign(yylst, lstfilename);

  reset(yyin);    if ioresult<>0 then fatal(cannot_open_file+yfilename);
  rewrite(yyout); if ioresult<>0 then fatal(cannot_open_file+pasfilename);
  rewrite(yylst); if ioresult<>0 then fatal(cannot_open_file+lstfilename);

  (* search code template in current directory, then on path where Yacc
     was executed from: *)
  codfilename := 'yyparse.cod';
  assign(yycod, codfilename);
  reset(yycod);
  if ioresult<>0 then
    begin
      codfilename := codfilepath+'yyparse.cod';
      assign(yycod, codfilename);
      reset(yycod);
      if ioresult<>0 then fatal(cannot_open_file+codfilename);
    end;

  (* parse source grammar: *)

  write('parse ... ');

  lno := 0; cno := 1; line := '';

  next_section;
  if debug then writeln(yyout, '{$define yydebug}');

  if yyparse=0 then
    { done }
  else if yychar=0 then
    error(unexpected_eof)
  else
    error(syntax_error);

  if errors=0 then writeln('DONE');

  (* close files: *)

  close(yyin); close(yyout); close(yylst); close(yycod);

  (* print statistics: *)

  if errors>0 then
    writeln( lno, ' lines, ',
             errors, ' errors found.' )
  else
    begin
      writeln( lno, ' lines, ',
               n_rules-1, '/', max_rules-1, ' rules, ',
               n_states, '/', max_states, ' s, ',
               n_items, '/', max_items, ' i, ',
               n_trans, '/', max_trans, ' t, ',
               n_redns, '/', max_redns, ' r.');
      if shift_reduce>0 then
        writeln(shift_reduce, ' shift/reduce conflicts.');
      if reduce_reduce>0 then
        writeln(reduce_reduce, ' reduce/reduce conflicts.');
      if never_reduced>0 then
        writeln(never_reduced, ' rules never reduced.');
    end;

  if warnings>0 then writeln(warnings, ' warnings.');

  (* terminate: *)

  if errors>0 then
    begin
      erase(yyout);
      if ioresult<>0 then ;
    end;

  if file_size(lstfilename)=0 then
    erase(yylst)
  else
    writeln('(see ', lstfilename, ' for more information)');

  halt(errors);

end(*Yacc*).
