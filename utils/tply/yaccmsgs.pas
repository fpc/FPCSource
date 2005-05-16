{
  TP Yacc message and error handling module 2-5-91 AG
  Note: this module should be USEd by any module using the heap during
        initialization, since it installs a heap error handler (which
        terminates the program with fatal error `memory overflow').


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
$Modtime: 96-07-31 14:50 $

$History: YACCMSGS.PAS $
 *
 * *****************  Version 2  *****************
 * User: Berend       Date: 96-10-10   Time: 21:16
 * Updated in $/Lex and Yacc/tply
 * Updated for protected mode, windows and Delphi 1.X and 2.X.

}


unit YaccMsgs;

interface


var errors, warnings : Integer;
  (* - current error and warning count *)
procedure error(msg : String);
  (* - print current input line and error message (pos denotes position to
       mark in source file line) *)
procedure warning(msg : String);
  (* - print warning message *)
procedure fatal(msg : String);
(* - writes a fatal error message, erases Yacc output file and terminates
     the program with errorlevel 1 *)

const

(* sign-on and usage message: *)

sign_on = 'TP Yacc Version 4.1a [April 2000], Copyright (c) 1990-2000 Albert Graef';
{$ifdef Unix}
usage   = 'Usage: pyacc [options] yacc-file[.y] [output-file[.pas]]';
{$else}
usage   = 'Usage: yacc [options] yacc-file[.y] [output-file[.pas]]';
{$endif}
options = 'Options: -v verbose, -d debug';

(* command line error messages: *)

invalid_option                  = 'invalid option ';
illegal_no_args                 = 'illegal number of parameters';

(* syntax errors: *)

open_comment_at_eof             = '101: open comment at end of file';
missing_string_terminator       = '102: missing string terminator';
rcurl_expected                  = '103: %} expected';
rbrace_expected                 = '104: } expected';
rangle_expected                 = '105: > expected';
ident_expected                  = '106: identifier expected';
error_in_def                    = '110: error in definition';
error_in_rule                   = '111: error in rule';
syntax_error                    = '112: syntax error';
unexpected_eof                  = '113: unexpected end of file';

(* semantic errors: *)

nonterm_expected                = '201: nonterminal expected';
literal_expected                = '202: literal expected';
double_tokennum_def             = '203: literal already defined';
unknown_identifier              = '204: unknown identifier';
type_error                      = '205: type error';
range_error                     = '206: range error';
empty_grammar                   = '207: empty grammar?';

(* fatal errors: *)

cannot_open_file                = 'FATAL: cannot open file ';
write_error                     = 'FATAL: write error';
mem_overflow                    = 'FATAL: memory overflow';
intset_overflow                 = 'FATAL: integer set overflow';
sym_table_overflow              = 'FATAL: symbol table overflow';
nt_table_overflow               = 'FATAL: nonterminal table overflow';
lit_table_overflow              = 'FATAL: literal table overflow';
type_table_overflow             = 'FATAL: type table overflow';
prec_table_overflow             = 'FATAL: precedence table overflow';
rule_table_overflow             = 'FATAL: rule table overflow';
state_table_overflow            = 'FATAL: state table overflow';
item_table_overflow             = 'FATAL: item table overflow';
trans_table_overflow            = 'FATAL: transition table overflow';
redn_table_overflow             = 'FATAL: reduction table overflow';

implementation

uses YaccBase;

procedure position(var f : Text;
            lineNo : integer;
            line : String;
            pos : integer);
  (* writes a position mark of the form
     lineno: line
               ^
     on f with the caret ^ positioned at pos in line
     a subsequent write starts at the next line, indented with tab *)
  var
    line1, line2 : String;
  begin
    (* this hack handles tab characters in line: *)
    line1 := intStr(lineNo)+': '+line;
    line2 := blankStr(intStr(lineNo)+': '+copy(line, 1, pos-1));
    writeln(f, line1);
    writeln(f, line2, '^');
    write(f, tab)
  end(*position*);

procedure error(msg : String);
  begin
    inc(errors);
    writeln;
    position(output, lno, line, cno-tokleng);
    writeln(msg);
    writeln(yylst);
    position(yylst, lno, line, cno-tokleng);
    writeln(yylst, msg);
    if ioresult<>0 then ;
  end(*error*);

procedure warning(msg : String);
  begin
    inc(warnings);
    writeln;
    position(output, lno, line, cno-tokleng);
    writeln(msg);
    writeln(yylst);
    position(yylst, lno, line, cno-tokleng);
    writeln(yylst, msg);
    if ioresult<>0 then ;
  end(*warning*);

procedure fatal(msg : String);
  begin
    writeln;
    writeln(msg);
    close(yyin); close(yyout); close(yylst); erase(yyout);
    halt(1)
  end(*fatal*);

{$ifndef fpc}
{$IFNDEF Win32}
function heapErrorHandler ( size : Word ) : Integer; far;
  begin
    if size>0 then
      fatal(mem_overflow) (* never returns *)
    else
      heapErrorHandler := 1
  end(*heapErrorHandler*);
{$ENDIF}
{$endif}

begin
  errors := 0; warnings := 0;
{$ifndef fpc}
{$IFNDEF Win32}
  (* install heap error handler: *)
  heapError := @heapErrorHandler;
{$ENDIF}
{$endif}
end(*YaccMsgs*).
