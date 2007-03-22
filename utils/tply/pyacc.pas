
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

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


$Revision: 1.5 $
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

{$X+}
{$I-}
program Yacc;

uses
{$IFDEF Debug}
{$IFDEF DPMI}
  { YaccChk, removed as obsolete,
    YaccChk source not available anymore PM }
{$ENDIF}
{$ENDIF}
  YaccLib, YaccBase, YaccMsgs, YaccSem, YaccTabl, YaccPars;

const ID = 257;
const C_ID = 258;
const LITERAL = 259;
const LITID = 260;
const NUMBER = 261;
const PTOKEN = 262;
const PLEFT = 263;
const PRIGHT = 264;
const PNONASSOC = 265;
const PTYPE = 266;
const PSTART = 267;
const PPREC = 268;
const PP = 269;
const LCURL = 270;
const RCURL = 271;
const ILLEGAL = 272;

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         yyval := yyv[yysp-0];
       end;
   2 : begin
         yyval := yyv[yysp-0];
       end;
   3 : begin
         yyval := yyv[yysp-0];
       end;
   4 : begin
         yyval := yyv[yysp-0];
       end;
   5 : begin
         yyval := yyv[yysp-0];
       end;
   6 : begin
         yyerrok;
       end;
   7 : begin
         yyerrok;
       end;
   8 : begin
         yyerrok;
       end;
   9 : begin
         yyerrok;
       end;
  10 : begin
         yyerrok;
       end;
  11 : begin
         yyerrok;
       end;
  12 : begin
         yyval := yyv[yysp-0];
       end;
  13 : begin
         yyerrok;
       end;
  14 : begin
         yyval := yyv[yysp-0];
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
         error(rcurl_expected);
       end;
  17 : begin
         yyval := yyv[yysp-0];
       end;
  18 : begin
         yyerrok;
       end;
  19 : begin
         yyerrok;
       end;
  20 : begin
         yyerrok;
       end;
  21 : begin
         yyval := yyv[yysp-0];
       end;
  22 : begin
         yyval := yyv[yysp-0];
       end;
  23 : begin
         error(rbrace_expected);
       end;
  24 : begin
         yyval := yyv[yysp-0];
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         error(rangle_expected);
       end;
  27 : begin
         yyval := yyv[yysp-0];
       end;
  28 : begin
         sort_types;
         definitions;
         next_section;
       end;
  29 : begin
         next_section;
         generate_parser;
         next_section;
       end;
  30 : begin
         yyval := yyv[yysp-5];
       end;
  31 : begin
       end;
  32 : begin
         copy_rest_of_file;
       end;
  33 : begin
       end;
  34 : begin
         yyerrok;
       end;
  35 : begin
         error(error_in_def);
       end;
  36 : begin
         startnt := ntsym(yyv[yysp-0]);
       end;
  37 : begin
         error(ident_expected);
       end;
  38 : begin
         copy_code;
       end;
  39 : begin
         yyval := yyv[yysp-2];
       end;
  40 : begin
         act_prec := 0;
       end;
  41 : begin
         yyval := yyv[yysp-3];
       end;
  42 : begin
         act_prec := new_prec_level(left);
       end;
  43 : begin
         yyval := yyv[yysp-3];
       end;
  44 : begin
         act_prec := new_prec_level(right);
       end;
  45 : begin
         yyval := yyv[yysp-3];
       end;
  46 : begin
         act_prec := new_prec_level(nonassoc);
       end;
  47 : begin
         yyval := yyv[yysp-3];
       end;
  48 : begin
         yyval := yyv[yysp-2];
       end;
  49 : begin
         yyval := yyv[yysp-1];
       end;
  50 : begin
         act_type := 0;
       end;
  51 : begin
         act_type := yyv[yysp-1]; add_type(yyv[yysp-1]);
       end;
  52 : begin
         yyval := yyv[yysp-0];
       end;
  53 : begin
         yyerrok;
       end;
  54 : begin
         yyerrok;
       end;
  55 : begin
         error(ident_expected);
       end;
  56 : begin
         error(error_in_def);
       end;
  57 : begin
         error(ident_expected);
       end;
  58 : begin
         if act_type<>0 then
         sym_type^[yyv[yysp-0]] := act_type;
         if act_prec<>0 then
         sym_prec^[yyv[yysp-0]] := act_prec;
       end;
  59 : begin
         litsym(yyv[yysp-0], 0);
         if act_type<>0 then
         sym_type^[litsym(yyv[yysp-0], 0)] := act_type;
         if act_prec<>0 then
         sym_prec^[litsym(yyv[yysp-0], 0)] := act_prec;
       end;
  60 : begin
         litsym(yyv[yysp-0], 0);
         if act_type<>0 then
         sym_type^[litsym(yyv[yysp-0], 0)] := act_type;
         if act_prec<>0 then
         sym_prec^[litsym(yyv[yysp-0], 0)] := act_prec;
       end;
  61 : begin
         litsym(yyv[yysp-1], 0);
         if act_type<>0 then
         sym_type^[litsym(yyv[yysp-1], yyv[yysp-0])] := act_type;
         if act_prec<>0 then
         sym_prec^[litsym(yyv[yysp-1], 0)]  := act_prec;
       end;
  62 : begin
         litsym(yyv[yysp-1], 0);
         if act_type<>0 then
         sym_type^[litsym(yyv[yysp-1], yyv[yysp-0])] := act_type;
         if act_prec<>0 then
         sym_prec^[litsym(yyv[yysp-1], 0)]  := act_prec;
       end;
  63 : begin
         yyval := yyv[yysp-0];
       end;
  64 : begin
         yyerrok;
       end;
  65 : begin
         yyerrok;
       end;
  66 : begin
         error(ident_expected);
       end;
  67 : begin
         error(error_in_def);
       end;
  68 : begin
         error(ident_expected);
       end;
  69 : begin
         if act_type<>0 then
         sym_type^[ntsym(yyv[yysp-0])] := act_type;
       end;
  70 : begin
         next_section;
       end;
  71 : begin
         yyval := yyv[yysp-1];
       end;
  72 : begin
         copy_code;
       end;
  73 : begin
         next_section;
       end;
  74 : begin
         yyval := yyv[yysp-4];
       end;
  75 : begin
         yyerrok;
       end;
  76 : begin
         error(error_in_rule);
       end;
  77 : begin
         error(error_in_rule);
       end;
  78 : begin
         start_rule(ntsym(yyv[yysp-0]));
       end;
  79 : begin
         start_body;
       end;
  80 : begin
         end_body;
       end;
  81 : begin
         yyval := yyv[yysp-0];
       end;
  82 : begin
         start_body;
       end;
  83 : begin
         end_body;
       end;
  84 : begin
       end;
  85 : begin
         add_symbol(yyv[yysp-0]); yyerrok;
       end;
  86 : begin
         add_symbol(sym(yyv[yysp-0])); yyerrok;
       end;
  87 : begin
         add_symbol(sym(yyv[yysp-0])); yyerrok;
       end;
  88 : begin
         add_action; yyerrok;
       end;
  89 : begin
         error(error_in_rule);
       end;
  90 : begin
         copy_action;
       end;
  91 : begin
         yyval := yyv[yysp-2];
       end;
  92 : begin
         copy_single_action;
       end;
  93 : begin
       end;
  94 : begin
         add_rule_prec(yyv[yysp-0]);
       end;
  95 : begin
         yyval := yyv[yysp-3];
       end;
  96 : begin
         add_rule_prec(litsym(yyv[yysp-0], 0));
       end;
  97 : begin
         yyval := yyv[yysp-3];
       end;
  98 : begin
         add_rule_prec(litsym(yyv[yysp-0], 0));
       end;
  99 : begin
         yyval := yyv[yysp-3];
       end;
 100 : begin
         yyval := yyv[yysp-1];
       end;
 101 : begin
       end;
 102 : begin
         add_action;
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 251;
yyngotos  = 146;
yynstates = 128;
yynrules  = 102;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 256; act: 12 ),
  ( sym: 262; act: 13 ),
  ( sym: 263; act: 14 ),
  ( sym: 264; act: 15 ),
  ( sym: 265; act: 16 ),
  ( sym: 266; act: 17 ),
  ( sym: 267; act: 18 ),
  ( sym: 269; act: 19 ),
  ( sym: 270; act: 20 ),
{ 2: }
  ( sym: 0; act: 0 ),
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  ( sym: 256; act: 24 ),
  ( sym: 257; act: 25 ),
{ 7: }
  ( sym: 60; act: 28 ),
  ( sym: 256; act: -50 ),
  ( sym: 257; act: -50 ),
  ( sym: 262; act: -50 ),
  ( sym: 263; act: -50 ),
  ( sym: 264; act: -50 ),
  ( sym: 265; act: -50 ),
  ( sym: 266; act: -50 ),
  ( sym: 267; act: -50 ),
  ( sym: 269; act: -50 ),
  ( sym: 270; act: -50 ),
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  ( sym: 256; act: 34 ),
  ( sym: 271; act: 35 ),
{ 22: }
  ( sym: 256; act: 39 ),
  ( sym: 270; act: 20 ),
  ( sym: 258; act: -70 ),
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( sym: 256; act: 43 ),
  ( sym: 257; act: 25 ),
  ( sym: 262; act: -49 ),
  ( sym: 263; act: -49 ),
  ( sym: 264; act: -49 ),
  ( sym: 265; act: -49 ),
  ( sym: 266; act: -49 ),
  ( sym: 267; act: -49 ),
  ( sym: 269; act: -49 ),
  ( sym: 270; act: -49 ),
{ 27: }
  ( sym: 257; act: 25 ),
{ 28: }
{ 29: }
  ( sym: 60; act: 28 ),
  ( sym: 256; act: -50 ),
  ( sym: 257; act: -50 ),
  ( sym: 259; act: -50 ),
  ( sym: 260; act: -50 ),
{ 30: }
  ( sym: 60; act: 28 ),
  ( sym: 256; act: -50 ),
  ( sym: 257; act: -50 ),
  ( sym: 259; act: -50 ),
  ( sym: 260; act: -50 ),
{ 31: }
  ( sym: 60; act: 28 ),
  ( sym: 256; act: -50 ),
  ( sym: 257; act: -50 ),
  ( sym: 259; act: -50 ),
  ( sym: 260; act: -50 ),
{ 32: }
  ( sym: 60; act: 28 ),
  ( sym: 256; act: -50 ),
  ( sym: 257; act: -50 ),
  ( sym: 259; act: -50 ),
  ( sym: 260; act: -50 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: 258; act: 51 ),
{ 37: }
  ( sym: 124; act: 56 ),
  ( sym: 256; act: 57 ),
  ( sym: 258; act: 51 ),
  ( sym: 0; act: -29 ),
  ( sym: 269; act: -29 ),
{ 38: }
{ 39: }
{ 40: }
{ 41: }
  ( sym: 44; act: 61 ),
  ( sym: 256; act: 62 ),
  ( sym: 257; act: 25 ),
  ( sym: 262; act: -48 ),
  ( sym: 263; act: -48 ),
  ( sym: 264; act: -48 ),
  ( sym: 265; act: -48 ),
  ( sym: 266; act: -48 ),
  ( sym: 267; act: -48 ),
  ( sym: 269; act: -48 ),
  ( sym: 270; act: -48 ),
{ 42: }
{ 43: }
{ 44: }
  ( sym: 62; act: 64 ),
  ( sym: 256; act: 65 ),
{ 45: }
  ( sym: 256; act: 71 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 46: }
  ( sym: 256; act: 71 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 47: }
  ( sym: 256; act: 71 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 48: }
  ( sym: 256; act: 71 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
  ( sym: 269; act: 19 ),
  ( sym: 0; act: -31 ),
{ 55: }
{ 56: }
{ 57: }
{ 58: }
  ( sym: 256; act: 34 ),
  ( sym: 271; act: 35 ),
{ 59: }
{ 60: }
  ( sym: 256; act: 83 ),
  ( sym: 257; act: 25 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: 44; act: 61 ),
  ( sym: 256; act: 86 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 262; act: -47 ),
  ( sym: 263; act: -47 ),
  ( sym: 264; act: -47 ),
  ( sym: 265; act: -47 ),
  ( sym: 266; act: -47 ),
  ( sym: 267; act: -47 ),
  ( sym: 269; act: -47 ),
  ( sym: 270; act: -47 ),
{ 68: }
  ( sym: 261; act: 88 ),
  ( sym: 44; act: -59 ),
  ( sym: 256; act: -59 ),
  ( sym: 257; act: -59 ),
  ( sym: 259; act: -59 ),
  ( sym: 260; act: -59 ),
  ( sym: 262; act: -59 ),
  ( sym: 263; act: -59 ),
  ( sym: 264; act: -59 ),
  ( sym: 265; act: -59 ),
  ( sym: 266; act: -59 ),
  ( sym: 267; act: -59 ),
  ( sym: 269; act: -59 ),
  ( sym: 270; act: -59 ),
{ 69: }
{ 70: }
  ( sym: 261; act: 88 ),
  ( sym: 44; act: -60 ),
  ( sym: 256; act: -60 ),
  ( sym: 257; act: -60 ),
  ( sym: 259; act: -60 ),
  ( sym: 260; act: -60 ),
  ( sym: 262; act: -60 ),
  ( sym: 263; act: -60 ),
  ( sym: 264; act: -60 ),
  ( sym: 265; act: -60 ),
  ( sym: 266; act: -60 ),
  ( sym: 267; act: -60 ),
  ( sym: 269; act: -60 ),
  ( sym: 270; act: -60 ),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
  ( sym: 44; act: 61 ),
  ( sym: 256; act: 86 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 262; act: -45 ),
  ( sym: 263; act: -45 ),
  ( sym: 264; act: -45 ),
  ( sym: 265; act: -45 ),
  ( sym: 266; act: -45 ),
  ( sym: 267; act: -45 ),
  ( sym: 269; act: -45 ),
  ( sym: 270; act: -45 ),
{ 75: }
  ( sym: 44; act: 61 ),
  ( sym: 256; act: 86 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 262; act: -43 ),
  ( sym: 263; act: -43 ),
  ( sym: 264; act: -43 ),
  ( sym: 265; act: -43 ),
  ( sym: 266; act: -43 ),
  ( sym: 267; act: -43 ),
  ( sym: 269; act: -43 ),
  ( sym: 270; act: -43 ),
{ 76: }
  ( sym: 44; act: 61 ),
  ( sym: 256; act: 86 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 262; act: -41 ),
  ( sym: 263; act: -41 ),
  ( sym: 264; act: -41 ),
  ( sym: 265; act: -41 ),
  ( sym: 266; act: -41 ),
  ( sym: 267; act: -41 ),
  ( sym: 269; act: -41 ),
  ( sym: 270; act: -41 ),
{ 77: }
  ( sym: 58; act: 91 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: 256; act: 95 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
  ( sym: 61; act: 105 ),
  ( sym: 123; act: 106 ),
  ( sym: 256; act: 107 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 268; act: 108 ),
  ( sym: 0; act: -93 ),
  ( sym: 59; act: -93 ),
  ( sym: 124; act: -93 ),
  ( sym: 258; act: -93 ),
  ( sym: 269; act: -93 ),
{ 93: }
  ( sym: 258; act: 51 ),
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
  ( sym: 59; act: 112 ),
  ( sym: 0; act: -83 ),
  ( sym: 124; act: -83 ),
  ( sym: 256; act: -83 ),
  ( sym: 258; act: -83 ),
  ( sym: 269; act: -83 ),
{ 99: }
{ 100: }
{ 101: }
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
  ( sym: 61; act: 105 ),
  ( sym: 123; act: 106 ),
  ( sym: 256; act: 107 ),
  ( sym: 257; act: 25 ),
  ( sym: 259; act: 72 ),
  ( sym: 260; act: 73 ),
  ( sym: 268; act: 108 ),
  ( sym: 0; act: -93 ),
  ( sym: 59; act: -93 ),
  ( sym: 124; act: -93 ),
  ( sym: 258; act: -93 ),
  ( sym: 269; act: -93 ),
{ 111: }
{ 112: }
{ 113: }
  ( sym: 125; act: 119 ),
  ( sym: 256; act: 120 ),
{ 114: }
{ 115: }
{ 116: }
{ 117: }
  ( sym: 59; act: 112 ),
  ( sym: 0; act: -80 ),
  ( sym: 124; act: -80 ),
  ( sym: 256; act: -80 ),
  ( sym: 258; act: -80 ),
  ( sym: 269; act: -80 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
  ( sym: 61; act: 105 ),
  ( sym: 123; act: 106 ),
  ( sym: 0; act: -101 ),
  ( sym: 59; act: -101 ),
  ( sym: 124; act: -101 ),
  ( sym: 256; act: -101 ),
  ( sym: 258; act: -101 ),
  ( sym: 269; act: -101 ),
{ 122: }
  ( sym: 61; act: 105 ),
  ( sym: 123; act: 106 ),
  ( sym: 0; act: -101 ),
  ( sym: 59; act: -101 ),
  ( sym: 124; act: -101 ),
  ( sym: 256; act: -101 ),
  ( sym: 258; act: -101 ),
  ( sym: 269; act: -101 ),
{ 123: }
  ( sym: 61; act: 105 ),
  ( sym: 123; act: 106 ),
  ( sym: 0; act: -101 ),
  ( sym: 59; act: -101 ),
  ( sym: 124; act: -101 ),
  ( sym: 256; act: -101 ),
  ( sym: 258; act: -101 ),
  ( sym: 269; act: -101 )
{ 124: }
{ 125: }
{ 126: }
{ 127: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -27; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
  ( sym: -32; act: 3 ),
  ( sym: -16; act: 4 ),
  ( sym: -15; act: 5 ),
  ( sym: -13; act: 6 ),
  ( sym: -12; act: 7 ),
  ( sym: -11; act: 8 ),
  ( sym: -10; act: 9 ),
  ( sym: -9; act: 10 ),
  ( sym: -8; act: 11 ),
{ 2: }
{ 3: }
{ 4: }
  ( sym: -33; act: 21 ),
{ 5: }
  ( sym: -29; act: 22 ),
{ 6: }
  ( sym: -3; act: 23 ),
{ 7: }
  ( sym: -34; act: 26 ),
  ( sym: -24; act: 27 ),
{ 8: }
  ( sym: -39; act: 29 ),
{ 9: }
  ( sym: -38; act: 30 ),
{ 10: }
  ( sym: -37; act: 31 ),
{ 11: }
  ( sym: -35; act: 32 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  ( sym: -17; act: 33 ),
{ 22: }
  ( sym: -44; act: 36 ),
  ( sym: -28; act: 37 ),
  ( sym: -16; act: 38 ),
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( sym: -42; act: 40 ),
  ( sym: -40; act: 41 ),
  ( sym: -3; act: 42 ),
{ 27: }
  ( sym: -3; act: 44 ),
{ 28: }
{ 29: }
  ( sym: -34; act: 45 ),
  ( sym: -24; act: 27 ),
{ 30: }
  ( sym: -34; act: 46 ),
  ( sym: -24; act: 27 ),
{ 31: }
  ( sym: -34; act: 47 ),
  ( sym: -24; act: 27 ),
{ 32: }
  ( sym: -34; act: 48 ),
  ( sym: -24; act: 27 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -43; act: 49 ),
  ( sym: -4; act: 50 ),
{ 37: }
  ( sym: -47; act: 52 ),
  ( sym: -43; act: 53 ),
  ( sym: -31; act: 54 ),
  ( sym: -21; act: 55 ),
  ( sym: -4; act: 50 ),
{ 38: }
  ( sym: -45; act: 58 ),
{ 39: }
{ 40: }
{ 41: }
  ( sym: -42; act: 59 ),
  ( sym: -18; act: 60 ),
  ( sym: -3; act: 42 ),
{ 42: }
{ 43: }
{ 44: }
  ( sym: -25; act: 63 ),
{ 45: }
  ( sym: -41; act: 66 ),
  ( sym: -36; act: 67 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 46: }
  ( sym: -41; act: 66 ),
  ( sym: -36; act: 74 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 47: }
  ( sym: -41; act: 66 ),
  ( sym: -36; act: 75 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 48: }
  ( sym: -41; act: 66 ),
  ( sym: -36; act: 76 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 49: }
{ 50: }
  ( sym: -48; act: 77 ),
{ 51: }
{ 52: }
{ 53: }
{ 54: }
  ( sym: -30; act: 78 ),
  ( sym: -15; act: 79 ),
{ 55: }
  ( sym: -52; act: 80 ),
{ 56: }
{ 57: }
{ 58: }
  ( sym: -17; act: 81 ),
{ 59: }
{ 60: }
  ( sym: -42; act: 82 ),
  ( sym: -3; act: 42 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: -41; act: 84 ),
  ( sym: -18; act: 85 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 68: }
  ( sym: -7; act: 87 ),
{ 69: }
{ 70: }
  ( sym: -7; act: 89 ),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
  ( sym: -41; act: 84 ),
  ( sym: -18; act: 85 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 75: }
  ( sym: -41; act: 84 ),
  ( sym: -18; act: 85 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 76: }
  ( sym: -41; act: 84 ),
  ( sym: -18; act: 85 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 77: }
  ( sym: -19; act: 90 ),
{ 78: }
{ 79: }
{ 80: }
  ( sym: -49; act: 92 ),
{ 81: }
  ( sym: -46; act: 93 ),
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: -41; act: 94 ),
  ( sym: -6; act: 68 ),
  ( sym: -5; act: 69 ),
  ( sym: -3; act: 70 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
  ( sym: -50; act: 96 ),
{ 91: }
{ 92: }
  ( sym: -53; act: 97 ),
  ( sym: -51; act: 98 ),
  ( sym: -26; act: 99 ),
  ( sym: -22; act: 100 ),
  ( sym: -14; act: 101 ),
  ( sym: -6; act: 102 ),
  ( sym: -5; act: 103 ),
  ( sym: -3; act: 104 ),
{ 93: }
  ( sym: -43; act: 109 ),
  ( sym: -4; act: 50 ),
{ 94: }
{ 95: }
{ 96: }
  ( sym: -49; act: 110 ),
{ 97: }
{ 98: }
  ( sym: -20; act: 111 ),
{ 99: }
{ 100: }
  ( sym: -54; act: 113 ),
{ 101: }
  ( sym: -6; act: 114 ),
  ( sym: -5; act: 115 ),
  ( sym: -3; act: 116 ),
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
  ( sym: -53; act: 97 ),
  ( sym: -51; act: 117 ),
  ( sym: -26; act: 99 ),
  ( sym: -22; act: 100 ),
  ( sym: -14; act: 101 ),
  ( sym: -6; act: 102 ),
  ( sym: -5; act: 103 ),
  ( sym: -3; act: 104 ),
{ 111: }
{ 112: }
{ 113: }
  ( sym: -23; act: 118 ),
{ 114: }
  ( sym: -57; act: 121 ),
{ 115: }
  ( sym: -56; act: 122 ),
{ 116: }
  ( sym: -58; act: 123 ),
{ 117: }
  ( sym: -20; act: 111 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
  ( sym: -55; act: 124 ),
  ( sym: -53; act: 125 ),
  ( sym: -26; act: 99 ),
  ( sym: -22; act: 100 ),
{ 122: }
  ( sym: -55; act: 126 ),
  ( sym: -53; act: 125 ),
  ( sym: -26; act: 99 ),
  ( sym: -22; act: 100 ),
{ 123: }
  ( sym: -55; act: 127 ),
  ( sym: -53; act: 125 ),
  ( sym: -26; act: 99 ),
  ( sym: -22; act: 100 )
{ 124: }
{ 125: }
{ 126: }
{ 127: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -33,
{ 1: } 0,
{ 2: } 0,
{ 3: } -34,
{ 4: } -38,
{ 5: } -28,
{ 6: } 0,
{ 7: } 0,
{ 8: } -46,
{ 9: } -44,
{ 10: } -42,
{ 11: } -40,
{ 12: } -35,
{ 13: } -6,
{ 14: } -7,
{ 15: } -8,
{ 16: } -9,
{ 17: } -10,
{ 18: } -11,
{ 19: } -13,
{ 20: } -14,
{ 21: } 0,
{ 22: } 0,
{ 23: } -36,
{ 24: } -37,
{ 25: } -1,
{ 26: } 0,
{ 27: } 0,
{ 28: } -24,
{ 29: } 0,
{ 30: } 0,
{ 31: } 0,
{ 32: } 0,
{ 33: } -39,
{ 34: } -16,
{ 35: } -15,
{ 36: } 0,
{ 37: } 0,
{ 38: } -72,
{ 39: } -76,
{ 40: } -63,
{ 41: } 0,
{ 42: } -69,
{ 43: } -66,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } 0,
{ 49: } -71,
{ 50: } -78,
{ 51: } -2,
{ 52: } -75,
{ 53: } -81,
{ 54: } 0,
{ 55: } -82,
{ 56: } -20,
{ 57: } -77,
{ 58: } 0,
{ 59: } -64,
{ 60: } 0,
{ 61: } -17,
{ 62: } -67,
{ 63: } -51,
{ 64: } -25,
{ 65: } -26,
{ 66: } -52,
{ 67: } 0,
{ 68: } 0,
{ 69: } -58,
{ 70: } 0,
{ 71: } -55,
{ 72: } -3,
{ 73: } -4,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -30,
{ 79: } -32,
{ 80: } -84,
{ 81: } -73,
{ 82: } -65,
{ 83: } -68,
{ 84: } -53,
{ 85: } 0,
{ 86: } -56,
{ 87: } -61,
{ 88: } -5,
{ 89: } -62,
{ 90: } -79,
{ 91: } -18,
{ 92: } 0,
{ 93: } 0,
{ 94: } -54,
{ 95: } -57,
{ 96: } -84,
{ 97: } -88,
{ 98: } 0,
{ 99: } -92,
{ 100: } -90,
{ 101: } 0,
{ 102: } -86,
{ 103: } -85,
{ 104: } -87,
{ 105: } -27,
{ 106: } -21,
{ 107: } -89,
{ 108: } -12,
{ 109: } -74,
{ 110: } 0,
{ 111: } -100,
{ 112: } -19,
{ 113: } 0,
{ 114: } -96,
{ 115: } -94,
{ 116: } -98,
{ 117: } 0,
{ 118: } -91,
{ 119: } -22,
{ 120: } -23,
{ 121: } 0,
{ 122: } 0,
{ 123: } 0,
{ 124: } -97,
{ 125: } -102,
{ 126: } -95,
{ 127: } -99
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 10,
{ 3: } 11,
{ 4: } 11,
{ 5: } 11,
{ 6: } 11,
{ 7: } 13,
{ 8: } 24,
{ 9: } 24,
{ 10: } 24,
{ 11: } 24,
{ 12: } 24,
{ 13: } 24,
{ 14: } 24,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 24,
{ 19: } 24,
{ 20: } 24,
{ 21: } 24,
{ 22: } 26,
{ 23: } 29,
{ 24: } 29,
{ 25: } 29,
{ 26: } 29,
{ 27: } 39,
{ 28: } 40,
{ 29: } 40,
{ 30: } 45,
{ 31: } 50,
{ 32: } 55,
{ 33: } 60,
{ 34: } 60,
{ 35: } 60,
{ 36: } 60,
{ 37: } 61,
{ 38: } 66,
{ 39: } 66,
{ 40: } 66,
{ 41: } 66,
{ 42: } 77,
{ 43: } 77,
{ 44: } 77,
{ 45: } 79,
{ 46: } 83,
{ 47: } 87,
{ 48: } 91,
{ 49: } 95,
{ 50: } 95,
{ 51: } 95,
{ 52: } 95,
{ 53: } 95,
{ 54: } 95,
{ 55: } 97,
{ 56: } 97,
{ 57: } 97,
{ 58: } 97,
{ 59: } 99,
{ 60: } 99,
{ 61: } 101,
{ 62: } 101,
{ 63: } 101,
{ 64: } 101,
{ 65: } 101,
{ 66: } 101,
{ 67: } 101,
{ 68: } 114,
{ 69: } 128,
{ 70: } 128,
{ 71: } 142,
{ 72: } 142,
{ 73: } 142,
{ 74: } 142,
{ 75: } 155,
{ 76: } 168,
{ 77: } 181,
{ 78: } 182,
{ 79: } 182,
{ 80: } 182,
{ 81: } 182,
{ 82: } 182,
{ 83: } 182,
{ 84: } 182,
{ 85: } 182,
{ 86: } 186,
{ 87: } 186,
{ 88: } 186,
{ 89: } 186,
{ 90: } 186,
{ 91: } 186,
{ 92: } 186,
{ 93: } 198,
{ 94: } 199,
{ 95: } 199,
{ 96: } 199,
{ 97: } 199,
{ 98: } 199,
{ 99: } 205,
{ 100: } 205,
{ 101: } 205,
{ 102: } 208,
{ 103: } 208,
{ 104: } 208,
{ 105: } 208,
{ 106: } 208,
{ 107: } 208,
{ 108: } 208,
{ 109: } 208,
{ 110: } 208,
{ 111: } 220,
{ 112: } 220,
{ 113: } 220,
{ 114: } 222,
{ 115: } 222,
{ 116: } 222,
{ 117: } 222,
{ 118: } 228,
{ 119: } 228,
{ 120: } 228,
{ 121: } 228,
{ 122: } 236,
{ 123: } 244,
{ 124: } 252,
{ 125: } 252,
{ 126: } 252,
{ 127: } 252
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 9,
{ 2: } 10,
{ 3: } 10,
{ 4: } 10,
{ 5: } 10,
{ 6: } 12,
{ 7: } 23,
{ 8: } 23,
{ 9: } 23,
{ 10: } 23,
{ 11: } 23,
{ 12: } 23,
{ 13: } 23,
{ 14: } 23,
{ 15: } 23,
{ 16: } 23,
{ 17: } 23,
{ 18: } 23,
{ 19: } 23,
{ 20: } 23,
{ 21: } 25,
{ 22: } 28,
{ 23: } 28,
{ 24: } 28,
{ 25: } 28,
{ 26: } 38,
{ 27: } 39,
{ 28: } 39,
{ 29: } 44,
{ 30: } 49,
{ 31: } 54,
{ 32: } 59,
{ 33: } 59,
{ 34: } 59,
{ 35: } 59,
{ 36: } 60,
{ 37: } 65,
{ 38: } 65,
{ 39: } 65,
{ 40: } 65,
{ 41: } 76,
{ 42: } 76,
{ 43: } 76,
{ 44: } 78,
{ 45: } 82,
{ 46: } 86,
{ 47: } 90,
{ 48: } 94,
{ 49: } 94,
{ 50: } 94,
{ 51: } 94,
{ 52: } 94,
{ 53: } 94,
{ 54: } 96,
{ 55: } 96,
{ 56: } 96,
{ 57: } 96,
{ 58: } 98,
{ 59: } 98,
{ 60: } 100,
{ 61: } 100,
{ 62: } 100,
{ 63: } 100,
{ 64: } 100,
{ 65: } 100,
{ 66: } 100,
{ 67: } 113,
{ 68: } 127,
{ 69: } 127,
{ 70: } 141,
{ 71: } 141,
{ 72: } 141,
{ 73: } 141,
{ 74: } 154,
{ 75: } 167,
{ 76: } 180,
{ 77: } 181,
{ 78: } 181,
{ 79: } 181,
{ 80: } 181,
{ 81: } 181,
{ 82: } 181,
{ 83: } 181,
{ 84: } 181,
{ 85: } 185,
{ 86: } 185,
{ 87: } 185,
{ 88: } 185,
{ 89: } 185,
{ 90: } 185,
{ 91: } 185,
{ 92: } 197,
{ 93: } 198,
{ 94: } 198,
{ 95: } 198,
{ 96: } 198,
{ 97: } 198,
{ 98: } 204,
{ 99: } 204,
{ 100: } 204,
{ 101: } 207,
{ 102: } 207,
{ 103: } 207,
{ 104: } 207,
{ 105: } 207,
{ 106: } 207,
{ 107: } 207,
{ 108: } 207,
{ 109: } 207,
{ 110: } 219,
{ 111: } 219,
{ 112: } 219,
{ 113: } 221,
{ 114: } 221,
{ 115: } 221,
{ 116: } 221,
{ 117: } 227,
{ 118: } 227,
{ 119: } 227,
{ 120: } 227,
{ 121: } 235,
{ 122: } 243,
{ 123: } 251,
{ 124: } 251,
{ 125: } 251,
{ 126: } 251,
{ 127: } 251
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 12,
{ 3: } 12,
{ 4: } 12,
{ 5: } 13,
{ 6: } 14,
{ 7: } 15,
{ 8: } 17,
{ 9: } 18,
{ 10: } 19,
{ 11: } 20,
{ 12: } 21,
{ 13: } 21,
{ 14: } 21,
{ 15: } 21,
{ 16: } 21,
{ 17: } 21,
{ 18: } 21,
{ 19: } 21,
{ 20: } 21,
{ 21: } 21,
{ 22: } 22,
{ 23: } 25,
{ 24: } 25,
{ 25: } 25,
{ 26: } 25,
{ 27: } 28,
{ 28: } 29,
{ 29: } 29,
{ 30: } 31,
{ 31: } 33,
{ 32: } 35,
{ 33: } 37,
{ 34: } 37,
{ 35: } 37,
{ 36: } 37,
{ 37: } 39,
{ 38: } 44,
{ 39: } 45,
{ 40: } 45,
{ 41: } 45,
{ 42: } 48,
{ 43: } 48,
{ 44: } 48,
{ 45: } 49,
{ 46: } 54,
{ 47: } 59,
{ 48: } 64,
{ 49: } 69,
{ 50: } 69,
{ 51: } 70,
{ 52: } 70,
{ 53: } 70,
{ 54: } 70,
{ 55: } 72,
{ 56: } 73,
{ 57: } 73,
{ 58: } 73,
{ 59: } 74,
{ 60: } 74,
{ 61: } 76,
{ 62: } 76,
{ 63: } 76,
{ 64: } 76,
{ 65: } 76,
{ 66: } 76,
{ 67: } 76,
{ 68: } 81,
{ 69: } 82,
{ 70: } 82,
{ 71: } 83,
{ 72: } 83,
{ 73: } 83,
{ 74: } 83,
{ 75: } 88,
{ 76: } 93,
{ 77: } 98,
{ 78: } 99,
{ 79: } 99,
{ 80: } 99,
{ 81: } 100,
{ 82: } 101,
{ 83: } 101,
{ 84: } 101,
{ 85: } 101,
{ 86: } 105,
{ 87: } 105,
{ 88: } 105,
{ 89: } 105,
{ 90: } 105,
{ 91: } 106,
{ 92: } 106,
{ 93: } 114,
{ 94: } 116,
{ 95: } 116,
{ 96: } 116,
{ 97: } 117,
{ 98: } 117,
{ 99: } 118,
{ 100: } 118,
{ 101: } 119,
{ 102: } 122,
{ 103: } 122,
{ 104: } 122,
{ 105: } 122,
{ 106: } 122,
{ 107: } 122,
{ 108: } 122,
{ 109: } 122,
{ 110: } 122,
{ 111: } 130,
{ 112: } 130,
{ 113: } 130,
{ 114: } 131,
{ 115: } 132,
{ 116: } 133,
{ 117: } 134,
{ 118: } 135,
{ 119: } 135,
{ 120: } 135,
{ 121: } 135,
{ 122: } 139,
{ 123: } 143,
{ 124: } 147,
{ 125: } 147,
{ 126: } 147,
{ 127: } 147
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 11,
{ 2: } 11,
{ 3: } 11,
{ 4: } 12,
{ 5: } 13,
{ 6: } 14,
{ 7: } 16,
{ 8: } 17,
{ 9: } 18,
{ 10: } 19,
{ 11: } 20,
{ 12: } 20,
{ 13: } 20,
{ 14: } 20,
{ 15: } 20,
{ 16: } 20,
{ 17: } 20,
{ 18: } 20,
{ 19: } 20,
{ 20: } 20,
{ 21: } 21,
{ 22: } 24,
{ 23: } 24,
{ 24: } 24,
{ 25: } 24,
{ 26: } 27,
{ 27: } 28,
{ 28: } 28,
{ 29: } 30,
{ 30: } 32,
{ 31: } 34,
{ 32: } 36,
{ 33: } 36,
{ 34: } 36,
{ 35: } 36,
{ 36: } 38,
{ 37: } 43,
{ 38: } 44,
{ 39: } 44,
{ 40: } 44,
{ 41: } 47,
{ 42: } 47,
{ 43: } 47,
{ 44: } 48,
{ 45: } 53,
{ 46: } 58,
{ 47: } 63,
{ 48: } 68,
{ 49: } 68,
{ 50: } 69,
{ 51: } 69,
{ 52: } 69,
{ 53: } 69,
{ 54: } 71,
{ 55: } 72,
{ 56: } 72,
{ 57: } 72,
{ 58: } 73,
{ 59: } 73,
{ 60: } 75,
{ 61: } 75,
{ 62: } 75,
{ 63: } 75,
{ 64: } 75,
{ 65: } 75,
{ 66: } 75,
{ 67: } 80,
{ 68: } 81,
{ 69: } 81,
{ 70: } 82,
{ 71: } 82,
{ 72: } 82,
{ 73: } 82,
{ 74: } 87,
{ 75: } 92,
{ 76: } 97,
{ 77: } 98,
{ 78: } 98,
{ 79: } 98,
{ 80: } 99,
{ 81: } 100,
{ 82: } 100,
{ 83: } 100,
{ 84: } 100,
{ 85: } 104,
{ 86: } 104,
{ 87: } 104,
{ 88: } 104,
{ 89: } 104,
{ 90: } 105,
{ 91: } 105,
{ 92: } 113,
{ 93: } 115,
{ 94: } 115,
{ 95: } 115,
{ 96: } 116,
{ 97: } 116,
{ 98: } 117,
{ 99: } 117,
{ 100: } 118,
{ 101: } 121,
{ 102: } 121,
{ 103: } 121,
{ 104: } 121,
{ 105: } 121,
{ 106: } 121,
{ 107: } 121,
{ 108: } 121,
{ 109: } 121,
{ 110: } 129,
{ 111: } 129,
{ 112: } 129,
{ 113: } 130,
{ 114: } 131,
{ 115: } 132,
{ 116: } 133,
{ 117: } 134,
{ 118: } 134,
{ 119: } 134,
{ 120: } 134,
{ 121: } 138,
{ 122: } 142,
{ 123: } 146,
{ 124: } 146,
{ 125: } 146,
{ 126: } 146,
{ 127: } 146
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -3 ),
{ 2: } ( len: 1; sym: -4 ),
{ 3: } ( len: 1; sym: -5 ),
{ 4: } ( len: 1; sym: -6 ),
{ 5: } ( len: 1; sym: -7 ),
{ 6: } ( len: 1; sym: -8 ),
{ 7: } ( len: 1; sym: -9 ),
{ 8: } ( len: 1; sym: -10 ),
{ 9: } ( len: 1; sym: -11 ),
{ 10: } ( len: 1; sym: -12 ),
{ 11: } ( len: 1; sym: -13 ),
{ 12: } ( len: 1; sym: -14 ),
{ 13: } ( len: 1; sym: -15 ),
{ 14: } ( len: 1; sym: -16 ),
{ 15: } ( len: 1; sym: -17 ),
{ 16: } ( len: 1; sym: -17 ),
{ 17: } ( len: 1; sym: -18 ),
{ 18: } ( len: 1; sym: -19 ),
{ 19: } ( len: 1; sym: -20 ),
{ 20: } ( len: 1; sym: -21 ),
{ 21: } ( len: 1; sym: -22 ),
{ 22: } ( len: 1; sym: -23 ),
{ 23: } ( len: 1; sym: -23 ),
{ 24: } ( len: 1; sym: -24 ),
{ 25: } ( len: 1; sym: -25 ),
{ 26: } ( len: 1; sym: -25 ),
{ 27: } ( len: 1; sym: -26 ),
{ 28: } ( len: 0; sym: -29 ),
{ 29: } ( len: 0; sym: -31 ),
{ 30: } ( len: 6; sym: -2 ),
{ 31: } ( len: 0; sym: -30 ),
{ 32: } ( len: 1; sym: -30 ),
{ 33: } ( len: 0; sym: -27 ),
{ 34: } ( len: 2; sym: -27 ),
{ 35: } ( len: 2; sym: -27 ),
{ 36: } ( len: 2; sym: -32 ),
{ 37: } ( len: 2; sym: -32 ),
{ 38: } ( len: 0; sym: -33 ),
{ 39: } ( len: 3; sym: -32 ),
{ 40: } ( len: 0; sym: -35 ),
{ 41: } ( len: 4; sym: -32 ),
{ 42: } ( len: 0; sym: -37 ),
{ 43: } ( len: 4; sym: -32 ),
{ 44: } ( len: 0; sym: -38 ),
{ 45: } ( len: 4; sym: -32 ),
{ 46: } ( len: 0; sym: -39 ),
{ 47: } ( len: 4; sym: -32 ),
{ 48: } ( len: 3; sym: -32 ),
{ 49: } ( len: 2; sym: -32 ),
{ 50: } ( len: 0; sym: -34 ),
{ 51: } ( len: 3; sym: -34 ),
{ 52: } ( len: 1; sym: -36 ),
{ 53: } ( len: 2; sym: -36 ),
{ 54: } ( len: 3; sym: -36 ),
{ 55: } ( len: 1; sym: -36 ),
{ 56: } ( len: 2; sym: -36 ),
{ 57: } ( len: 3; sym: -36 ),
{ 58: } ( len: 1; sym: -41 ),
{ 59: } ( len: 1; sym: -41 ),
{ 60: } ( len: 1; sym: -41 ),
{ 61: } ( len: 2; sym: -41 ),
{ 62: } ( len: 2; sym: -41 ),
{ 63: } ( len: 1; sym: -40 ),
{ 64: } ( len: 2; sym: -40 ),
{ 65: } ( len: 3; sym: -40 ),
{ 66: } ( len: 1; sym: -40 ),
{ 67: } ( len: 2; sym: -40 ),
{ 68: } ( len: 3; sym: -40 ),
{ 69: } ( len: 1; sym: -42 ),
{ 70: } ( len: 0; sym: -44 ),
{ 71: } ( len: 2; sym: -28 ),
{ 72: } ( len: 0; sym: -45 ),
{ 73: } ( len: 0; sym: -46 ),
{ 74: } ( len: 5; sym: -28 ),
{ 75: } ( len: 2; sym: -28 ),
{ 76: } ( len: 1; sym: -28 ),
{ 77: } ( len: 2; sym: -28 ),
{ 78: } ( len: 0; sym: -48 ),
{ 79: } ( len: 0; sym: -50 ),
{ 80: } ( len: 6; sym: -43 ),
{ 81: } ( len: 1; sym: -47 ),
{ 82: } ( len: 0; sym: -52 ),
{ 83: } ( len: 4; sym: -47 ),
{ 84: } ( len: 0; sym: -49 ),
{ 85: } ( len: 2; sym: -49 ),
{ 86: } ( len: 2; sym: -49 ),
{ 87: } ( len: 2; sym: -49 ),
{ 88: } ( len: 2; sym: -49 ),
{ 89: } ( len: 2; sym: -49 ),
{ 90: } ( len: 0; sym: -54 ),
{ 91: } ( len: 3; sym: -53 ),
{ 92: } ( len: 1; sym: -53 ),
{ 93: } ( len: 0; sym: -51 ),
{ 94: } ( len: 0; sym: -56 ),
{ 95: } ( len: 4; sym: -51 ),
{ 96: } ( len: 0; sym: -57 ),
{ 97: } ( len: 4; sym: -51 ),
{ 98: } ( len: 0; sym: -58 ),
{ 99: } ( len: 4; sym: -51 ),
{ 100: } ( len: 2; sym: -51 ),
{ 101: } ( len: 0; sym: -55 ),
{ 102: } ( len: 1; sym: -55 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


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
