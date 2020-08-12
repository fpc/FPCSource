
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)

const INCOMLINE = 2;
const INCOMMENT = 4;
const INSTRING = 6;



function yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                        start(INCOMLINE);
  2:
                        begin start(0); unget_char(nl); end;
  3:
                        yymore;

  4:
                        start(INCOMMENT);
  5:
                        ;
  6:
                        start(0);
  7:
                        return(ILLEGAL);

  8:
                         begin
                           if ypreproc.isdefine(yytext) then begin
                             unget_char(' ');
                             unget_string(ypreproc.getdefine(yytext));
                           end else
                             return(ID);
                         end;
  9:
                         return(ID);
(*
[ \t\n\f]               ;
#define
#else
#endif
#ifdef
#ifndef
#include
#undef
              
.                       begin
                           writeln(erroutput, 'Illegal character in line ',yylineno, ' col ', yycolno);
                           writeln(erroutput, '"',yyline,'"');
                           return(ILLEGAL);
                        end;          
*)
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 13;
yynmatches = 13;
yyntrans   = 21;
yynstates  = 20;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  { 3: }
  { 4: }
  { 5: }
  { 6: }
  { 7: }
  { 8: }
  9,
  { 9: }
  8,
  9,
  { 10: }
  9,
  { 11: }
  2,
  { 12: }
  3,
  { 13: }
  5,
  { 14: }
  5,
  { 15: }
  7,
  { 16: }
  1,
  { 17: }
  4,
  { 18: }
  8,
  { 19: }
  6
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
  9,
{ 9: }
  8,
  9,
{ 10: }
  9,
{ 11: }
  2,
{ 12: }
  3,
{ 13: }
  5,
{ 14: }
  5,
{ 15: }
  7,
{ 16: }
  1,
{ 17: }
  4,
{ 18: }
  8,
{ 19: }
  6
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#9,#11..'.','0'..'@','['..'^','`','{'..#255 ]; s: 10),
  ( cc: [ '/' ]; s: 8),
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 9),
{ 1: }
  ( cc: [ #1..#9,#11..'.','0'..'@','['..'^','`','{'..#255 ]; s: 10),
  ( cc: [ '/' ]; s: 8),
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 9),
{ 2: }
  ( cc: [ #1..#9,#11..#255 ]; s: 12),
  ( cc: [ #10 ]; s: 11),
{ 3: }
  ( cc: [ #1..#9,#11..#255 ]; s: 12),
  ( cc: [ #10 ]; s: 11),
{ 4: }
  ( cc: [ #0 ]; s: 15),
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 13),
  ( cc: [ '*' ]; s: 14),
{ 5: }
  ( cc: [ #0 ]; s: 15),
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 13),
  ( cc: [ '*' ]; s: 14),
{ 6: }
{ 7: }
{ 8: }
  ( cc: [ '*' ]; s: 17),
  ( cc: [ '/' ]; s: 16),
{ 9: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 18),
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
  ( cc: [ '/' ]; s: 19),
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 18)
{ 19: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 1,
{ 8: } 1,
{ 9: } 2,
{ 10: } 4,
{ 11: } 5,
{ 12: } 6,
{ 13: } 7,
{ 14: } 8,
{ 15: } 9,
{ 16: } 10,
{ 17: } 11,
{ 18: } 12,
{ 19: } 13
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 0,
{ 7: } 0,
{ 8: } 1,
{ 9: } 3,
{ 10: } 4,
{ 11: } 5,
{ 12: } 6,
{ 13: } 7,
{ 14: } 8,
{ 15: } 9,
{ 16: } 10,
{ 17: } 11,
{ 18: } 12,
{ 19: } 13
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 1,
{ 4: } 1,
{ 5: } 1,
{ 6: } 1,
{ 7: } 1,
{ 8: } 1,
{ 9: } 2,
{ 10: } 4,
{ 11: } 5,
{ 12: } 6,
{ 13: } 7,
{ 14: } 8,
{ 15: } 9,
{ 16: } 10,
{ 17: } 11,
{ 18: } 12,
{ 19: } 13
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } 0,
{ 7: } 0,
{ 8: } 1,
{ 9: } 3,
{ 10: } 4,
{ 11: } 5,
{ 12: } 6,
{ 13: } 7,
{ 14: } 8,
{ 15: } 9,
{ 16: } 10,
{ 17: } 11,
{ 18: } 12,
{ 19: } 13
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 4,
{ 2: } 7,
{ 3: } 9,
{ 4: } 11,
{ 5: } 14,
{ 6: } 17,
{ 7: } 17,
{ 8: } 17,
{ 9: } 19,
{ 10: } 20,
{ 11: } 20,
{ 12: } 20,
{ 13: } 20,
{ 14: } 20,
{ 15: } 21,
{ 16: } 21,
{ 17: } 21,
{ 18: } 21,
{ 19: } 22
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 3,
{ 1: } 6,
{ 2: } 8,
{ 3: } 10,
{ 4: } 13,
{ 5: } 16,
{ 6: } 16,
{ 7: } 16,
{ 8: } 18,
{ 9: } 19,
{ 10: } 19,
{ 11: } 19,
{ 12: } 19,
{ 13: } 19,
{ 14: } 20,
{ 15: } 20,
{ 16: } 20,
{ 17: } 20,
{ 18: } 21,
{ 19: } 21
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap() then
    begin
      yyclear;
      return(0);
    end;

  if not yydone then goto start;

  yylex := yyretval;

end(*yylex*);



// end.





