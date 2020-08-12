
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)
var
  kwtmp: integer;
  strbuf: string;

const
  KeywordDefs: array [0..23] of TIdentMapEntry = (
    // attribs
    (Value: _LANGUAGE;             Name: 'LANGUAGE'),
    (Value: _CHARACTERISTICS;      Name: 'CHARACTERISTICS'),
    (Value: _VERSION;              Name: 'VERSION'),
    (Value: _MOVEABLE;             Name: 'MOVEABLE'),
    (Value: _FIXED;                Name: 'FIXED'),
    (Value: _PURE;                 Name: 'PURE'),
    (Value: _IMPURE;               Name: 'IMPURE'),
    (Value: _PRELOAD;              Name: 'PRELOAD'),
    (Value: _LOADONCALL;           Name: 'LOADONCALL'),
    (Value: _DISCARDABLE;          Name: 'DISCARDABLE'),
    // resource names
    (Value: _ANICURSOR;            Name: 'ANICURSOR'),
    (Value: _ANIICON;              Name: 'ANIICON'),
    (Value: _BITMAP;               Name: 'BITMAP'),
    (Value: _CURSOR;               Name: 'CURSOR'),
    (Value: _DLGINCLUDE;           Name: 'DLGINCLUDE'),
    (Value: _DLGINIT;              Name: 'DLGINIT'),
    (Value: _HTML;                 Name: 'HTML'),
    (Value: _ICON;                 Name: 'ICON'),
    (Value: _MANIFEST;             Name: 'MANIFEST'),
    (Value: _MESSAGETABLE;         Name: 'MESSAGETABLE'),
    (Value: _PLUGPLAY;             Name: 'PLUGPLAY'),
    (Value: _STRINGTABLE;          Name: 'STRINGTABLE'),
    (Value: _RCDATA;               Name: 'RCDATA'),
    (Value: _VXD;                  Name: 'VXD')
  );

function KeywordToInt(k: string; out kw: integer): boolean;
var
  i: integer;
begin
  Result:= False;
  for i:= low(KeywordDefs) to high(KeywordDefs) do begin
    if k = KeywordDefs[i].Name then begin
      kw:= KeywordDefs[i].Value;
      Exit(True);
    end;
  end;
end;

const INCOMLINE = 2;
const INCOMMENT = 4;
const INSTRING = 6;
const INSTRINGL = 8;



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
                        return(_ILLEGAL);

  8:
                        return(_NUMDECIMAL);
  9:
                        return(_NUMHEX);
  10:
                        begin start(INSTRING); strbuf:= ''; end;
  11:
                        strbuf:= strbuf + '"';
  12:
                        begin
                          start(0);
                          yytext:= strbuf;
                          return(_QUOTEDSTR);
                        end;
  13:
                        ;
  14:
                        return(_ILLEGAL);
  15:
                        strbuf:= strbuf + yytext;

  16:
                        return(_BEGIN);
  17:
                        return(_END);
  18:
                        begin
                          if ypreproc.isdefine(yytext) then begin
                            unget_char(' ');
                            unget_string(ypreproc.getdefine(yytext));
                          end else
                            if KeywordToInt(yytext, kwtmp) then
                              return(kwtmp)
                            else
                              return(_ID);
                         end;
  19:
                        ;
  20:
                        returnc(',');
  21:
                        return(_ILLEGAL);
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 56;
yynmatches = 56;
yyntrans   = 77;
yynstates  = 49;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  8,
  { 1: }
  8,
  { 2: }
  { 3: }
  { 4: }
  { 5: }
  { 6: }
  { 7: }
  { 8: }
  { 9: }
  { 10: }
  21,
  { 11: }
  8,
  21,
  { 12: }
  8,
  18,
  21,
  { 13: }
  8,
  21,
  { 14: }
  10,
  21,
  { 15: }
  18,
  21,
  { 16: }
  16,
  21,
  { 17: }
  18,
  21,
  { 18: }
  17,
  21,
  { 19: }
  18,
  21,
  { 20: }
  19,
  { 21: }
  20,
  21,
  { 22: }
  19,
  21,
  { 23: }
  21,
  { 24: }
  2,
  { 25: }
  3,
  { 26: }
  5,
  { 27: }
  5,
  { 28: }
  7,
  { 29: }
  12,
  15,
  { 30: }
  15,
  { 31: }
  14,
  { 32: }
  15,
  { 33: }
  1,
  { 34: }
  4,
  { 35: }
  8,
  { 36: }
  8,
  { 37: }
  18,
  { 38: }
  9,
  { 39: }
  18,
  { 40: }
  18,
  { 41: }
  6,
  { 42: }
  11,
  { 43: }
  13,
  { 44: }
  9,
  { 45: }
  18,
  { 46: }
  17,
  18,
  { 47: }
  18,
  { 48: }
  16,
  18
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
  8,
{ 1: }
  8,
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  21,
{ 11: }
  8,
  21,
{ 12: }
  8,
  18,
  21,
{ 13: }
  8,
  21,
{ 14: }
  10,
  21,
{ 15: }
  18,
  21,
{ 16: }
  16,
  21,
{ 17: }
  18,
  21,
{ 18: }
  17,
  21,
{ 19: }
  18,
  21,
{ 20: }
  19,
{ 21: }
  20,
  21,
{ 22: }
  19,
  21,
{ 23: }
  21,
{ 24: }
  2,
{ 25: }
  3,
{ 26: }
  5,
{ 27: }
  5,
{ 28: }
  7,
{ 29: }
  12,
  15,
{ 30: }
  15,
{ 31: }
  14,
{ 32: }
  15,
{ 33: }
  1,
{ 34: }
  4,
{ 35: }
  8,
{ 36: }
  8,
{ 37: }
  18,
{ 38: }
  9,
{ 39: }
  18,
{ 40: }
  18,
{ 41: }
  6,
{ 42: }
  11,
{ 43: }
  13,
{ 44: }
  9,
{ 45: }
  18,
{ 46: }
  17,
  18,
{ 47: }
  18,
{ 48: }
  16,
  18
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'!','#'..'+','-','.',
            ':'..'@','['..'^','`','|','~'..#255 ]; s: 23),
  ( cc: [ #9,#12,' ' ]; s: 22),
  ( cc: [ #10 ]; s: 20),
  ( cc: [ '"' ]; s: 14),
  ( cc: [ ',' ]; s: 21),
  ( cc: [ '/' ]; s: 10),
  ( cc: [ '0' ]; s: 13),
  ( cc: [ '1'..'9' ]; s: 11),
  ( cc: [ 'A','C','D','F'..'K','M'..'Z','_','a'..'z' ]; s: 19),
  ( cc: [ 'B' ]; s: 15),
  ( cc: [ 'E' ]; s: 17),
  ( cc: [ 'L' ]; s: 12),
  ( cc: [ '{' ]; s: 16),
  ( cc: [ '}' ]; s: 18),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'!','#'..'+','-','.',
            ':'..'@','['..'^','`','|','~'..#255 ]; s: 23),
  ( cc: [ #9,#12,' ' ]; s: 22),
  ( cc: [ #10 ]; s: 20),
  ( cc: [ '"' ]; s: 14),
  ( cc: [ ',' ]; s: 21),
  ( cc: [ '/' ]; s: 10),
  ( cc: [ '0' ]; s: 13),
  ( cc: [ '1'..'9' ]; s: 11),
  ( cc: [ 'A','C','D','F'..'K','M'..'Z','_','a'..'z' ]; s: 19),
  ( cc: [ 'B' ]; s: 15),
  ( cc: [ 'E' ]; s: 17),
  ( cc: [ 'L' ]; s: 12),
  ( cc: [ '{' ]; s: 16),
  ( cc: [ '}' ]; s: 18),
{ 2: }
  ( cc: [ #1..#9,#11..#255 ]; s: 25),
  ( cc: [ #10 ]; s: 24),
{ 3: }
  ( cc: [ #1..#9,#11..#255 ]; s: 25),
  ( cc: [ #10 ]; s: 24),
{ 4: }
  ( cc: [ #0 ]; s: 28),
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 26),
  ( cc: [ '*' ]; s: 27),
{ 5: }
  ( cc: [ #0 ]; s: 28),
  ( cc: [ #1..#9,#11..')','+'..#255 ]; s: 26),
  ( cc: [ '*' ]; s: 27),
{ 6: }
  ( cc: [ #1..#9,#11..'!','#'..'[',']'..#255 ]; s: 32),
  ( cc: [ #10 ]; s: 31),
  ( cc: [ '"' ]; s: 29),
  ( cc: [ '\' ]; s: 30),
{ 7: }
  ( cc: [ #1..#9,#11..'!','#'..'[',']'..#255 ]; s: 32),
  ( cc: [ #10 ]; s: 31),
  ( cc: [ '"' ]; s: 29),
  ( cc: [ '\' ]; s: 30),
{ 8: }
{ 9: }
{ 10: }
  ( cc: [ '*' ]; s: 34),
  ( cc: [ '/' ]; s: 33),
{ 11: }
  ( cc: [ '0'..'9' ]; s: 35),
  ( cc: [ 'L' ]; s: 36),
{ 12: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 37),
{ 13: }
  ( cc: [ '0'..'9' ]; s: 35),
  ( cc: [ 'L' ]; s: 36),
  ( cc: [ 'x' ]; s: 38),
{ 14: }
{ 15: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'E' ]; s: 39),
{ 16: }
{ 17: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'N' ]; s: 40),
{ 18: }
{ 19: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 37),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( cc: [ '/' ]; s: 41),
{ 28: }
{ 29: }
  ( cc: [ '"' ]; s: 42),
{ 30: }
  ( cc: [ #10 ]; s: 43),
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( cc: [ '0'..'9' ]; s: 35),
  ( cc: [ 'L' ]; s: 36),
{ 36: }
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 37),
{ 38: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 38),
  ( cc: [ 'L' ]; s: 44),
{ 39: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'G' ]; s: 45),
{ 40: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'D' ]; s: 46),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'I' ]; s: 47),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 37),
{ 47: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 37),
  ( cc: [ 'N' ]; s: 48),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 37)
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 3,
{ 5: } 3,
{ 6: } 3,
{ 7: } 3,
{ 8: } 3,
{ 9: } 3,
{ 10: } 3,
{ 11: } 4,
{ 12: } 6,
{ 13: } 9,
{ 14: } 11,
{ 15: } 13,
{ 16: } 15,
{ 17: } 17,
{ 18: } 19,
{ 19: } 21,
{ 20: } 23,
{ 21: } 24,
{ 22: } 26,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 34,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 40,
{ 35: } 41,
{ 36: } 42,
{ 37: } 43,
{ 38: } 44,
{ 39: } 45,
{ 40: } 46,
{ 41: } 47,
{ 42: } 48,
{ 43: } 49,
{ 44: } 50,
{ 45: } 51,
{ 46: } 52,
{ 47: } 54,
{ 48: } 55
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 2,
{ 3: } 2,
{ 4: } 2,
{ 5: } 2,
{ 6: } 2,
{ 7: } 2,
{ 8: } 2,
{ 9: } 2,
{ 10: } 3,
{ 11: } 5,
{ 12: } 8,
{ 13: } 10,
{ 14: } 12,
{ 15: } 14,
{ 16: } 16,
{ 17: } 18,
{ 18: } 20,
{ 19: } 22,
{ 20: } 23,
{ 21: } 25,
{ 22: } 27,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 35,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 40,
{ 35: } 41,
{ 36: } 42,
{ 37: } 43,
{ 38: } 44,
{ 39: } 45,
{ 40: } 46,
{ 41: } 47,
{ 42: } 48,
{ 43: } 49,
{ 44: } 50,
{ 45: } 51,
{ 46: } 53,
{ 47: } 54,
{ 48: } 56
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 3,
{ 5: } 3,
{ 6: } 3,
{ 7: } 3,
{ 8: } 3,
{ 9: } 3,
{ 10: } 3,
{ 11: } 4,
{ 12: } 6,
{ 13: } 9,
{ 14: } 11,
{ 15: } 13,
{ 16: } 15,
{ 17: } 17,
{ 18: } 19,
{ 19: } 21,
{ 20: } 23,
{ 21: } 24,
{ 22: } 26,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 34,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 40,
{ 35: } 41,
{ 36: } 42,
{ 37: } 43,
{ 38: } 44,
{ 39: } 45,
{ 40: } 46,
{ 41: } 47,
{ 42: } 48,
{ 43: } 49,
{ 44: } 50,
{ 45: } 51,
{ 46: } 52,
{ 47: } 54,
{ 48: } 55
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 2,
{ 3: } 2,
{ 4: } 2,
{ 5: } 2,
{ 6: } 2,
{ 7: } 2,
{ 8: } 2,
{ 9: } 2,
{ 10: } 3,
{ 11: } 5,
{ 12: } 8,
{ 13: } 10,
{ 14: } 12,
{ 15: } 14,
{ 16: } 16,
{ 17: } 18,
{ 18: } 20,
{ 19: } 22,
{ 20: } 23,
{ 21: } 25,
{ 22: } 27,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 35,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 40,
{ 35: } 41,
{ 36: } 42,
{ 37: } 43,
{ 38: } 44,
{ 39: } 45,
{ 40: } 46,
{ 41: } 47,
{ 42: } 48,
{ 43: } 49,
{ 44: } 50,
{ 45: } 51,
{ 46: } 53,
{ 47: } 54,
{ 48: } 56
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 15,
{ 2: } 29,
{ 3: } 31,
{ 4: } 33,
{ 5: } 36,
{ 6: } 39,
{ 7: } 43,
{ 8: } 47,
{ 9: } 47,
{ 10: } 47,
{ 11: } 49,
{ 12: } 51,
{ 13: } 52,
{ 14: } 55,
{ 15: } 55,
{ 16: } 57,
{ 17: } 57,
{ 18: } 59,
{ 19: } 59,
{ 20: } 60,
{ 21: } 60,
{ 22: } 60,
{ 23: } 60,
{ 24: } 60,
{ 25: } 60,
{ 26: } 60,
{ 27: } 60,
{ 28: } 61,
{ 29: } 61,
{ 30: } 62,
{ 31: } 63,
{ 32: } 63,
{ 33: } 63,
{ 34: } 63,
{ 35: } 63,
{ 36: } 65,
{ 37: } 65,
{ 38: } 66,
{ 39: } 68,
{ 40: } 70,
{ 41: } 72,
{ 42: } 72,
{ 43: } 72,
{ 44: } 72,
{ 45: } 72,
{ 46: } 74,
{ 47: } 75,
{ 48: } 77
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 14,
{ 1: } 28,
{ 2: } 30,
{ 3: } 32,
{ 4: } 35,
{ 5: } 38,
{ 6: } 42,
{ 7: } 46,
{ 8: } 46,
{ 9: } 46,
{ 10: } 48,
{ 11: } 50,
{ 12: } 51,
{ 13: } 54,
{ 14: } 54,
{ 15: } 56,
{ 16: } 56,
{ 17: } 58,
{ 18: } 58,
{ 19: } 59,
{ 20: } 59,
{ 21: } 59,
{ 22: } 59,
{ 23: } 59,
{ 24: } 59,
{ 25: } 59,
{ 26: } 59,
{ 27: } 60,
{ 28: } 60,
{ 29: } 61,
{ 30: } 62,
{ 31: } 62,
{ 32: } 62,
{ 33: } 62,
{ 34: } 62,
{ 35: } 64,
{ 36: } 64,
{ 37: } 65,
{ 38: } 67,
{ 39: } 69,
{ 40: } 71,
{ 41: } 71,
{ 42: } 71,
{ 43: } 71,
{ 44: } 71,
{ 45: } 73,
{ 46: } 74,
{ 47: } 76,
{ 48: } 77
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





