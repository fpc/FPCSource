
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

(*
Vorspann
 ****************************************************************************)

unit rcparser;

{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, StrUtils, lexlib, yacclib, resource;

function yyparse : Integer;

var
  aktresources: TResources;
  opt_code_page: TSystemCodePage;
  yyfilename: AnsiString;
  yyparseresult: YYSType;

procedure PragmaCodePage(cp: string);

{$DEFINE INC_HEADER}
{$I yyinclude.pp}
{$I yypreproc.pp}
{$UNDEF INC_HEADER}

implementation

procedure yyerror ( msg : String );
begin
  writeln(ErrOutput, yyfilename, '(',yylineno,':',yycolno,'): at "',yytext,'"');
  WriteLn(ErrOutput, '  ',msg);
end(*yyerrmsg*);

{$I yyinclude.pp}
{$I yypreproc.pp}

(* I/O routines: *)

const nl = #10;  (* newline character *)

const max_chars = 2048;

var
  bufptr : Integer;
  buf    : array [1..max_chars] of Char;

function rc_get_char : Char;
  var i : Integer;
      ok : boolean;
  begin
    if (bufptr=0) and not eof(yyinput) then
      begin
        repeat
          readln(yyinput, yyline);
          inc(yylineno); yycolno := 1;
          ok:= ypreproc.useline(yyline);
        until (ok or eof(yyinput));
        if ok then begin
          buf[1] := nl;
          for i := 1 to length(yyline) do
            buf[i+1] := yyline[length(yyline)-i+1];
          inc(bufptr, length(yyline)+1);
        end;
      end;
    if bufptr>0 then
      begin
        rc_get_char := buf[bufptr];
        dec(bufptr);
        inc(yycolno);
      end
    else
      rc_get_char := #0;
  end(*get_char*);

procedure rc_unget_char ( c : Char );
  begin
    if bufptr=max_chars then yyerror('input buffer overflow');
    inc(bufptr);
    dec(yycolno);
    buf[bufptr] := c;
  end(*unget_char*);

procedure unget_string(s: string);
var
  i: integer;
begin
  for i:= Length(s) downto 1 do
    rc_unget_char(s[i]);
end;

procedure PragmaCodePage(cp: string);
var cpi: integer;
begin
  if Uppercase(cp) = 'DEFAULT' then
    opt_code_page:= DefaultFileSystemCodePage
  else begin
    if TryStrToInt(cp, cpi) and (cpi>=0) and (cpi<=high(TSystemCodePage)) then
      opt_code_page:= cpi
    else
      yyerror('Invalid code_page pragma: "' + cp + '"');
  end;
end;

type
  rcnumtype = record
    v: LongWord;
    long: boolean;
  end;

var
  aktresource: TAbstractResource;
  language: TLangID;
  filestream: TFileStream;

procedure create_resource(aId, aType: TResourceDesc; aClass: TResourceClass);
var
  r: TAbstractResource;
begin
  r:= aClass.Create(aId, aType);
  r.LangID:= language;
  aktresources.Add(r);
  aktresource:= r;
end;

procedure create_resource(aId, aType: TResourceDesc); overload;
begin
  create_resource(aId, aType, TGenericResource);
end;

procedure assign_custom_stream(fn: string);
var
  fs: TFileStream;
begin
  fs:= TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  aktresource.SetCustomRawDataStream(fs);
end;


var
  yycapture: AnsiString;
const _ILLEGAL = 257;
const _NUMDECIMAL = 258;
const _NUMHEX = 259;
const _NUMDECIMALL = 260;
const _NUMHEXL = 261;
const _QUOTEDSTR = 262;
const _BEGIN = 263;
const _END = 264;
const _LANGUAGE = 265;
const _CHARACTERISTICS = 266;
const _VERSION = 267;
const _MOVEABLE = 268;
const _FIXED = 269;
const _PURE = 270;
const _IMPURE = 271;
const _PRELOAD = 272;
const _LOADONCALL = 273;
const _DISCARDABLE = 274;
const _ID = 275;

type YYSType = record case Integer of
                 1 : ( yyString : String );
                 2 : ( yyTMemoryStream : TMemoryStream );
                 3 : ( yyTResourceDesc : TResourceDesc );
                 4 : ( yyrcnumtype : rcnumtype );
               end(*YYSType*);

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
       end;
   2 : begin
         yyval := yyv[yysp-1];
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
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
   7 : begin
         assign_custom_stream(yyv[yysp-0].yyString); 
       end;
   8 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
   9 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-1].yyTMemoryStream); 
       end;
  10 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcnumtype.v); 
       end;
  11 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyString); 
       end;
  12 : begin
         yyval := yyv[yysp-1];
       end;
  13 : begin
         aktresource.LangID:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  14 : begin
         aktresource.Characteristics:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  15 : begin
         aktresource.Version:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  16 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; 
       end;
  17 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; 
       end;
  18 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; 
       end;
  19 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; 
       end;
  20 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; 
       end;
  21 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; 
       end;
  22 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; 
       end;
  23 : begin
       end;
  24 : begin
         language:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         yyval.yyrcnumtype.v:= StrToInt(yytext); yyval.yyrcnumtype.long:= False; 
       end;
  27 : begin
         yyval.yyrcnumtype.v:= StrToInt(Copy(yytext,1,length(yytext)-1)); yyval.yyrcnumtype.long:= True; 
       end;
  28 : begin
         yyval.yyrcnumtype.v:= StrToInt('$'+Copy(yytext,3,Maxint)); yyval.yyrcnumtype.long:= False; 
       end;
  29 : begin
         yyval.yyrcnumtype.v:= StrToInt('$'+Copy(yytext,3,length(yytext)-3)); yyval.yyrcnumtype.long:= True; 
       end;
  30 : begin
         yyval.yyString:= yytext; 
       end;
  31 : begin
         yyval.yyString:= yytext; 
       end;
  32 : begin
         yyval.yyString:= yytext; 
       end;
  33 : begin
         yyval.yyString:= yytext; 
       end;
  34 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  35 : begin
         yyval := yyv[yysp-1];
       end;
  36 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  37 : begin
         yyval := yyv[yysp-3];
       end;
  38 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         yyval.yyTMemoryStream.WriteBuffer(yyv[yysp-0].yyString[1], Length(yyv[yysp-0].yyString));
         
       end;
  39 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         if yyv[yysp-0].yyrcnumtype.long then
         yyval.yyTMemoryStream.WriteDWord(NtoLE(yyv[yysp-0].yyrcnumtype.v))
         else
         yyval.yyTMemoryStream.WriteWord(NtoLE(Word(yyv[yysp-0].yyrcnumtype.v)));
         
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

yynacts   = 120;
yyngotos  = 40;
yynstates = 54;
yynrules  = 39;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
  ( sym: 262; act: 13 ),
  ( sym: 265; act: 14 ),
  ( sym: 275; act: 15 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
  ( sym: 262; act: 13 ),
  ( sym: 275; act: 15 ),
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 15: }
{ 16: }
  ( sym: 262; act: -6 ),
  ( sym: 265; act: -6 ),
  ( sym: 266; act: -6 ),
  ( sym: 267; act: -6 ),
  ( sym: 268; act: -6 ),
  ( sym: 269; act: -6 ),
  ( sym: 270; act: -6 ),
  ( sym: 271; act: -6 ),
  ( sym: 272; act: -6 ),
  ( sym: 273; act: -6 ),
  ( sym: 274; act: -6 ),
  ( sym: 263; act: -8 ),
{ 17: }
{ 18: }
  ( sym: 44; act: 21 ),
{ 19: }
  ( sym: 265; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 268; act: 26 ),
  ( sym: 269; act: 27 ),
  ( sym: 270; act: 28 ),
  ( sym: 271; act: 29 ),
  ( sym: 272; act: 30 ),
  ( sym: 273; act: 31 ),
  ( sym: 274; act: 32 ),
  ( sym: 263; act: -23 ),
{ 20: }
  ( sym: 265; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 268; act: 26 ),
  ( sym: 269; act: 27 ),
  ( sym: 270; act: 28 ),
  ( sym: 271; act: 29 ),
  ( sym: 272; act: 30 ),
  ( sym: 273; act: 31 ),
  ( sym: 274; act: 32 ),
  ( sym: 262; act: -23 ),
{ 21: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 22: }
  ( sym: 263; act: 36 ),
  ( sym: 265; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 268; act: 26 ),
  ( sym: 269; act: 27 ),
  ( sym: 270; act: 28 ),
  ( sym: 271; act: 29 ),
  ( sym: 272; act: 30 ),
  ( sym: 273; act: 31 ),
  ( sym: 274; act: 32 ),
{ 23: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 24: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 25: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: 262; act: 41 ),
  ( sym: 265; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 268; act: 26 ),
  ( sym: 269; act: 27 ),
  ( sym: 270; act: 28 ),
  ( sym: 271; act: 29 ),
  ( sym: 272; act: 30 ),
  ( sym: 273; act: 31 ),
  ( sym: 274; act: 32 ),
{ 34: }
{ 35: }
  ( sym: 265; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 268; act: 26 ),
  ( sym: 269; act: 27 ),
  ( sym: 270; act: 28 ),
  ( sym: 271; act: 29 ),
  ( sym: 272; act: 30 ),
  ( sym: 273; act: 31 ),
  ( sym: 274; act: 32 ),
  ( sym: 262; act: -12 ),
  ( sym: 263; act: -12 ),
{ 36: }
{ 37: }
  ( sym: 44; act: 44 ),
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
  ( sym: 262; act: 48 ),
{ 43: }
  ( sym: 44; act: 49 ),
  ( sym: 264; act: 50 ),
{ 44: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
  ( sym: 258; act: 9 ),
  ( sym: 259; act: 10 ),
  ( sym: 260; act: 11 ),
  ( sym: 261; act: 12 ),
  ( sym: 262; act: 48 )
{ 53: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -10; act: 1 ),
{ 1: }
  ( sym: -14; act: 2 ),
  ( sym: -13; act: 3 ),
  ( sym: -12; act: 4 ),
  ( sym: -11; act: 5 ),
  ( sym: -7; act: 6 ),
  ( sym: -4; act: 7 ),
  ( sym: -3; act: 8 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  ( sym: -7; act: 16 ),
  ( sym: -4; act: 7 ),
  ( sym: -3; act: 8 ),
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 18 ),
{ 15: }
{ 16: }
  ( sym: -17; act: 19 ),
  ( sym: -16; act: 20 ),
{ 17: }
{ 18: }
{ 19: }
  ( sym: -15; act: 22 ),
{ 20: }
  ( sym: -15; act: 33 ),
{ 21: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 34 ),
{ 22: }
  ( sym: -15; act: 35 ),
{ 23: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 37 ),
{ 24: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 38 ),
{ 25: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 39 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: -15; act: 35 ),
  ( sym: -5; act: 40 ),
{ 34: }
{ 35: }
  ( sym: -15; act: 35 ),
{ 36: }
  ( sym: -18; act: 42 ),
  ( sym: -8; act: 43 ),
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: -9; act: 45 ),
  ( sym: -6; act: 46 ),
  ( sym: -3; act: 47 ),
{ 43: }
{ 44: }
  ( sym: -3; act: 17 ),
  ( sym: -2; act: 51 ),
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
  ( sym: -19; act: 52 ),
{ 50: }
{ 51: }
{ 52: }
  ( sym: -9; act: 53 ),
  ( sym: -6; act: 46 ),
  ( sym: -3; act: 47 )
{ 53: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } -5,
{ 3: } -4,
{ 4: } -3,
{ 5: } -2,
{ 6: } 0,
{ 7: } -11,
{ 8: } -10,
{ 9: } -26,
{ 10: } -28,
{ 11: } -27,
{ 12: } -29,
{ 13: } -31,
{ 14: } 0,
{ 15: } -30,
{ 16: } 0,
{ 17: } -25,
{ 18: } 0,
{ 19: } 0,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } 0,
{ 24: } 0,
{ 25: } 0,
{ 26: } -16,
{ 27: } -17,
{ 28: } -18,
{ 29: } -19,
{ 30: } -20,
{ 31: } -21,
{ 32: } -22,
{ 33: } 0,
{ 34: } -24,
{ 35: } 0,
{ 36: } -34,
{ 37: } 0,
{ 38: } -14,
{ 39: } -15,
{ 40: } -7,
{ 41: } -32,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } -35,
{ 46: } -38,
{ 47: } -39,
{ 48: } -33,
{ 49: } -36,
{ 50: } -9,
{ 51: } -13,
{ 52: } 0,
{ 53: } -37
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 9,
{ 3: } 9,
{ 4: } 9,
{ 5: } 9,
{ 6: } 9,
{ 7: } 15,
{ 8: } 15,
{ 9: } 15,
{ 10: } 15,
{ 11: } 15,
{ 12: } 15,
{ 13: } 15,
{ 14: } 15,
{ 15: } 19,
{ 16: } 19,
{ 17: } 31,
{ 18: } 31,
{ 19: } 32,
{ 20: } 43,
{ 21: } 54,
{ 22: } 58,
{ 23: } 69,
{ 24: } 73,
{ 25: } 77,
{ 26: } 81,
{ 27: } 81,
{ 28: } 81,
{ 29: } 81,
{ 30: } 81,
{ 31: } 81,
{ 32: } 81,
{ 33: } 81,
{ 34: } 92,
{ 35: } 92,
{ 36: } 104,
{ 37: } 104,
{ 38: } 105,
{ 39: } 105,
{ 40: } 105,
{ 41: } 105,
{ 42: } 105,
{ 43: } 110,
{ 44: } 112,
{ 45: } 116,
{ 46: } 116,
{ 47: } 116,
{ 48: } 116,
{ 49: } 116,
{ 50: } 116,
{ 51: } 116,
{ 52: } 116,
{ 53: } 121
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 8,
{ 2: } 8,
{ 3: } 8,
{ 4: } 8,
{ 5: } 8,
{ 6: } 14,
{ 7: } 14,
{ 8: } 14,
{ 9: } 14,
{ 10: } 14,
{ 11: } 14,
{ 12: } 14,
{ 13: } 14,
{ 14: } 18,
{ 15: } 18,
{ 16: } 30,
{ 17: } 30,
{ 18: } 31,
{ 19: } 42,
{ 20: } 53,
{ 21: } 57,
{ 22: } 68,
{ 23: } 72,
{ 24: } 76,
{ 25: } 80,
{ 26: } 80,
{ 27: } 80,
{ 28: } 80,
{ 29: } 80,
{ 30: } 80,
{ 31: } 80,
{ 32: } 80,
{ 33: } 91,
{ 34: } 91,
{ 35: } 103,
{ 36: } 103,
{ 37: } 104,
{ 38: } 104,
{ 39: } 104,
{ 40: } 104,
{ 41: } 104,
{ 42: } 109,
{ 43: } 111,
{ 44: } 115,
{ 45: } 115,
{ 46: } 115,
{ 47: } 115,
{ 48: } 115,
{ 49: } 115,
{ 50: } 115,
{ 51: } 115,
{ 52: } 120,
{ 53: } 120
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 9,
{ 3: } 9,
{ 4: } 9,
{ 5: } 9,
{ 6: } 9,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 12,
{ 12: } 12,
{ 13: } 12,
{ 14: } 12,
{ 15: } 14,
{ 16: } 14,
{ 17: } 16,
{ 18: } 16,
{ 19: } 16,
{ 20: } 17,
{ 21: } 18,
{ 22: } 20,
{ 23: } 21,
{ 24: } 23,
{ 25: } 25,
{ 26: } 27,
{ 27: } 27,
{ 28: } 27,
{ 29: } 27,
{ 30: } 27,
{ 31: } 27,
{ 32: } 27,
{ 33: } 27,
{ 34: } 29,
{ 35: } 29,
{ 36: } 30,
{ 37: } 32,
{ 38: } 32,
{ 39: } 32,
{ 40: } 32,
{ 41: } 32,
{ 42: } 32,
{ 43: } 35,
{ 44: } 35,
{ 45: } 37,
{ 46: } 37,
{ 47: } 37,
{ 48: } 37,
{ 49: } 37,
{ 50: } 38,
{ 51: } 38,
{ 52: } 38,
{ 53: } 41
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 8,
{ 2: } 8,
{ 3: } 8,
{ 4: } 8,
{ 5: } 8,
{ 6: } 11,
{ 7: } 11,
{ 8: } 11,
{ 9: } 11,
{ 10: } 11,
{ 11: } 11,
{ 12: } 11,
{ 13: } 11,
{ 14: } 13,
{ 15: } 13,
{ 16: } 15,
{ 17: } 15,
{ 18: } 15,
{ 19: } 16,
{ 20: } 17,
{ 21: } 19,
{ 22: } 20,
{ 23: } 22,
{ 24: } 24,
{ 25: } 26,
{ 26: } 26,
{ 27: } 26,
{ 28: } 26,
{ 29: } 26,
{ 30: } 26,
{ 31: } 26,
{ 32: } 26,
{ 33: } 28,
{ 34: } 28,
{ 35: } 29,
{ 36: } 31,
{ 37: } 31,
{ 38: } 31,
{ 39: } 31,
{ 40: } 31,
{ 41: } 31,
{ 42: } 34,
{ 43: } 34,
{ 44: } 36,
{ 45: } 36,
{ 46: } 36,
{ 47: } 36,
{ 48: } 36,
{ 49: } 37,
{ 50: } 37,
{ 51: } 37,
{ 52: } 40,
{ 53: } 40
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -10 ),
{ 2: } ( len: 2; sym: -10 ),
{ 3: } ( len: 1; sym: -11 ),
{ 4: } ( len: 1; sym: -11 ),
{ 5: } ( len: 1; sym: -12 ),
{ 6: } ( len: 0; sym: -16 ),
{ 7: } ( len: 5; sym: -14 ),
{ 8: } ( len: 0; sym: -17 ),
{ 9: } ( len: 7; sym: -14 ),
{ 10: } ( len: 1; sym: -7 ),
{ 11: } ( len: 1; sym: -7 ),
{ 12: } ( len: 2; sym: -15 ),
{ 13: } ( len: 4; sym: -15 ),
{ 14: } ( len: 2; sym: -15 ),
{ 15: } ( len: 2; sym: -15 ),
{ 16: } ( len: 1; sym: -15 ),
{ 17: } ( len: 1; sym: -15 ),
{ 18: } ( len: 1; sym: -15 ),
{ 19: } ( len: 1; sym: -15 ),
{ 20: } ( len: 1; sym: -15 ),
{ 21: } ( len: 1; sym: -15 ),
{ 22: } ( len: 1; sym: -15 ),
{ 23: } ( len: 0; sym: -15 ),
{ 24: } ( len: 4; sym: -13 ),
{ 25: } ( len: 1; sym: -2 ),
{ 26: } ( len: 1; sym: -3 ),
{ 27: } ( len: 1; sym: -3 ),
{ 28: } ( len: 1; sym: -3 ),
{ 29: } ( len: 1; sym: -3 ),
{ 30: } ( len: 1; sym: -4 ),
{ 31: } ( len: 1; sym: -4 ),
{ 32: } ( len: 1; sym: -5 ),
{ 33: } ( len: 1; sym: -6 ),
{ 34: } ( len: 0; sym: -18 ),
{ 35: } ( len: 2; sym: -8 ),
{ 36: } ( len: 0; sym: -19 ),
{ 37: } ( len: 4; sym: -8 ),
{ 38: } ( len: 1; sym: -9 ),
{ 39: } ( len: 1; sym: -9 )
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


{$I rclex.inc}
begin
  bufptr:= 0;
  lexlib.get_char:= @rc_get_char;
  lexlib.unget_char:= @rc_unget_char;
end.
