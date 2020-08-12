
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

(*
Vorspann
 ****************************************************************************)

unit rcparser;

{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, StrUtils, lexlib, yacclib, resource,
  acceleratorsresource, groupiconresource, stringtableresource,
  bitmapresource, versionresource, groupcursorresource;

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
  r:= aClass.Create(aType, aId);
  r.LangID:= language;
  aktresources.Add(r);
  aktresource:= r;
end;

procedure create_resource(aId, aType: TResourceDesc); overload;
begin
  create_resource(aId, aType, TGenericResource);
end;

procedure create_resource(aId: TResourceDesc; aType: Word); overload;
var
  cls: TResourceClass;
begin
  case aType of
    RT_BITMAP: cls:= TBitmapResource;
    RT_ICON: cls:= TGroupIconResource;
    RT_CURSOR: cls:= TGroupCursorResource;
  else
    raise EResourceDescTypeException.CreateFmt('Resource type not supported: %d', [aType]);
  end;
  create_resource(aId, nil, cls);
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
const _ID = 265;
const _LANGUAGE = 266;
const _CHARACTERISTICS = 267;
const _VERSION = 268;
const _MOVEABLE = 269;
const _FIXED = 270;
const _PURE = 271;
const _IMPURE = 272;
const _PRELOAD = 273;
const _LOADONCALL = 274;
const _DISCARDABLE = 275;
const _BITMAP = 276;
const _CURSOR = 277;
const _ICON = 278;
const _ANICURSOR = 279;
const _ANIICON = 280;
const _DLGINCLUDE = 281;
const _DLGINIT = 282;
const _HTML = 283;
const _MANIFEST = 284;
const _MESSAGETABLE = 285;
const _PLUGPLAY = 286;
const _RCDATA = 287;
const _VXD = 288;
const _ACCELERATORS = 289;
const _DIALOG = 290;
const _DIALOGEX = 291;
const _MENU = 292;
const _MENUEX = 293;
const _STRINGTABLE = 294;
const _VERSIONINFO = 295;

type YYSType = record case Integer of
                 1 : ( yyString : String );
                 2 : ( yyTFileStream : TFileStream );
                 3 : ( yyTMemoryStream : TMemoryStream );
                 4 : ( yyTResourceDesc : TResourceDesc );
                 5 : ( yyrcnumtype : rcnumtype );
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
         yyval := yyv[yysp-0];
       end;
   7 : begin
         yyval := yyv[yysp-0];
       end;
   8 : begin
         yyval := yyv[yysp-0];
       end;
   9 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_BITMAP); 
       end;
  10 : begin
         TBitmapResource(aktresource).SetCustomBitmapDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  11 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_CURSOR); 
       end;
  12 : begin
         TGroupCursorResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  13 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_ICON); 
       end;
  14 : begin
         TGroupIconResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  15 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  16 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  17 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  18 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-1].yyTMemoryStream); 
       end;
  19 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANICURSOR); 
       end;
  20 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANIICON); 
       end;
  21 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINCLUDE); 
       end;
  22 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINIT); 
       end;
  23 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(23); 
       end;
  24 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MANIFEST); 
       end;
  25 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MESSAGETABLE); 
       end;
  26 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_PLUGPLAY); 
       end;
  27 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_RCDATA); 
       end;
  28 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_VXD); 
       end;
  29 : begin
         yyval := yyv[yysp-0];
       end;
  30 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcnumtype.v); 
       end;
  31 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyString); 
       end;
  32 : begin
       end;
  33 : begin
         yyval := yyv[yysp-1];
       end;
  34 : begin
         aktresource.LangID:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  35 : begin
         aktresource.Characteristics:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  36 : begin
         aktresource.Version:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  37 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; 
       end;
  38 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; 
       end;
  39 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; 
       end;
  40 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; 
       end;
  41 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; 
       end;
  42 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; 
       end;
  43 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; 
       end;
  44 : begin
         language:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  45 : begin
         yyval := yyv[yysp-0];
       end;
  46 : begin
         yyval.yyrcnumtype.v:= StrToInt(yytext); yyval.yyrcnumtype.long:= False; 
       end;
  47 : begin
         yyval.yyrcnumtype.v:= StrToInt(Copy(yytext,1,length(yytext)-1)); yyval.yyrcnumtype.long:= True; 
       end;
  48 : begin
         yyval.yyrcnumtype.v:= StrToInt('$'+Copy(yytext,3,Maxint)); yyval.yyrcnumtype.long:= False; 
       end;
  49 : begin
         yyval.yyrcnumtype.v:= StrToInt('$'+Copy(yytext,3,length(yytext)-3)); yyval.yyrcnumtype.long:= True; 
       end;
  50 : begin
         yyval.yyString:= yytext; 
       end;
  51 : begin
         yyval.yyString:= yytext; 
       end;
  52 : begin
         yyval.yyTFileStream:= TFileStream.Create(yytext, fmOpenRead or fmShareDenyWrite); 
       end;
  53 : begin
         yyval.yyString:= yytext; 
       end;
  54 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  55 : begin
         yyval := yyv[yysp-1];
       end;
  56 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  57 : begin
         yyval := yyv[yysp-3];
       end;
  58 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         yyval.yyTMemoryStream.WriteBuffer(yyv[yysp-0].yyString[1], Length(yyv[yysp-0].yyString));
         
       end;
  59 : begin
         
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

yynacts   = 132;
yyngotos  = 55;
yynstates = 80;
yynrules  = 59;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
  ( sym: 262; act: 16 ),
  ( sym: 265; act: 17 ),
  ( sym: 266; act: 18 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
  ( sym: 262; act: 16 ),
  ( sym: 265; act: 17 ),
  ( sym: 276; act: 21 ),
  ( sym: 277; act: 22 ),
  ( sym: 278; act: 23 ),
  ( sym: 279; act: 24 ),
  ( sym: 280; act: 25 ),
  ( sym: 281; act: 26 ),
  ( sym: 282; act: 27 ),
  ( sym: 283; act: 28 ),
  ( sym: 284; act: 29 ),
  ( sym: 285; act: 30 ),
  ( sym: 286; act: 31 ),
  ( sym: 287; act: 32 ),
  ( sym: 288; act: 33 ),
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 19: }
  ( sym: 262; act: -15 ),
  ( sym: 266; act: -15 ),
  ( sym: 267; act: -15 ),
  ( sym: 268; act: -15 ),
  ( sym: 269; act: -15 ),
  ( sym: 270; act: -15 ),
  ( sym: 271; act: -15 ),
  ( sym: 272; act: -15 ),
  ( sym: 273; act: -15 ),
  ( sym: 274; act: -15 ),
  ( sym: 275; act: -15 ),
  ( sym: 263; act: -17 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 44; act: 41 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 42: }
  ( sym: 263; act: 49 ),
  ( sym: 266; act: 50 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 270; act: 54 ),
  ( sym: 271; act: 55 ),
  ( sym: 272; act: 56 ),
  ( sym: 273; act: 57 ),
  ( sym: 274; act: 58 ),
  ( sym: 275; act: 59 ),
{ 43: }
  ( sym: 262; act: 61 ),
  ( sym: 266; act: 50 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 270; act: 54 ),
  ( sym: 271; act: 55 ),
  ( sym: 272; act: 56 ),
  ( sym: 273; act: 57 ),
  ( sym: 274; act: 58 ),
  ( sym: 275; act: 59 ),
{ 44: }
  ( sym: 262; act: 61 ),
  ( sym: 266; act: 50 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 270; act: 54 ),
  ( sym: 271; act: 55 ),
  ( sym: 272; act: 56 ),
  ( sym: 273; act: 57 ),
  ( sym: 274; act: 58 ),
  ( sym: 275; act: 59 ),
{ 45: }
  ( sym: 262; act: 61 ),
  ( sym: 266; act: 50 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 270; act: 54 ),
  ( sym: 271; act: 55 ),
  ( sym: 272; act: 56 ),
  ( sym: 273; act: 57 ),
  ( sym: 274; act: 58 ),
  ( sym: 275; act: 59 ),
{ 46: }
  ( sym: 262; act: 61 ),
  ( sym: 266; act: 50 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 270; act: 54 ),
  ( sym: 271; act: 55 ),
  ( sym: 272; act: 56 ),
  ( sym: 273; act: 57 ),
  ( sym: 274; act: 58 ),
  ( sym: 275; act: 59 ),
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 51: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 52: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 53: }
{ 54: }
{ 55: }
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
  ( sym: 262; act: 73 ),
{ 66: }
  ( sym: 44; act: 74 ),
  ( sym: 264; act: 75 ),
{ 67: }
  ( sym: 44; act: 76 ),
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
{ 77: }
  ( sym: 258; act: 12 ),
  ( sym: 259; act: 13 ),
  ( sym: 260; act: 14 ),
  ( sym: 261; act: 15 ),
  ( sym: 262; act: 73 )
{ 78: }
{ 79: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -11; act: 1 ),
{ 1: }
  ( sym: -18; act: 2 ),
  ( sym: -17; act: 3 ),
  ( sym: -16; act: 4 ),
  ( sym: -15; act: 5 ),
  ( sym: -14; act: 6 ),
  ( sym: -13; act: 7 ),
  ( sym: -12; act: 8 ),
  ( sym: -7; act: 9 ),
  ( sym: -4; act: 10 ),
  ( sym: -3; act: 11 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
  ( sym: -8; act: 19 ),
  ( sym: -7; act: 20 ),
  ( sym: -4; act: 10 ),
  ( sym: -3; act: 11 ),
{ 10: }
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 35 ),
{ 19: }
  ( sym: -24; act: 36 ),
  ( sym: -23; act: 37 ),
{ 20: }
{ 21: }
  ( sym: -20; act: 38 ),
{ 22: }
  ( sym: -21; act: 39 ),
{ 23: }
  ( sym: -22; act: 40 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -19; act: 42 ),
{ 37: }
  ( sym: -19; act: 43 ),
{ 38: }
  ( sym: -19; act: 44 ),
{ 39: }
  ( sym: -19; act: 45 ),
{ 40: }
  ( sym: -19; act: 46 ),
{ 41: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 47 ),
{ 42: }
  ( sym: -25; act: 48 ),
{ 43: }
  ( sym: -25; act: 48 ),
  ( sym: -5; act: 60 ),
{ 44: }
  ( sym: -25; act: 48 ),
  ( sym: -5; act: 62 ),
{ 45: }
  ( sym: -25; act: 48 ),
  ( sym: -5; act: 63 ),
{ 46: }
  ( sym: -25; act: 48 ),
  ( sym: -5; act: 64 ),
{ 47: }
{ 48: }
{ 49: }
  ( sym: -26; act: 65 ),
  ( sym: -9; act: 66 ),
{ 50: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 67 ),
{ 51: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 68 ),
{ 52: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 69 ),
{ 53: }
{ 54: }
{ 55: }
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: -10; act: 70 ),
  ( sym: -6; act: 71 ),
  ( sym: -3; act: 72 ),
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
  ( sym: -27; act: 77 ),
{ 75: }
{ 76: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 78 ),
{ 77: }
  ( sym: -10; act: 79 ),
  ( sym: -6; act: 71 ),
  ( sym: -3; act: 72 )
{ 78: }
{ 79: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } -8,
{ 3: } -7,
{ 4: } -6,
{ 5: } -5,
{ 6: } -4,
{ 7: } -3,
{ 8: } -2,
{ 9: } 0,
{ 10: } -31,
{ 11: } -30,
{ 12: } -46,
{ 13: } -48,
{ 14: } -47,
{ 15: } -49,
{ 16: } -51,
{ 17: } -50,
{ 18: } 0,
{ 19: } 0,
{ 20: } -29,
{ 21: } -9,
{ 22: } -11,
{ 23: } -13,
{ 24: } -19,
{ 25: } -20,
{ 26: } -21,
{ 27: } -22,
{ 28: } -23,
{ 29: } -24,
{ 30: } -25,
{ 31: } -26,
{ 32: } -27,
{ 33: } -28,
{ 34: } -45,
{ 35: } 0,
{ 36: } -32,
{ 37: } -32,
{ 38: } -32,
{ 39: } -32,
{ 40: } -32,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } -44,
{ 48: } -33,
{ 49: } -54,
{ 50: } 0,
{ 51: } 0,
{ 52: } 0,
{ 53: } -37,
{ 54: } -38,
{ 55: } -39,
{ 56: } -40,
{ 57: } -41,
{ 58: } -42,
{ 59: } -43,
{ 60: } -16,
{ 61: } -52,
{ 62: } -10,
{ 63: } -12,
{ 64: } -14,
{ 65: } 0,
{ 66: } 0,
{ 67: } 0,
{ 68: } -35,
{ 69: } -36,
{ 70: } -55,
{ 71: } -58,
{ 72: } -59,
{ 73: } -53,
{ 74: } -56,
{ 75: } -18,
{ 76: } 0,
{ 77: } 0,
{ 78: } -34,
{ 79: } -57
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 9,
{ 3: } 9,
{ 4: } 9,
{ 5: } 9,
{ 6: } 9,
{ 7: } 9,
{ 8: } 9,
{ 9: } 9,
{ 10: } 28,
{ 11: } 28,
{ 12: } 28,
{ 13: } 28,
{ 14: } 28,
{ 15: } 28,
{ 16: } 28,
{ 17: } 28,
{ 18: } 28,
{ 19: } 32,
{ 20: } 44,
{ 21: } 44,
{ 22: } 44,
{ 23: } 44,
{ 24: } 44,
{ 25: } 44,
{ 26: } 44,
{ 27: } 44,
{ 28: } 44,
{ 29: } 44,
{ 30: } 44,
{ 31: } 44,
{ 32: } 44,
{ 33: } 44,
{ 34: } 44,
{ 35: } 44,
{ 36: } 45,
{ 37: } 45,
{ 38: } 45,
{ 39: } 45,
{ 40: } 45,
{ 41: } 45,
{ 42: } 49,
{ 43: } 60,
{ 44: } 71,
{ 45: } 82,
{ 46: } 93,
{ 47: } 104,
{ 48: } 104,
{ 49: } 104,
{ 50: } 104,
{ 51: } 108,
{ 52: } 112,
{ 53: } 116,
{ 54: } 116,
{ 55: } 116,
{ 56: } 116,
{ 57: } 116,
{ 58: } 116,
{ 59: } 116,
{ 60: } 116,
{ 61: } 116,
{ 62: } 116,
{ 63: } 116,
{ 64: } 116,
{ 65: } 116,
{ 66: } 121,
{ 67: } 123,
{ 68: } 124,
{ 69: } 124,
{ 70: } 124,
{ 71: } 124,
{ 72: } 124,
{ 73: } 124,
{ 74: } 124,
{ 75: } 124,
{ 76: } 124,
{ 77: } 128,
{ 78: } 133,
{ 79: } 133
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 8,
{ 2: } 8,
{ 3: } 8,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 8,
{ 8: } 8,
{ 9: } 27,
{ 10: } 27,
{ 11: } 27,
{ 12: } 27,
{ 13: } 27,
{ 14: } 27,
{ 15: } 27,
{ 16: } 27,
{ 17: } 27,
{ 18: } 31,
{ 19: } 43,
{ 20: } 43,
{ 21: } 43,
{ 22: } 43,
{ 23: } 43,
{ 24: } 43,
{ 25: } 43,
{ 26: } 43,
{ 27: } 43,
{ 28: } 43,
{ 29: } 43,
{ 30: } 43,
{ 31: } 43,
{ 32: } 43,
{ 33: } 43,
{ 34: } 43,
{ 35: } 44,
{ 36: } 44,
{ 37: } 44,
{ 38: } 44,
{ 39: } 44,
{ 40: } 44,
{ 41: } 48,
{ 42: } 59,
{ 43: } 70,
{ 44: } 81,
{ 45: } 92,
{ 46: } 103,
{ 47: } 103,
{ 48: } 103,
{ 49: } 103,
{ 50: } 107,
{ 51: } 111,
{ 52: } 115,
{ 53: } 115,
{ 54: } 115,
{ 55: } 115,
{ 56: } 115,
{ 57: } 115,
{ 58: } 115,
{ 59: } 115,
{ 60: } 115,
{ 61: } 115,
{ 62: } 115,
{ 63: } 115,
{ 64: } 115,
{ 65: } 120,
{ 66: } 122,
{ 67: } 123,
{ 68: } 123,
{ 69: } 123,
{ 70: } 123,
{ 71: } 123,
{ 72: } 123,
{ 73: } 123,
{ 74: } 123,
{ 75: } 123,
{ 76: } 127,
{ 77: } 132,
{ 78: } 132,
{ 79: } 132
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 12,
{ 3: } 12,
{ 4: } 12,
{ 5: } 12,
{ 6: } 12,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 16,
{ 11: } 16,
{ 12: } 16,
{ 13: } 16,
{ 14: } 16,
{ 15: } 16,
{ 16: } 16,
{ 17: } 16,
{ 18: } 16,
{ 19: } 18,
{ 20: } 20,
{ 21: } 20,
{ 22: } 21,
{ 23: } 22,
{ 24: } 23,
{ 25: } 23,
{ 26: } 23,
{ 27: } 23,
{ 28: } 23,
{ 29: } 23,
{ 30: } 23,
{ 31: } 23,
{ 32: } 23,
{ 33: } 23,
{ 34: } 23,
{ 35: } 23,
{ 36: } 23,
{ 37: } 24,
{ 38: } 25,
{ 39: } 26,
{ 40: } 27,
{ 41: } 28,
{ 42: } 30,
{ 43: } 31,
{ 44: } 33,
{ 45: } 35,
{ 46: } 37,
{ 47: } 39,
{ 48: } 39,
{ 49: } 39,
{ 50: } 41,
{ 51: } 43,
{ 52: } 45,
{ 53: } 47,
{ 54: } 47,
{ 55: } 47,
{ 56: } 47,
{ 57: } 47,
{ 58: } 47,
{ 59: } 47,
{ 60: } 47,
{ 61: } 47,
{ 62: } 47,
{ 63: } 47,
{ 64: } 47,
{ 65: } 47,
{ 66: } 50,
{ 67: } 50,
{ 68: } 50,
{ 69: } 50,
{ 70: } 50,
{ 71: } 50,
{ 72: } 50,
{ 73: } 50,
{ 74: } 50,
{ 75: } 51,
{ 76: } 51,
{ 77: } 53,
{ 78: } 56,
{ 79: } 56
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 11,
{ 2: } 11,
{ 3: } 11,
{ 4: } 11,
{ 5: } 11,
{ 6: } 11,
{ 7: } 11,
{ 8: } 11,
{ 9: } 15,
{ 10: } 15,
{ 11: } 15,
{ 12: } 15,
{ 13: } 15,
{ 14: } 15,
{ 15: } 15,
{ 16: } 15,
{ 17: } 15,
{ 18: } 17,
{ 19: } 19,
{ 20: } 19,
{ 21: } 20,
{ 22: } 21,
{ 23: } 22,
{ 24: } 22,
{ 25: } 22,
{ 26: } 22,
{ 27: } 22,
{ 28: } 22,
{ 29: } 22,
{ 30: } 22,
{ 31: } 22,
{ 32: } 22,
{ 33: } 22,
{ 34: } 22,
{ 35: } 22,
{ 36: } 23,
{ 37: } 24,
{ 38: } 25,
{ 39: } 26,
{ 40: } 27,
{ 41: } 29,
{ 42: } 30,
{ 43: } 32,
{ 44: } 34,
{ 45: } 36,
{ 46: } 38,
{ 47: } 38,
{ 48: } 38,
{ 49: } 40,
{ 50: } 42,
{ 51: } 44,
{ 52: } 46,
{ 53: } 46,
{ 54: } 46,
{ 55: } 46,
{ 56: } 46,
{ 57: } 46,
{ 58: } 46,
{ 59: } 46,
{ 60: } 46,
{ 61: } 46,
{ 62: } 46,
{ 63: } 46,
{ 64: } 46,
{ 65: } 49,
{ 66: } 49,
{ 67: } 49,
{ 68: } 49,
{ 69: } 49,
{ 70: } 49,
{ 71: } 49,
{ 72: } 49,
{ 73: } 49,
{ 74: } 50,
{ 75: } 50,
{ 76: } 52,
{ 77: } 55,
{ 78: } 55,
{ 79: } 55
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -11 ),
{ 2: } ( len: 2; sym: -11 ),
{ 3: } ( len: 1; sym: -12 ),
{ 4: } ( len: 1; sym: -12 ),
{ 5: } ( len: 1; sym: -13 ),
{ 6: } ( len: 1; sym: -13 ),
{ 7: } ( len: 1; sym: -13 ),
{ 8: } ( len: 1; sym: -13 ),
{ 9: } ( len: 0; sym: -20 ),
{ 10: } ( len: 5; sym: -15 ),
{ 11: } ( len: 0; sym: -21 ),
{ 12: } ( len: 5; sym: -16 ),
{ 13: } ( len: 0; sym: -22 ),
{ 14: } ( len: 5; sym: -17 ),
{ 15: } ( len: 0; sym: -23 ),
{ 16: } ( len: 5; sym: -18 ),
{ 17: } ( len: 0; sym: -24 ),
{ 18: } ( len: 7; sym: -18 ),
{ 19: } ( len: 1; sym: -8 ),
{ 20: } ( len: 1; sym: -8 ),
{ 21: } ( len: 1; sym: -8 ),
{ 22: } ( len: 1; sym: -8 ),
{ 23: } ( len: 1; sym: -8 ),
{ 24: } ( len: 1; sym: -8 ),
{ 25: } ( len: 1; sym: -8 ),
{ 26: } ( len: 1; sym: -8 ),
{ 27: } ( len: 1; sym: -8 ),
{ 28: } ( len: 1; sym: -8 ),
{ 29: } ( len: 1; sym: -8 ),
{ 30: } ( len: 1; sym: -7 ),
{ 31: } ( len: 1; sym: -7 ),
{ 32: } ( len: 0; sym: -19 ),
{ 33: } ( len: 2; sym: -19 ),
{ 34: } ( len: 4; sym: -25 ),
{ 35: } ( len: 2; sym: -25 ),
{ 36: } ( len: 2; sym: -25 ),
{ 37: } ( len: 1; sym: -25 ),
{ 38: } ( len: 1; sym: -25 ),
{ 39: } ( len: 1; sym: -25 ),
{ 40: } ( len: 1; sym: -25 ),
{ 41: } ( len: 1; sym: -25 ),
{ 42: } ( len: 1; sym: -25 ),
{ 43: } ( len: 1; sym: -25 ),
{ 44: } ( len: 4; sym: -14 ),
{ 45: } ( len: 1; sym: -2 ),
{ 46: } ( len: 1; sym: -3 ),
{ 47: } ( len: 1; sym: -3 ),
{ 48: } ( len: 1; sym: -3 ),
{ 49: } ( len: 1; sym: -3 ),
{ 50: } ( len: 1; sym: -4 ),
{ 51: } ( len: 1; sym: -4 ),
{ 52: } ( len: 1; sym: -5 ),
{ 53: } ( len: 1; sym: -6 ),
{ 54: } ( len: 0; sym: -26 ),
{ 55: } ( len: 2; sym: -9 ),
{ 56: } ( len: 0; sym: -27 ),
{ 57: } ( len: 4; sym: -9 ),
{ 58: } ( len: 1; sym: -10 ),
{ 59: } ( len: 1; sym: -10 )
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
