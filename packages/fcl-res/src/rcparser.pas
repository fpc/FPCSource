
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

function str_to_num(s:string): rcnumtype;
begin
  // this does not handle empty strings - should never get them from the lexer
  Result.long:= s[Length(s)] = 'L';
  if Result.Long then
    setlength(s, Length(s) - 1);
  if Copy(s, 1, 2) = '0x' then
    Result.v:= StrToInt('$' + Copy(s, 2, Maxint))
  else
    Result.v:= StrToInt(s);
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

procedure stringtable_begin();
begin
  // create dummy resource that we will use to capture suboptions
  create_resource(TResourceDesc.create(1), TResourceDesc.create(1));
  aktresources.Remove(aktresource);
end;

procedure stringtable_add(ident: Word; str: string);
var
  table: word;
  r: TStringTableResource;
begin
  table:= (ident div 16) + 1;
  try
    { TODO : This is stupid }
    r:= aktresources.Find(RT_STRING, table, aktresource.LangID) as TStringTableResource;
  except
    on e: EResourceNotFoundException do begin
      r:= TStringTableResource.Create;
      r.LangID:= aktresource.LangID;
      r.MemoryFlags:= aktresource.MemoryFlags;
      r.Characteristics:= aktresource.Characteristics;
      r.FirstID:= ident;
      aktresources.Add(r);
    end;
  end;
  r.Strings[ident]:= str;
end;


procedure stringtable_end();
begin
  FreeAndNil(aktresource);
end;

var
  yycapture: AnsiString;
const _ILLEGAL = 257;
const _NUMDECIMAL = 258;
const _NUMHEX = 259;
const _QUOTEDSTR = 260;
const _BEGIN = 261;
const _END = 262;
const _ID = 263;
const _LANGUAGE = 264;
const _CHARACTERISTICS = 265;
const _VERSION = 266;
const _MOVEABLE = 267;
const _FIXED = 268;
const _PURE = 269;
const _IMPURE = 270;
const _PRELOAD = 271;
const _LOADONCALL = 272;
const _DISCARDABLE = 273;
const _BITMAP = 274;
const _CURSOR = 275;
const _ICON = 276;
const _ANICURSOR = 277;
const _ANIICON = 278;
const _DLGINCLUDE = 279;
const _DLGINIT = 280;
const _HTML = 281;
const _MANIFEST = 282;
const _MESSAGETABLE = 283;
const _PLUGPLAY = 284;
const _RCDATA = 285;
const _VXD = 286;
const _ACCELERATORS = 287;
const _DIALOG = 288;
const _DIALOGEX = 289;
const _MENU = 290;
const _MENUEX = 291;
const _STRINGTABLE = 292;
const _VERSIONINFO = 293;

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
         yyval := yyv[yysp-0];
       end;
  10 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_BITMAP); 
       end;
  11 : begin
         TBitmapResource(aktresource).SetCustomBitmapDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  12 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_CURSOR); 
       end;
  13 : begin
         TGroupCursorResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  14 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_ICON); 
       end;
  15 : begin
         TGroupIconResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  16 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  17 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  18 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  19 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-1].yyTMemoryStream); 
       end;
  20 : begin
         stringtable_begin(); 
       end;
  21 : begin
         stringtable_end(); 
       end;
  22 : begin
       end;
  23 : begin
         yyval := yyv[yysp-1];
       end;
  24 : begin
         stringtable_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyString); 
       end;
  25 : begin
         stringtable_add(yyv[yysp-1].yyrcnumtype.v, yyv[yysp-0].yyString); 
       end;
  26 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANICURSOR); 
       end;
  27 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANIICON); 
       end;
  28 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINCLUDE); 
       end;
  29 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINIT); 
       end;
  30 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(23); 
       end;
  31 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MANIFEST); 
       end;
  32 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MESSAGETABLE); 
       end;
  33 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_PLUGPLAY); 
       end;
  34 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_RCDATA); 
       end;
  35 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_VXD); 
       end;
  36 : begin
         yyval := yyv[yysp-0];
       end;
  37 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcnumtype.v); 
       end;
  38 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyString); 
       end;
  39 : begin
       end;
  40 : begin
         yyval := yyv[yysp-1];
       end;
  41 : begin
         aktresource.LangID:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  42 : begin
         aktresource.Characteristics:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  43 : begin
         aktresource.Version:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  44 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; 
       end;
  45 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; 
       end;
  46 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; 
       end;
  47 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; 
       end;
  48 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; 
       end;
  49 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; 
       end;
  50 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; 
       end;
  51 : begin
         language:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  52 : begin
         yyval := yyv[yysp-0];
       end;
  53 : begin
         yyval.yyrcnumtype:= str_to_num(yytext); 
       end;
  54 : begin
         yyval.yyrcnumtype:= str_to_num(yytext); 
       end;
  55 : begin
         yyval.yyString:= yytext; 
       end;
  56 : begin
         yyval.yyString:= yytext; 
       end;
  57 : begin
         yyval.yyTFileStream:= TFileStream.Create(yytext, fmOpenRead or fmShareDenyWrite); 
       end;
  58 : begin
         yyval.yyString:= yytext; 
       end;
  59 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  60 : begin
         yyval := yyv[yysp-1];
       end;
  61 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  62 : begin
         yyval := yyv[yysp-3];
       end;
  63 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         yyval.yyTMemoryStream.WriteBuffer(yyv[yysp-0].yyString[1], Length(yyv[yysp-0].yyString));
         
       end;
  64 : begin
         
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

yynacts   = 130;
yyngotos  = 64;
yynstates = 90;
yynrules  = 64;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
  ( sym: 260; act: 15 ),
  ( sym: 263; act: 16 ),
  ( sym: 264; act: 17 ),
  ( sym: 292; act: 18 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
  ( sym: 260; act: 15 ),
  ( sym: 263; act: 16 ),
  ( sym: 274; act: 21 ),
  ( sym: 275; act: 22 ),
  ( sym: 276; act: 23 ),
  ( sym: 277; act: 24 ),
  ( sym: 278; act: 25 ),
  ( sym: 279; act: 26 ),
  ( sym: 280; act: 27 ),
  ( sym: 281; act: 28 ),
  ( sym: 282; act: 29 ),
  ( sym: 283; act: 30 ),
  ( sym: 284; act: 31 ),
  ( sym: 285; act: 32 ),
  ( sym: 286; act: 33 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
{ 18: }
{ 19: }
  ( sym: 260; act: -16 ),
  ( sym: 264; act: -16 ),
  ( sym: 265; act: -16 ),
  ( sym: 266; act: -16 ),
  ( sym: 267; act: -16 ),
  ( sym: 268; act: -16 ),
  ( sym: 269; act: -16 ),
  ( sym: 270; act: -16 ),
  ( sym: 271; act: -16 ),
  ( sym: 272; act: -16 ),
  ( sym: 273; act: -16 ),
  ( sym: 261; act: -18 ),
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
  ( sym: 44; act: 42 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
{ 43: }
  ( sym: 261; act: 51 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 44: }
  ( sym: 261; act: 62 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 45: }
  ( sym: 260; act: 64 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 46: }
  ( sym: 260; act: 64 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 47: }
  ( sym: 260; act: 64 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 48: }
  ( sym: 260; act: 64 ),
  ( sym: 264; act: 52 ),
  ( sym: 265; act: 53 ),
  ( sym: 266; act: 54 ),
  ( sym: 267; act: 55 ),
  ( sym: 268; act: 56 ),
  ( sym: 269; act: 57 ),
  ( sym: 270; act: 58 ),
  ( sym: 271; act: 59 ),
  ( sym: 272; act: 60 ),
  ( sym: 273; act: 61 ),
{ 49: }
{ 50: }
{ 51: }
{ 52: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
{ 53: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
{ 54: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
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
{ 66: }
{ 67: }
{ 68: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
  ( sym: 262; act: 76 ),
{ 69: }
  ( sym: 44; act: 77 ),
{ 70: }
{ 71: }
{ 72: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
  ( sym: 260; act: 81 ),
{ 73: }
  ( sym: 44; act: 82 ),
  ( sym: 262; act: 83 ),
{ 74: }
{ 75: }
  ( sym: 44; act: 85 ),
  ( sym: 260; act: 81 ),
{ 76: }
{ 77: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: 260; act: 81 ),
{ 86: }
{ 87: }
  ( sym: 258; act: 13 ),
  ( sym: 259; act: 14 ),
  ( sym: 260; act: 81 )
{ 88: }
{ 89: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -11; act: 1 ),
{ 1: }
  ( sym: -19; act: 2 ),
  ( sym: -18; act: 3 ),
  ( sym: -17; act: 4 ),
  ( sym: -16; act: 5 ),
  ( sym: -15; act: 6 ),
  ( sym: -14; act: 7 ),
  ( sym: -13; act: 8 ),
  ( sym: -12; act: 9 ),
  ( sym: -7; act: 10 ),
  ( sym: -4; act: 11 ),
  ( sym: -3; act: 12 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: -8; act: 19 ),
  ( sym: -7; act: 20 ),
  ( sym: -4; act: 11 ),
  ( sym: -3; act: 12 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 35 ),
{ 18: }
  ( sym: -26; act: 36 ),
{ 19: }
  ( sym: -25; act: 37 ),
  ( sym: -24; act: 38 ),
{ 20: }
{ 21: }
  ( sym: -21; act: 39 ),
{ 22: }
  ( sym: -22; act: 40 ),
{ 23: }
  ( sym: -23; act: 41 ),
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
  ( sym: -20; act: 43 ),
{ 37: }
  ( sym: -20; act: 44 ),
{ 38: }
  ( sym: -20; act: 45 ),
{ 39: }
  ( sym: -20; act: 46 ),
{ 40: }
  ( sym: -20; act: 47 ),
{ 41: }
  ( sym: -20; act: 48 ),
{ 42: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 49 ),
{ 43: }
  ( sym: -29; act: 50 ),
{ 44: }
  ( sym: -29; act: 50 ),
{ 45: }
  ( sym: -29; act: 50 ),
  ( sym: -5; act: 63 ),
{ 46: }
  ( sym: -29; act: 50 ),
  ( sym: -5; act: 65 ),
{ 47: }
  ( sym: -29; act: 50 ),
  ( sym: -5; act: 66 ),
{ 48: }
  ( sym: -29; act: 50 ),
  ( sym: -5; act: 67 ),
{ 49: }
{ 50: }
{ 51: }
  ( sym: -27; act: 68 ),
{ 52: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 69 ),
{ 53: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 70 ),
{ 54: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 71 ),
{ 55: }
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
  ( sym: -30; act: 72 ),
  ( sym: -9; act: 73 ),
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
  ( sym: -28; act: 74 ),
  ( sym: -3; act: 75 ),
{ 69: }
{ 70: }
{ 71: }
{ 72: }
  ( sym: -10; act: 78 ),
  ( sym: -6; act: 79 ),
  ( sym: -3; act: 80 ),
{ 73: }
{ 74: }
{ 75: }
  ( sym: -6; act: 84 ),
{ 76: }
{ 77: }
  ( sym: -3; act: 34 ),
  ( sym: -2; act: 86 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: -31; act: 87 ),
{ 83: }
{ 84: }
{ 85: }
  ( sym: -6; act: 88 ),
{ 86: }
{ 87: }
  ( sym: -10; act: 89 ),
  ( sym: -6; act: 79 ),
  ( sym: -3; act: 80 )
{ 88: }
{ 89: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } -9,
{ 3: } -8,
{ 4: } -7,
{ 5: } -6,
{ 6: } -5,
{ 7: } -4,
{ 8: } -3,
{ 9: } -2,
{ 10: } 0,
{ 11: } -38,
{ 12: } -37,
{ 13: } -53,
{ 14: } -54,
{ 15: } -56,
{ 16: } -55,
{ 17: } 0,
{ 18: } -20,
{ 19: } 0,
{ 20: } -36,
{ 21: } -10,
{ 22: } -12,
{ 23: } -14,
{ 24: } -26,
{ 25: } -27,
{ 26: } -28,
{ 27: } -29,
{ 28: } -30,
{ 29: } -31,
{ 30: } -32,
{ 31: } -33,
{ 32: } -34,
{ 33: } -35,
{ 34: } -52,
{ 35: } 0,
{ 36: } -39,
{ 37: } -39,
{ 38: } -39,
{ 39: } -39,
{ 40: } -39,
{ 41: } -39,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } 0,
{ 49: } -51,
{ 50: } -40,
{ 51: } -22,
{ 52: } 0,
{ 53: } 0,
{ 54: } 0,
{ 55: } -44,
{ 56: } -45,
{ 57: } -46,
{ 58: } -47,
{ 59: } -48,
{ 60: } -49,
{ 61: } -50,
{ 62: } -59,
{ 63: } -17,
{ 64: } -57,
{ 65: } -11,
{ 66: } -13,
{ 67: } -15,
{ 68: } 0,
{ 69: } 0,
{ 70: } -42,
{ 71: } -43,
{ 72: } 0,
{ 73: } 0,
{ 74: } -23,
{ 75: } 0,
{ 76: } -21,
{ 77: } 0,
{ 78: } -60,
{ 79: } -63,
{ 80: } -64,
{ 81: } -58,
{ 82: } -61,
{ 83: } -19,
{ 84: } -25,
{ 85: } 0,
{ 86: } -41,
{ 87: } 0,
{ 88: } -24,
{ 89: } -62
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 8,
{ 3: } 8,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 8,
{ 8: } 8,
{ 9: } 8,
{ 10: } 8,
{ 11: } 25,
{ 12: } 25,
{ 13: } 25,
{ 14: } 25,
{ 15: } 25,
{ 16: } 25,
{ 17: } 25,
{ 18: } 27,
{ 19: } 27,
{ 20: } 39,
{ 21: } 39,
{ 22: } 39,
{ 23: } 39,
{ 24: } 39,
{ 25: } 39,
{ 26: } 39,
{ 27: } 39,
{ 28: } 39,
{ 29: } 39,
{ 30: } 39,
{ 31: } 39,
{ 32: } 39,
{ 33: } 39,
{ 34: } 39,
{ 35: } 39,
{ 36: } 40,
{ 37: } 40,
{ 38: } 40,
{ 39: } 40,
{ 40: } 40,
{ 41: } 40,
{ 42: } 40,
{ 43: } 42,
{ 44: } 53,
{ 45: } 64,
{ 46: } 75,
{ 47: } 86,
{ 48: } 97,
{ 49: } 108,
{ 50: } 108,
{ 51: } 108,
{ 52: } 108,
{ 53: } 110,
{ 54: } 112,
{ 55: } 114,
{ 56: } 114,
{ 57: } 114,
{ 58: } 114,
{ 59: } 114,
{ 60: } 114,
{ 61: } 114,
{ 62: } 114,
{ 63: } 114,
{ 64: } 114,
{ 65: } 114,
{ 66: } 114,
{ 67: } 114,
{ 68: } 114,
{ 69: } 117,
{ 70: } 118,
{ 71: } 118,
{ 72: } 118,
{ 73: } 121,
{ 74: } 123,
{ 75: } 123,
{ 76: } 125,
{ 77: } 125,
{ 78: } 127,
{ 79: } 127,
{ 80: } 127,
{ 81: } 127,
{ 82: } 127,
{ 83: } 127,
{ 84: } 127,
{ 85: } 127,
{ 86: } 128,
{ 87: } 128,
{ 88: } 131,
{ 89: } 131
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 7,
{ 2: } 7,
{ 3: } 7,
{ 4: } 7,
{ 5: } 7,
{ 6: } 7,
{ 7: } 7,
{ 8: } 7,
{ 9: } 7,
{ 10: } 24,
{ 11: } 24,
{ 12: } 24,
{ 13: } 24,
{ 14: } 24,
{ 15: } 24,
{ 16: } 24,
{ 17: } 26,
{ 18: } 26,
{ 19: } 38,
{ 20: } 38,
{ 21: } 38,
{ 22: } 38,
{ 23: } 38,
{ 24: } 38,
{ 25: } 38,
{ 26: } 38,
{ 27: } 38,
{ 28: } 38,
{ 29: } 38,
{ 30: } 38,
{ 31: } 38,
{ 32: } 38,
{ 33: } 38,
{ 34: } 38,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 41,
{ 43: } 52,
{ 44: } 63,
{ 45: } 74,
{ 46: } 85,
{ 47: } 96,
{ 48: } 107,
{ 49: } 107,
{ 50: } 107,
{ 51: } 107,
{ 52: } 109,
{ 53: } 111,
{ 54: } 113,
{ 55: } 113,
{ 56: } 113,
{ 57: } 113,
{ 58: } 113,
{ 59: } 113,
{ 60: } 113,
{ 61: } 113,
{ 62: } 113,
{ 63: } 113,
{ 64: } 113,
{ 65: } 113,
{ 66: } 113,
{ 67: } 113,
{ 68: } 116,
{ 69: } 117,
{ 70: } 117,
{ 71: } 117,
{ 72: } 120,
{ 73: } 122,
{ 74: } 122,
{ 75: } 124,
{ 76: } 124,
{ 77: } 126,
{ 78: } 126,
{ 79: } 126,
{ 80: } 126,
{ 81: } 126,
{ 82: } 126,
{ 83: } 126,
{ 84: } 126,
{ 85: } 127,
{ 86: } 127,
{ 87: } 130,
{ 88: } 130,
{ 89: } 130
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 13,
{ 3: } 13,
{ 4: } 13,
{ 5: } 13,
{ 6: } 13,
{ 7: } 13,
{ 8: } 13,
{ 9: } 13,
{ 10: } 13,
{ 11: } 17,
{ 12: } 17,
{ 13: } 17,
{ 14: } 17,
{ 15: } 17,
{ 16: } 17,
{ 17: } 17,
{ 18: } 19,
{ 19: } 20,
{ 20: } 22,
{ 21: } 22,
{ 22: } 23,
{ 23: } 24,
{ 24: } 25,
{ 25: } 25,
{ 26: } 25,
{ 27: } 25,
{ 28: } 25,
{ 29: } 25,
{ 30: } 25,
{ 31: } 25,
{ 32: } 25,
{ 33: } 25,
{ 34: } 25,
{ 35: } 25,
{ 36: } 25,
{ 37: } 26,
{ 38: } 27,
{ 39: } 28,
{ 40: } 29,
{ 41: } 30,
{ 42: } 31,
{ 43: } 33,
{ 44: } 34,
{ 45: } 35,
{ 46: } 37,
{ 47: } 39,
{ 48: } 41,
{ 49: } 43,
{ 50: } 43,
{ 51: } 43,
{ 52: } 44,
{ 53: } 46,
{ 54: } 48,
{ 55: } 50,
{ 56: } 50,
{ 57: } 50,
{ 58: } 50,
{ 59: } 50,
{ 60: } 50,
{ 61: } 50,
{ 62: } 50,
{ 63: } 52,
{ 64: } 52,
{ 65: } 52,
{ 66: } 52,
{ 67: } 52,
{ 68: } 52,
{ 69: } 54,
{ 70: } 54,
{ 71: } 54,
{ 72: } 54,
{ 73: } 57,
{ 74: } 57,
{ 75: } 57,
{ 76: } 58,
{ 77: } 58,
{ 78: } 60,
{ 79: } 60,
{ 80: } 60,
{ 81: } 60,
{ 82: } 60,
{ 83: } 61,
{ 84: } 61,
{ 85: } 61,
{ 86: } 62,
{ 87: } 62,
{ 88: } 65,
{ 89: } 65
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 12,
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
{ 17: } 18,
{ 18: } 19,
{ 19: } 21,
{ 20: } 21,
{ 21: } 22,
{ 22: } 23,
{ 23: } 24,
{ 24: } 24,
{ 25: } 24,
{ 26: } 24,
{ 27: } 24,
{ 28: } 24,
{ 29: } 24,
{ 30: } 24,
{ 31: } 24,
{ 32: } 24,
{ 33: } 24,
{ 34: } 24,
{ 35: } 24,
{ 36: } 25,
{ 37: } 26,
{ 38: } 27,
{ 39: } 28,
{ 40: } 29,
{ 41: } 30,
{ 42: } 32,
{ 43: } 33,
{ 44: } 34,
{ 45: } 36,
{ 46: } 38,
{ 47: } 40,
{ 48: } 42,
{ 49: } 42,
{ 50: } 42,
{ 51: } 43,
{ 52: } 45,
{ 53: } 47,
{ 54: } 49,
{ 55: } 49,
{ 56: } 49,
{ 57: } 49,
{ 58: } 49,
{ 59: } 49,
{ 60: } 49,
{ 61: } 49,
{ 62: } 51,
{ 63: } 51,
{ 64: } 51,
{ 65: } 51,
{ 66: } 51,
{ 67: } 51,
{ 68: } 53,
{ 69: } 53,
{ 70: } 53,
{ 71: } 53,
{ 72: } 56,
{ 73: } 56,
{ 74: } 56,
{ 75: } 57,
{ 76: } 57,
{ 77: } 59,
{ 78: } 59,
{ 79: } 59,
{ 80: } 59,
{ 81: } 59,
{ 82: } 60,
{ 83: } 60,
{ 84: } 60,
{ 85: } 61,
{ 86: } 61,
{ 87: } 64,
{ 88: } 64,
{ 89: } 64
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
{ 9: } ( len: 1; sym: -13 ),
{ 10: } ( len: 0; sym: -21 ),
{ 11: } ( len: 5; sym: -16 ),
{ 12: } ( len: 0; sym: -22 ),
{ 13: } ( len: 5; sym: -17 ),
{ 14: } ( len: 0; sym: -23 ),
{ 15: } ( len: 5; sym: -18 ),
{ 16: } ( len: 0; sym: -24 ),
{ 17: } ( len: 5; sym: -19 ),
{ 18: } ( len: 0; sym: -25 ),
{ 19: } ( len: 7; sym: -19 ),
{ 20: } ( len: 0; sym: -26 ),
{ 21: } ( len: 6; sym: -15 ),
{ 22: } ( len: 0; sym: -27 ),
{ 23: } ( len: 2; sym: -27 ),
{ 24: } ( len: 3; sym: -28 ),
{ 25: } ( len: 2; sym: -28 ),
{ 26: } ( len: 1; sym: -8 ),
{ 27: } ( len: 1; sym: -8 ),
{ 28: } ( len: 1; sym: -8 ),
{ 29: } ( len: 1; sym: -8 ),
{ 30: } ( len: 1; sym: -8 ),
{ 31: } ( len: 1; sym: -8 ),
{ 32: } ( len: 1; sym: -8 ),
{ 33: } ( len: 1; sym: -8 ),
{ 34: } ( len: 1; sym: -8 ),
{ 35: } ( len: 1; sym: -8 ),
{ 36: } ( len: 1; sym: -8 ),
{ 37: } ( len: 1; sym: -7 ),
{ 38: } ( len: 1; sym: -7 ),
{ 39: } ( len: 0; sym: -20 ),
{ 40: } ( len: 2; sym: -20 ),
{ 41: } ( len: 4; sym: -29 ),
{ 42: } ( len: 2; sym: -29 ),
{ 43: } ( len: 2; sym: -29 ),
{ 44: } ( len: 1; sym: -29 ),
{ 45: } ( len: 1; sym: -29 ),
{ 46: } ( len: 1; sym: -29 ),
{ 47: } ( len: 1; sym: -29 ),
{ 48: } ( len: 1; sym: -29 ),
{ 49: } ( len: 1; sym: -29 ),
{ 50: } ( len: 1; sym: -29 ),
{ 51: } ( len: 4; sym: -14 ),
{ 52: } ( len: 1; sym: -2 ),
{ 53: } ( len: 1; sym: -3 ),
{ 54: } ( len: 1; sym: -3 ),
{ 55: } ( len: 1; sym: -4 ),
{ 56: } ( len: 1; sym: -4 ),
{ 57: } ( len: 1; sym: -5 ),
{ 58: } ( len: 1; sym: -6 ),
{ 59: } ( len: 0; sym: -30 ),
{ 60: } ( len: 2; sym: -9 ),
{ 61: } ( len: 0; sym: -31 ),
{ 62: } ( len: 4; sym: -9 ),
{ 63: } ( len: 1; sym: -10 ),
{ 64: } ( len: 1; sym: -10 )
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
