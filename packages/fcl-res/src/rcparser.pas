
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
  bitmapresource, versionresource, versiontypes, groupcursorresource;

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
  writeln(ErrOutput, yyfilename, '(',yylineno,':',yycolno,'): at "',yytext,'": ', msg);
  WriteLn(ErrOutput, yyline);
  WriteLn(ErrOutput, '^':yycolno);
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
  if Result.long then
    setlength(s, Length(s) - 1);
  if Copy(s, 1, 2) = '0x' then
    Result.v:= StrToInt('$' + Copy(s, 3, Maxint))
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
    RT_VERSION: cls:= TVersionResource;
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
      r.Version:= aktresource.Version;
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

function make_version(a, b, c, d: Word): TFileProductVersion;
begin
  Result[0]:= a;
  Result[1]:= b;
  Result[2]:= c;
  Result[3]:= d;
end;

procedure version_string_tab_begin(lcs: string);
var
  vst: TVersionStringTable;
begin
  vst:= TVersionStringTable.Create(lcs);
  TVersionResource(aktresource).StringFileInfo.Add(vst);
end;

procedure version_string_tab_add(key, value: string);
begin
  TVersionResource(aktresource).StringFileInfo.Items[TVersionResource(aktresource).StringFileInfo.Count-1].Add(key, value);
end;

procedure version_var_translation_add(langid, cpid: word);
var
  ti: TVerTranslationInfo;
begin
  ti.language:= langid;
  ti.codepage:= cpid;
  TVersionResource(aktresource).VarFileInfo.Add(ti);
end;

var
  yycapture: AnsiString;
const _ILLEGAL = 257;
const _NUMDECIMAL = 258;
const _NUMHEX = 259;
const _QUOTEDSTR = 260;
const _STR_StringFileInfo = 261;
const _STR_VarFileInfo = 262;
const _STR_Translation = 263;
const _BEGIN = 264;
const _END = 265;
const _ID = 266;
const _LANGUAGE = 267;
const _CHARACTERISTICS = 268;
const _VERSION = 269;
const _MOVEABLE = 270;
const _FIXED = 271;
const _PURE = 272;
const _IMPURE = 273;
const _PRELOAD = 274;
const _LOADONCALL = 275;
const _DISCARDABLE = 276;
const _BITMAP = 277;
const _CURSOR = 278;
const _ICON = 279;
const _STRINGTABLE = 280;
const _VERSIONINFO = 281;
const _ANICURSOR = 282;
const _ANIICON = 283;
const _DLGINCLUDE = 284;
const _DLGINIT = 285;
const _HTML = 286;
const _MANIFEST = 287;
const _MESSAGETABLE = 288;
const _PLUGPLAY = 289;
const _RCDATA = 290;
const _VXD = 291;
const _FILEVERSION = 292;
const _PRODUCTVERSION = 293;
const _FILEFLAGSMASK = 294;
const _FILEFLAGS = 295;
const _FILEOS = 296;
const _FILETYPE = 297;
const _FILESUBTYPE = 298;
const _BLOCK = 299;
const _VALUE = 300;
const _ACCELERATORS = 301;
const _DIALOG = 302;
const _DIALOGEX = 303;
const _MENU = 304;
const _MENUEX = 305;

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
         yyval := yyv[yysp-0];
       end;
  11 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_BITMAP); 
       end;
  12 : begin
         TBitmapResource(aktresource).SetCustomBitmapDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  13 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_CURSOR); 
       end;
  14 : begin
         TGroupCursorResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  15 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_ICON); 
       end;
  16 : begin
         TGroupIconResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  17 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_VERSION); 
       end;
  18 : begin
         yyval := yyv[yysp-6];
       end;
  19 : begin
       end;
  20 : begin
         TVersionResource(aktresource).FixedInfo.FileVersion:= make_version(yyv[yysp-6].yyrcnumtype.v, yyv[yysp-4].yyrcnumtype.v, yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  21 : begin
         TVersionResource(aktresource).FixedInfo.ProductVersion:= make_version(yyv[yysp-6].yyrcnumtype.v, yyv[yysp-4].yyrcnumtype.v, yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  22 : begin
         TVersionResource(aktresource).FixedInfo.FileFlagsMask:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  23 : begin
         TVersionResource(aktresource).FixedInfo.FileFlags:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  24 : begin
         TVersionResource(aktresource).FixedInfo.FileOS:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  25 : begin
         TVersionResource(aktresource).FixedInfo.FileType:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  26 : begin
         TVersionResource(aktresource).FixedInfo.FileSubType:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  27 : begin
       end;
  28 : begin
         yyval := yyv[yysp-5];
       end;
  29 : begin
         yyval := yyv[yysp-5];
       end;
  30 : begin
       end;
  31 : begin
         version_string_tab_begin(yyv[yysp-1].yyString); 
       end;
  32 : begin
         yyval := yyv[yysp-6];
       end;
  33 : begin
       end;
  34 : begin
         version_string_tab_add(yyv[yysp-2].yyString, yyv[yysp-0].yyString); 
       end;
  35 : begin
         yyval := yyv[yysp-3];
       end;
  36 : begin
         version_var_translation_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  37 : begin
         version_var_translation_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  38 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  39 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  40 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  41 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-1].yyTMemoryStream); 
       end;
  42 : begin
         stringtable_begin(); 
       end;
  43 : begin
         stringtable_end(); 
       end;
  44 : begin
       end;
  45 : begin
         yyval := yyv[yysp-1];
       end;
  46 : begin
         stringtable_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyString); 
       end;
  47 : begin
         stringtable_add(yyv[yysp-1].yyrcnumtype.v, yyv[yysp-0].yyString); 
       end;
  48 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANICURSOR); 
       end;
  49 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANIICON); 
       end;
  50 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINCLUDE); 
       end;
  51 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINIT); 
       end;
  52 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(23); 
       end;
  53 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MANIFEST); 
       end;
  54 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MESSAGETABLE); 
       end;
  55 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_PLUGPLAY); 
       end;
  56 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_RCDATA); 
       end;
  57 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_VXD); 
       end;
  58 : begin
         yyval := yyv[yysp-0];
       end;
  59 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcnumtype.v); 
       end;
  60 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyString); 
       end;
  61 : begin
       end;
  62 : begin
         yyval := yyv[yysp-1];
       end;
  63 : begin
         aktresource.LangID:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  64 : begin
         aktresource.Characteristics:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  65 : begin
         aktresource.Version:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  66 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; 
       end;
  67 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; 
       end;
  68 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; 
       end;
  69 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; 
       end;
  70 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; 
       end;
  71 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; 
       end;
  72 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; 
       end;
  73 : begin
         language:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  74 : begin
         yyval := yyv[yysp-0];
       end;
  75 : begin
         yyval.yyrcnumtype:= str_to_num(yytext); 
       end;
  76 : begin
         yyval.yyrcnumtype:= str_to_num(yytext); 
       end;
  77 : begin
         yyval := yyv[yysp-0];
       end;
  78 : begin
         yyval.yyrcnumtype:= yyv[yysp-1].yyrcnumtype; 
       end;
  79 : begin
         yyval.yyString:= yytext; 
       end;
  80 : begin
         yyval := yyv[yysp-0];
       end;
  81 : begin
         yyval.yyTFileStream:= TFileStream.Create(yytext, fmOpenRead or fmShareDenyWrite); 
       end;
  82 : begin
         yyval.yyString:= yytext; 
       end;
  83 : begin
         yyval.yyString:= yytext; 
       end;
  84 : begin
         yyval.yyString:= yytext; 
       end;
  85 : begin
         yyval.yyString:= yytext; 
       end;
  86 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  87 : begin
         yyval := yyv[yysp-1];
       end;
  88 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  89 : begin
         yyval := yyv[yysp-3];
       end;
  90 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         yyval.yyTMemoryStream.WriteBuffer(yyv[yysp-0].yyString[1], Length(yyv[yysp-0].yyString));
         
       end;
  91 : begin
         
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

yynacts   = 261;
yyngotos  = 121;
yynstates = 160;
yynrules  = 91;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 267; act: 25 ),
  ( sym: 280; act: 26 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 266; act: 24 ),
  ( sym: 277; act: 29 ),
  ( sym: 278; act: 30 ),
  ( sym: 279; act: 31 ),
  ( sym: 281; act: 32 ),
  ( sym: 282; act: 33 ),
  ( sym: 283; act: 34 ),
  ( sym: 284; act: 35 ),
  ( sym: 285; act: 36 ),
  ( sym: 286; act: 37 ),
  ( sym: 287; act: 38 ),
  ( sym: 288; act: 39 ),
  ( sym: 289; act: 40 ),
  ( sym: 290; act: 41 ),
  ( sym: 291; act: 42 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 26: }
{ 27: }
  ( sym: 260; act: -38 ),
  ( sym: 261; act: -38 ),
  ( sym: 262; act: -38 ),
  ( sym: 263; act: -38 ),
  ( sym: 267; act: -38 ),
  ( sym: 268; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 270; act: -38 ),
  ( sym: 271; act: -38 ),
  ( sym: 272; act: -38 ),
  ( sym: 273; act: -38 ),
  ( sym: 274; act: -38 ),
  ( sym: 275; act: -38 ),
  ( sym: 276; act: -38 ),
  ( sym: 264; act: -40 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  ( sym: 41; act: 52 ),
{ 44: }
  ( sym: 44; act: 53 ),
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 54: }
  ( sym: 264; act: 63 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 55: }
  ( sym: 264; act: 74 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 56: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 57: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 58: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 59: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 267; act: 64 ),
  ( sym: 268; act: 65 ),
  ( sym: 269; act: 66 ),
  ( sym: 270; act: 67 ),
  ( sym: 271; act: 68 ),
  ( sym: 272; act: 69 ),
  ( sym: 273; act: 70 ),
  ( sym: 274; act: 71 ),
  ( sym: 275; act: 72 ),
  ( sym: 276; act: 73 ),
{ 60: }
  ( sym: 264; act: 80 ),
  ( sym: 292; act: 81 ),
  ( sym: 293; act: 82 ),
  ( sym: 294; act: 83 ),
  ( sym: 295; act: 84 ),
  ( sym: 296; act: 85 ),
  ( sym: 297; act: 86 ),
  ( sym: 298; act: 87 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 65: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 66: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 82: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 83: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 84: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 85: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 86: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 87: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 88: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
  ( sym: 265; act: 104 ),
{ 89: }
  ( sym: 44; act: 105 ),
{ 90: }
{ 91: }
{ 92: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 93: }
  ( sym: 44; act: 109 ),
  ( sym: 265; act: 110 ),
{ 94: }
  ( sym: 265; act: 111 ),
  ( sym: 299; act: 112 ),
{ 95: }
  ( sym: 44; act: 113 ),
{ 96: }
  ( sym: 44; act: 114 ),
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
  ( sym: 44; act: 116 ),
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 104: }
{ 105: }
  ( sym: 40; act: 17 ),
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
{ 111: }
{ 112: }
  ( sym: 261; act: 119 ),
  ( sym: 262; act: 120 ),
{ 113: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 114: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 115: }
{ 116: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 117: }
{ 118: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 119: }
  ( sym: 264; act: 125 ),
{ 120: }
  ( sym: 264; act: 126 ),
{ 121: }
  ( sym: 44; act: 127 ),
{ 122: }
  ( sym: 44; act: 128 ),
{ 123: }
{ 124: }
{ 125: }
{ 126: }
  ( sym: 300; act: 131 ),
{ 127: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 128: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 129: }
  ( sym: 265; act: 134 ),
  ( sym: 299; act: 135 ),
{ 130: }
  ( sym: 265; act: 136 ),
{ 131: }
  ( sym: 263; act: 137 ),
{ 132: }
  ( sym: 44; act: 138 ),
{ 133: }
  ( sym: 44; act: 139 ),
{ 134: }
{ 135: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 136: }
{ 137: }
  ( sym: 44; act: 141 ),
{ 138: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 139: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 140: }
  ( sym: 264; act: 144 ),
{ 141: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 142: }
{ 143: }
{ 144: }
{ 145: }
  ( sym: 44; act: 148 ),
  ( sym: 265; act: -35 ),
{ 146: }
  ( sym: 44; act: 149 ),
{ 147: }
{ 148: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 149: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 150: }
  ( sym: 265; act: 153 ),
  ( sym: 300; act: 154 ),
{ 151: }
  ( sym: 44; act: 155 ),
{ 152: }
{ 153: }
{ 154: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
{ 155: }
  ( sym: 258; act: 18 ),
  ( sym: 259; act: 19 ),
{ 156: }
  ( sym: 44; act: 158 ),
{ 157: }
{ 158: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 )
{ 159: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -12; act: 1 ),
{ 1: }
  ( sym: -21; act: 2 ),
  ( sym: -20; act: 3 ),
  ( sym: -19; act: 4 ),
  ( sym: -18; act: 5 ),
  ( sym: -17; act: 6 ),
  ( sym: -16; act: 7 ),
  ( sym: -15; act: 8 ),
  ( sym: -14; act: 9 ),
  ( sym: -13; act: 10 ),
  ( sym: -8; act: 11 ),
  ( sym: -7; act: 12 ),
  ( sym: -5; act: 13 ),
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 16 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
  ( sym: -9; act: 27 ),
  ( sym: -8; act: 28 ),
  ( sym: -7; act: 12 ),
  ( sym: -5; act: 13 ),
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 16 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 43 ),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 44 ),
{ 26: }
  ( sym: -36; act: 45 ),
{ 27: }
  ( sym: -35; act: 46 ),
  ( sym: -34; act: 47 ),
{ 28: }
{ 29: }
  ( sym: -23; act: 48 ),
{ 30: }
  ( sym: -24; act: 49 ),
{ 31: }
  ( sym: -25; act: 50 ),
{ 32: }
  ( sym: -27; act: 51 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: -22; act: 54 ),
{ 46: }
  ( sym: -22; act: 55 ),
{ 47: }
  ( sym: -22; act: 56 ),
{ 48: }
  ( sym: -22; act: 57 ),
{ 49: }
  ( sym: -22; act: 58 ),
{ 50: }
  ( sym: -22; act: 59 ),
{ 51: }
  ( sym: -26; act: 60 ),
{ 52: }
{ 53: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 61 ),
{ 54: }
  ( sym: -39; act: 62 ),
{ 55: }
  ( sym: -39; act: 62 ),
{ 56: }
  ( sym: -39; act: 62 ),
  ( sym: -7; act: 75 ),
  ( sym: -6; act: 76 ),
{ 57: }
  ( sym: -39; act: 62 ),
  ( sym: -7; act: 75 ),
  ( sym: -6; act: 77 ),
{ 58: }
  ( sym: -39; act: 62 ),
  ( sym: -7; act: 75 ),
  ( sym: -6; act: 78 ),
{ 59: }
  ( sym: -39; act: 62 ),
  ( sym: -7; act: 75 ),
  ( sym: -6; act: 79 ),
{ 60: }
{ 61: }
{ 62: }
{ 63: }
  ( sym: -37; act: 88 ),
{ 64: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 89 ),
{ 65: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 90 ),
{ 66: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 91 ),
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
  ( sym: -40; act: 92 ),
  ( sym: -10; act: 93 ),
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
{ 80: }
  ( sym: -28; act: 94 ),
{ 81: }
  ( sym: -4; act: 95 ),
{ 82: }
  ( sym: -4; act: 96 ),
{ 83: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 97 ),
{ 84: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 98 ),
{ 85: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 99 ),
{ 86: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 100 ),
{ 87: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 101 ),
{ 88: }
  ( sym: -38; act: 102 ),
  ( sym: -4; act: 103 ),
{ 89: }
{ 90: }
{ 91: }
{ 92: }
  ( sym: -11; act: 106 ),
  ( sym: -7; act: 107 ),
  ( sym: -4; act: 108 ),
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
  ( sym: -7; act: 115 ),
{ 104: }
{ 105: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 117 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: -41; act: 118 ),
{ 110: }
{ 111: }
{ 112: }
{ 113: }
  ( sym: -4; act: 121 ),
{ 114: }
  ( sym: -4; act: 122 ),
{ 115: }
{ 116: }
  ( sym: -7; act: 123 ),
{ 117: }
{ 118: }
  ( sym: -11; act: 124 ),
  ( sym: -7; act: 107 ),
  ( sym: -4; act: 108 ),
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
  ( sym: -29; act: 129 ),
{ 126: }
  ( sym: -30; act: 130 ),
{ 127: }
  ( sym: -4; act: 132 ),
{ 128: }
  ( sym: -4; act: 133 ),
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: -7; act: 140 ),
{ 136: }
{ 137: }
{ 138: }
  ( sym: -4; act: 142 ),
{ 139: }
  ( sym: -4; act: 143 ),
{ 140: }
{ 141: }
  ( sym: -33; act: 145 ),
  ( sym: -4; act: 146 ),
{ 142: }
{ 143: }
{ 144: }
  ( sym: -32; act: 147 ),
{ 145: }
{ 146: }
{ 147: }
  ( sym: -31; act: 150 ),
{ 148: }
  ( sym: -4; act: 151 ),
{ 149: }
  ( sym: -4; act: 152 ),
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
  ( sym: -7; act: 156 ),
{ 155: }
  ( sym: -4; act: 157 ),
{ 156: }
{ 157: }
{ 158: }
  ( sym: -7; act: 159 )
{ 159: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } -10,
{ 3: } -9,
{ 4: } -8,
{ 5: } -7,
{ 6: } -6,
{ 7: } -5,
{ 8: } -4,
{ 9: } -3,
{ 10: } -2,
{ 11: } 0,
{ 12: } -80,
{ 13: } -60,
{ 14: } -77,
{ 15: } -74,
{ 16: } -59,
{ 17: } 0,
{ 18: } -75,
{ 19: } -76,
{ 20: } -82,
{ 21: } -83,
{ 22: } -84,
{ 23: } -85,
{ 24: } -79,
{ 25: } 0,
{ 26: } -42,
{ 27: } 0,
{ 28: } -58,
{ 29: } -11,
{ 30: } -13,
{ 31: } -15,
{ 32: } -17,
{ 33: } -48,
{ 34: } -49,
{ 35: } -50,
{ 36: } -51,
{ 37: } -52,
{ 38: } -53,
{ 39: } -54,
{ 40: } -55,
{ 41: } -56,
{ 42: } -57,
{ 43: } 0,
{ 44: } 0,
{ 45: } -61,
{ 46: } -61,
{ 47: } -61,
{ 48: } -61,
{ 49: } -61,
{ 50: } -61,
{ 51: } -19,
{ 52: } -78,
{ 53: } 0,
{ 54: } 0,
{ 55: } 0,
{ 56: } 0,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } 0,
{ 61: } -73,
{ 62: } -62,
{ 63: } -44,
{ 64: } 0,
{ 65: } 0,
{ 66: } 0,
{ 67: } -66,
{ 68: } -67,
{ 69: } -68,
{ 70: } -69,
{ 71: } -70,
{ 72: } -71,
{ 73: } -72,
{ 74: } -86,
{ 75: } -81,
{ 76: } -39,
{ 77: } -12,
{ 78: } -14,
{ 79: } -16,
{ 80: } -27,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } -64,
{ 91: } -65,
{ 92: } 0,
{ 93: } 0,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } -22,
{ 98: } -23,
{ 99: } -24,
{ 100: } -25,
{ 101: } -26,
{ 102: } -45,
{ 103: } 0,
{ 104: } -43,
{ 105: } 0,
{ 106: } -87,
{ 107: } -90,
{ 108: } -91,
{ 109: } -88,
{ 110: } -41,
{ 111: } -18,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } -47,
{ 116: } 0,
{ 117: } -63,
{ 118: } 0,
{ 119: } 0,
{ 120: } 0,
{ 121: } 0,
{ 122: } 0,
{ 123: } -46,
{ 124: } -89,
{ 125: } -30,
{ 126: } 0,
{ 127: } 0,
{ 128: } 0,
{ 129: } 0,
{ 130: } 0,
{ 131: } 0,
{ 132: } 0,
{ 133: } 0,
{ 134: } -28,
{ 135: } 0,
{ 136: } -29,
{ 137: } 0,
{ 138: } 0,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } -20,
{ 143: } -21,
{ 144: } -31,
{ 145: } 0,
{ 146: } 0,
{ 147: } -33,
{ 148: } 0,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } -36,
{ 153: } -32,
{ 154: } 0,
{ 155: } 0,
{ 156: } 0,
{ 157: } -37,
{ 158: } 0,
{ 159: } -34
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 12,
{ 3: } 12,
{ 4: } 12,
{ 5: } 12,
{ 6: } 12,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 12,
{ 12: } 34,
{ 13: } 34,
{ 14: } 34,
{ 15: } 34,
{ 16: } 34,
{ 17: } 34,
{ 18: } 37,
{ 19: } 37,
{ 20: } 37,
{ 21: } 37,
{ 22: } 37,
{ 23: } 37,
{ 24: } 37,
{ 25: } 37,
{ 26: } 40,
{ 27: } 40,
{ 28: } 55,
{ 29: } 55,
{ 30: } 55,
{ 31: } 55,
{ 32: } 55,
{ 33: } 55,
{ 34: } 55,
{ 35: } 55,
{ 36: } 55,
{ 37: } 55,
{ 38: } 55,
{ 39: } 55,
{ 40: } 55,
{ 41: } 55,
{ 42: } 55,
{ 43: } 55,
{ 44: } 56,
{ 45: } 57,
{ 46: } 57,
{ 47: } 57,
{ 48: } 57,
{ 49: } 57,
{ 50: } 57,
{ 51: } 57,
{ 52: } 57,
{ 53: } 57,
{ 54: } 60,
{ 55: } 71,
{ 56: } 82,
{ 57: } 96,
{ 58: } 110,
{ 59: } 124,
{ 60: } 138,
{ 61: } 146,
{ 62: } 146,
{ 63: } 146,
{ 64: } 146,
{ 65: } 149,
{ 66: } 152,
{ 67: } 155,
{ 68: } 155,
{ 69: } 155,
{ 70: } 155,
{ 71: } 155,
{ 72: } 155,
{ 73: } 155,
{ 74: } 155,
{ 75: } 155,
{ 76: } 155,
{ 77: } 155,
{ 78: } 155,
{ 79: } 155,
{ 80: } 155,
{ 81: } 155,
{ 82: } 157,
{ 83: } 159,
{ 84: } 162,
{ 85: } 165,
{ 86: } 168,
{ 87: } 171,
{ 88: } 174,
{ 89: } 177,
{ 90: } 178,
{ 91: } 178,
{ 92: } 178,
{ 93: } 184,
{ 94: } 186,
{ 95: } 188,
{ 96: } 189,
{ 97: } 190,
{ 98: } 190,
{ 99: } 190,
{ 100: } 190,
{ 101: } 190,
{ 102: } 190,
{ 103: } 190,
{ 104: } 195,
{ 105: } 195,
{ 106: } 198,
{ 107: } 198,
{ 108: } 198,
{ 109: } 198,
{ 110: } 198,
{ 111: } 198,
{ 112: } 198,
{ 113: } 200,
{ 114: } 202,
{ 115: } 204,
{ 116: } 204,
{ 117: } 208,
{ 118: } 208,
{ 119: } 214,
{ 120: } 215,
{ 121: } 216,
{ 122: } 217,
{ 123: } 218,
{ 124: } 218,
{ 125: } 218,
{ 126: } 218,
{ 127: } 219,
{ 128: } 221,
{ 129: } 223,
{ 130: } 225,
{ 131: } 226,
{ 132: } 227,
{ 133: } 228,
{ 134: } 229,
{ 135: } 229,
{ 136: } 233,
{ 137: } 233,
{ 138: } 234,
{ 139: } 236,
{ 140: } 238,
{ 141: } 239,
{ 142: } 241,
{ 143: } 241,
{ 144: } 241,
{ 145: } 241,
{ 146: } 243,
{ 147: } 244,
{ 148: } 244,
{ 149: } 246,
{ 150: } 248,
{ 151: } 250,
{ 152: } 251,
{ 153: } 251,
{ 154: } 251,
{ 155: } 255,
{ 156: } 257,
{ 157: } 258,
{ 158: } 258,
{ 159: } 262
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 11,
{ 2: } 11,
{ 3: } 11,
{ 4: } 11,
{ 5: } 11,
{ 6: } 11,
{ 7: } 11,
{ 8: } 11,
{ 9: } 11,
{ 10: } 11,
{ 11: } 33,
{ 12: } 33,
{ 13: } 33,
{ 14: } 33,
{ 15: } 33,
{ 16: } 33,
{ 17: } 36,
{ 18: } 36,
{ 19: } 36,
{ 20: } 36,
{ 21: } 36,
{ 22: } 36,
{ 23: } 36,
{ 24: } 36,
{ 25: } 39,
{ 26: } 39,
{ 27: } 54,
{ 28: } 54,
{ 29: } 54,
{ 30: } 54,
{ 31: } 54,
{ 32: } 54,
{ 33: } 54,
{ 34: } 54,
{ 35: } 54,
{ 36: } 54,
{ 37: } 54,
{ 38: } 54,
{ 39: } 54,
{ 40: } 54,
{ 41: } 54,
{ 42: } 54,
{ 43: } 55,
{ 44: } 56,
{ 45: } 56,
{ 46: } 56,
{ 47: } 56,
{ 48: } 56,
{ 49: } 56,
{ 50: } 56,
{ 51: } 56,
{ 52: } 56,
{ 53: } 59,
{ 54: } 70,
{ 55: } 81,
{ 56: } 95,
{ 57: } 109,
{ 58: } 123,
{ 59: } 137,
{ 60: } 145,
{ 61: } 145,
{ 62: } 145,
{ 63: } 145,
{ 64: } 148,
{ 65: } 151,
{ 66: } 154,
{ 67: } 154,
{ 68: } 154,
{ 69: } 154,
{ 70: } 154,
{ 71: } 154,
{ 72: } 154,
{ 73: } 154,
{ 74: } 154,
{ 75: } 154,
{ 76: } 154,
{ 77: } 154,
{ 78: } 154,
{ 79: } 154,
{ 80: } 154,
{ 81: } 156,
{ 82: } 158,
{ 83: } 161,
{ 84: } 164,
{ 85: } 167,
{ 86: } 170,
{ 87: } 173,
{ 88: } 176,
{ 89: } 177,
{ 90: } 177,
{ 91: } 177,
{ 92: } 183,
{ 93: } 185,
{ 94: } 187,
{ 95: } 188,
{ 96: } 189,
{ 97: } 189,
{ 98: } 189,
{ 99: } 189,
{ 100: } 189,
{ 101: } 189,
{ 102: } 189,
{ 103: } 194,
{ 104: } 194,
{ 105: } 197,
{ 106: } 197,
{ 107: } 197,
{ 108: } 197,
{ 109: } 197,
{ 110: } 197,
{ 111: } 197,
{ 112: } 199,
{ 113: } 201,
{ 114: } 203,
{ 115: } 203,
{ 116: } 207,
{ 117: } 207,
{ 118: } 213,
{ 119: } 214,
{ 120: } 215,
{ 121: } 216,
{ 122: } 217,
{ 123: } 217,
{ 124: } 217,
{ 125: } 217,
{ 126: } 218,
{ 127: } 220,
{ 128: } 222,
{ 129: } 224,
{ 130: } 225,
{ 131: } 226,
{ 132: } 227,
{ 133: } 228,
{ 134: } 228,
{ 135: } 232,
{ 136: } 232,
{ 137: } 233,
{ 138: } 235,
{ 139: } 237,
{ 140: } 238,
{ 141: } 240,
{ 142: } 240,
{ 143: } 240,
{ 144: } 240,
{ 145: } 242,
{ 146: } 243,
{ 147: } 243,
{ 148: } 245,
{ 149: } 247,
{ 150: } 249,
{ 151: } 250,
{ 152: } 250,
{ 153: } 250,
{ 154: } 254,
{ 155: } 256,
{ 156: } 257,
{ 157: } 257,
{ 158: } 261,
{ 159: } 261
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 17,
{ 3: } 17,
{ 4: } 17,
{ 5: } 17,
{ 6: } 17,
{ 7: } 17,
{ 8: } 17,
{ 9: } 17,
{ 10: } 17,
{ 11: } 17,
{ 12: } 24,
{ 13: } 24,
{ 14: } 24,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 26,
{ 19: } 26,
{ 20: } 26,
{ 21: } 26,
{ 22: } 26,
{ 23: } 26,
{ 24: } 26,
{ 25: } 26,
{ 26: } 29,
{ 27: } 30,
{ 28: } 32,
{ 29: } 32,
{ 30: } 33,
{ 31: } 34,
{ 32: } 35,
{ 33: } 36,
{ 34: } 36,
{ 35: } 36,
{ 36: } 36,
{ 37: } 36,
{ 38: } 36,
{ 39: } 36,
{ 40: } 36,
{ 41: } 36,
{ 42: } 36,
{ 43: } 36,
{ 44: } 36,
{ 45: } 36,
{ 46: } 37,
{ 47: } 38,
{ 48: } 39,
{ 49: } 40,
{ 50: } 41,
{ 51: } 42,
{ 52: } 43,
{ 53: } 43,
{ 54: } 46,
{ 55: } 47,
{ 56: } 48,
{ 57: } 51,
{ 58: } 54,
{ 59: } 57,
{ 60: } 60,
{ 61: } 60,
{ 62: } 60,
{ 63: } 60,
{ 64: } 61,
{ 65: } 64,
{ 66: } 67,
{ 67: } 70,
{ 68: } 70,
{ 69: } 70,
{ 70: } 70,
{ 71: } 70,
{ 72: } 70,
{ 73: } 70,
{ 74: } 70,
{ 75: } 72,
{ 76: } 72,
{ 77: } 72,
{ 78: } 72,
{ 79: } 72,
{ 80: } 72,
{ 81: } 73,
{ 82: } 74,
{ 83: } 75,
{ 84: } 78,
{ 85: } 81,
{ 86: } 84,
{ 87: } 87,
{ 88: } 90,
{ 89: } 92,
{ 90: } 92,
{ 91: } 92,
{ 92: } 92,
{ 93: } 95,
{ 94: } 95,
{ 95: } 95,
{ 96: } 95,
{ 97: } 95,
{ 98: } 95,
{ 99: } 95,
{ 100: } 95,
{ 101: } 95,
{ 102: } 95,
{ 103: } 95,
{ 104: } 96,
{ 105: } 96,
{ 106: } 99,
{ 107: } 99,
{ 108: } 99,
{ 109: } 99,
{ 110: } 100,
{ 111: } 100,
{ 112: } 100,
{ 113: } 100,
{ 114: } 101,
{ 115: } 102,
{ 116: } 102,
{ 117: } 103,
{ 118: } 103,
{ 119: } 106,
{ 120: } 106,
{ 121: } 106,
{ 122: } 106,
{ 123: } 106,
{ 124: } 106,
{ 125: } 106,
{ 126: } 107,
{ 127: } 108,
{ 128: } 109,
{ 129: } 110,
{ 130: } 110,
{ 131: } 110,
{ 132: } 110,
{ 133: } 110,
{ 134: } 110,
{ 135: } 110,
{ 136: } 111,
{ 137: } 111,
{ 138: } 111,
{ 139: } 112,
{ 140: } 113,
{ 141: } 113,
{ 142: } 115,
{ 143: } 115,
{ 144: } 115,
{ 145: } 116,
{ 146: } 116,
{ 147: } 116,
{ 148: } 117,
{ 149: } 118,
{ 150: } 119,
{ 151: } 119,
{ 152: } 119,
{ 153: } 119,
{ 154: } 119,
{ 155: } 120,
{ 156: } 121,
{ 157: } 121,
{ 158: } 121,
{ 159: } 122
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 16,
{ 2: } 16,
{ 3: } 16,
{ 4: } 16,
{ 5: } 16,
{ 6: } 16,
{ 7: } 16,
{ 8: } 16,
{ 9: } 16,
{ 10: } 16,
{ 11: } 23,
{ 12: } 23,
{ 13: } 23,
{ 14: } 23,
{ 15: } 23,
{ 16: } 23,
{ 17: } 25,
{ 18: } 25,
{ 19: } 25,
{ 20: } 25,
{ 21: } 25,
{ 22: } 25,
{ 23: } 25,
{ 24: } 25,
{ 25: } 28,
{ 26: } 29,
{ 27: } 31,
{ 28: } 31,
{ 29: } 32,
{ 30: } 33,
{ 31: } 34,
{ 32: } 35,
{ 33: } 35,
{ 34: } 35,
{ 35: } 35,
{ 36: } 35,
{ 37: } 35,
{ 38: } 35,
{ 39: } 35,
{ 40: } 35,
{ 41: } 35,
{ 42: } 35,
{ 43: } 35,
{ 44: } 35,
{ 45: } 36,
{ 46: } 37,
{ 47: } 38,
{ 48: } 39,
{ 49: } 40,
{ 50: } 41,
{ 51: } 42,
{ 52: } 42,
{ 53: } 45,
{ 54: } 46,
{ 55: } 47,
{ 56: } 50,
{ 57: } 53,
{ 58: } 56,
{ 59: } 59,
{ 60: } 59,
{ 61: } 59,
{ 62: } 59,
{ 63: } 60,
{ 64: } 63,
{ 65: } 66,
{ 66: } 69,
{ 67: } 69,
{ 68: } 69,
{ 69: } 69,
{ 70: } 69,
{ 71: } 69,
{ 72: } 69,
{ 73: } 69,
{ 74: } 71,
{ 75: } 71,
{ 76: } 71,
{ 77: } 71,
{ 78: } 71,
{ 79: } 71,
{ 80: } 72,
{ 81: } 73,
{ 82: } 74,
{ 83: } 77,
{ 84: } 80,
{ 85: } 83,
{ 86: } 86,
{ 87: } 89,
{ 88: } 91,
{ 89: } 91,
{ 90: } 91,
{ 91: } 91,
{ 92: } 94,
{ 93: } 94,
{ 94: } 94,
{ 95: } 94,
{ 96: } 94,
{ 97: } 94,
{ 98: } 94,
{ 99: } 94,
{ 100: } 94,
{ 101: } 94,
{ 102: } 94,
{ 103: } 95,
{ 104: } 95,
{ 105: } 98,
{ 106: } 98,
{ 107: } 98,
{ 108: } 98,
{ 109: } 99,
{ 110: } 99,
{ 111: } 99,
{ 112: } 99,
{ 113: } 100,
{ 114: } 101,
{ 115: } 101,
{ 116: } 102,
{ 117: } 102,
{ 118: } 105,
{ 119: } 105,
{ 120: } 105,
{ 121: } 105,
{ 122: } 105,
{ 123: } 105,
{ 124: } 105,
{ 125: } 106,
{ 126: } 107,
{ 127: } 108,
{ 128: } 109,
{ 129: } 109,
{ 130: } 109,
{ 131: } 109,
{ 132: } 109,
{ 133: } 109,
{ 134: } 109,
{ 135: } 110,
{ 136: } 110,
{ 137: } 110,
{ 138: } 111,
{ 139: } 112,
{ 140: } 112,
{ 141: } 114,
{ 142: } 114,
{ 143: } 114,
{ 144: } 115,
{ 145: } 115,
{ 146: } 115,
{ 147: } 116,
{ 148: } 117,
{ 149: } 118,
{ 150: } 118,
{ 151: } 118,
{ 152: } 118,
{ 153: } 118,
{ 154: } 119,
{ 155: } 120,
{ 156: } 120,
{ 157: } 120,
{ 158: } 121,
{ 159: } 121
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -12 ),
{ 2: } ( len: 2; sym: -12 ),
{ 3: } ( len: 1; sym: -13 ),
{ 4: } ( len: 1; sym: -13 ),
{ 5: } ( len: 1; sym: -14 ),
{ 6: } ( len: 1; sym: -14 ),
{ 7: } ( len: 1; sym: -14 ),
{ 8: } ( len: 1; sym: -14 ),
{ 9: } ( len: 1; sym: -14 ),
{ 10: } ( len: 1; sym: -14 ),
{ 11: } ( len: 0; sym: -23 ),
{ 12: } ( len: 5; sym: -17 ),
{ 13: } ( len: 0; sym: -24 ),
{ 14: } ( len: 5; sym: -18 ),
{ 15: } ( len: 0; sym: -25 ),
{ 16: } ( len: 5; sym: -19 ),
{ 17: } ( len: 0; sym: -27 ),
{ 18: } ( len: 7; sym: -20 ),
{ 19: } ( len: 0; sym: -26 ),
{ 20: } ( len: 9; sym: -26 ),
{ 21: } ( len: 9; sym: -26 ),
{ 22: } ( len: 3; sym: -26 ),
{ 23: } ( len: 3; sym: -26 ),
{ 24: } ( len: 3; sym: -26 ),
{ 25: } ( len: 3; sym: -26 ),
{ 26: } ( len: 3; sym: -26 ),
{ 27: } ( len: 0; sym: -28 ),
{ 28: } ( len: 6; sym: -28 ),
{ 29: } ( len: 6; sym: -28 ),
{ 30: } ( len: 0; sym: -29 ),
{ 31: } ( len: 0; sym: -32 ),
{ 32: } ( len: 7; sym: -29 ),
{ 33: } ( len: 0; sym: -31 ),
{ 34: } ( len: 5; sym: -31 ),
{ 35: } ( len: 4; sym: -30 ),
{ 36: } ( len: 3; sym: -33 ),
{ 37: } ( len: 5; sym: -33 ),
{ 38: } ( len: 0; sym: -34 ),
{ 39: } ( len: 5; sym: -21 ),
{ 40: } ( len: 0; sym: -35 ),
{ 41: } ( len: 7; sym: -21 ),
{ 42: } ( len: 0; sym: -36 ),
{ 43: } ( len: 6; sym: -16 ),
{ 44: } ( len: 0; sym: -37 ),
{ 45: } ( len: 2; sym: -37 ),
{ 46: } ( len: 3; sym: -38 ),
{ 47: } ( len: 2; sym: -38 ),
{ 48: } ( len: 1; sym: -9 ),
{ 49: } ( len: 1; sym: -9 ),
{ 50: } ( len: 1; sym: -9 ),
{ 51: } ( len: 1; sym: -9 ),
{ 52: } ( len: 1; sym: -9 ),
{ 53: } ( len: 1; sym: -9 ),
{ 54: } ( len: 1; sym: -9 ),
{ 55: } ( len: 1; sym: -9 ),
{ 56: } ( len: 1; sym: -9 ),
{ 57: } ( len: 1; sym: -9 ),
{ 58: } ( len: 1; sym: -9 ),
{ 59: } ( len: 1; sym: -8 ),
{ 60: } ( len: 1; sym: -8 ),
{ 61: } ( len: 0; sym: -22 ),
{ 62: } ( len: 2; sym: -22 ),
{ 63: } ( len: 4; sym: -39 ),
{ 64: } ( len: 2; sym: -39 ),
{ 65: } ( len: 2; sym: -39 ),
{ 66: } ( len: 1; sym: -39 ),
{ 67: } ( len: 1; sym: -39 ),
{ 68: } ( len: 1; sym: -39 ),
{ 69: } ( len: 1; sym: -39 ),
{ 70: } ( len: 1; sym: -39 ),
{ 71: } ( len: 1; sym: -39 ),
{ 72: } ( len: 1; sym: -39 ),
{ 73: } ( len: 4; sym: -15 ),
{ 74: } ( len: 1; sym: -2 ),
{ 75: } ( len: 1; sym: -4 ),
{ 76: } ( len: 1; sym: -4 ),
{ 77: } ( len: 1; sym: -3 ),
{ 78: } ( len: 3; sym: -3 ),
{ 79: } ( len: 1; sym: -5 ),
{ 80: } ( len: 1; sym: -5 ),
{ 81: } ( len: 1; sym: -6 ),
{ 82: } ( len: 1; sym: -7 ),
{ 83: } ( len: 1; sym: -7 ),
{ 84: } ( len: 1; sym: -7 ),
{ 85: } ( len: 1; sym: -7 ),
{ 86: } ( len: 0; sym: -40 ),
{ 87: } ( len: 2; sym: -10 ),
{ 88: } ( len: 0; sym: -41 ),
{ 89: } ( len: 4; sym: -10 ),
{ 90: } ( len: 1; sym: -11 ),
{ 91: } ( len: 1; sym: -11 )
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
