
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

function Max(a, b: LongWord): LongWord; inline;
begin
  if a > b then
    Result:= a
  else
    Result:= b;
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
const _NUMBER = 258;
const _QUOTEDSTR = 259;
const _STR_StringFileInfo = 260;
const _STR_VarFileInfo = 261;
const _STR_Translation = 262;
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
const _STRINGTABLE = 279;
const _VERSIONINFO = 280;
const _ANICURSOR = 281;
const _ANIICON = 282;
const _DLGINCLUDE = 283;
const _DLGINIT = 284;
const _HTML = 285;
const _MANIFEST = 286;
const _MESSAGETABLE = 287;
const _PLUGPLAY = 288;
const _RCDATA = 289;
const _VXD = 290;
const _FILEVERSION = 291;
const _PRODUCTVERSION = 292;
const _FILEFLAGSMASK = 293;
const _FILEFLAGS = 294;
const _FILEOS = 295;
const _FILETYPE = 296;
const _FILESUBTYPE = 297;
const _BLOCK = 298;
const _VALUE = 299;
const _ACCELERATORS = 300;
const _DIALOG = 301;
const _DIALOGEX = 302;
const _MENU = 303;
const _MENUEX = 304;
const _NUMNEG = 305;

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
         yyval := yyv[yysp-0];
       end;
  77 : begin
         yyval.yyrcnumtype:= yyv[yysp-1].yyrcnumtype; 
       end;
  78 : begin
         yyval.yyrcnumtype.v:= not yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-0].yyrcnumtype.long; 
       end;
  79 : begin
         yyval.yyrcnumtype.v:= -yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-0].yyrcnumtype.long; 
       end;
  80 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v * yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  81 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v div Max(1, yyv[yysp-0].yyrcnumtype.v); yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  82 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v mod Max(1, yyv[yysp-0].yyrcnumtype.v); yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  83 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v + yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  84 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v - yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  85 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v and yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  86 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v xor yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  87 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v or yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  88 : begin
         yyval.yyString:= yytext; 
       end;
  89 : begin
         yyval := yyv[yysp-0];
       end;
  90 : begin
         yyval.yyTFileStream:= TFileStream.Create(yytext, fmOpenRead or fmShareDenyWrite); 
       end;
  91 : begin
         yyval.yyString:= yytext; 
       end;
  92 : begin
         yyval.yyString:= yytext; 
       end;
  93 : begin
         yyval.yyString:= yytext; 
       end;
  94 : begin
         yyval.yyString:= yytext; 
       end;
  95 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  96 : begin
         yyval := yyv[yysp-1];
       end;
  97 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  98 : begin
         yyval := yyv[yysp-3];
       end;
  99 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         yyval.yyTMemoryStream.WriteBuffer(yyv[yysp-0].yyString[1], Length(yyv[yysp-0].yyString));
         
       end;
 100 : begin
         
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

yynacts   = 619;
yyngotos  = 141;
yynstates = 179;
yynrules  = 100;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 265; act: 25 ),
  ( sym: 266; act: 26 ),
  ( sym: 279; act: 27 ),
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
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 265; act: 25 ),
  ( sym: 276; act: 30 ),
  ( sym: 277; act: 31 ),
  ( sym: 278; act: 32 ),
  ( sym: 280; act: 33 ),
  ( sym: 281; act: 34 ),
  ( sym: 282; act: 35 ),
  ( sym: 283; act: 36 ),
  ( sym: 284; act: 37 ),
  ( sym: 285; act: 38 ),
  ( sym: 286; act: 39 ),
  ( sym: 287; act: 40 ),
  ( sym: 288; act: 41 ),
  ( sym: 289; act: 42 ),
  ( sym: 290; act: 43 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: 37; act: 44 ),
  ( sym: 38; act: 45 ),
  ( sym: 42; act: 46 ),
  ( sym: 43; act: 47 ),
  ( sym: 45; act: 48 ),
  ( sym: 47; act: 49 ),
  ( sym: 94; act: 50 ),
  ( sym: 124; act: 51 ),
  ( sym: 0; act: -74 ),
  ( sym: 40; act: -74 ),
  ( sym: 44; act: -74 ),
  ( sym: 126; act: -74 ),
  ( sym: 258; act: -74 ),
  ( sym: 259; act: -74 ),
  ( sym: 260; act: -74 ),
  ( sym: 261; act: -74 ),
  ( sym: 262; act: -74 ),
  ( sym: 263; act: -74 ),
  ( sym: 265; act: -74 ),
  ( sym: 266; act: -74 ),
  ( sym: 267; act: -74 ),
  ( sym: 268; act: -74 ),
  ( sym: 269; act: -74 ),
  ( sym: 270; act: -74 ),
  ( sym: 271; act: -74 ),
  ( sym: 272; act: -74 ),
  ( sym: 273; act: -74 ),
  ( sym: 274; act: -74 ),
  ( sym: 275; act: -74 ),
  ( sym: 276; act: -74 ),
  ( sym: 277; act: -74 ),
  ( sym: 278; act: -74 ),
  ( sym: 279; act: -74 ),
  ( sym: 280; act: -74 ),
  ( sym: 281; act: -74 ),
  ( sym: 282; act: -74 ),
  ( sym: 283; act: -74 ),
  ( sym: 284; act: -74 ),
  ( sym: 285; act: -74 ),
  ( sym: 286; act: -74 ),
  ( sym: 287; act: -74 ),
  ( sym: 288; act: -74 ),
  ( sym: 289; act: -74 ),
  ( sym: 290; act: -74 ),
  ( sym: 291; act: -74 ),
  ( sym: 292; act: -74 ),
  ( sym: 293; act: -74 ),
  ( sym: 294; act: -74 ),
  ( sym: 295; act: -74 ),
  ( sym: 296; act: -74 ),
  ( sym: 297; act: -74 ),
{ 16: }
{ 17: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 18: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 19: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 27: }
{ 28: }
  ( sym: 259; act: -38 ),
  ( sym: 260; act: -38 ),
  ( sym: 261; act: -38 ),
  ( sym: 262; act: -38 ),
  ( sym: 266; act: -38 ),
  ( sym: 267; act: -38 ),
  ( sym: 268; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 270; act: -38 ),
  ( sym: 271; act: -38 ),
  ( sym: 272; act: -38 ),
  ( sym: 273; act: -38 ),
  ( sym: 274; act: -38 ),
  ( sym: 275; act: -38 ),
  ( sym: 263; act: -40 ),
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
{ 44: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 45: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 46: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 47: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 48: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 49: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 50: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 51: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 52: }
  ( sym: 37; act: 44 ),
  ( sym: 38; act: 45 ),
  ( sym: 41; act: 71 ),
  ( sym: 42; act: 46 ),
  ( sym: 43; act: 47 ),
  ( sym: 45; act: 48 ),
  ( sym: 47; act: 49 ),
  ( sym: 94; act: 50 ),
  ( sym: 124; act: 51 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: 44; act: 72 ),
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
  ( sym: 37; act: 44 ),
  ( sym: 42; act: 46 ),
  ( sym: 43; act: 47 ),
  ( sym: 45; act: 48 ),
  ( sym: 47; act: 49 ),
  ( sym: 0; act: -85 ),
  ( sym: 38; act: -85 ),
  ( sym: 40; act: -85 ),
  ( sym: 41; act: -85 ),
  ( sym: 44; act: -85 ),
  ( sym: 94; act: -85 ),
  ( sym: 124; act: -85 ),
  ( sym: 126; act: -85 ),
  ( sym: 258; act: -85 ),
  ( sym: 259; act: -85 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 262; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 265; act: -85 ),
  ( sym: 266; act: -85 ),
  ( sym: 267; act: -85 ),
  ( sym: 268; act: -85 ),
  ( sym: 269; act: -85 ),
  ( sym: 270; act: -85 ),
  ( sym: 271; act: -85 ),
  ( sym: 272; act: -85 ),
  ( sym: 273; act: -85 ),
  ( sym: 274; act: -85 ),
  ( sym: 275; act: -85 ),
  ( sym: 276; act: -85 ),
  ( sym: 277; act: -85 ),
  ( sym: 278; act: -85 ),
  ( sym: 279; act: -85 ),
  ( sym: 280; act: -85 ),
  ( sym: 281; act: -85 ),
  ( sym: 282; act: -85 ),
  ( sym: 283; act: -85 ),
  ( sym: 284; act: -85 ),
  ( sym: 285; act: -85 ),
  ( sym: 286; act: -85 ),
  ( sym: 287; act: -85 ),
  ( sym: 288; act: -85 ),
  ( sym: 289; act: -85 ),
  ( sym: 290; act: -85 ),
  ( sym: 291; act: -85 ),
  ( sym: 292; act: -85 ),
  ( sym: 293; act: -85 ),
  ( sym: 294; act: -85 ),
  ( sym: 295; act: -85 ),
  ( sym: 296; act: -85 ),
  ( sym: 297; act: -85 ),
{ 65: }
{ 66: }
  ( sym: 37; act: 44 ),
  ( sym: 42; act: 46 ),
  ( sym: 47; act: 49 ),
  ( sym: 0; act: -83 ),
  ( sym: 38; act: -83 ),
  ( sym: 40; act: -83 ),
  ( sym: 41; act: -83 ),
  ( sym: 43; act: -83 ),
  ( sym: 44; act: -83 ),
  ( sym: 45; act: -83 ),
  ( sym: 94; act: -83 ),
  ( sym: 124; act: -83 ),
  ( sym: 126; act: -83 ),
  ( sym: 258; act: -83 ),
  ( sym: 259; act: -83 ),
  ( sym: 260; act: -83 ),
  ( sym: 261; act: -83 ),
  ( sym: 262; act: -83 ),
  ( sym: 263; act: -83 ),
  ( sym: 265; act: -83 ),
  ( sym: 266; act: -83 ),
  ( sym: 267; act: -83 ),
  ( sym: 268; act: -83 ),
  ( sym: 269; act: -83 ),
  ( sym: 270; act: -83 ),
  ( sym: 271; act: -83 ),
  ( sym: 272; act: -83 ),
  ( sym: 273; act: -83 ),
  ( sym: 274; act: -83 ),
  ( sym: 275; act: -83 ),
  ( sym: 276; act: -83 ),
  ( sym: 277; act: -83 ),
  ( sym: 278; act: -83 ),
  ( sym: 279; act: -83 ),
  ( sym: 280; act: -83 ),
  ( sym: 281; act: -83 ),
  ( sym: 282; act: -83 ),
  ( sym: 283; act: -83 ),
  ( sym: 284; act: -83 ),
  ( sym: 285; act: -83 ),
  ( sym: 286; act: -83 ),
  ( sym: 287; act: -83 ),
  ( sym: 288; act: -83 ),
  ( sym: 289; act: -83 ),
  ( sym: 290; act: -83 ),
  ( sym: 291; act: -83 ),
  ( sym: 292; act: -83 ),
  ( sym: 293; act: -83 ),
  ( sym: 294; act: -83 ),
  ( sym: 295; act: -83 ),
  ( sym: 296; act: -83 ),
  ( sym: 297; act: -83 ),
{ 67: }
  ( sym: 37; act: 44 ),
  ( sym: 42; act: 46 ),
  ( sym: 47; act: 49 ),
  ( sym: 0; act: -84 ),
  ( sym: 38; act: -84 ),
  ( sym: 40; act: -84 ),
  ( sym: 41; act: -84 ),
  ( sym: 43; act: -84 ),
  ( sym: 44; act: -84 ),
  ( sym: 45; act: -84 ),
  ( sym: 94; act: -84 ),
  ( sym: 124; act: -84 ),
  ( sym: 126; act: -84 ),
  ( sym: 258; act: -84 ),
  ( sym: 259; act: -84 ),
  ( sym: 260; act: -84 ),
  ( sym: 261; act: -84 ),
  ( sym: 262; act: -84 ),
  ( sym: 263; act: -84 ),
  ( sym: 265; act: -84 ),
  ( sym: 266; act: -84 ),
  ( sym: 267; act: -84 ),
  ( sym: 268; act: -84 ),
  ( sym: 269; act: -84 ),
  ( sym: 270; act: -84 ),
  ( sym: 271; act: -84 ),
  ( sym: 272; act: -84 ),
  ( sym: 273; act: -84 ),
  ( sym: 274; act: -84 ),
  ( sym: 275; act: -84 ),
  ( sym: 276; act: -84 ),
  ( sym: 277; act: -84 ),
  ( sym: 278; act: -84 ),
  ( sym: 279; act: -84 ),
  ( sym: 280; act: -84 ),
  ( sym: 281; act: -84 ),
  ( sym: 282; act: -84 ),
  ( sym: 283; act: -84 ),
  ( sym: 284; act: -84 ),
  ( sym: 285; act: -84 ),
  ( sym: 286; act: -84 ),
  ( sym: 287; act: -84 ),
  ( sym: 288; act: -84 ),
  ( sym: 289; act: -84 ),
  ( sym: 290; act: -84 ),
  ( sym: 291; act: -84 ),
  ( sym: 292; act: -84 ),
  ( sym: 293; act: -84 ),
  ( sym: 294; act: -84 ),
  ( sym: 295; act: -84 ),
  ( sym: 296; act: -84 ),
  ( sym: 297; act: -84 ),
{ 68: }
{ 69: }
  ( sym: 37; act: 44 ),
  ( sym: 38; act: 45 ),
  ( sym: 42; act: 46 ),
  ( sym: 43; act: 47 ),
  ( sym: 45; act: 48 ),
  ( sym: 47; act: 49 ),
  ( sym: 0; act: -86 ),
  ( sym: 40; act: -86 ),
  ( sym: 41; act: -86 ),
  ( sym: 44; act: -86 ),
  ( sym: 94; act: -86 ),
  ( sym: 124; act: -86 ),
  ( sym: 126; act: -86 ),
  ( sym: 258; act: -86 ),
  ( sym: 259; act: -86 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 262; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 265; act: -86 ),
  ( sym: 266; act: -86 ),
  ( sym: 267; act: -86 ),
  ( sym: 268; act: -86 ),
  ( sym: 269; act: -86 ),
  ( sym: 270; act: -86 ),
  ( sym: 271; act: -86 ),
  ( sym: 272; act: -86 ),
  ( sym: 273; act: -86 ),
  ( sym: 274; act: -86 ),
  ( sym: 275; act: -86 ),
  ( sym: 276; act: -86 ),
  ( sym: 277; act: -86 ),
  ( sym: 278; act: -86 ),
  ( sym: 279; act: -86 ),
  ( sym: 280; act: -86 ),
  ( sym: 281; act: -86 ),
  ( sym: 282; act: -86 ),
  ( sym: 283; act: -86 ),
  ( sym: 284; act: -86 ),
  ( sym: 285; act: -86 ),
  ( sym: 286; act: -86 ),
  ( sym: 287; act: -86 ),
  ( sym: 288; act: -86 ),
  ( sym: 289; act: -86 ),
  ( sym: 290; act: -86 ),
  ( sym: 291; act: -86 ),
  ( sym: 292; act: -86 ),
  ( sym: 293; act: -86 ),
  ( sym: 294; act: -86 ),
  ( sym: 295; act: -86 ),
  ( sym: 296; act: -86 ),
  ( sym: 297; act: -86 ),
{ 70: }
  ( sym: 37; act: 44 ),
  ( sym: 38; act: 45 ),
  ( sym: 42; act: 46 ),
  ( sym: 43; act: 47 ),
  ( sym: 45; act: 48 ),
  ( sym: 47; act: 49 ),
  ( sym: 94; act: 50 ),
  ( sym: 0; act: -87 ),
  ( sym: 40; act: -87 ),
  ( sym: 41; act: -87 ),
  ( sym: 44; act: -87 ),
  ( sym: 124; act: -87 ),
  ( sym: 126; act: -87 ),
  ( sym: 258; act: -87 ),
  ( sym: 259; act: -87 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 262; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 265; act: -87 ),
  ( sym: 266; act: -87 ),
  ( sym: 267; act: -87 ),
  ( sym: 268; act: -87 ),
  ( sym: 269; act: -87 ),
  ( sym: 270; act: -87 ),
  ( sym: 271; act: -87 ),
  ( sym: 272; act: -87 ),
  ( sym: 273; act: -87 ),
  ( sym: 274; act: -87 ),
  ( sym: 275; act: -87 ),
  ( sym: 276; act: -87 ),
  ( sym: 277; act: -87 ),
  ( sym: 278; act: -87 ),
  ( sym: 279; act: -87 ),
  ( sym: 280; act: -87 ),
  ( sym: 281; act: -87 ),
  ( sym: 282; act: -87 ),
  ( sym: 283; act: -87 ),
  ( sym: 284; act: -87 ),
  ( sym: 285; act: -87 ),
  ( sym: 286; act: -87 ),
  ( sym: 287; act: -87 ),
  ( sym: 288; act: -87 ),
  ( sym: 289; act: -87 ),
  ( sym: 290; act: -87 ),
  ( sym: 291; act: -87 ),
  ( sym: 292; act: -87 ),
  ( sym: 293; act: -87 ),
  ( sym: 294; act: -87 ),
  ( sym: 295; act: -87 ),
  ( sym: 296; act: -87 ),
  ( sym: 297; act: -87 ),
{ 71: }
{ 72: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 73: }
  ( sym: 263; act: 82 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 74: }
  ( sym: 263; act: 93 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 75: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 76: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 77: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 78: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 266; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
{ 79: }
  ( sym: 263; act: 99 ),
  ( sym: 291; act: 100 ),
  ( sym: 292; act: 101 ),
  ( sym: 293; act: 102 ),
  ( sym: 294; act: 103 ),
  ( sym: 295; act: 104 ),
  ( sym: 296; act: 105 ),
  ( sym: 297; act: 106 ),
{ 80: }
{ 81: }
{ 82: }
{ 83: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 84: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 85: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: 258; act: 20 ),
{ 101: }
  ( sym: 258; act: 20 ),
{ 102: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 103: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 104: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 105: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 106: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 107: }
  ( sym: 258; act: 20 ),
  ( sym: 264; act: 123 ),
{ 108: }
  ( sym: 44; act: 124 ),
{ 109: }
{ 110: }
{ 111: }
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 112: }
  ( sym: 44; act: 128 ),
  ( sym: 264; act: 129 ),
{ 113: }
  ( sym: 264; act: 130 ),
  ( sym: 298; act: 131 ),
{ 114: }
  ( sym: 44; act: 132 ),
{ 115: }
  ( sym: 44; act: 133 ),
{ 116: }
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
  ( sym: 44; act: 135 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 123: }
{ 124: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
  ( sym: 260; act: 138 ),
  ( sym: 261; act: 139 ),
{ 132: }
  ( sym: 258; act: 20 ),
{ 133: }
  ( sym: 258; act: 20 ),
{ 134: }
{ 135: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 136: }
{ 137: }
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 138: }
  ( sym: 263; act: 144 ),
{ 139: }
  ( sym: 263; act: 145 ),
{ 140: }
  ( sym: 44; act: 146 ),
{ 141: }
  ( sym: 44; act: 147 ),
{ 142: }
{ 143: }
{ 144: }
{ 145: }
  ( sym: 299; act: 150 ),
{ 146: }
  ( sym: 258; act: 20 ),
{ 147: }
  ( sym: 258; act: 20 ),
{ 148: }
  ( sym: 264; act: 153 ),
  ( sym: 298; act: 154 ),
{ 149: }
  ( sym: 264; act: 155 ),
{ 150: }
  ( sym: 262; act: 156 ),
{ 151: }
  ( sym: 44; act: 157 ),
{ 152: }
  ( sym: 44; act: 158 ),
{ 153: }
{ 154: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 155: }
{ 156: }
  ( sym: 44; act: 160 ),
{ 157: }
  ( sym: 258; act: 20 ),
{ 158: }
  ( sym: 258; act: 20 ),
{ 159: }
  ( sym: 263; act: 163 ),
{ 160: }
  ( sym: 258; act: 20 ),
{ 161: }
{ 162: }
{ 163: }
{ 164: }
  ( sym: 44; act: 167 ),
  ( sym: 264; act: -35 ),
{ 165: }
  ( sym: 44; act: 168 ),
{ 166: }
{ 167: }
  ( sym: 258; act: 20 ),
{ 168: }
  ( sym: 258; act: 20 ),
{ 169: }
  ( sym: 264; act: 172 ),
  ( sym: 299; act: 173 ),
{ 170: }
  ( sym: 44; act: 174 ),
{ 171: }
{ 172: }
{ 173: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
{ 174: }
  ( sym: 258; act: 20 ),
{ 175: }
  ( sym: 44; act: 177 ),
{ 176: }
{ 177: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 )
{ 178: }
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
  ( sym: -9; act: 28 ),
  ( sym: -8; act: 29 ),
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
  ( sym: -3; act: 52 ),
{ 18: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 53 ),
{ 19: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 54 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 55 ),
{ 27: }
  ( sym: -36; act: 56 ),
{ 28: }
  ( sym: -35; act: 57 ),
  ( sym: -34; act: 58 ),
{ 29: }
{ 30: }
  ( sym: -23; act: 59 ),
{ 31: }
  ( sym: -24; act: 60 ),
{ 32: }
  ( sym: -25; act: 61 ),
{ 33: }
  ( sym: -27; act: 62 ),
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
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 63 ),
{ 45: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 64 ),
{ 46: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 65 ),
{ 47: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 66 ),
{ 48: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 67 ),
{ 49: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 68 ),
{ 50: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 69 ),
{ 51: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 70 ),
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
  ( sym: -22; act: 73 ),
{ 57: }
  ( sym: -22; act: 74 ),
{ 58: }
  ( sym: -22; act: 75 ),
{ 59: }
  ( sym: -22; act: 76 ),
{ 60: }
  ( sym: -22; act: 77 ),
{ 61: }
  ( sym: -22; act: 78 ),
{ 62: }
  ( sym: -26; act: 79 ),
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 80 ),
{ 73: }
  ( sym: -39; act: 81 ),
{ 74: }
  ( sym: -39; act: 81 ),
{ 75: }
  ( sym: -39; act: 81 ),
  ( sym: -7; act: 94 ),
  ( sym: -6; act: 95 ),
{ 76: }
  ( sym: -39; act: 81 ),
  ( sym: -7; act: 94 ),
  ( sym: -6; act: 96 ),
{ 77: }
  ( sym: -39; act: 81 ),
  ( sym: -7; act: 94 ),
  ( sym: -6; act: 97 ),
{ 78: }
  ( sym: -39; act: 81 ),
  ( sym: -7; act: 94 ),
  ( sym: -6; act: 98 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: -37; act: 107 ),
{ 83: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 108 ),
{ 84: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 109 ),
{ 85: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 110 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
  ( sym: -40; act: 111 ),
  ( sym: -10; act: 112 ),
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
  ( sym: -28; act: 113 ),
{ 100: }
  ( sym: -4; act: 114 ),
{ 101: }
  ( sym: -4; act: 115 ),
{ 102: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 116 ),
{ 103: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 117 ),
{ 104: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 118 ),
{ 105: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 119 ),
{ 106: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 120 ),
{ 107: }
  ( sym: -38; act: 121 ),
  ( sym: -4; act: 122 ),
{ 108: }
{ 109: }
{ 110: }
{ 111: }
  ( sym: -11; act: 125 ),
  ( sym: -7; act: 126 ),
  ( sym: -4; act: 127 ),
{ 112: }
{ 113: }
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
  ( sym: -7; act: 134 ),
{ 123: }
{ 124: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 136 ),
{ 125: }
{ 126: }
{ 127: }
{ 128: }
  ( sym: -41; act: 137 ),
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  ( sym: -4; act: 140 ),
{ 133: }
  ( sym: -4; act: 141 ),
{ 134: }
{ 135: }
  ( sym: -7; act: 142 ),
{ 136: }
{ 137: }
  ( sym: -11; act: 143 ),
  ( sym: -7; act: 126 ),
  ( sym: -4; act: 127 ),
{ 138: }
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
{ 144: }
  ( sym: -29; act: 148 ),
{ 145: }
  ( sym: -30; act: 149 ),
{ 146: }
  ( sym: -4; act: 151 ),
{ 147: }
  ( sym: -4; act: 152 ),
{ 148: }
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
  ( sym: -7; act: 159 ),
{ 155: }
{ 156: }
{ 157: }
  ( sym: -4; act: 161 ),
{ 158: }
  ( sym: -4; act: 162 ),
{ 159: }
{ 160: }
  ( sym: -33; act: 164 ),
  ( sym: -4; act: 165 ),
{ 161: }
{ 162: }
{ 163: }
  ( sym: -32; act: 166 ),
{ 164: }
{ 165: }
{ 166: }
  ( sym: -31; act: 169 ),
{ 167: }
  ( sym: -4; act: 170 ),
{ 168: }
  ( sym: -4; act: 171 ),
{ 169: }
{ 170: }
{ 171: }
{ 172: }
{ 173: }
  ( sym: -7; act: 175 ),
{ 174: }
  ( sym: -4; act: 176 ),
{ 175: }
{ 176: }
{ 177: }
  ( sym: -7; act: 178 )
{ 178: }
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
{ 12: } -89,
{ 13: } -60,
{ 14: } -76,
{ 15: } 0,
{ 16: } -59,
{ 17: } 0,
{ 18: } 0,
{ 19: } 0,
{ 20: } -75,
{ 21: } -91,
{ 22: } -92,
{ 23: } -93,
{ 24: } -94,
{ 25: } -88,
{ 26: } 0,
{ 27: } -42,
{ 28: } 0,
{ 29: } -58,
{ 30: } -11,
{ 31: } -13,
{ 32: } -15,
{ 33: } -17,
{ 34: } -48,
{ 35: } -49,
{ 36: } -50,
{ 37: } -51,
{ 38: } -52,
{ 39: } -53,
{ 40: } -54,
{ 41: } -55,
{ 42: } -56,
{ 43: } -57,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } 0,
{ 49: } 0,
{ 50: } 0,
{ 51: } 0,
{ 52: } 0,
{ 53: } -79,
{ 54: } -78,
{ 55: } 0,
{ 56: } -61,
{ 57: } -61,
{ 58: } -61,
{ 59: } -61,
{ 60: } -61,
{ 61: } -61,
{ 62: } -19,
{ 63: } -82,
{ 64: } 0,
{ 65: } -80,
{ 66: } 0,
{ 67: } 0,
{ 68: } -81,
{ 69: } 0,
{ 70: } 0,
{ 71: } -77,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } -73,
{ 81: } -62,
{ 82: } -44,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } -66,
{ 87: } -67,
{ 88: } -68,
{ 89: } -69,
{ 90: } -70,
{ 91: } -71,
{ 92: } -72,
{ 93: } -95,
{ 94: } -90,
{ 95: } -39,
{ 96: } -12,
{ 97: } -14,
{ 98: } -16,
{ 99: } -27,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } 0,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } 0,
{ 109: } -64,
{ 110: } -65,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } 0,
{ 116: } -22,
{ 117: } -23,
{ 118: } -24,
{ 119: } -25,
{ 120: } -26,
{ 121: } -45,
{ 122: } 0,
{ 123: } -43,
{ 124: } 0,
{ 125: } -96,
{ 126: } -99,
{ 127: } -100,
{ 128: } -97,
{ 129: } -41,
{ 130: } -18,
{ 131: } 0,
{ 132: } 0,
{ 133: } 0,
{ 134: } -47,
{ 135: } 0,
{ 136: } -63,
{ 137: } 0,
{ 138: } 0,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } -46,
{ 143: } -98,
{ 144: } -30,
{ 145: } 0,
{ 146: } 0,
{ 147: } 0,
{ 148: } 0,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } -28,
{ 154: } 0,
{ 155: } -29,
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } -20,
{ 162: } -21,
{ 163: } -31,
{ 164: } 0,
{ 165: } 0,
{ 166: } -33,
{ 167: } 0,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } -36,
{ 172: } -32,
{ 173: } 0,
{ 174: } 0,
{ 175: } 0,
{ 176: } -37,
{ 177: } 0,
{ 178: } -34
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 13,
{ 3: } 13,
{ 4: } 13,
{ 5: } 13,
{ 6: } 13,
{ 7: } 13,
{ 8: } 13,
{ 9: } 13,
{ 10: } 13,
{ 11: } 13,
{ 12: } 36,
{ 13: } 36,
{ 14: } 36,
{ 15: } 36,
{ 16: } 87,
{ 17: } 87,
{ 18: } 91,
{ 19: } 95,
{ 20: } 99,
{ 21: } 99,
{ 22: } 99,
{ 23: } 99,
{ 24: } 99,
{ 25: } 99,
{ 26: } 99,
{ 27: } 103,
{ 28: } 103,
{ 29: } 118,
{ 30: } 118,
{ 31: } 118,
{ 32: } 118,
{ 33: } 118,
{ 34: } 118,
{ 35: } 118,
{ 36: } 118,
{ 37: } 118,
{ 38: } 118,
{ 39: } 118,
{ 40: } 118,
{ 41: } 118,
{ 42: } 118,
{ 43: } 118,
{ 44: } 118,
{ 45: } 122,
{ 46: } 126,
{ 47: } 130,
{ 48: } 134,
{ 49: } 138,
{ 50: } 142,
{ 51: } 146,
{ 52: } 150,
{ 53: } 159,
{ 54: } 159,
{ 55: } 159,
{ 56: } 160,
{ 57: } 160,
{ 58: } 160,
{ 59: } 160,
{ 60: } 160,
{ 61: } 160,
{ 62: } 160,
{ 63: } 160,
{ 64: } 160,
{ 65: } 212,
{ 66: } 212,
{ 67: } 264,
{ 68: } 316,
{ 69: } 316,
{ 70: } 368,
{ 71: } 420,
{ 72: } 420,
{ 73: } 424,
{ 74: } 435,
{ 75: } 446,
{ 76: } 460,
{ 77: } 474,
{ 78: } 488,
{ 79: } 502,
{ 80: } 510,
{ 81: } 510,
{ 82: } 510,
{ 83: } 510,
{ 84: } 514,
{ 85: } 518,
{ 86: } 522,
{ 87: } 522,
{ 88: } 522,
{ 89: } 522,
{ 90: } 522,
{ 91: } 522,
{ 92: } 522,
{ 93: } 522,
{ 94: } 522,
{ 95: } 522,
{ 96: } 522,
{ 97: } 522,
{ 98: } 522,
{ 99: } 522,
{ 100: } 522,
{ 101: } 523,
{ 102: } 524,
{ 103: } 528,
{ 104: } 532,
{ 105: } 536,
{ 106: } 540,
{ 107: } 544,
{ 108: } 546,
{ 109: } 547,
{ 110: } 547,
{ 111: } 547,
{ 112: } 552,
{ 113: } 554,
{ 114: } 556,
{ 115: } 557,
{ 116: } 558,
{ 117: } 558,
{ 118: } 558,
{ 119: } 558,
{ 120: } 558,
{ 121: } 558,
{ 122: } 558,
{ 123: } 563,
{ 124: } 563,
{ 125: } 567,
{ 126: } 567,
{ 127: } 567,
{ 128: } 567,
{ 129: } 567,
{ 130: } 567,
{ 131: } 567,
{ 132: } 569,
{ 133: } 570,
{ 134: } 571,
{ 135: } 571,
{ 136: } 575,
{ 137: } 575,
{ 138: } 580,
{ 139: } 581,
{ 140: } 582,
{ 141: } 583,
{ 142: } 584,
{ 143: } 584,
{ 144: } 584,
{ 145: } 584,
{ 146: } 585,
{ 147: } 586,
{ 148: } 587,
{ 149: } 589,
{ 150: } 590,
{ 151: } 591,
{ 152: } 592,
{ 153: } 593,
{ 154: } 593,
{ 155: } 597,
{ 156: } 597,
{ 157: } 598,
{ 158: } 599,
{ 159: } 600,
{ 160: } 601,
{ 161: } 602,
{ 162: } 602,
{ 163: } 602,
{ 164: } 602,
{ 165: } 604,
{ 166: } 605,
{ 167: } 605,
{ 168: } 606,
{ 169: } 607,
{ 170: } 609,
{ 171: } 610,
{ 172: } 610,
{ 173: } 610,
{ 174: } 614,
{ 175: } 615,
{ 176: } 616,
{ 177: } 616,
{ 178: } 620
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 12,
{ 2: } 12,
{ 3: } 12,
{ 4: } 12,
{ 5: } 12,
{ 6: } 12,
{ 7: } 12,
{ 8: } 12,
{ 9: } 12,
{ 10: } 12,
{ 11: } 35,
{ 12: } 35,
{ 13: } 35,
{ 14: } 35,
{ 15: } 86,
{ 16: } 86,
{ 17: } 90,
{ 18: } 94,
{ 19: } 98,
{ 20: } 98,
{ 21: } 98,
{ 22: } 98,
{ 23: } 98,
{ 24: } 98,
{ 25: } 98,
{ 26: } 102,
{ 27: } 102,
{ 28: } 117,
{ 29: } 117,
{ 30: } 117,
{ 31: } 117,
{ 32: } 117,
{ 33: } 117,
{ 34: } 117,
{ 35: } 117,
{ 36: } 117,
{ 37: } 117,
{ 38: } 117,
{ 39: } 117,
{ 40: } 117,
{ 41: } 117,
{ 42: } 117,
{ 43: } 117,
{ 44: } 121,
{ 45: } 125,
{ 46: } 129,
{ 47: } 133,
{ 48: } 137,
{ 49: } 141,
{ 50: } 145,
{ 51: } 149,
{ 52: } 158,
{ 53: } 158,
{ 54: } 158,
{ 55: } 159,
{ 56: } 159,
{ 57: } 159,
{ 58: } 159,
{ 59: } 159,
{ 60: } 159,
{ 61: } 159,
{ 62: } 159,
{ 63: } 159,
{ 64: } 211,
{ 65: } 211,
{ 66: } 263,
{ 67: } 315,
{ 68: } 315,
{ 69: } 367,
{ 70: } 419,
{ 71: } 419,
{ 72: } 423,
{ 73: } 434,
{ 74: } 445,
{ 75: } 459,
{ 76: } 473,
{ 77: } 487,
{ 78: } 501,
{ 79: } 509,
{ 80: } 509,
{ 81: } 509,
{ 82: } 509,
{ 83: } 513,
{ 84: } 517,
{ 85: } 521,
{ 86: } 521,
{ 87: } 521,
{ 88: } 521,
{ 89: } 521,
{ 90: } 521,
{ 91: } 521,
{ 92: } 521,
{ 93: } 521,
{ 94: } 521,
{ 95: } 521,
{ 96: } 521,
{ 97: } 521,
{ 98: } 521,
{ 99: } 521,
{ 100: } 522,
{ 101: } 523,
{ 102: } 527,
{ 103: } 531,
{ 104: } 535,
{ 105: } 539,
{ 106: } 543,
{ 107: } 545,
{ 108: } 546,
{ 109: } 546,
{ 110: } 546,
{ 111: } 551,
{ 112: } 553,
{ 113: } 555,
{ 114: } 556,
{ 115: } 557,
{ 116: } 557,
{ 117: } 557,
{ 118: } 557,
{ 119: } 557,
{ 120: } 557,
{ 121: } 557,
{ 122: } 562,
{ 123: } 562,
{ 124: } 566,
{ 125: } 566,
{ 126: } 566,
{ 127: } 566,
{ 128: } 566,
{ 129: } 566,
{ 130: } 566,
{ 131: } 568,
{ 132: } 569,
{ 133: } 570,
{ 134: } 570,
{ 135: } 574,
{ 136: } 574,
{ 137: } 579,
{ 138: } 580,
{ 139: } 581,
{ 140: } 582,
{ 141: } 583,
{ 142: } 583,
{ 143: } 583,
{ 144: } 583,
{ 145: } 584,
{ 146: } 585,
{ 147: } 586,
{ 148: } 588,
{ 149: } 589,
{ 150: } 590,
{ 151: } 591,
{ 152: } 592,
{ 153: } 592,
{ 154: } 596,
{ 155: } 596,
{ 156: } 597,
{ 157: } 598,
{ 158: } 599,
{ 159: } 600,
{ 160: } 601,
{ 161: } 601,
{ 162: } 601,
{ 163: } 601,
{ 164: } 603,
{ 165: } 604,
{ 166: } 604,
{ 167: } 605,
{ 168: } 606,
{ 169: } 608,
{ 170: } 609,
{ 171: } 609,
{ 172: } 609,
{ 173: } 613,
{ 174: } 614,
{ 175: } 615,
{ 176: } 615,
{ 177: } 619,
{ 178: } 619
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
{ 19: } 28,
{ 20: } 30,
{ 21: } 30,
{ 22: } 30,
{ 23: } 30,
{ 24: } 30,
{ 25: } 30,
{ 26: } 30,
{ 27: } 33,
{ 28: } 34,
{ 29: } 36,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 40,
{ 35: } 40,
{ 36: } 40,
{ 37: } 40,
{ 38: } 40,
{ 39: } 40,
{ 40: } 40,
{ 41: } 40,
{ 42: } 40,
{ 43: } 40,
{ 44: } 40,
{ 45: } 42,
{ 46: } 44,
{ 47: } 46,
{ 48: } 48,
{ 49: } 50,
{ 50: } 52,
{ 51: } 54,
{ 52: } 56,
{ 53: } 56,
{ 54: } 56,
{ 55: } 56,
{ 56: } 56,
{ 57: } 57,
{ 58: } 58,
{ 59: } 59,
{ 60: } 60,
{ 61: } 61,
{ 62: } 62,
{ 63: } 63,
{ 64: } 63,
{ 65: } 63,
{ 66: } 63,
{ 67: } 63,
{ 68: } 63,
{ 69: } 63,
{ 70: } 63,
{ 71: } 63,
{ 72: } 63,
{ 73: } 66,
{ 74: } 67,
{ 75: } 68,
{ 76: } 71,
{ 77: } 74,
{ 78: } 77,
{ 79: } 80,
{ 80: } 80,
{ 81: } 80,
{ 82: } 80,
{ 83: } 81,
{ 84: } 84,
{ 85: } 87,
{ 86: } 90,
{ 87: } 90,
{ 88: } 90,
{ 89: } 90,
{ 90: } 90,
{ 91: } 90,
{ 92: } 90,
{ 93: } 90,
{ 94: } 92,
{ 95: } 92,
{ 96: } 92,
{ 97: } 92,
{ 98: } 92,
{ 99: } 92,
{ 100: } 93,
{ 101: } 94,
{ 102: } 95,
{ 103: } 98,
{ 104: } 101,
{ 105: } 104,
{ 106: } 107,
{ 107: } 110,
{ 108: } 112,
{ 109: } 112,
{ 110: } 112,
{ 111: } 112,
{ 112: } 115,
{ 113: } 115,
{ 114: } 115,
{ 115: } 115,
{ 116: } 115,
{ 117: } 115,
{ 118: } 115,
{ 119: } 115,
{ 120: } 115,
{ 121: } 115,
{ 122: } 115,
{ 123: } 116,
{ 124: } 116,
{ 125: } 119,
{ 126: } 119,
{ 127: } 119,
{ 128: } 119,
{ 129: } 120,
{ 130: } 120,
{ 131: } 120,
{ 132: } 120,
{ 133: } 121,
{ 134: } 122,
{ 135: } 122,
{ 136: } 123,
{ 137: } 123,
{ 138: } 126,
{ 139: } 126,
{ 140: } 126,
{ 141: } 126,
{ 142: } 126,
{ 143: } 126,
{ 144: } 126,
{ 145: } 127,
{ 146: } 128,
{ 147: } 129,
{ 148: } 130,
{ 149: } 130,
{ 150: } 130,
{ 151: } 130,
{ 152: } 130,
{ 153: } 130,
{ 154: } 130,
{ 155: } 131,
{ 156: } 131,
{ 157: } 131,
{ 158: } 132,
{ 159: } 133,
{ 160: } 133,
{ 161: } 135,
{ 162: } 135,
{ 163: } 135,
{ 164: } 136,
{ 165: } 136,
{ 166: } 136,
{ 167: } 137,
{ 168: } 138,
{ 169: } 139,
{ 170: } 139,
{ 171: } 139,
{ 172: } 139,
{ 173: } 139,
{ 174: } 140,
{ 175: } 141,
{ 176: } 141,
{ 177: } 141,
{ 178: } 142
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
{ 18: } 27,
{ 19: } 29,
{ 20: } 29,
{ 21: } 29,
{ 22: } 29,
{ 23: } 29,
{ 24: } 29,
{ 25: } 29,
{ 26: } 32,
{ 27: } 33,
{ 28: } 35,
{ 29: } 35,
{ 30: } 36,
{ 31: } 37,
{ 32: } 38,
{ 33: } 39,
{ 34: } 39,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 41,
{ 45: } 43,
{ 46: } 45,
{ 47: } 47,
{ 48: } 49,
{ 49: } 51,
{ 50: } 53,
{ 51: } 55,
{ 52: } 55,
{ 53: } 55,
{ 54: } 55,
{ 55: } 55,
{ 56: } 56,
{ 57: } 57,
{ 58: } 58,
{ 59: } 59,
{ 60: } 60,
{ 61: } 61,
{ 62: } 62,
{ 63: } 62,
{ 64: } 62,
{ 65: } 62,
{ 66: } 62,
{ 67: } 62,
{ 68: } 62,
{ 69: } 62,
{ 70: } 62,
{ 71: } 62,
{ 72: } 65,
{ 73: } 66,
{ 74: } 67,
{ 75: } 70,
{ 76: } 73,
{ 77: } 76,
{ 78: } 79,
{ 79: } 79,
{ 80: } 79,
{ 81: } 79,
{ 82: } 80,
{ 83: } 83,
{ 84: } 86,
{ 85: } 89,
{ 86: } 89,
{ 87: } 89,
{ 88: } 89,
{ 89: } 89,
{ 90: } 89,
{ 91: } 89,
{ 92: } 89,
{ 93: } 91,
{ 94: } 91,
{ 95: } 91,
{ 96: } 91,
{ 97: } 91,
{ 98: } 91,
{ 99: } 92,
{ 100: } 93,
{ 101: } 94,
{ 102: } 97,
{ 103: } 100,
{ 104: } 103,
{ 105: } 106,
{ 106: } 109,
{ 107: } 111,
{ 108: } 111,
{ 109: } 111,
{ 110: } 111,
{ 111: } 114,
{ 112: } 114,
{ 113: } 114,
{ 114: } 114,
{ 115: } 114,
{ 116: } 114,
{ 117: } 114,
{ 118: } 114,
{ 119: } 114,
{ 120: } 114,
{ 121: } 114,
{ 122: } 115,
{ 123: } 115,
{ 124: } 118,
{ 125: } 118,
{ 126: } 118,
{ 127: } 118,
{ 128: } 119,
{ 129: } 119,
{ 130: } 119,
{ 131: } 119,
{ 132: } 120,
{ 133: } 121,
{ 134: } 121,
{ 135: } 122,
{ 136: } 122,
{ 137: } 125,
{ 138: } 125,
{ 139: } 125,
{ 140: } 125,
{ 141: } 125,
{ 142: } 125,
{ 143: } 125,
{ 144: } 126,
{ 145: } 127,
{ 146: } 128,
{ 147: } 129,
{ 148: } 129,
{ 149: } 129,
{ 150: } 129,
{ 151: } 129,
{ 152: } 129,
{ 153: } 129,
{ 154: } 130,
{ 155: } 130,
{ 156: } 130,
{ 157: } 131,
{ 158: } 132,
{ 159: } 132,
{ 160: } 134,
{ 161: } 134,
{ 162: } 134,
{ 163: } 135,
{ 164: } 135,
{ 165: } 135,
{ 166: } 136,
{ 167: } 137,
{ 168: } 138,
{ 169: } 138,
{ 170: } 138,
{ 171: } 138,
{ 172: } 138,
{ 173: } 139,
{ 174: } 140,
{ 175: } 140,
{ 176: } 140,
{ 177: } 141,
{ 178: } 141
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
{ 76: } ( len: 1; sym: -3 ),
{ 77: } ( len: 3; sym: -3 ),
{ 78: } ( len: 2; sym: -3 ),
{ 79: } ( len: 2; sym: -3 ),
{ 80: } ( len: 3; sym: -3 ),
{ 81: } ( len: 3; sym: -3 ),
{ 82: } ( len: 3; sym: -3 ),
{ 83: } ( len: 3; sym: -3 ),
{ 84: } ( len: 3; sym: -3 ),
{ 85: } ( len: 3; sym: -3 ),
{ 86: } ( len: 3; sym: -3 ),
{ 87: } ( len: 3; sym: -3 ),
{ 88: } ( len: 1; sym: -5 ),
{ 89: } ( len: 1; sym: -5 ),
{ 90: } ( len: 1; sym: -6 ),
{ 91: } ( len: 1; sym: -7 ),
{ 92: } ( len: 1; sym: -7 ),
{ 93: } ( len: 1; sym: -7 ),
{ 94: } ( len: 1; sym: -7 ),
{ 95: } ( len: 0; sym: -40 ),
{ 96: } ( len: 2; sym: -10 ),
{ 97: } ( len: 0; sym: -41 ),
{ 98: } ( len: 4; sym: -10 ),
{ 99: } ( len: 1; sym: -11 ),
{ 100: } ( len: 1; sym: -11 )
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
