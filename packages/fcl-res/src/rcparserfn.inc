{%MainUnit rcparser.pas}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource parser functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
interface

{$mode objfpc}{$H+}
{$COPERATORS ON}
{$GOTO ON}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.StrUtils, System.FGL, Pascal.Lexlib, Pascal.Yacclib, System.Resources.Resource,
  System.Resources.Accelerator, System.Resources.GroupIcon, System.Resources.StringTable,
  System.Resources.Bitmap, System.Resources.Version, System.Resources.VersionTypes, System.Resources.GroupCursor,
  System.Resources.DialogResource;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, StrUtils, fgl, lexlib, yacclib, resource,
  acceleratorsresource, groupiconresource, stringtableresource,
  bitmapresource, versionresource, versiontypes, groupcursorresource,
  dialogresource;
{$ENDIF FPC_DOTTEDUNITS}

type
  TStringHashTable = specialize TFPGMap<AnsiString, AnsiString>;

function yyparse : Integer;
function yylex : Integer;
procedure ResetLexer;

{ Token constants - must match pyacc-generated values in rcparser.pas }
const _ILLEGAL = 257;
const _NUMBER = 258;
const _QUOTEDSTR = 259;
const _QUOTEDSTRL = 260;
const _STR_StringFileInfo = 261;
const _STR_VarFileInfo = 262;
const _STR_Translation = 263;
const _BEGIN = 264;
const _END = 265;
const _ID = 266;
const _NSWPSTR = 267;
const _LANGUAGE = 268;
const _CHARACTERISTICS = 269;
const _VERSION = 270;
const _MOVEABLE = 271;
const _FIXED = 272;
const _PURE = 273;
const _IMPURE = 274;
const _PRELOAD = 275;
const _LOADONCALL = 276;
const _DISCARDABLE = 277;
const _BITMAP = 278;
const _CURSOR = 279;
const _ICON = 280;
const _STRINGTABLE = 281;
const _VERSIONINFO = 282;
const _ANICURSOR = 283;
const _ANIICON = 284;
const _DLGINCLUDE = 285;
const _DLGINIT = 286;
const _HTML = 287;
const _MANIFEST = 288;
const _MESSAGETABLE = 289;
const _PLUGPLAY = 290;
const _RCDATA = 291;
const _VXD = 292;
const _FILEVERSION = 293;
const _PRODUCTVERSION = 294;
const _FILEFLAGSMASK = 295;
const _FILEFLAGS = 296;
const _FILEOS = 297;
const _FILETYPE = 298;
const _FILESUBTYPE = 299;
const _BLOCK = 300;
const _VALUE = 301;
const _ACCELERATORS = 302;
const _DIALOG = 303;
const _DIALOGEX = 304;
const _MENU = 305;
const _MENUEX = 306;
const _CAPTION = 307;
const _CLASS = 308;
const _EXSTYLE = 309;
const _STYLE = 310;
const _FONT = 311;
const _NOT = 312;
const _CONTROL = 313;
const _LTEXT = 314;
const _RTEXT = 315;
const _CTEXT = 316;
const _PUSHBUTTON = 317;
const _DEFPUSHBUTTON = 318;
const _CHECKBOX = 319;
const _AUTOCHECKBOX = 320;
const _RADIOBUTTON = 321;
const _AUTORADIOBUTTON = 322;
const _AUTO3STATE = 323;
const _STATE3 = 324;
const _GROUPBOX = 325;
const _EDITTEXT = 326;
const _COMBOBOX = 327;
const _LISTBOX = 328;
const _SCROLLBAR = 329;
const _NUMNEG = 330;

var
  aktresources: TResources;
  opt_code_page: TSystemCodePage;
  yyfilename: AnsiString;
  yyparseresult: YYSType;

procedure DisposePools;
procedure SetDefaults;
procedure PragmaCodePage(cp: string);

{$DEFINE INC_HEADER}
{$I yyinclude.pp}
{$I yypreproc.pp}
{$UNDEF INC_HEADER}

implementation

procedure yyerror ( msg : AnsiString );
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
  buf    : array [1..max_chars] of AnsiChar;

function rc_get_char : AnsiChar;
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

procedure rc_unget_char ( c : AnsiChar );
  begin
    if bufptr=max_chars then yyerror('input buffer overflow');
    inc(bufptr);
    dec(yycolno);
    buf[bufptr] := c;
  end(*unget_char*);

procedure unget_string(s: ansistring);
var
  i: integer;
begin
  for i:= Length(s) downto 1 do
    rc_unget_char(s[i]);
end;

procedure PragmaCodePage(cp: ansistring);
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

  rcstrtype = record
    v: PUnicodeString;
    cp: TSystemCodePage;
  end;

function str_to_cbase(s: ansistring): LongWord;
begin
  if s = '0' then
    Exit(0);
  if Copy(s, 1, 2) = '0x' then
    Exit(StrToInt('$' + Copy(s, 3, Maxint)));
  if Copy(s, 1, 2) = '0o' then
    Exit(StrToInt('&' + Copy(s, 3, Maxint)));
  if Copy(s, 1, 1) = '0' then
    Exit(StrToInt('&' + Copy(s, 2, Maxint)));
  Result:= StrToInt(s);
end;

function str_to_num(s:ansistring): rcnumtype;
begin
  // this does not handle empty strings - should never get them from the lexer
  Result.long:= s[Length(s)] = 'L';
  if Result.long then
    setlength(s, Length(s) - 1);
  Result.v:= str_to_cbase(s);
end;

type
  PStrPoolItem = ^TStrPoolItem;
  TStrPoolItem = record
    str: PUnicodeString;
    next: PStrPoolItem;
  end;

const
  MAX_RCSTR_LEN = 4096;
var
  strbuf: array[0..MAX_RCSTR_LEN + 1] of AnsiChar;
  strbuflen: Integer;
  stringpool: PStrPoolItem = nil;

procedure strbuf_begin();
begin
  FillChar(strbuf[0], sizeof(strbuf), 0);
  strbuflen:= 0;
end;

procedure strbuf_append(s: ansistring);
var
  rem: integer;
begin
  rem:= MAX_RCSTR_LEN - strbuflen;
  if Length(s) < rem then
    rem:= Length(s);
  Move(s[1], strbuf[strbuflen], rem);
  inc(strbuflen, rem);
end;

procedure string_new(var str: rcstrtype; val: UnicodeString; cp: TSystemCodePage);
var
  s: PStrPoolItem;
begin
  New(str.v);
  str.v^:= val;
  str.cp:= cp;

  New(s);
  s^.next:= stringpool;
  s^.str:= str.v;
  stringpool:= s;
end;

procedure string_new_uni(var str: rcstrtype; val: PAnsiChar; len: integer; cp: TSystemCodePage; escapes: boolean);
  function translateChar(c: AnsiChar): UnicodeChar;
  var
    u: UnicodeString = '';
  begin
    if cp = CP_UTF16 then
      Result:= c
    else begin
      // TODO: there has to be a better way to translate a single codepoint
      widestringmanager.Ansi2UnicodeMoveProc(@c, cp, u, 1);
      Result:= u[1];
    end;
  end;

var
  uni: UnicodeString;
  wc: PUnicodeChar;
  rc, endin: PAnsiChar;
  h: string;
  hexlen, i: integer;
begin
  uni:= '';
  if not escapes then
    widestringmanager.Ansi2UnicodeMoveProc(val, cp, uni, len)
  else begin
    if cp = CP_UTF16 then
      hexlen:= 4
    else
      hexlen:= 2;
    setlength(uni, len);
    wc:= @uni[1];
    rc:= val;
    endin:= @val[len];
    while rc < endin do begin
      if (rc^ = '\') then begin
        inc(rc);
        case rc^ of
          #0: exit {Error: End too soon};
          '\': wc^:= '\';
          'f': wc^:= #&14;
          'n': wc^:= #&12;
          'r': wc^:= #&15;
          't': wc^:= #&11;
          'x',
          'X': begin
            h:= '$';
            for i:= 1 to hexlen do begin
              inc(rc);
              if rc >= endin then
                exit {Error: End too soon};
              h += rc^;
            end;
            if cp = CP_UTF16 then
              wc^:= WideChar(StrToInt(h))
            else
              wc^:= translateChar(AnsiChar(StrToInt(h)));
          end;
          '0'..'7': begin
            h:= '&' + rc^;
            for i:= 2 to 3 do begin
              inc(rc);
              if (rc >= endin) or not (rc^ in ['0'..'7']) then begin
                dec(rc);
                break;
              end;
              h += rc^;
            end;
            if cp = CP_UTF16 then
              wc^:= WideChar(StrToInt(h))
            else
              wc^:= translateChar(AnsiChar(StrToInt(h)));
          end;
        else
          wc^:= translateChar(rc^);
        end;
      end else
        wc^:= translateChar(rc^);
      inc(wc);
      inc(rc);
    end;
    i:= (PtrUInt(wc) - PtrUInt(@uni[1])) div SizeOf(WideChar);
    SetLength(uni, i);
  end;
  string_new(str, uni, cp);
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

procedure create_resource(aId, aType: TResourceDesc; aClass: TResourceClass);
var
  r: TAbstractResource;
begin
  r:= aClass.Create(aType, aId);
  r.LangID:= language;
  aktresources.Add(r);
  aktresource:= r;
  aId.Free;
  aType.Free;
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
    RT_DIALOG: cls:= TDialogResource;
  else
    raise EResourceDescTypeException.CreateFmt('Resource type not supported: %d', [aType]);
  end;
  create_resource(aId, nil, cls);
end;

procedure change_lang_id(newlang: TLangID);
begin
  // cannot change a language id while it is contained in a list, so remove and re-add
  aktresources.Remove(aktresource);
  aktresource.LangID:= newlang;
  aktresources.Add(aktresource);
end;

procedure raw_write_string(Stream: TMemoryStream; str: rcstrtype);
var
  i: integer;
  u: UnicodeString;
  r: RawByteString = '';
begin
  u:= str.v^;
  if str.cp = CP_UTF16 then begin
    for i:=1 to length(u) do
      Stream.WriteWord(NtoLE(Word(u[i])));
  end else begin
    widestringmanager.Unicode2AnsiMoveProc(@u[1], r, str.cp, Length(u));
    Stream.WriteBuffer(r[1], Length(r));
  end;
end;

procedure raw_write_int(Stream: TMemoryStream; num: rcnumtype);
begin
  if num.long then
    Stream.WriteDWord(NtoLE(num.v))
  else
    Stream.WriteWord(NtoLE(Word(num.v)));
end;

procedure stringtable_begin();
begin
  // create dummy resource that we will use to capture suboptions
  create_resource(TResourceDesc.create(1), TResourceDesc.create(1));
  aktresources.Remove(aktresource);
end;

procedure stringtable_add(ident: Word; str: AnsiString);
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

procedure version_string_tab_begin(lcs: AnsiString);
var
  vst: TVersionStringTable;
begin
  vst:= TVersionStringTable.Create(lcs);
  TVersionResource(aktresource).StringFileInfo.Add(vst);
end;

procedure version_string_tab_add(key, value: AnsiString);
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

procedure SetDefaults;
begin
  language:= $0409;                    // MS RC starts up as en-US
  PragmaCodePage('DEFAULT');
end;

procedure DisposePools;
var
  s: PStrPoolItem;
begin
  while stringpool <> nil do begin
    s:= stringpool;
    stringpool:= s^.next;
    dispose(s^.str);
    dispose(s);
  end;
end;

procedure ResetLexer;
begin
  bufptr:= 0;
end;

{ Dialog constants }

const
  { Dialog styles }
  DS_SETFONT         = $0040;

  { Window styles }
  WS_CHILD           = DWord($40000000);
  WS_VISIBLE         = DWord($10000000);
  WS_TABSTOP         = $00010000;
  WS_GROUP           = $00020000;
  WS_BORDER          = $00800000;

  { Button styles }
  BS_PUSHBUTTON      = $0000;
  BS_DEFPUSHBUTTON   = $0001;
  BS_CHECKBOX        = $0002;
  BS_AUTOCHECKBOX    = $0003;
  BS_RADIOBUTTON     = $0004;
  BS_3STATE          = $0005;
  BS_AUTO3STATE      = $0006;
  BS_GROUPBOX        = $0007;
  BS_AUTORADIOBUTTON = $0009;

  { Static styles }
  SS_LEFT            = $0000;
  SS_CENTER          = $0001;
  SS_RIGHT           = $0002;
  SS_ICON            = $0003;

  { Listbox styles }
  LBS_NOTIFY         = $0001;

  { Predefined window class ordinals }
  CTL_BUTTON         = $0080;
  CTL_EDIT           = $0081;
  CTL_STATIC         = $0082;
  CTL_LISTBOX        = $0083;
  CTL_SCROLLBAR      = $0084;
  CTL_COMBOBOX       = $0085;

  { Font defaults }
  FW_DONTCARE        = 0;
  DEFAULT_CHARSET    = 1;

{ Dialog state variables }

var
  dialog_stream: TMemoryStream;
  dialog_count: Word;
  dialog_is_ex: Boolean;
  dialog_style, dialog_exstyle: DWord;
  dialog_x, dialog_y, dialog_cx, dialog_cy: SmallInt;
  dialog_helpid: DWord;
  dialog_caption: UnicodeString;
  dialog_class_name: UnicodeString;
  dialog_class_ord: Word;
  dialog_has_class_ord: Boolean;
  dialog_menu_name: UnicodeString;
  dialog_menu_ord: Word;
  dialog_has_menu_ord: Boolean;
  dialog_has_font: Boolean;
  dialog_font_size: Word;
  dialog_font_name: UnicodeString;
  dialog_font_weight: Word;
  dialog_font_italic: Byte;
  dialog_font_charset: Byte;
  { Per-control temporaries }
  dialog_ctrl_style_val: DWord;
  dialog_ctrl_exstyle_val: DWord;
  dialog_ctrl_has_style: Boolean;
  dialog_ctrl_has_exstyle: Boolean;
  dialog_ctrl_w: SmallInt;
  dialog_ctrl_h: SmallInt;
  dialog_ctrl_has_wh: Boolean;

{ Dialog helper procedures }

procedure dialog_begin(aId: TResourceDesc; isEx: Boolean);
begin
  create_resource(aId, RT_DIALOG);
  dialog_stream:= TMemoryStream.Create;
  dialog_count:= 0;
  dialog_is_ex:= isEx;
  dialog_style:= 0;
  dialog_exstyle:= 0;
  dialog_x:= 0;
  dialog_y:= 0;
  dialog_cx:= 0;
  dialog_cy:= 0;
  dialog_helpid:= 0;
  dialog_caption:= '';
  dialog_class_name:= '';
  dialog_class_ord:= 0;
  dialog_has_class_ord:= false;
  dialog_menu_name:= '';
  dialog_menu_ord:= 0;
  dialog_has_menu_ord:= false;
  dialog_has_font:= false;
  dialog_font_size:= 0;
  dialog_font_name:= '';
  dialog_font_weight:= FW_DONTCARE;
  dialog_font_italic:= 0;
  dialog_font_charset:= DEFAULT_CHARSET;
end;

procedure dialog_set_coords(x, y, cx, cy: LongWord);
begin
  dialog_x:= SmallInt(x);
  dialog_y:= SmallInt(y);
  dialog_cx:= SmallInt(cx);
  dialog_cy:= SmallInt(cy);
end;

procedure dialog_set_helpid(id: LongWord);
begin
  dialog_helpid:= id;
end;

procedure dialog_set_caption(const s: UnicodeString);
begin
  dialog_caption:= s;
end;

procedure dialog_set_style(s: LongWord);
begin
  dialog_style:= s;
end;

procedure dialog_set_exstyle(s: LongWord);
begin
  dialog_exstyle:= s;
end;

procedure dialog_set_font(size: LongWord; const name: UnicodeString);
begin
  dialog_has_font:= true;
  dialog_font_size:= Word(size);
  dialog_font_name:= name;
  dialog_style:= dialog_style or DS_SETFONT;
  { Reset extended font params to defaults; overridden by dialog_set_font_ex_params }
  dialog_font_weight:= FW_DONTCARE;
  dialog_font_italic:= 0;
  dialog_font_charset:= DEFAULT_CHARSET;
end;

procedure dialog_set_font_ex_params(weight, italic, charset: LongWord);
begin
  dialog_font_weight:= Word(weight);
  dialog_font_italic:= Byte(italic);
  dialog_font_charset:= Byte(charset);
end;

procedure dialog_set_class(const name: UnicodeString);
begin
  dialog_class_name:= name;
  dialog_has_class_ord:= false;
end;

procedure dialog_set_class_ord(ord: LongWord);
begin
  dialog_class_ord:= Word(ord);
  dialog_has_class_ord:= true;
end;

procedure dialog_set_menu(const name: UnicodeString);
begin
  dialog_menu_name:= name;
  dialog_has_menu_ord:= false;
end;

procedure dialog_set_menu_ord(ord: LongWord);
begin
  dialog_menu_ord:= Word(ord);
  dialog_has_menu_ord:= true;
end;

{ Per-control state }

procedure dialog_ctrl_reset;
begin
  dialog_ctrl_has_style:= false;
  dialog_ctrl_has_exstyle:= false;
  dialog_ctrl_has_wh:= false;
  dialog_ctrl_style_val:= 0;
  dialog_ctrl_exstyle_val:= 0;
  dialog_ctrl_w:= 0;
  dialog_ctrl_h:= 0;
end;

procedure dialog_ctrl_set_style(s: LongWord);
begin
  dialog_ctrl_style_val:= s;
  dialog_ctrl_has_style:= true;
end;

procedure dialog_ctrl_set_exstyle(s: LongWord);
begin
  dialog_ctrl_exstyle_val:= s;
  dialog_ctrl_has_exstyle:= true;
end;

procedure dialog_ctrl_set_wh(w, h: LongWord);
begin
  dialog_ctrl_w:= SmallInt(w);
  dialog_ctrl_h:= SmallInt(h);
  dialog_ctrl_has_wh:= true;
end;

{ Binary output helpers }

procedure dialog_align_dword(stream: TMemoryStream);
var
  pad: Integer;
  zero: Byte;
begin
  pad:= (4 - (stream.Position mod 4)) mod 4;
  zero:= 0;
  while pad > 0 do begin
    stream.WriteByte(zero);
    dec(pad);
  end;
end;

procedure dialog_write_sz_or_ord(stream: TMemoryStream; const name: UnicodeString;
  ord: Word; hasOrd: Boolean);
var
  i: Integer;
  w: Word;
begin
  if hasOrd then begin
    w:= NtoLE(Word($FFFF));
    stream.WriteWord(w);
    w:= NtoLE(ord);
    stream.WriteWord(w);
  end else if name = '' then begin
    w:= NtoLE(Word(0));
    stream.WriteWord(w);
  end else begin
    for i:= 1 to Length(name) do begin
      w:= NtoLE(Word(name[i]));
      stream.WriteWord(w);
    end;
    w:= NtoLE(Word(0));
    stream.WriteWord(w);
  end;
end;

procedure dialog_write_unicode_string(stream: TMemoryStream; const s: UnicodeString);
var
  i: Integer;
  w: Word;
begin
  for i:= 1 to Length(s) do begin
    w:= NtoLE(Word(s[i]));
    stream.WriteWord(w);
  end;
  w:= NtoLE(Word(0));
  stream.WriteWord(w);
end;

{ Add a control item to dialog_stream }

procedure dialog_write_control(id: LongWord; x, y, cx, cy: SmallInt;
  style, exstyle: DWord; const title: UnicodeString;
  const className: UnicodeString; classOrd: Word; classIsOrd: Boolean);
var
  w: Word;
  dw: DWord;
  sw: SmallInt;
begin
  dialog_align_dword(dialog_stream);

  if dialog_is_ex then begin
    { DLGITEMTEMPLATEEX }
    dw:= NtoLE(DWord(0));           { helpID }
    dialog_stream.WriteDWord(dw);
    dw:= NtoLE(exstyle);
    dialog_stream.WriteDWord(dw);
    dw:= NtoLE(style);
    dialog_stream.WriteDWord(dw);
    sw:= SmallInt(NtoLE(Word(x)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(y)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(cx)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(cy)));
    dialog_stream.WriteWord(Word(sw));
    dw:= NtoLE(DWord(id));          { id is DWORD in DIALOGEX }
    dialog_stream.WriteDWord(dw);
  end else begin
    { DLGITEMTEMPLATE }
    dw:= NtoLE(style);
    dialog_stream.WriteDWord(dw);
    dw:= NtoLE(exstyle);
    dialog_stream.WriteDWord(dw);
    sw:= SmallInt(NtoLE(Word(x)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(y)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(cx)));
    dialog_stream.WriteWord(Word(sw));
    sw:= SmallInt(NtoLE(Word(cy)));
    dialog_stream.WriteWord(Word(sw));
    w:= NtoLE(Word(id));            { id is WORD in DIALOG }
    dialog_stream.WriteWord(w);
  end;

  { windowClass - sz_Or_Ord }
  if classIsOrd then
    dialog_write_sz_or_ord(dialog_stream, '', classOrd, true)
  else
    dialog_write_sz_or_ord(dialog_stream, className, 0, false);

  { title - sz_Or_Ord }
  if title <> '' then
    dialog_write_sz_or_ord(dialog_stream, title, 0, false)
  else begin
    w:= NtoLE(Word(0));
    dialog_stream.WriteWord(w);
  end;

  { extraCount }
  w:= NtoLE(Word(0));
  dialog_stream.WriteWord(w);

  Inc(dialog_count);
end;

{ Standard control: text/button/edit with optional style/exstyle override }

procedure dialog_add_control_std(id, x, y, cx, cy: LongWord;
  defaultStyle: DWord; classOrd: Word; const title: UnicodeString);
var
  style, exstyle: DWord;
begin
  if dialog_ctrl_has_style then
    style:= dialog_ctrl_style_val
  else
    style:= defaultStyle;
  if dialog_ctrl_has_exstyle then
    exstyle:= dialog_ctrl_exstyle_val
  else
    exstyle:= 0;
  dialog_write_control(id, SmallInt(x), SmallInt(y), SmallInt(cx), SmallInt(cy),
    style, exstyle, title, '', classOrd, true);
end;

{ Generic CONTROL statement }

procedure dialog_add_control_generic(id, x, y, cx, cy: LongWord;
  style: DWord; const title: UnicodeString; const className: UnicodeString);
var
  exstyle: DWord;
begin
  if dialog_ctrl_has_exstyle then
    exstyle:= dialog_ctrl_exstyle_val
  else
    exstyle:= 0;
  dialog_write_control(id, SmallInt(x), SmallInt(y), SmallInt(cx), SmallInt(cy),
    style, exstyle, title, className, 0, false);
end;

{ ICON control with optional w,h }

procedure dialog_add_control_icon(id, x, y: LongWord;
  const iconName: UnicodeString);
var
  w, h: SmallInt;
  style, exstyle: DWord;
begin
  if dialog_ctrl_has_wh then begin
    w:= dialog_ctrl_w;
    h:= dialog_ctrl_h;
  end else begin
    w:= 0;
    h:= 0;
  end;
  if dialog_ctrl_has_style then
    style:= dialog_ctrl_style_val
  else
    style:= WS_CHILD or WS_VISIBLE or SS_ICON;
  if dialog_ctrl_has_exstyle then
    exstyle:= dialog_ctrl_exstyle_val
  else
    exstyle:= 0;
  dialog_write_control(id, SmallInt(x), SmallInt(y), w, h,
    style, exstyle, iconName, '', CTL_STATIC, true);
end;

{ Finalize dialog and write binary resource data }

procedure dialog_end;
var
  hdr: TMemoryStream;
  w: Word;
  dw: DWord;
  sw: SmallInt;
begin
  hdr:= TMemoryStream.Create;
  try
    if dialog_is_ex then begin
      { DLGTEMPLATEEX header }
      w:= NtoLE(Word(1));              { dlgVer }
      hdr.WriteWord(w);
      w:= NtoLE(Word($FFFF));          { signature }
      hdr.WriteWord(w);
      dw:= NtoLE(dialog_helpid);
      hdr.WriteDWord(dw);
      dw:= NtoLE(dialog_exstyle);
      hdr.WriteDWord(dw);
      dw:= NtoLE(dialog_style);
      hdr.WriteDWord(dw);
      w:= NtoLE(dialog_count);
      hdr.WriteWord(w);
      sw:= SmallInt(NtoLE(Word(dialog_x)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_y)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_cx)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_cy)));
      hdr.WriteWord(Word(sw));
    end else begin
      { DLGTEMPLATE header }
      dw:= NtoLE(dialog_style);
      hdr.WriteDWord(dw);
      dw:= NtoLE(dialog_exstyle);
      hdr.WriteDWord(dw);
      w:= NtoLE(dialog_count);
      hdr.WriteWord(w);
      sw:= SmallInt(NtoLE(Word(dialog_x)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_y)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_cx)));
      hdr.WriteWord(Word(sw));
      sw:= SmallInt(NtoLE(Word(dialog_cy)));
      hdr.WriteWord(Word(sw));
    end;

    { menu - sz_Or_Ord }
    dialog_write_sz_or_ord(hdr, dialog_menu_name, dialog_menu_ord, dialog_has_menu_ord);

    { windowClass - sz_Or_Ord }
    dialog_write_sz_or_ord(hdr, dialog_class_name, dialog_class_ord, dialog_has_class_ord);

    { title - null-terminated UTF-16LE }
    dialog_write_unicode_string(hdr, dialog_caption);

    { Font data (only if DS_SETFONT is set) }
    if dialog_has_font then begin
      w:= NtoLE(dialog_font_size);
      hdr.WriteWord(w);
      if dialog_is_ex then begin
        w:= NtoLE(dialog_font_weight);
        hdr.WriteWord(w);
        hdr.WriteByte(dialog_font_italic);
        hdr.WriteByte(dialog_font_charset);
      end;
      dialog_write_unicode_string(hdr, dialog_font_name);
    end;

    { DWORD-align after header, before first control }
    dialog_align_dword(hdr);

    { Append all control items }
    if dialog_stream.Size > 0 then begin
      dialog_stream.Position:= 0;
      hdr.CopyFrom(dialog_stream, dialog_stream.Size);
    end;

    { Set resource raw data }
    hdr.Position:= 0;
    aktresource.SetCustomRawDataStream(hdr);
  finally
    FreeAndNil(dialog_stream);
  end;
end;

