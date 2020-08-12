%{
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

  rcstrtype = record
    v: PUnicodeString;
    cp: TSystemCodePage;
  end;

function str_to_cbase(s: string): LongWord;
begin
  if s = '0' then
    Exit(0);
  if Copy(s, 1, 2) = '0x' then
    Exit(StrToInt('$' + Copy(s, 3, Maxint)));
  if Copy(s, 1, 1) = '0' then
    Exit(StrToInt('&' + Copy(s, 2, Maxint)));
  Result:= StrToInt(s);
end;

function str_to_num(s:string): rcnumtype;
begin
  // this does not handle empty strings - should never get them from the lexer
  Result.long:= s[Length(s)] = 'L';
  if Result.long then
    setlength(s, Length(s) - 1);
  Result.v:= str_to_cbase(s);
end;

const
  MAX_RCSTR_LEN = 4096;
var
  strbuf: array[0..MAX_RCSTR_LEN + 1] of char;
  strbuflen: Integer;

procedure strbuf_begin();
begin
  FillChar(strbuf[0], sizeof(strbuf), 0);
  strbuflen:= 0;
end;

procedure strbuf_append(s: string);
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
begin
  New(str.v);
  str.v^:= val;
  str.cp:= cp;
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
    while rc <= endin do begin  // val must contain the final #0!
      // treat as null-terminated - nulls may exist *after* this proc, but not before
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
              wc^:= translateChar(Char(StrToInt(h)));
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
              wc^:= translateChar(Char(StrToInt(h)));
          end;
        else
          wc^:= translateChar(rc^);
        end;
      end else
        wc^:= translateChar(rc^);
      inc(wc);
      inc(rc);
    end;
    i:= (PtrUInt(wc) - PtrUInt(@uni[1])) div 2; // includes final wc that was not written to
    SetLength(uni, i - 1);
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

%}

%token _ILLEGAL
%token _NUMBER _QUOTEDSTR _QUOTEDSTRL
%token _STR_StringFileInfo _STR_VarFileInfo _STR_Translation
%token _BEGIN _END _ID
%token _LANGUAGE _CHARACTERISTICS _VERSION _MOVEABLE _FIXED _PURE _IMPURE _PRELOAD _LOADONCALL _DISCARDABLE
%token _BITMAP _CURSOR _ICON _STRINGTABLE _VERSIONINFO
%token _ANICURSOR _ANIICON _DLGINCLUDE _DLGINIT _HTML _MANIFEST _MESSAGETABLE _PLUGPLAY _RCDATA _VXD
%token _FILEVERSION _PRODUCTVERSION _FILEFLAGSMASK _FILEFLAGS _FILEOS _FILETYPE _FILESUBTYPE _BLOCK _VALUE
%token _ACCELERATORS _DIALOG _DIALOGEX _MENU _MENUEX

%type <rcnumtype> numpos numexpr numeral
%type <rcstrtype> ident_string long_string
%type <TResourceDesc> resid rcdataid
%type <TMemoryStream> raw_data raw_item
%type <TFileStream> filename_string

%left '|'
%left '^'
%left '&'
%left '+' '-'
%left '*' '/' '%'
%right '~' _NUMNEG

%%

rcfile
    : /* empty */
    | rcfile defnstatement
    ;

defnstatement
    : resourcedef
    | languagedef
    ;

resourcedef
    : res_stringtable
    | res_bitmap
    | res_cursor
    | res_icon
    | res_version
    | res_rcdata
    ;

res_bitmap
    : resid _BITMAP { create_resource($1, RT_BITMAP); } suboptions filename_string            { TBitmapResource(aktresource).SetCustomBitmapDataStream($5); }

res_cursor
    : resid _CURSOR { create_resource($1, RT_CURSOR); } suboptions filename_string            { TGroupCursorResource(aktresource).SetCustomItemDataStream($5); }

res_icon
    : resid _ICON { create_resource($1, RT_ICON); } suboptions filename_string                { TGroupIconResource(aktresource).SetCustomItemDataStream($5); }

res_version
    : resid _VERSIONINFO { create_resource($1, RT_VERSION); } version_fixed _BEGIN version_blocks _END

version_fixed
    : /* empty */
    | version_fixed _FILEVERSION numeral ',' numeral ',' numeral ',' numeral                  { TVersionResource(aktresource).FixedInfo.FileVersion:= make_version($3.v, $5.v, $7.v, $9.v); }
    | version_fixed _PRODUCTVERSION numeral ',' numeral ',' numeral ',' numeral               { TVersionResource(aktresource).FixedInfo.ProductVersion:= make_version($3.v, $5.v, $7.v, $9.v); }
    | version_fixed _FILEFLAGSMASK numpos                                                     { TVersionResource(aktresource).FixedInfo.FileFlagsMask:= $3.v; }
    | version_fixed _FILEFLAGS numpos                                                         { TVersionResource(aktresource).FixedInfo.FileFlags:= $3.v; }
    | version_fixed _FILEOS numpos                                                            { TVersionResource(aktresource).FixedInfo.FileOS:= $3.v; }
    | version_fixed _FILETYPE numpos                                                          { TVersionResource(aktresource).FixedInfo.FileType:= $3.v; }
    | version_fixed _FILESUBTYPE numpos                                                       { TVersionResource(aktresource).FixedInfo.FileSubType:= $3.v; }
    ;

version_blocks
    : /* empty */
    | version_blocks _BLOCK _STR_StringFileInfo _BEGIN ver_strings_lang _END
    | version_blocks _BLOCK _STR_VarFileInfo _BEGIN ver_translation_data _END
    ;

ver_strings_lang
    : /* empty */
    | ver_strings_lang _BLOCK long_string _BEGIN                                              { version_string_tab_begin($3.v^); }
                                          ver_strings_data _END
    ;

ver_strings_data
    : /* empty */
    | ver_strings_data _VALUE long_string ',' long_string                                     { version_string_tab_add($3.v^, $5.v^); }
    ;

ver_translation_data
    : _VALUE _STR_Translation ',' ver_translation_pair
    ;

ver_translation_pair
    : numeral ',' numeral                                                                     { version_var_translation_add($1.v, $3.v); }
    | ver_translation_pair ',' numeral ',' numeral                                            { version_var_translation_add($3.v, $5.v); }
    ;

res_rcdata
    : resid rcdataid { create_resource($1, $2); } suboptions filename_string                  { aktresource.SetCustomRawDataStream($5); }
    | resid rcdataid { create_resource($1, $2); } suboptions _BEGIN raw_data _END             { aktresource.SetCustomRawDataStream($6); }
    ;

res_stringtable
    : _STRINGTABLE { stringtable_begin(); } suboptions _BEGIN stringtable_data _END { stringtable_end(); }

stringtable_data
    : /* empty */
    | stringtable_data stringtable_entry
    ;

stringtable_entry
    : numeral ',' long_string                      { stringtable_add($1.v, $3.v^); }
    | numeral long_string                          { stringtable_add($1.v, $2.v^); }
    ;

rcdataid
    : _ANICURSOR                                   { $$:= TResourceDesc.Create(RT_ANICURSOR); }
    | _ANIICON                                     { $$:= TResourceDesc.Create(RT_ANIICON); }
    | _DLGINCLUDE                                  { $$:= TResourceDesc.Create(RT_DLGINCLUDE); }
    | _DLGINIT                                     { $$:= TResourceDesc.Create(RT_DLGINIT); }
    | _HTML                                        { $$:= TResourceDesc.Create(23); }
    | _MANIFEST                                    { $$:= TResourceDesc.Create(RT_MANIFEST); }
    | _MESSAGETABLE                                { $$:= TResourceDesc.Create(RT_MESSAGETABLE); }
    | _PLUGPLAY                                    { $$:= TResourceDesc.Create(RT_PLUGPLAY); }
    | _RCDATA                                      { $$:= TResourceDesc.Create(RT_RCDATA); }
    | _VXD                                         { $$:= TResourceDesc.Create(RT_VXD); }
    | resid
    ;

resid
    : numpos                                       { $$:= TResourceDesc.Create($1.v); }
    | ident_string                                 { $$:= TResourceDesc.Create($1.v^); }
    ;

suboptions
    : /* empty */
    | suboptions suboption
    ;

suboption
    : _LANGUAGE numpos ',' numpos                  { aktresource.LangID:= MakeLangID($2.v, $4.v); }
    | _CHARACTERISTICS numpos                      { aktresource.Characteristics:= $2.v; }
    | _VERSION numpos                              { aktresource.Version:= $2.v; }
    | _MOVEABLE                                    { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; }
    | _FIXED                                       { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; }
    | _PURE                                        { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; }
    | _IMPURE                                      { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; }
    | _PRELOAD                                     { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; }
    | _LOADONCALL                                  { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; }
    | _DISCARDABLE                                 { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; }
    ;

languagedef
    : _LANGUAGE numpos ',' numpos                  { language:= MakeLangID($2.v, $4.v); }

numpos
    : numexpr
    ;

numeral
    : _NUMBER                                      { $$:= str_to_num(yytext); }
    ;

numexpr
    : numeral
    | '(' numexpr ')'                              { $$:= $2; }
    | '~' numexpr %prec '~'                        { $$.v:= not $2.v; $$.long:= $2.long; }
    | '-' numexpr %prec _NUMNEG                    { $$.v:= -$2.v; $$.long:= $2.long; }
    | numexpr '*' numexpr                          { $$.v:= $1.v * $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '/' numexpr                          { $$.v:= $1.v div Max(1, $3.v); $$.long:= $1.long or $3.long; }
    | numexpr '%' numexpr                          { $$.v:= $1.v mod Max(1, $3.v); $$.long:= $1.long or $3.long; }
    | numexpr '+' numexpr                          { $$.v:= $1.v + $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '-' numexpr                          { $$.v:= $1.v - $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '&' numexpr                          { $$.v:= $1.v and $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '^' numexpr                          { $$.v:= $1.v xor $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '|' numexpr                          { $$.v:= $1.v or $3.v; $$.long:= $1.long or $3.long; }
    ;

ident_string
    : _ID                                          { string_new($$, yytext, opt_code_page); }
    | long_string
    ;

filename_string
    : long_string                                  { $$:= TFileStream.Create($1.v^, fmOpenRead or fmShareDenyWrite); }
    ;

long_string
    : _QUOTEDSTR                                   { string_new_uni($$, @strbuf[0], strbuflen, opt_code_page, true); }
    | _QUOTEDSTRL                                  { string_new_uni($$, @strbuf[0], strbuflen, CP_UTF16, true); }
    | _STR_StringFileInfo                          { string_new($$, yytext, opt_code_page); }
    | _STR_VarFileInfo                             { string_new($$, yytext, opt_code_page); }
    | _STR_Translation                             { string_new($$, yytext, opt_code_page); }
    ;

raw_data
    :                                              { $$:= TMemoryStream.Create; }
      raw_item
    | raw_data ',' { $$:= $1; } raw_item
    ;

raw_item
    : long_string
      {
        $$:= $<TMemoryStream>0;
        raw_write_string($$, $1);
      }
    | numeral
      {
        $$:= $<TMemoryStream>0;
        raw_write_int($$, $1);
      }
    ;

%%

{$I rclex.inc}
begin
  bufptr:= 0;
  lexlib.get_char:= @rc_get_char;
  lexlib.unget_char:= @rc_unget_char;
end.

