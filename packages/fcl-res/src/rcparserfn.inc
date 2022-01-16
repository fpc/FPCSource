{%MainUnit rcparser.pas}

interface

{$mode objfpc}{$H+}
{$COPERATORS ON}
{$GOTO ON}

uses
  SysUtils, Classes, StrUtils, fgl, lexlib, yacclib, resource,
  acceleratorsresource, groupiconresource, stringtableresource,
  bitmapresource, versionresource, versiontypes, groupcursorresource;

type
  TStringHashTable = specialize TFPGMap<String, String>;

function yyparse : Integer;

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
  if Copy(s, 1, 2) = '0o' then
    Exit(StrToInt('&' + Copy(s, 3, Maxint)));
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

type
  PStrPoolItem = ^TStrPoolItem;
  TStrPoolItem = record
    str: PUnicodeString;
    next: PStrPoolItem;
  end;

const
  MAX_RCSTR_LEN = 4096;
var
  strbuf: array[0..MAX_RCSTR_LEN + 1] of char;
  strbuflen: Integer;
  stringpool: PStrPoolItem = nil;

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

