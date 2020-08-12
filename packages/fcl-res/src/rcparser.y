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

var
  yycapture: AnsiString;
%}

%token _ILLEGAL
%token _NUMDECIMAL _NUMHEX _QUOTEDSTR
%token _BEGIN _END _ID
%token _LANGUAGE _CHARACTERISTICS _VERSION _MOVEABLE _FIXED _PURE _IMPURE _PRELOAD _LOADONCALL _DISCARDABLE
%token _BITMAP _CURSOR _ICON
%token _ANICURSOR _ANIICON _DLGINCLUDE _DLGINIT _HTML _MANIFEST _MESSAGETABLE _PLUGPLAY _RCDATA _VXD
%token _ACCELERATORS _DIALOG _DIALOGEX _MENU _MENUEX _STRINGTABLE _VERSIONINFO

%type <rcnumtype> numpos numeral
%type <String> ident_string filename_string long_string
%type <TResourceDesc> resid rcdataid
%type <TMemoryStream> raw_data raw_item
%type <TFileStream> filename_string

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
    | res_rcdata
    ;

res_bitmap
    : resid _BITMAP { create_resource($1, RT_BITMAP); } suboptions filename_string            { TBitmapResource(aktresource).SetCustomBitmapDataStream($5); }

res_cursor
    : resid _CURSOR { create_resource($1, RT_CURSOR); } suboptions filename_string            { TGroupCursorResource(aktresource).SetCustomItemDataStream($5); }

res_icon
    : resid _ICON { create_resource($1, RT_ICON); } suboptions filename_string                { TGroupIconResource(aktresource).SetCustomItemDataStream($5); }

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
    : numeral ',' long_string                      { stringtable_add($1.v, $3); }
    | numeral long_string                          { stringtable_add($1.v, $2); }
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
    : numeral                                      { $$:= TResourceDesc.Create($1.v); }
    | ident_string                                 { $$:= TResourceDesc.Create($1); }
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
    : numeral
    ;

numeral
    : _NUMDECIMAL                                  { $$:= str_to_num(yytext); }
    | _NUMHEX                                      { $$:= str_to_num(yytext); }
    ;

ident_string
    : _ID                                          { $$:= yytext; }
    | _QUOTEDSTR                                   { $$:= yytext; }
    ;

filename_string
    : _QUOTEDSTR                                   { $$:= TFileStream.Create(yytext, fmOpenRead or fmShareDenyWrite); }
    ;

long_string
    : _QUOTEDSTR                                   { $$:= yytext; }
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
        $$.WriteBuffer($1[1], Length($1));
      }
    | numeral
      {
        $$:= $<TMemoryStream>0;
        if $1.long then
          $$.WriteDWord(NtoLE($1.v))
        else
          $$.WriteWord(NtoLE(Word($1.v)));
      }
    ;

%%

{$I rclex.inc}
begin
  bufptr:= 0;
  lexlib.get_char:= @rc_get_char;
  lexlib.unget_char:= @rc_unget_char;
end.

