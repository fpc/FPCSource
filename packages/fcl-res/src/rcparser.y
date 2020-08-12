%{
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
%}

%token _ILLEGAL
%token _NUMDECIMAL _NUMHEX _NUMDECIMALL _NUMHEXL _QUOTEDSTR
%token _BEGIN _END
%token _LANGUAGE _CHARACTERISTICS _VERSION _MOVEABLE _FIXED _PURE _IMPURE _PRELOAD _LOADONCALL _DISCARDABLE

%token _ID

%type <rcnumtype> numpos numeral
%type <String> ident_string filename_string long_string
%type <TResourceDesc> resid
%type <TMemoryStream> raw_data raw_item

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
    : res_user
    ;

res_user
    : resid resid { create_resource($1, $2); } suboptions filename_string                  { assign_custom_stream($5); }
    | resid resid { create_resource($1, $2); } suboptions _BEGIN raw_data _END             { aktresource.SetCustomRawDataStream($6); }
    ;


resid
    : numeral                                      { $$:= TResourceDesc.Create($1.v); }
    | ident_string                                 { $$:= TResourceDesc.Create($1); }
    ;

suboptions
    : suboptions suboptions
    | _LANGUAGE numpos ',' numpos                  { aktresource.LangID:= MakeLangID($2.v, $4.v); }
    | _CHARACTERISTICS numpos                      { aktresource.Characteristics:= $2.v; }
    | _VERSION numpos                              { aktresource.Version:= $2.v; }
    | _MOVEABLE                                    { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; }
    | _FIXED                                       { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; }
    | _PURE                                        { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; }
    | _IMPURE                                      { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; }
    | _PRELOAD                                     { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; }
    | _LOADONCALL                                  { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; }
    | _DISCARDABLE                                 { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; }
    | /* empty */
    ;

languagedef
    : _LANGUAGE numpos ',' numpos                  { language:= MakeLangID($2.v, $4.v); }

numpos
    : numeral
    ;

numeral
    : _NUMDECIMAL                                  { $$.v:= StrToInt(yytext); $$.long:= False; }
    | _NUMDECIMALL                                 { $$.v:= StrToInt(Copy(yytext,1,length(yytext)-1)); $$.long:= True; }
    | _NUMHEX                                      { $$.v:= StrToInt('$'+Copy(yytext,3,Maxint)); $$.long:= False; }
    | _NUMHEXL                                     { $$.v:= StrToInt('$'+Copy(yytext,3,length(yytext)-3)); $$.long:= True; }
    ;

ident_string
    : _ID                                          { $$:= yytext; }
    | _QUOTEDSTR                                   { $$:= yytext; }
    ;

filename_string
    : _QUOTEDSTR                                   { $$:= yytext; }
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

