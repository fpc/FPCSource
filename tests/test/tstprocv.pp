{ %FAIL }
{ this compilation should fail
  because an ansitring should not be allowed
  as equivalent to a normal short string
  for procvars PM }

{$mode fpc}
{$H-}

uses
  strings;

Type
  type_error_proc = procedure (Const St : String);

Const
   error_proc : type_error_proc = nil;
   has_errors : boolean = false;
var
  st :  string;
  ast : ansistring;
  pst : pchar;



procedure string_error_proc(const err : string);
begin
{$ifdef DEBUG}
  writeln('String error proc: ',err);
{$endif DEBUG}
  if err<>st then
    has_errors:=true;
end;

procedure ansistring_error_proc(const err : ansistring);
begin
{$ifdef DEBUG}
  writeln('Ansistring error proc: ',err);
{$endif DEBUG}
  if err<>ast then
    has_errors:=true;
end;

procedure pchar_error_proc(const err : pchar);
begin
{$ifdef DEBUG}
  writeln('Pchar error proc: ',err);
{$endif DEBUG}
  if strcomp(err,pst)<>0 then
    has_errors:=true;
end;

begin
  st:='direct short string';
  string_error_proc(st);
  ast:='direct ansistring';
  ansistring_error_proc(ast);
  pst:='direct short string';
  pchar_error_proc(pst);

  error_proc:=@string_error_proc;
  st:='short string via procvar';
  error_proc(st);

  error_proc:=@ansistring_error_proc;
  ast:='ansistring via procvar';
  error_proc(ast);

  error_proc:=@pchar_error_proc;
  pst:='pchar via procvar';
  error_proc(pst);
  if has_errors then
    begin
      Writeln('Wrong code is generated');
      halt(1);
    end;
end.
