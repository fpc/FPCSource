{ %INTERACTIVE }
{ %NOTE=This test requires libffi on most platforms }

program tw39506;

{$if not defined(cpui386)}
{$define useffi}
{$endif}
{$if defined(cpux86_64) and defined(win64)}
{$undef useffi}
{$endif}

uses
{$ifdef useffi}
  ffi.manager,
{$endif}
  rtti,
  typinfo;

var
  ok: Boolean = False;

procedure p(aArg1: ShortString; aArg2: AnsiString; aArg3: UnicodeString; aArg4: WideString);
begin
  if aArg1 <> 'hello' then
    Halt(1);
  if aArg2 <> 'world' then
    Halt(2);
  if aArg3 <> 'foo' then
    Halt(3);
  if aArg4 <> 'bar' then
    Halt(4);
  ok := True;
end;

var
  a: TValueArray;
begin
  a := [ShortString('hello'), AnsiString('world'), UnicodeString('foo'), WideString('bar')];
  Invoke(@p,a,ccReg,nil,false,false);
  if not ok then
    Halt(5);
end.

