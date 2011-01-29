program WideVariant;

{$mode objfpc}{$H+}

uses
  Classes;

// A literal casted to Wide/UnicodeString is expected to be of type vtWideString.
const
  expected: array[0..3] of integer = (vtAnsiString,
{$ifdef windows}
    vtWideString, vtUnicodeString, vtWideString
{$else}
    vtUnicodeString, vtUnicodeString, vtUnicodeString
{$endif}
  );

procedure variantfunc(vars: array of const);
var
  i: integer;
begin
  for i := Low(vars) to High(vars) do
  begin
    writeln('vars[',i,'].VType=',ord(vars[i].VType));
    if vars[i].VType <> expected[i] then
      Halt(1);
  end;  
end;

var
  wstr: WideString;
begin
  variantfunc(['abc',WideString('def'),UnicodeString('123'),wstr]);
end.

