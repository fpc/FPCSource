program tmultilinestring24;

{ engkin's bug example }

{$mode objfpc}
{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 15}
{$MultiLineStringLineEnding Platform}

var
{$MultiLineStringLineEnding CR}
  a: array[0..3] of string = (
``
,
`
`
,
`

`
,
`


`);

  {$MultiLineStringLineEnding CRLF}
b: array[0..3] of string = (
`1`
,
`1
2`
,
`1
2
3`
,
`1
2
3
4`);

procedure Test(constref StrArray:array of string);
var
  s,sHex: string;
  c: char;
begin
  for s in StrArray do
  begin
    WriteLn('Length: ',Length(s));
    sHex := '';
    for c in s do
      sHex := sHex+'$'+hexStr(ord(c),2);
    WriteLn(sHex);
  end;
  WriteLn('---------------');
end;

begin
  Test(a);
  Test(b);
end.
