{ In a Unicode RTL (the unicodertl subtarget) PString points to the RTL's string, a UnicodeString, so a PString
  dereference can be passed as a var parameter of that type. }
program tpstringrtl;

{$mode delphiunicode}

uses
  SysUtils;

{$ifdef FPC_SUBTARGET_UNICODERTL}
procedure AppendX(var aText: UnicodeString);
begin
  aText := aText + 'x';
end;

var
  lS: UnicodeString;
  lP: PString;
{$endif}

begin
{$ifdef FPC_SUBTARGET_UNICODERTL}
  lS := 'a';
  {$T+}
  lP := @lS;
  {$T-}
  AppendX(lP^);
  if lS <> 'ax' then
    Halt(1);
  lP := NewStr('b');
  AppendX(lP^);
  if lP^ <> 'bx' then
    Halt(2);
  DisposeStr(lP);
{$endif}
  WriteLn('ok');
end.
