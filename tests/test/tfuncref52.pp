// Test to determine that function references are enabled in mode delphi
{$mode delphi}
{ $modeswitch functionreferences}
{%NORUN}
program tfuncref52;

Type
  TProc = Reference to Procedure;

var
  P : TProc;

Procedure Testit;

  Procedure SoSo;

  begin

  end;

begin
  P:=SoSo;
  P;
end;

begin
  Testit;
end.

