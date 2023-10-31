// Test to determine that anonymus functions are enabled in mode delphi
{$mode delphi}
{%NORUN}
program tfuncref52;

Type
  TProc = Reference to Procedure;

var
  P : TProc;

Procedure Testit;

begin
  P:=procedure 
    begin
  
    end;
  P;
end;

begin
  Testit;
end.

