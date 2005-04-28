{ Source provided for Free Pascal Bug Report 3898 }
{ Submitted by "alphax" on  2005-04-19 }
{ e-mail: acmui_2004@163.com }
program Project1;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

uses
  SysUtils,
  Variants;

var
  DT: TDateTime;
  V: Variant;
begin
  DT := Now();
  V := DT;
  if VarType(V)<>varDate then
    halt(1);
  writeln('ok');
end.
