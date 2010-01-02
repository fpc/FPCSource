program varfunc_test;

{$IFDEF FPC}
  {$mode Delphi}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$H+}

uses sysutils;

function TestFunc1 : Longint;
begin
  Result := 100;
end;

Type Tfunc1 = function : Longint;
var
  TestFunc2 : Tfunc1 = TestFunc1;

begin
  writeln({$IFDEF FPC}'FPC'{$ELSE}'Delphi'{$ENDIF});

  writeln( Format('%d',[TestFunc1]) );
  writeln( Format('%d',[TestFunc2]) ); 
end.
