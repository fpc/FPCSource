program tw40593;

{$DEFINE USEOBJFPC}
{$IFDEF USEOBJFPC}
{$mode objfpc}
{$modeswitch functionreferences}
{$ELSE}
{$MODE DELPHI}
{$ENDIF}

Type
  TSomeClass = class(TObject)
    class procedure Y (X : Integer); static;
  end;
  TSomeProc = reference to procedure (X : integer);

Var
  P : TSomeProc;
  V : Integer;

class procedure TSomeClass.Y(X : Integer);
begin
  //Writeln(X);
  V := X;
end;


var
  C : TSomeClass;
begin
  C:=TSomeClass.Create;
  {$IFDEF USEOBJFPC}
  P:=@C.Y;
  {$ELSE}
  P:=C.Y;
  {$ENDIF}
  P(42);
  if V <> 42 then
    Halt(1);
end.

