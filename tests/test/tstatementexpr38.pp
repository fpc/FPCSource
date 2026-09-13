{ anonymous procedures in the branches of an if-expression }
{$Mode Delphi}

type
  TProcRef = reference to procedure;
  TIntFuncRef = reference to function(a: Integer): Integer;
  TProc = procedure;

var
  Called: Integer;

procedure Test(b: Boolean; Expected: Integer; Code: Integer);
var
  SomeProc: TProcRef;
  Local: Integer;
begin
  Local:=Expected*10;
  Called:=0;
  SomeProc:=if b then
      procedure
      begin
        Called:=1+Local;
      end
    else
      procedure
      begin
        Called:=2+Local;
      end;
  SomeProc();
  WriteLn(Called);
  if Called<>Expected+Local then
    Halt(Code);
end;

function GetFunc(b: Boolean): TIntFuncRef;
begin
  Result:=if b then
      function(a: Integer): Integer
      begin
        Result:=a+1;
      end
    else
      function(a: Integer): Integer
      begin
        Result:=a*2;
      end;
end;

procedure TestProcVar(b: Boolean; Expected: Integer; Code: Integer);
var
  SomeProc: TProc;
begin
  Called:=0;
  SomeProc:=if b then
      procedure
      begin
        Called:=3;
      end
    else
      procedure
      begin
        Called:=4;
      end;
  SomeProc();
  WriteLn(Called);
  if Called<>Expected then
    Halt(Code);
end;

begin
  Test(true,1,1);
  Test(false,2,2);
  if GetFunc(true)(10)<>11 then
    Halt(3);
  if GetFunc(false)(10)<>20 then
    Halt(4);
  TestProcVar(true,3,5);
  TestProcVar(false,4,6);
end.
