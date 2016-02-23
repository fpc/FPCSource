program project1;

{$mode objfpc}{$h+}

type
  ITestInt = interface
    function GetN(a:Integer):Integer;
    function GetX(a:Integer):Integer;
  end;

  { TIntTest }

  TIntTest = class(TInterfacedObject,ITestInt)
    function GetN(a: Integer): Integer;
    function GetX(a: Integer): Integer;
  end;

  TIntTestVal = record
    FTestInt : ITestInt;
  end;

  TIntTestFunc = function(a:Integer):Integer of object;

  TIntTestInclude = class
    FValue : TIntTestVal;
  end;

  ttestobj = object
    a, b : TIntTestFunc;
  end;

var
  inttest : TIntTest;
  inttestvalinc : TIntTestInclude;
  x : ttestobj;

{ TIntTest }

function TIntTest.GetN(a: Integer): Integer;
begin
  Result:=a+1;
end;

function TIntTest.GetX(a: Integer): Integer;
begin
  Result:=a+2;
end;


begin
  inttest:=TIntTest.Create;
  inttestvalinc:=TIntTestInclude.Create;
  inttestvalinc.FValue.FTestInt:=inttest;
  x.a := @inttestvalinc.FValue.FTestInt.GetN;
  x.b := @inttestvalinc.FValue.FTestInt.GetX;
  writeln(x.a(1));
  writeln(x.b(1));
end.

