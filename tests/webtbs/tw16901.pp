{ %opt=-g-h }

program project1;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TClassA = class(TInterfacedObject,IInterface)
  public
    constructor Create();
  end;

constructor TClassA.Create(); 
var
  x : IInterface;
begin
  x := Self;
end;

var
  y : IInterface;
begin
  HaltOnNotReleased := true;
  y := TClassA.Create();
end.


