program test;

{$mode objfpc}{$H+}

type
  TCallback = procedure of object;

  TTestObject = class (TObject)
  public
    class procedure Test;
  end;

class procedure TTestObject.Test;
begin
  writeln(Self.ClassName); // Self should point to TTestObject (class)
  if Self.ClassName<>'TTestObject' then
    halt(1);
end;

var
  Callback: TCallback;
  O: TTestObject;
begin
  O := TTestObject.Create;
  Callback := @O.Test;
  Callback();
  O.Free;
end.
