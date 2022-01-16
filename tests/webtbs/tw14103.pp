program test;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TCallback = procedure of object;

  { TTestObject }

  TTestObject = class (TObject)
  public
    class procedure Test;
  end;
  TTestClass = class of TTestObject;

  TTestObject2 = class(TTestObject)
  end;

{ TTestObject }

var
  global: boolean;
  compareclass: TTestClass;

class procedure TTestObject.Test;
begin
  global:=true;
  if self <> compareclass then
    halt(2);
end;

var
  Cls: TTestClass;
  Callback: TCallback;
begin
  // Doesn't work
  global:=false;
  Callback := @TTestObject.Test;
  compareclass:=TTestObject;
  Callback();
  if not global then
    halt(1);

  global:=false;
  Callback := @TTestObject2.Test;
  compareclass:=TTestObject2;
  Callback();
  if not global then
    halt(1);
end.

