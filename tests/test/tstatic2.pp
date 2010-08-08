program tstatic2;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TSomeClass = class
  private
    class var
      FSomethingStatic: Integer;
  public
    class procedure SetSomethingStatic(AValue: Integer); static;
    class property SomethingStatic: Integer read FSomethingStatic write SetSomethingStatic;
  end;

  TAnotherClass = class(TSomeClass)
  end;

{ TSomeClass }

class procedure TSomeClass.SetSomethingStatic(AValue: Integer);
begin
  FSomethingStatic := AValue;
  WriteLn('SomethingStatic:', SomethingStatic);
end;

begin
  TSomeClass.SomethingStatic := 4;
  if TSomeClass.SomethingStatic <> 4 then
    halt(1);
  TAnotherClass.SomethingStatic := 10;
  if TSomeClass.SomethingStatic <> 10 then
    halt(2); // outputs 10
end.
