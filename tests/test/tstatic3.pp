{ %FAIL}
program tstatic3;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TSomeClass = class
  private
    class var FSomethingStatic: Integer;
    var FSomethingRegular: Integer;
    class procedure SetSomethingStatic(AValue: Integer); static;
  public
    class property SomethingStatic: Integer read FSomethingStatic write SetSomethingStatic;
    property SomethingRegular: Integer read FSomethingRegular write FSomethingRegular;
  end;

{ TSomeClass }

class procedure TSomeClass.SetSomethingStatic(AValue: Integer);
begin
  FSomethingRegular := AValue;
end;

begin
end.
