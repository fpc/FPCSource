program tstatic5;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type

  { TSomeClass }

  TSomeClass = class
  public
    class var
      FSomethingStatic: Integer;
      FSomethingStatic1: String;
    class procedure SetSomethingStatic(AValue: Integer); static;
    var
      FSomeRegularField: Integer;
      FSomeRegularField1: String;
    class var
      FSomethingStatic2: byte;
    class property SomethingStatic: Integer read FSomethingStatic write SetSomethingStatic;
    class property SomethingStatic1: String read FSomethingStatic1 write FSomethingStatic1;
    class property SomethingStatic2: byte read FSomethingStatic2 write FSomethingStatic2;
    property SomethingRegular: Integer read FSomeRegularField write FSomeRegularField;
    property SomethingRegular1: String read FSomeRegularField1 write FSomeRegularField1;
  end;

{ TSomeClass }

class procedure TSomeClass.SetSomethingStatic(AValue: Integer);
begin
  FSomethingStatic := AValue;
end;

begin
end.
