unit tw5023;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

type
  TScanLine = array of Integer;

implementation

procedure TestSwap(var A, B: Integer);
begin
  // do something
end;

procedure Main(const ALine: TScanLine);
begin
  TestSwap(ALine[0], ALine[1]);  // <-- error here
end;

end.
