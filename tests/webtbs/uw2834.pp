unit uw2834;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface

type
  TMyType = 1..2;

function PrintTypeInfo:pointer;

implementation

function PrintTypeInfo:pointer;
begin
  result:=pointer(TypeInfo(TMyType));
end;

end.
