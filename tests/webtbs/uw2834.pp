unit uw2834;

{$mode objfpc}{$H+}

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
