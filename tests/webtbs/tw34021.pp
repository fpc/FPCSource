{ %NORUN }

program tw34021;

{$mode objfpc}{$H+}
{$COperators On}

type
  TMyClass = class
  end;

operator + (left: TMyClass; right: array of integer): TMyClass; overload;
var
    i: integer;
begin
    for i in right do
        writeln('add ', i);
    result := left;
end;

var
  c: TMyClass;
begin
  c += [1, 2, 3]; // ERROR: Operator is not overloaded: "TMyClass" + "Set Of Byte"
end.
