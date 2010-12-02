{ %norun% }
program tw18123;

{$mode objfpc}{$H+}

type
  TFoo2 = object
  type
    TFoo3 = object
    end;
    TFoo4 = object
      function GetFoo3: TFoo3;
    end;
  end;

function TFoo2.TFoo4.GetFoo3: TFoo2.TFoo3; // was error: Syntax error, ";" expected but "." found
begin
end;

begin
end.

