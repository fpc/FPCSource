{ %FAIL }

program tprocvar13;

{$mode delphi}

type O = object
    class procedure Static; static;
end;
class procedure O.Static; begin end;

var IncompatWStatic: procedure of object;
begin
    IncompatWStatic := O.Static;
end.
