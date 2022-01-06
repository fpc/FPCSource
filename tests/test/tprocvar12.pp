{ %FAIL }

program tprocvar12;

{$mode delphi}

type C = class
    class procedure Static; static;
end;
class procedure C.Static; begin end;

type CC = class of C;

var IncompatWStatic: procedure of object;
begin
    IncompatWStatic := CC.Static;
end.
