{ %FAIL }

program tprocvar15;

{$mode delphi}

type C = class end;
type H = class helper for C
    class procedure Static; static;
end;
class procedure H.Static; begin end;

var IncompatWStatic: procedure of object;
begin
    IncompatWStatic := H.Static;
end.
