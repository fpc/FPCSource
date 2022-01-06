{ %FAIL }

program tprocvar14;

{$mode delphi}

type C = class end;
type H = class helper for C
    class procedure NonStatic;
end;
class procedure H.NonStatic; begin end;

var IncompatWNonStatic: procedure;
begin
    IncompatWNonStatic := H.NonStatic;
end.
