{ %FAIL }

program tprocvar11;

{$mode delphi}

type C = class
    class procedure NonStatic;
end;
class procedure C.NonStatic; begin end;

type CC = class of C;

var IncompatWNonStatic: procedure;
begin
    IncompatWNonStatic := CC.NonStatic;
end.
