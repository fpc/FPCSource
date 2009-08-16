
unit tval3;

{$mode fpc}

interface

function TestAllval3 : boolean;

implementation

uses
  tvalc;

type
  IntegerType = int64;

{$i tval.inc}


function TestAllval3 : boolean;
begin
  Writeln('Test val for int64 type');
  TestAllval3:=TestAll;
end;

end.
