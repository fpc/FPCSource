
unit tval4;

{$mode fpc}

interface

function TestAllval4 : boolean;

implementation

uses
  tvalc;

type
  IntegerType = qword;

{$i tval.inc}


function TestAllval4 : boolean;
begin
  Writeln('Test val for qword type');
  TestAllval4:=TestAll;
end;

end.
