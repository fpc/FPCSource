
unit tval2;

{$mode fpc}

interface

function TestAllval2 : boolean;

implementation

uses
  tvalc;

type
  IntegerType = dword;

{$i tval.inc}


function TestAllval2 : boolean;
begin
  Writeln('Test val for dword type');
  TestAllval2:=TestAll;
end;

end.
