
unit tval1;

{$mode fpc}

interface

function TestAllVal1 : boolean;

implementation

type
  IntegerType = longint;

{$i tval.inc}


function TestAllVal1 : boolean;
begin
  if (paramcount>0) and
     (paramstr(1)='verbose') then
       silent:=false;
  Writeln('Test val for longint type');
  TestAllVal1:=TestAll;
end;

end.
