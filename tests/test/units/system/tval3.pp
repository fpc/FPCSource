
unit tval3;

{$mode fpc}

interface

function TestAllval3 : boolean;

implementation

type
  IntegerType = int64;

{$i tval.inc}


function TestAllval3 : boolean;
begin
  if (paramcount>0) and
     (paramstr(1)='verbose') then
       silent:=false;
  Writeln('Test val for int64 type');
  TestAllval3:=TestAll;
end;

end.
