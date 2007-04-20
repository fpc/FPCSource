
unit tval4;

{$mode fpc}

interface

function TestAllval4 : boolean;

implementation

type
  IntegerType = qword;

{$i tval.inc}


function TestAllval4 : boolean;
begin
  if (paramcount>0) and
     (paramstr(1)='verbose') then
       silent:=false;
  Writeln('Test val for qword type');
  TestAllval4:=TestAll;
end;

end.
