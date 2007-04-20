
unit tval2;

{$mode fpc}

interface

function TestAllval2 : boolean;

implementation

type
  IntegerType = dword;

{$i tval.inc}


function TestAllval2 : boolean;
begin
  if (paramcount>0) and
     (paramstr(1)='verbose') then
       silent:=false;
  Writeln('Test val for dword type');
  TestAllval2:=TestAll;
end;

end.
