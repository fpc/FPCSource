
unit tval4;

{$mode fpc}

interface

{$ifdef cpujvm}
uses
  jdk15;

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}
{$endif}


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
