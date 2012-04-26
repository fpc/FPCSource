
unit tval1;

{$mode fpc}

interface

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}
{$endif}


function TestAllVal1 : boolean;

implementation

uses
  tvalc;

type
  IntegerType = longint;

{$i tval.inc}


function TestAllVal1 : boolean;
begin
  Writeln('Test val for longint type');
  TestAllVal1:=TestAll;
end;

end.
