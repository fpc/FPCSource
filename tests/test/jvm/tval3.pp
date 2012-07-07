
unit tval3;

{$mode fpc}

interface

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}
{$endif}


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
