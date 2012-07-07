
unit tval2;

{$mode fpc}

interface

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}
{$endif}


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
