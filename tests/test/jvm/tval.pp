
program tval;

{$ifdef cpujvm}

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}
{$endif}

uses
{$ifdef cpujvm}
  {$ifdef java}jdk15{$else}androidr14{$endif},
{$endif}
  { longint type, short string }
  tval1,
  { dword type, short string }
  tval2,
  { int64 type, short string }
  tval3,
  { uint64 type, short string }
  tval4,
  { common variables and functions }
  tvalc;



begin
(*
  if (paramcount>0) and
     (paramstr(1)='verbose') then
       silent:=false;
*)
  TestAllVal1;
  TestAllVal2;
  TestAllVal3;
  TestAllVal4;
  if HasErrors then
    begin
      Writeln('Test tval failed');
      Halt(1);
    end;
end.
