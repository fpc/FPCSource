Program Example89;

{ Program to demonstrate the FreeMem function. }
{$Mode Delphi}

Var P : Pointer;

begin
  Writeln ('Memory before : ',Memavail);
  GetMem(P,10000);
  FreeMem(P);
  Writeln ('Memory after  : ',Memavail);
end.
