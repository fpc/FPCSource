Program Example28;

{ Program to demonstrate the FreeMem and GetMem functions. }

Var P : Pointer;
    MM : Longint;

begin
  { Get memory for P }
  MM:=MemAvail;
  Writeln ('Memory available before GetMem : ',MemAvail);
  GetMem (P,80);
  MM:=MM-Memavail;
  Write   ('Memory available after GetMem  : ',MemAvail);
  Writeln (' or ',MM,' bytes less than before the call.');
  { fill it with spaces }
  FillChar (P^,80,' ');
  { Free the memory again }
  FreeMem (P,80);
  Writeln ('Memory available after FreeMem : ',MemAvail);
end.
