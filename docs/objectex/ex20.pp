Program ex20;

{ Program to demonstrate the TMemoryStream.Truncate method }

Uses Objects;

Var L : String;
    P : PString;
    S : PMemoryStream;
    I,InitMem : Longint;

begin
  initMem:=Memavail;
  L:='Some constant string';
  { Buffer size of 100 }
  S:=New(PMemoryStream,Init(1000,100));
  Writeln ('Free memory : ',Memavail);
  Writeln ('Writing 100 times "',L,'" to stream.');
  For I:=1 to 100 do
    S^.WriteStr(@L);
  Writeln ('Finished. Free memory : ',Memavail);
  S^.Seek(100);
  S^.Truncate;
  Writeln ('Truncated at byte 100. Free memory : ',Memavail);
  Dispose (S,Done);
  Writeln ('Finished. Lost ',InitMem-Memavail,' Bytes.');
end.