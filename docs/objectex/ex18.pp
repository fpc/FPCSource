program ex18;

{ Program to demonstrate the TStream.Read method }

Uses Objects;

Var Buf1,Buf2 : Array[1..1000] of Byte;
    I : longint;
    S : PMemorySTream;

begin
  For I:=1 to 1000 do
    Buf1[I]:=Random(1000);
  Buf2:=Buf1;
  S:=New(PMemoryStream,Init(100,10));
  S^.Write(Buf1,SizeOf(Buf1));
  S^.Seek(0);
  For I:=1 to 1000 do
    Buf1[I]:=0;
  S^.Read(Buf1,SizeOf(Buf1));
  For I:=1 to 1000 do
    If Buf1[I]<>buf2[i] then
      Writeln ('Buffer differs at position ',I);
  Dispose(S,Done);
end.