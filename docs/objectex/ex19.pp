Program ex19;

{ Program to demonstrate the TStream.CopyFrom function }

Uses objects;

Var P : PString;
    L : String;
    S1,S2 : PStream;

begin
  L:='Constant string line';
  Writeln ('Writing to stream 1 : "',L,'"');
  S1:=New(PMemoryStream,Init(100,10));
  S2:=New(PMemoryStream,Init(100,10));
  S1^.WriteStr(@L);
  S1^.Seek(0);
  Writeln ('Copying contents of stream 1 to stream 2');
  S2^.Copyfrom(S1^,S1^.GetSize);
  S2^.Seek(0);
  P:=S2^.ReadStr;
  L:=P^;
  DisposeStr(P);
  Dispose (S1,Done);
  Dispose (S2,Done);
  Writeln ('Read from stream 2 : "',L,'"');
end.