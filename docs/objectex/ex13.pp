Program ex13;

{
Program to demonstrate the TStream.ReadStr TStream.WriteStr functions
}

Uses objects;

Var P : PString;
    L : String;
    S : PStream;

begin
  L:='Constant string line';
  Writeln ('Writing to stream : "',L,'"');
  S:=New(PMemoryStream,Init(100,10));
  S^.WriteStr(@L);
  S^.Seek(0);
  P:=S^.ReadStr;
  L:=P^;
  DisposeStr(P);
  DisPose (S,Done);
  Writeln ('Read from stream : "',L,'"');
end.