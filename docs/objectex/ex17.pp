Program ex17;

{ Program to demonstrate the TStream.Seek method }

Uses Objects;

Var L : String;
    Marker : Word;
    P : PString;
    S : PDosStream;

begin
  L:='Some constant string';
  { Buffer size of 100 }
  S:=New(PDosStream,Init('test.dat',stcreate));
  Writeln ('Writing "',L,'" to stream.');
  S^.WriteStr(@L);
  Marker:=S^.GetPos;
  Writeln ('Set marker at ',Marker);
  L:='Some other constant String';
  Writeln ('Writing "',L,'" to stream.');
  S^.WriteStr(@L);
  S^.Close;
  S^.Open (stOpenRead);
  Writeln ('Size of stream is : ',S^.GetSize);
  Writeln ('Seeking to marker');
  S^.Seek(Marker);
  P:=S^.ReadStr;
  L:=P^;
  DisposeStr(P);
  Writeln ('Read "',L,'" from stream.');
  S^.Close;
  Dispose (S,Done);
end.