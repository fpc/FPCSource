Program ex16;

{ Program to demonstrate the TStream.Truncate method }

Uses Objects;

Var L : String;
    P : PString;
    S : PDosStream; { Only one with Truncate implemented. }

begin
  L:='Some constant string';
  { Buffer size of 100 }
  S:=New(PDosStream,Init('test.dat',stcreate));
  Writeln ('Writing "',L,'" to stream with handle ',S^.Handle);
  S^.WriteStr(@L);
  S^.WriteStr(@L);
  { Close calls flush first }
  S^.Close;
  S^.Open (stOpen);
  Writeln ('Size of stream is : ',S^.GetSize);
  P:=S^.ReadStr;
  L:=P^;
  DisposeStr(P);
  Writeln ('Read "',L,'" from stream with handle ',S^.Handle);
  S^.Truncate;
  Writeln ('Truncated stream. Size is : ',S^.GetSize);
  S^.Close;
  Dispose (S,Done);
end.