Program ex15;

{ Program to demonstrate the TStream.Flush method }

Uses Objects;

Var L : String;
    P : PString;
    S : PBufStream; { Only one with Flush implemented. }

begin
  L:='Some constant string';
  { Buffer size of 100 }
  S:=New(PBufStream,Init('test.dat',stcreate,100));
  Writeln ('Writing "',L,'" to stream with handle ',S^.Handle);
  S^.WriteStr(@L);
  { At this moment, there is no data on disk yet. }
  S^.Flush;
  { Now there is. }
  S^.WriteStr(@L);
  { Close calls flush first }
  S^.Close;
  Writeln ('Closed stream. File handle is ',S^.Handle);
  S^.Open (stOpenRead);
  P:=S^.ReadStr;
  L:=P^;
  DisposeStr(P);
  Writeln ('Read "',L,'" from stream with handle ',S^.Handle);
  S^.Close;
  Dispose (S,Done);
end.