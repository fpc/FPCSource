Program ex27;

{ Program to demonstrate the TCollection.Pack method }

Uses Objects,MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M : PMyObject;
    I,cnt : Longint;

begin
  Randomize;
  C:=New(PCollection,Init(100,10));
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(I-1);
    C^.Insert(M);
    end;
  cnt:=0;
  For I:=0 to 99 do
    begin
    If Random<0.1 then
      begin
      Inc(Cnt);
      C^.FreeItem(C^.At(I));
      C^.AtPut(I,Nil);
      end;
    end;
  Writeln ('Set ',cnt,' pointers to Nil. Count is ',C^.Count);
  Writeln ('Available memory : ',Memavail);
  C^.Pack;
  Writeln ('Packed collection. Count is ',C^.Count);
  cnt:=Memavail;
  Writeln ('Available memory : ',Cnt);
  C^.SetLimit(C^.Count);
  Writeln ('Set limit to ',C^.Count);
  Write   ('Available memory : ',Memavail,'.');
  Writeln (' Gained ',Memavail-cnt,' bytes.');
  Dispose(C,Done);
end.