Program ex22;

{ Program to demonstrate the TCollection.Load method }

Uses Objects,MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M : PMyObject;
    I : Longint;
    S : PMemoryStream;

begin
  C:=New(PCollection,Init(100,10));
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(100-I);
    C^.Insert(M);
    end;
  Writeln ('Inserted ',C^.Count,' objects');
  S:=New(PMemorySTream,Init(1000,10));
  C^.Store(S^);
  C^.FreeAll;
  Dispose(C,Done);
  S^.Seek(0);
  C^.Load(S^);
  Writeln ('Read ',C^.Count,' objects from stream.');
  Dispose(S,Done);
  Dispose(C,Done);
end.