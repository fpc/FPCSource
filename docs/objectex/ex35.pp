Program ex35;

{ Program to demonstrate the TSortedCollection.Insert method }

Uses Objects,MyObject,MySortC;
 { For TMyObject,TMySortedCollection definition and registration }

Var C : PSortedCollection;
    M : PMyObject;
    I : Longint;

Procedure PrintField (Dummy: Pointer;P : PMyObject);

begin
  Writeln ('Field : ',P^.GetField);
end;


begin
  Randomize;
  C:=New(PMySortedCollection,Init(120,10));
  Writeln ('Inserting 100 records at random places.');
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(Random(100));
    C^.Insert(M)
    end;
  Writeln ('Values : ');
  C^.Foreach(@PrintField);
  Dispose(C,Done);
end.