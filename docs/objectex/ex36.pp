Program ex36;

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
  C^.Duplicates:=True;
  Writeln ('Inserting 100 records at random places.');
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(Random(100));
    C^.Insert(M)
    end;
  M:=New(PMyObject,Init);
  Repeat;
    Write ('Value to search for (-1 stops) :');
    read (I);
    If I<>-1 then
      begin
      M^.SetField(i);
      If Not C^.Search (M,I) then
        Writeln ('No such value found')
      else
        begin
        Write ('Value ',PMyObject(C^.At(I))^.GetField);
        Writeln (' present at position ',I);
        end;
      end;
  Until I=-1;
  Dispose(M,Done);
  Dispose(C,Done);
end.