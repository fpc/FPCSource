Program ex37;

{ Program to demonstrate the TStringCollection.Compare method }

Uses Objects;

Var C : PStringCollection;
    S : String;
    I : longint;

begin
  Randomize;
  C:=New(PStringCollection,Init(120,10));
  C^.Duplicates:=True; { Duplicates allowed }
  Writeln ('Inserting 100 records at random places.');
  For I:=1 to 100 do
    begin
    Str(Random(100),S);
    S:='String with value '+S;
    C^.Insert(NewStr(S));
    end;
  For I:=0 to 98 do
    With C^ do
    If Compare (At(i),At(I+1))=0 then
      Writeln ('Duplicate string found at position ',i);
  Dispose(C,Done);
end.