Program ex39;

{ Program to demonstrate the TUnsortedStrCollection.Insert method }

Uses Objects,Strings;

Var C : PUnsortedStrCollection;
    S : String;
    I : longint;
    P : Pchar;
    
begin
  Randomize;
  C:=New(PUnsortedStrCollection,Init(120,10));
  Writeln ('Inserting 100 records at random places.');
  For I:=1 to 100 do
    begin
    Str(Random(100),S);
    S:='String with value '+S;
    P:=StrAlloc(Length(S)+1);
    C^.Insert(StrPCopy(P,S));
    end;
  For I:=0 to 99 do
    Writeln (I:2,': ',PChar(C^.At(i)));
  Dispose(C,Done);
end.