Program ex33;

{ Program to demonstrate the TCollection.AtDelete method }

Uses Objects,MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M : PMyObject;
    I,InitMem : Longint;

begin
  Randomize;
  C:=New(PCollection,Init(120,10));
  InitMem:=Memavail;
  Writeln ('Initial memory : ',InitMem);
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(I-1);
    C^.Insert(M);
    end;
  Writeln ('Added 100 Items. Memory available : ',Memavail);
  Write ('Lost : ',Initmem-Memavail,' bytes.');
  Write   ('(Should be 100*',SizeOF(TMyObject));
  Writeln ('=',100*SizeOf(TMyObject),')');
  With C^ do
    While Count>0 do AtDelete(Count-1);
  Writeln ('Freed all objects. Memory available : ',Memavail);
  Writeln ('Lost : ',Initmem-Memavail,' bytes.');
  Dispose(C,Done);
end.