Program ex24;

{ Program to demonstrate the TCollection.IndexOf method }

Uses Objects,MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M,Keep : PMyObject;
    I : Longint;

begin
  Randomize;
  C:=New(PCollection,Init(100,10));
  Keep:=Nil;
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(I-1);
    If Random<0.1 then
     Keep:=M;
    C^.Insert(M);
    end;
  If Keep=Nil then
    begin
    Writeln ('Please run again. No object selected');
    Halt(1);
    end;
  Writeln ('Selected object has field : ',Keep^.GetField);
  Write ('Selected object has index : ',C^.IndexOf(Keep));
  Writeln (' should match it''s field.');
  C^.FreeAll;
  Dispose(C,Done);
end.