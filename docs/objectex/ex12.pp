Program ex12;

{ Program to demonstrate the TStream.GetSize function }

Uses objects;

Var L : String;
    S : PStream;

begin
  L:='Some kind of string';
  S:=New(PMemoryStream,Init(100,10));
  Writeln ('Stream size before write: ',S^.GetSize);
  S^.WriteStr(@L);
  Writeln ('Stream size after write : ',S^.GetSize);
  Dispose(S,Done);
end.    