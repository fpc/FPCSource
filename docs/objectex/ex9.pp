Program ex9;

{ Program to demonstrate TStream.Get and TStream.Put }

Uses Objects,MyObject;  { Definition and registration of TMyObject}

Var Obj : PMyObject;
    S : PStream;

begin
  Obj:=New(PMyObject,Init);
  Obj^.SetField($1111) ;
  Writeln ('Field value : ',Obj^.GetField);
  { Since Stream is an abstract type, we instantiate a TMemoryStream }
  S:=New(PMemoryStream,Init(100,10));
  S^.Put(Obj);
  Writeln ('Disposing object');
  S^.Seek(0);
  Dispose(Obj,Done);
  Writeln ('Reading object');
  Obj:=PMyObject(S^.Get);
  Writeln ('Field Value : ',Obj^.GetField);
  Dispose(Obj,Done);
end.