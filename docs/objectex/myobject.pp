Unit MyObject;


Interface

Uses Objects;

Type
     PMyObject = ^TMyObject;
     TMyObject = Object(TObject)
       Field : Longint;
       Constructor Init;
       Constructor Load (Var Stream : TStream);
       Destructor Done;
       Procedure Store (Var Stream : TStream);
       Function  GetField : Longint;
       Procedure SetField (Value : Longint);
       end;

Implementation

Constructor TMyobject.Init;

begin
  Inherited Init;
  Field:=-1;
end;

Constructor TMyobject.Load (Var Stream : TStream);

begin
  Stream.Read(Field,Sizeof(Field));
end;

Destructor TMyObject.Done;

begin
end;

Function TMyObject.GetField : Longint;

begin
  GetField:=Field;
end;

Procedure TMyObject.SetField (Value : Longint);

begin
  Field:=Value;
end;

Procedure TMyObject.Store (Var Stream : TStream);

begin
  Stream.Write(Field,SizeOf(Field));
end;

Const MyObjectRec : TStreamRec = (
        Objtype : 666;
        vmtlink : Ofs(TypeOf(TMyObject)^);
        Load : @TMyObject.Load;
        Store : @TMyObject.Store;
        );

begin
  RegisterObjects;
  RegisterType (MyObjectRec);
end.