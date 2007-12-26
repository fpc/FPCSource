{$mode objfpc}

type
  TMyObject = object
    Int: longint;
  end;

  TMyObject2 = object(TMyObject)
  end;

  TMyClass = class
    FByte: byte;
    Obj: TMyObject2;  // instance of this object is not aligned
  end;

var
  myclass: TMyClass;
begin
  myclass:=TMyClass.Create;
  myclass.obj.int:=1; // Crash due to unaligned data access
  myclass.Free;
end.
