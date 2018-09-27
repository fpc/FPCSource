program tw34239;
uses
  objects;
type
  PTObj=^TObj;
  TObj=object(TObject)
  end;
  TObj2=object(TObj)
  end;
  TSuperObj=object(TObj)
  end;

var
  t2:TObj2;

begin
  t2.init;
  if not t2.Is_Object(TypeOf(TObj)) then
    Halt(1);
  if t2.Is_Object(TypeOf(TSuperObj)) then
    Halt(2);
  //writeln(t2.Is_Object(TypeOf(TObj)));
  //writeln(t2.Is_Object(TypeOf(TSuperObj)));
  //readln;
end.
