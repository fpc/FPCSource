{ %norun }
var

 offset : Integer;
 o      : TObject;

begin

 // OK
 o := TObject(1);

 offset := PtrInt(IInterface(TInterfacedObject(o))) - 1;
 //

 // project1.lpr(17,19) Error: Internal error 121120001
 offset := PtrInt(IInterface(TInterfacedObject(1))) - 1;
end.
