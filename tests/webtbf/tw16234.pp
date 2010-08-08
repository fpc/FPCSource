{ %fail }

type
MyObjType = object	
 Field1 : String;
 Field2 : String;
 Field3 : String;
 property Prop3:String read Field3;
end;

var // or const
 Obj: MyObjType = (Prop3:'prop3';Field3:'field3'); // actually sets Field1 ?!

begin
 writeln(Obj.Field1); // prints 'prop3' ?!
 writeln(Obj.Field3); // prints 'field3'
 writeln(Obj.Prop3); // prints 'field3'
end.
