{ %fail }

{ Source provided for Free Pascal Bug Report 2285 }
{ Submitted by "Sergey Kosarevsky" on  2002-12-25 }
{ e-mail: netsurfer@au.ru }
Type CLASS_CONSTRUCTOR=Function(Param:String):Boolean Of Object;

Type tObject=Object
        Constructor Init(Param:String);
     End;

var
  a,b : longint;

Constructor tObject.Init(Param:String);
Begin
End;

Procedure CheckConstructor(C:CLASS_CONSTRUCTOR);
Begin
   a:=Longint(Pointer(C));
   WriteLn('a: ',a);
End;

Begin
   { This should fail, @tobject.init returns a pointer and
     is not compatible with a methodpointer }
   CheckConstructor(@tObject.Init);
   b:=Longint(Pointer(@tObject.Init));
   WriteLn('b: ',b);
   if a<>b then
    begin
      writeln('Error!');
      halt(1);
    end;
End.
