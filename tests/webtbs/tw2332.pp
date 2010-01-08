{ Source provided for Free Pascal Bug Report 2332 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-21 }
{ e-mail: netsurfer@au.ru }

Type tObject=Object
        Constructor Init;
        Function GetVMT:Pointer;Static;
        Destructor Done;Virtual;
     End;

Function tObject.GetVMT:Pointer;
Begin
   Exit(Self);
End;

Constructor tObject.Init;
Begin
End;

Destructor tObject.Done;
Begin
End;

Var O:tObject;

Begin
   O.Init;
   WriteLn(Longint(TypeOf(tObject)));
   WriteLn(Longint(O.GetVMT));
   WriteLn(Longint(tObject.GetVMT));
   if (O.GetVMT<>TypeOf(tObject)) or (TypeOf(tObject)<>tObject.GetVMT) then
     begin
       writeln('Error with typeof');
       halt(1);
     end;
End.
