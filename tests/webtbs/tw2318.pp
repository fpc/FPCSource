{ Source provided for Free Pascal Bug Report 2318 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-09 }
{ e-mail: netsurfer@au.ru }

Type tObject=Object
        Constructor Init;
        Function GetVMT:Pointer;Static;
        Destructor Done;Virtual;
     End;

Constructor tObject.Init;
Begin
End;


Function tObject.GetVMT:Pointer;
Begin
  GetVMT:=self;
End;


Destructor tObject.Done;
Begin
End;

Var O:tObject;

Begin
   O.Init;
   if (O.GetVMT= nil) or (O.getVMT<>tObject.GetVMT) then
     begin
       Writeln('Problem with static methods');
       Writeln('O.getVMT=',hexstr(longint(O.getVMT),8));
       Writeln('tObject.GetVMT=',hexstr(longint(tobject.GetVMT),8));
       halt(1);
     end;
End.
