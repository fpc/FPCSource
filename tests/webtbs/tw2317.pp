{ Source provided for Free Pascal Bug Report 2317 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-09 }
{ e-mail: netsurfer@au.ru }

{$static on}

Type tObject=Object
        Function GetVMT:Pointer;Static;
     End;

Function tObject.GetVMT:Pointer;
Begin
   Exit(TypeOf(Self));
End;

Begin
   WriteLn(Longint(tObject.GetVMT()));
End.

