{ Source provided for Free Pascal Bug Report 2305 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-03 }
{ e-mail: netsurfer@au.ru }
Type tObject=Object
        Constructor Init;
     End;

Constructor tObject.Init;
Begin
End;

Begin
   WriteLn(Int64(@tObject.Init));
End.
