{ %fail }

{ Source provided for Free Pascal Bug Report 2273 }
{ Submitted by "Sergey Kosarevsky" on  2002-12-21 }
{ e-mail: netsurfer@au.ru }
Type tSomething=Object
        Constructor Init;
     End;

Constructor tSomething.Init;
Begin
   Inherited Init;
End;

Begin
End.
