{ %fail }

{ Source provided for Free Pascal Bug Report 3186 }
{ Submitted by "Sergey Kosarevsky" on  2004-06-25 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tMyType=Class
     End;

Begin
   If Assigned(tMyType) Then;
End.
