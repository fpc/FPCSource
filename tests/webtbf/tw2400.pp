{ %fail }

{ Source provided for Free Pascal Bug Report 2400 }
{ Submitted by "Sergey Kosarevsky" on  2003-02-26 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tObj=Class
        Class Constructor Init;
     End;

Class Constructor tObj.Init;
Begin
End;

Begin
   tObj.Init;
End.
