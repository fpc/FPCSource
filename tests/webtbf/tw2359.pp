{ %FAIL }
{ %OPT=-St }
{ Source provided for Free Pascal Bug Report 2359 }
{ Submitted by "Sergey Kosarevsky" on  2003-02-06 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tObj=Class
        ClassFlags:Longint;Static;
        Class Constructor Init;
     End;

Class Constructor tObj.Init;
Begin
   ClassFlags:=0;
End;

Begin
   tObj.Init;
End.
