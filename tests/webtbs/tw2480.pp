{ Source provided for Free Pascal Bug Report 2480 }
{ Submitted by "Sergey Kosarevsky" on  2003-05-01 }
{ e-mail: netsurfer@au.ru }
{$mode objfpc}

Type tSingleton=Class
       Private
        _Instance:tSingleton;Static;
       Public
        Constructor Init;
        Class Function Instance:tSingleton;
     End;

class Function tSingleton.Instance:tSingleton;
Begin
   If _Instance=Nil Then _Instance:=tSingleton.Init;
   Exit(_Instance);
End;

Constructor tSingleton.Init;
Begin
End;

Begin
End.
