{ %opt=-O2 -CR }

{ Source provided for Free Pascal Bug Report 2923 }
{ Submitted by "Sergey Kosarevsky" on  2004-01-30 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tMyClass=Class
       Public
        Constructor Init;
     End;

Constructor tMyClass.Init;
Begin
   Inherited Create;
End;                        // <- line 9

Var A:tMyClass;

Begin
   A:=tMyClass.Init;
End.
