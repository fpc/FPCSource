{ Source provided for Free Pascal Bug Report 2999 }
{ Submitted by "Sergey Kosarevsky" on  2004-03-03 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tSelector=(FIRST, SECOND);

Type tMyClass=Class
       Private
        T:Array[tSelector] Of Longint;
       Public
        Property T1:Longint Read T[FIRST];
        Property T2:Longint Read T[SECOND];
     End;

Begin
End.
