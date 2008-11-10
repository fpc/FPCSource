{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 3364 }
{ Submitted by "Sergey Kosarevsky" on  2004-10-22 }
{ e-mail: netsurfer@au.ru }
Type pMyObject1 = ^tMyObject1;
     tMyObject1 = Object
        Constructor Init;
        Destructor Done;
        Procedure MyProc;Virtual;Abstract;
     End;

Type pMyObject2 = ^tMyObject2;
     tMyObject2 = Object(tMyObject1)
        Constructor Init;
        Procedure MyProc;Virtual;
     End;

Constructor tMyObject1.Init;
Begin
End;

Destructor tMyObject1.Done;
Begin
End;

Constructor tMyObject2.Init;
Begin
End;

Procedure tMyObject2.MyProc;
Begin
End;

Var T:pMyObject1;

Begin
   T:=New(pMyObject2, Init);
   Dispose(T,Done);
End.
