{ Source provided for Free Pascal Bug Report 2627 }
{ Submitted by "Sergey Kosarevsky" on  2003-08-10 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Type tMyClass=Class
        Procedure DoSomething;Virtual;Abstract;
        Class Procedure Process(C:tMyClass);
     End;

Class Procedure tMyClass.Process(C:tMyClass);
Begin
   With C Do DoSomething;
End;

Begin
End.
