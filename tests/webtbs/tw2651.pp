{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2651 }
{ Submitted by "Sergey Kosarevsky" on  2003-08-23 }
{ e-mail: netsurfer@au.ru }
{$mode objfpc}
{$inline on}

Type tMyClass=Class
        Class Procedure InlineProc;Inline;
     End;

Class Procedure tmyClass.InlineProc;Inline;
Begin
End;

Begin
   tMyClass.InlineProc;
End.
