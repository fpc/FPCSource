{ %fail }

{ Source provided for Free Pascal Bug Report 2018 }
{ Submitted by "Sergey Kosarevsky" on  2002-06-29 }
{ e-mail: netsurfer@au.ru }
Unit tw2018;

Interface

Type tExtObject=Object
        { The next line should be refused }
        Procedure Draw;External;
     End;

Implementation

Procedure tExtObject.Draw;External;

Begin
End.
