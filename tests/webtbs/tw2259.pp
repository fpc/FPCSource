{ Source provided for Free Pascal Bug Report 2259 }
{ Submitted by "Sergey Kosarevsky" on  2002-12-14 }
{ e-mail: netsurfer@au.ru }
{$STATIC ON}

Type tObject=Object
        ClassFlags:Longint;Static;
     End;

Begin
   tObject.ClassFlags:=255;
End.
