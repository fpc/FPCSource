{ %fail }

{ Source provided for Free Pascal Bug Report 2403 }
{ Submitted by "Sergey Kosarevsky" on  2003-03-01 }
{ e-mail: netsurfer@au.ru }
{$MODE DELPHI}

Type tObj=Class
     End;

Begin
   If tObj Is 'tClass' Then WriteLn('OK');
End.
