{ %OPT=-Sew }
{ Source provided for Free Pascal Bug Report 2307 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-03 }
{ e-mail: netsurfer@au.ru }
{$WARNINGS ON}

Function BugHint:String;
Begin
   Exit('ResultString');
End;

Begin
   BugHint;
End.
