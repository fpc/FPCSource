{ %version=1.1 }
{ %opt=-Seh -vh }

{ Source provided for Free Pascal Bug Report 2351 }
{ Submitted by "Sergey Kosarevsky" on  2003-02-02 }
{ e-mail: netsurfer@au.ru }

{$mode objfpc}

Procedure DefaultParam(Param:String='10');
Begin
   WriteLn(Param);
End;

Begin
   DefaultParam();
End.
