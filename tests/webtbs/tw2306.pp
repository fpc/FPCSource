{ Source provided for Free Pascal Bug Report 2306 }
{ Submitted by "Sergey Kosarevsky" on  2003-01-03 }
{ e-mail: netsurfer@au.ru }
{$H-}

Const LONG_STR_SIZE=4096;

Type tLongStr=Object  // try to change this
                      // to Record
        LStr:Array[1..LONG_STR_SIZE] Of Char;
        LLength:Longint;
     End;

Operator := (S:String) R:tLongStr;
Begin
   R.LLength:=Length(S);
   Move(S[1],R.LStr[1],Length(S));
End;

Var T:tLongStr;

Begin
   T:='Hello';
End.
