{ Source provided for Free Pascal Bug Report 2300 }
{ Submitted by "Sergey Kosarevsky" on  2002-12-30 }
{ e-mail: netsurfer@au.ru }
Unit tw2300;
Interface
Const LONG_STR_SIZE=4096;
Type tLongStr=Object
        LStr:Array[1..LONG_STR_SIZE] Of Char;
        LLength:Longint;
     End;
Operator := (S:String) R:tLongStr;
Operator := (S:tLongStr) R:pChar;
Operator + (S1:tLongStr;S2:String) R:tLongStr;
Operator + (S1:String;S2:tLongStr) R:tLongStr;
Operator + (S1:tLongStr;S2:pChar ) R:tLongStr;
Implementation
Operator := (S:String) R:tLongStr;
Begin
   R.LLength:=Length(S);
   Move(S[1],R.LStr[1],Length(S));
End;
Operator := (S:tLongStr) R:pChar;
Begin
   S.LStr[S.LLength+1]:=#0;
   R:=pChar(@(S.LStr[1]));
End;
Operator + (S1:tLongStr;S2:String) R:tLongStr;
Begin
   R.LLength:=S1.LLength+Length(S2);
   Move(S1.LStr[1],R.LStr[1],S1.LLength);
   Move(S2[1],R.LStr[S1.LLength+1],Length(S2));
End;
Operator + (S1:String;S2:tLongStr) R:tLongStr;
Begin
   R.LLength:=Length(S1)+S2.LLength;
   Move(S1[1],R.LStr[1],Length(S1));
   Move(S2.LStr[1],R.LStr[Length(S1)+1],S2.LLength);
End;
Operator + (S1:tLongStr;S2:pChar) R:tLongStr;
Begin
   R.LLength:=S1.LLength+StrLen(S2);
   Move(S1.LStr[1],R.LStr[1],S1.LLength);
   Move(S2^,R.LStr[S1.LLength+1],StrLen(S2));
End;
Begin
End.
