Program CharArr;

Var CharArray : Array[1..4] Of Char;

    S : String;

Begin
 CharArray:='BUG?';
 S:=CharArray;
 WriteLn(S);         { * This is O.K. * }
 WriteLn(CharArray); { * GENERAL PROTECTION FAULT. * }
End.

