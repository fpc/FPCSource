{ Source provided for Free Pascal Bug Report 2069 }
{ Submitted by "Sergey Kosarevsky" on  2002-08-07 }
{ e-mail: netsurfer@au.ru }
Const WhiteSpace = [' ',#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,#16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31];

Function Trim(Const S:String):String;
Var Ofs,Len:Integer;
Begin
   Len:=Length(S);
   While (Len>0) And
         (S[Len] In WhiteSpace) Do Dec(Len);
   Ofs:=1;
   While (Ofs<=Len) And
         (S[Ofs] In WhiteSpace) Do Inc(Ofs);
   Exit(Copy(S,Ofs,1+Len-Ofs));
End;

Begin
End.
