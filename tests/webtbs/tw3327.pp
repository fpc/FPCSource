{ Source provided for Free Pascal Bug Report 3327 }
{ Submitted by "Andreas Dorn" on  2004-09-21 }
{ e-mail: andreasd1@gmx.de }

Program BugReport;

Type Type1 = Record
               a: Word;
               b: LongWord;
     End;

Type Type2 = Record
               a: Word;
               b: LongWord;
               c: Array[0..1024] Of Char;
     End;

Type Type3 = Record
               a: Word;
               b: LongWord;
               c: Array[0..1024] Of LongWord;
     End;

Var One: Array [1..100000] Of Type1;
    Two: Type2;
    Three: Type3 absolute two;
    Four: Type1 absolute Three;
Begin;
  One[1].a:=12;
  Four := One[1];
  if Four.a<>12 then
    halt(1);
End.
