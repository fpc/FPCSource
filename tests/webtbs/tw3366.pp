{ %opt=-Sc }

{ Source provided for Free Pascal Bug Report 3366 }
{ Submitted by "Sergey Kosarevsky" on  2004-10-22 }
{ e-mail: netsurfer@au.ru }
Type tVector3=Array[1..3] Of Single;

Operator * (A:tVector3;B:Single) R:tVector3;
Var I:Longint;
Begin
   For I:=1 To 3 Do R[I]:=A[I]*B;

   Exit(R);
End;

Var A:tVector3;

Begin
   A[1]:=1;
   A[2]:=1;
   A[3]:=1;

   A*=2;
End.
