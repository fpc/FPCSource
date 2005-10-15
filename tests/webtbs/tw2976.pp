{ Source provided for Free Pascal Bug Report 2976 }
{ Submitted by "Soeren Haastrup" on  2004-02-17 }
{ e-mail: haastrupsoeren@hotmail.com }
Type vector=array[1..2] of double;

     pair=record
           v1:double;
           v2:vector;
          end;


operator := (x:shortint) r:pair;
var i:longint;
Begin
    r.v1:=x;
    for i:=1 to 2 do r.v2[i]:=0;
End;


operator := (x:double) r:pair;
var i:longint;
Begin
    r.v1:=x;
    for i:=1 to 2 do r.v2[i]:=0;
End;

{
operator + (x:double;p:pair) r:pair; // Marked
var i:longint;
Begin
 r.v1:=x+p.v1;
 for i:=1 to 2 do r.v2[i]:=p.v2[i];
End;
}

operator + (p:pair;x:double) r:pair;
var i:longint;
Begin
 r.v1:=p.v1+x;
 for i:=1 to 2 do r.v2[i]:=p.v2[i];
End;

operator + (p1,p2:pair) r:pair;
var i:longint;
Begin
 r.v1:=p1.v1+p2.v1;
 for i:=1 to 2 do r.v2[i]:=p1.v2[i]+p2.v2[i];
End;


var a,b:pair;

Begin           //main

a:=2;            //ok
b:=a+a;          //ok
a:=2+b;          // ups?

End.
