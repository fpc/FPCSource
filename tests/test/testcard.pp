Program TestCardinal;

{ Tests different features of the cardinal type }
{ We must also test range checking thereafter   }
Procedure TestEqualAssign;
var
 l : longint;
 i : cardinal;
 j : cardinal;
Begin
 l:=$80000000; { longint }
 i:=l;  { longint  -> cardinal }
 j:=i;  { cardinal -> cardinal }
 l:=j;  { cardinal -> longint  }
end;


Procedure TestBiggerAssign;
var
 b: byte;
 c: char;
 s: shortint;
 i: integer;
 w: word;
 j: cardinal;
Begin
  b:=0;
  c:=#$7f;
  s:=120;
  i:=16384;
  w:=32767;
  j:=b;     { byte -> cardinal     }
  { THIS LINE CRASHES THE COMPILER FPC v0.99.5a }
{  j:=c;}     { char -> cardinal     }
  j:=ord(c);{ char -> cardinal     }
  j:=s;     { shortint -> cardinal }
  j:=i;     { integer -> cardinal  }
  j:=w;     { word -> cardinal     }
end;

Procedure  TestSmallerAssign;
var
 b: byte;
 c: char;
 s: shortint;
 i: integer;
 w: word;
 j: cardinal;
Begin
 j:=$ffffffff;
 b:=byte(j);
 c:=char(j);
 s:=shortint(j);
 i:=integer(j);
 w:=word(j);
end;


Procedure TestMul;
var
  j: cardinal;
  k: cardinal;
Begin
   j:=1;
   k:=$8000000;
   j:=j*16384;
   j:=j*k
end;


Procedure TestDiv;
var
  j: cardinal;
  k: cardinal;
Begin
   j:=1;
   k:=$8000000;
   j:=j div 16384;
   j:=j div k;
   k:=k mod 200;
end;


Procedure TestAdd;
Begin
end;


Procedure TestSub;
Begin
end;


Begin
  TestEqualAssign;
  TestBiggerAssign;
  TestSmallerAssign;
  TestMul;
  TestDiv;
end.


