{ Old file: tbs0058.pp }
{  causes an internal error 10 (problem with getregisterOK 0.99.1 (FK) }

{$r+}
var
   a1 : array[0..1,0..1] of word;
   a2 : array[0..1,0..1] of longint;
   i,j,l,n : longint;

begin
   a1[i,j]:=a2[l,n];
end.
