{ Old file: tbs0072.pp }
{  causes an internal error 10  ( i386 ONLY )           OK 0.99.1 (FK) }

type
   tarraysingle = array[0..1] of single;

procedure test(var a : tarraysingle);

var
   i,j,k : integer;

begin
   a[i]:=a[j]-a[k];
end;

begin
end.
