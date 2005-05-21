{ Old file: tbs0055.pp }
{  internal error 10 (means too few registers           OK 0.99.1 (FK) }

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
