{ Old file: tbs0031.pp }
{  tests array[boolean] of ....                        OK 0.9.8 }

var
   a : array[boolean] of longint;

begin
   a[true]:=1234;
   a[false]:=123;
end.
