{ Old file: tbs0033.pp }
{  tests var p : pchar; begin p:='c'; end.             OK 0.9.9 }

var
   p1 : pchar;
   p2 : array[0..10] of char;
   s : string;
   c : char;

begin
   p1:='c';
   s:='c';
   { this isn't allowed
   p1:=c;
   }
end.
