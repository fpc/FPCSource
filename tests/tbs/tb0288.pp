{ Old file: tbs0340.pp }
{  }

{$packenum 1}
type
  t = (a,b,c,d,e);

const arr:  array[0..4] of t = (a,b,c,d,e);

var
  x: byte;

begin
  x := 0;
  writeln(ord(arr[x]),' ',ord(arr[x+1]),' ',ord(arr[x+2]),' ',ord(arr[x+3]),' ',ord(arr[x+4]));
  for x:=0 to 4 do
   if ord(arr[x])<>x then
    begin
      writeln('error in {$packenum 1}');
      halt(1);
    end;
end.
