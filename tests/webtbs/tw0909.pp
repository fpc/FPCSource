uses sysutils;
 var r:array[0..3] of real;
begin
 r[0]:=1; r[1]:=2; r[2]:=3; r[3]:=4;
 // the following is supposed to print "1, 2, 3, 4", instead it prints "4, 4, 4, 4"
 writeln(format('%g, %g, %g, %g',[r[0],r[1],r[2],r[3]]));
end.
