{ %FAIL }
{ Old file: tbf0197.pp }
{ should produce an error: problem with c1:=c2<c3 where c? is OK 0.99.11 (PM) a comp type }


var i : DWord;
    c1, c2 : comp;

begin
     c1 := 20000; c2 := 100;
     i := 0;
     repeat
           inc(i);
           c1 := (abs(3*c1)-c2) < c2;   { notice this !!! :) :) }
     until (i > 1000);
     Writeln(c1);
end.
