{ Old file: tbs0134.pp }
{ 'continue' keyword is bugsgy.                          OK 0.99.6 (FK) }

{
In this simple examply, the even loop is wrong.  When continue; is called,
it should go back to the top and check the loop conditions and exit when i =
4, but continue skips checking the loop conditions and does i=5 too, then it
is odd, doesn't run the continue, and the loop terminates properly.
}


procedure demoloop( max:integer );
var i : integer;
begin
i := 1;
while (i <= max) do
    begin
    if (i mod 2 = 0) then
        begin
        writeln('Even ',i,' of ',max);
        inc(i);
        continue;
        end;
    writeln('Odd ',i,' of ',max);
    inc(i);
    end;
end;

begin
writeln('Odd loop (continue is *not* last call):');
demoloop(3);
writeln('Even loop (continue is last call):');
demoloop(4);
end.
