program test_random;

const
  buckets = 1000000;
  repeats = 20;
var
  hist : array[1..buckets] of LongInt;
  i : longint;
  chisquare : extended;
begin
  randomize;
  WriteLn('Random Values II:');
  for i:=0 to 100 do
   begin
     WriteLn(Random(100));
   end;
  randomize;
  WriteLn('Random Values I:');
  for i:=0 to 100 do
   begin
     WriteLn(Random(100));
   end;

  { do a Chi^2 test, hopefully I got it right }
  fillchar(hist,sizeof(hist),0);
  for i:=1 to buckets*repeats do
    inc(hist[longint(trunc(random*buckets)+1)]);

  chisquare:=0.0;
  for i:=low(hist) to high(hist) do
    chisquare:=chisquare+sqr(hist[i]-repeats)/repeats;
  writeln(chisquare);

  { m=1000000; p=0.1 }
  if chisquare>1001741 then
    halt(1)
  else
    writeln('ok');
end.
