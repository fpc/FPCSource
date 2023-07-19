
{ This test was created to check
  a code generation problem on sparc CPU
  in which OS_32 LD/ST operations were
  used on memory not aligned on a 4-byte
  boundary, leading to BUS error at execution.
  The test execution also checks that the value
  parameter changes are not passed back
  to the original variable in calling code. }

type
  trec = record
    w1 : word;
    b1 : boolean;
    b2 : byte;
  end;

  trec2 = record
    b3 : boolean;
    rec : trec;
  end;

const
  const_b2 = 45;
  const_w1 = 23456;
  count : integer = 0;

procedure check_trec(tr : trec);

begin
  if not tr.b1 or (tr.b2<>const_b2) or (tr.w1<>const_w1) then
    begin
      writeln('Wrong value passed to check_trec');
      halt(1);
    end
  else
    begin
      writeln(count,': check_rec ok');
    end;
  inc(tr.b2);
  dec(tr.w1);
  inc(count);
end;

var
  tr : trec;
  tr2 : trec2;

begin
  tr.b1:=true;
  tr.b2:=const_b2;
  tr.w1:=const_w1;
  check_trec(tr);
  check_trec(tr);
  tr2.b3:=true;
  tr2.rec:=tr;
  check_trec(tr2.rec);
  check_trec(tr2.rec);
end.
