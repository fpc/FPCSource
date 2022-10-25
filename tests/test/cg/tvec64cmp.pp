{ Small test to check correct handling of 
  comparison of 64-bit values as boolean
  index of a vector, which lead
  to internal error for riscv32 cpu
  after commit #c83e6c34 }

const
  error_count : longint = 0;

procedure error(v : integer);
begin
  writeln('Error ',v);
  inc(error_count);
end;

const
  test_counter : longint = 0;

procedure check(b1,b2 : boolean);
begin
  inc(test_counter);
  if (b1<>b2) then
    error(test_counter);
end;

const
  Res : array[boolean] of boolean = (False,True);

var
  A,B : Int64;
  UA,UB : Qword;


begin
  A:=1;
  B:=-1;
  check(Res[A<B],false);
  check(Res[A>B],true);
  check(Res[A<=B],false);
  check(Res[A>=B],true);
  check(Res[A=B],false);
  check(Res[A<>B],true);
  A:=$123456789;
  B:=-$ABCDEF12345;
  check(Res[A<B],false);
  check(Res[A>B],true);
  check(Res[A<=B],false);
  check(Res[A>=B],true);
  check(Res[A=B],false);
  check(Res[A<>B],true);

  UA:=qword(1 shl 63) + qword(1 shl 36) + qword(1 shl 9);
  UB:=1;
  check(Res[UA<UB],false);
  check(Res[UA>UB],true);
  check(Res[UA<=UB],false);
  check(Res[UA>=UB],true);
  check(Res[UA=UB],false);
  check(Res[UA<>UB],true);

  if (error_count>0) then
    begin
      writeln('The test generated ',error_count,' failures');
      halt(1);
    end
  else
    writeln('Test completed without error');
end.
