{$C-}
program tassert1;

var
 global_boolean : boolean;
 counter : longint;

const
  RESULT_BOOLEAN = false;


procedure fail;
 begin
   Writeln('Failure!');
   Halt(1);
 end;

function get_boolean : boolean;
 begin
   get_boolean := RESULT_BOOLEAN;
 end;

procedure test_assert_reference_global;
 begin
  global_boolean:=RESULT_BOOLEAN;
  assert(global_boolean);
 end;

procedure test_assert_reference_local;
 var
  b: boolean;
 begin
  b:=RESULT_BOOLEAN;
  assert(b);
 end;


procedure test_assert_register;
 begin
  assert(get_boolean);
 end;

procedure test_assert_flags;
 var
  i,j : integer;
 begin
  i:=0;
  j:=-12;
  assert(i < j);
 end;

 procedure test_assert_constant;
  begin
    assert(RESULT_BOOLEAN);
  end;

  { Handle the assertion failed ourselves, so we can test everything in
    one shot.
  }
  Procedure MyAssertRoutine(const msg,fname:ShortString;lineno:longint;addr:{$ifdef VER1_0}longint{$else}pointer{$endif});
   begin
     Inc(counter);
   end;




begin
  counter:=0;
  AssertErrorProc := @MyAssertRoutine;
  Write('Assert test (FALSE) with assertions off...');
  test_assert_reference_global;
  test_assert_reference_local;
  test_assert_register;
  test_assert_flags;
  test_assert_constant;
  if counter <> 0 then
     fail
  else
     WriteLn('Success!');
end.
