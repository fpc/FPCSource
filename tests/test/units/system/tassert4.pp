{$C+}
program tassert4;

var
 global_boolean : boolean;
 counter : longint;

const
  RESULT_BOOLEAN = false;
  RESULT_STRING = 'hello world';

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
  assert(global_boolean,RESULT_STRING);
 end;

procedure test_assert_reference_local;
 var
  b: boolean;
 begin
  b:=RESULT_BOOLEAN;
  assert(b,RESULT_STRING);
 end;


procedure test_assert_register;
 begin
  assert(get_boolean,RESULT_STRING);
 end;

procedure test_assert_flags;
 var
  i,j : integer;
 begin
  i:=0;
  j:=-12;
  assert(i < j,RESULT_STRING);
 end;

 procedure test_assert_constant;
  begin
    assert(RESULT_BOOLEAN,RESULT_STRING);
  end;

  { Handle the assertion failed ourselves, so we can test everything in
    one shot.
  }
  Procedure MyAssertRoutine(const msg,fname:ShortString;lineno:longint;erroraddr:{$ifdef VER1_0}longint{$else}pointer{$endif});
   begin
     Inc(counter);
     if msg <> RESULT_STRING then
       fail;
   end;




begin
  counter:=0;
  AssertErrorProc := @MyAssertRoutine;
  Write('Assert test (FALSE) with assertions on...');
  test_assert_reference_global;
  test_assert_reference_local;
  test_assert_register;
  test_assert_flags;
  test_assert_constant;
  if counter <> 5 then
     fail
  else
     WriteLn('Success!');
end.
