{$C+}
program tassert1;

var
 global_boolean : boolean;

const
  RESULT_BOOLEAN = true;

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
 var
  b: boolean;
 begin
  assert(get_boolean);
 end;

procedure test_assert_flags;
 var
  b: boolean;
  i,j : integer;
 begin
  i:=0;
  j:=-12;
  assert(i > j);
 end;

procedure test_assert_constant;
  begin
    assert(RESULT_BOOLEAN);
  end;



begin
  Write('Assert test (TRUE)...');
  test_assert_reference_global;
  test_assert_reference_local;
  test_assert_register;
  test_assert_flags;
  test_assert_constant;
  WriteLn('Success!');
end.
