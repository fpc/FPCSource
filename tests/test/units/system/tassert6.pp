{ %RESULT=227 }
{$C+}
program tassert6;

var
 global_boolean : boolean;

const
  RESULT_BOOLEAN = false;



procedure test_assert_reference_global;
 begin
  global_boolean:=RESULT_BOOLEAN;
  assert(global_boolean);
 end;




begin
  test_assert_reference_global;
end.
