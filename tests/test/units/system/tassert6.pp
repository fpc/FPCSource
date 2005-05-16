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

{
  $Log: tassert6.pp,v $
  Revision 1.2  2005/02/14 17:13:37  peter
    * truncate log

}
