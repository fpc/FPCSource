{%fail}
{
  This program should fail compilation since the high()
  parameter cannot be used in a cdecl'ed function using
  array of const, it uses the exact calling conventions
  as a C compiler, and the length is not passed!
}

  procedure proc_const_smallarray_const_2(const arr : array of const);cdecl;
  var
   i: integer;
  begin
     if high(arr)<0 then
       WriteLn('hello world!');
  end;


Begin
end.
