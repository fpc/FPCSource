{%fail}
{
  This program should fail compilation since array of const
  with cdecl modifier can only be used on last parameter of
  routine
}

  procedure proc_const_smallarray_const_2(const arr : array of const; b: byte);cdecl;
  begin
  end;


Begin
end.

{
  $Log$
  Revision 1.1  2002-11-09 21:57:57  carl
    + added testing for cdecl and array of const. (should not compile)

}
