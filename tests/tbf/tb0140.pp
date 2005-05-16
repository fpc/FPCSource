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
  $Log: tb0140.pp,v $
  Revision 1.2  2005/02/14 17:13:35  peter
    * truncate log

}
