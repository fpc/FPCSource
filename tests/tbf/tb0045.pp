{ %FAIL }
{ Old file: tbf0231.pp }
{ Problem with comments                                OK 0.99.11 (PFV) }


{$undef dummy}

{$ifdef DUMMY}
   (* <= this should not be considered as a
   higher comment level !!

  test
{$endif dummy}

var
   e : extended;

begin
 e:=1.0;
 writeln(ln(e));
end.
