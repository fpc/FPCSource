
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
