{ Source provided for Free Pascal Bug Report 4078 }
{ Submitted by "Thomas Schatzl" on  2005-06-13 }
{ e-mail:  }
{$ASSERTIONS ON}
var
  q : dword;

begin
  q := $80000000;
  q := q div 16;
  assert(q = $8000000);
end.
