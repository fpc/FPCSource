{ Source provided for Free Pascal Bug Report 4152 }
{ Submitted by "C Western" on  2005-07-03 }
{ e-mail: mftq75@dsl.pipex.com }
{$R+}{$Q+}
var
  p:^Byte;
  c:Byte;
  d:Integer;
{$ifdef cpu64}
  v : qword;
{$else}
  v : cardinal;
{$endif}
begin
  v:=100;
  inc(v,-1);
  p:=@c;
  Inc(p,-1);  // Gives compile time error: range check error while evaluating constants
  d:=2;
  Inc(d,-1);
  Inc(p,d); // This fails at run time
end.
