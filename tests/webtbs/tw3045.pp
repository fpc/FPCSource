{ Source provided for Free Pascal Bug Report 3045 }
{ Submitted by "marco" on  2004-04-10 }
{ e-mail:  }

{$mode delphi}
procedure raiselastwin32error;

begin
end;

const raiselastoserror : procedure = raiselastwin32error;   // ok

var varraiselastoserror : procedure = raiselastwin32error;  // fail

begin
end.
