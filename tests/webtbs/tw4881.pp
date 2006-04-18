{ Source provided for Free Pascal Bug Report 4881 }
{ Submitted by "Jasper Neumann" on  2006-03-07 }
{ e-mail: _-jane-_@web.de }
type
  openstring=integer;

procedure test(var x:openstring);
begin end;

var
  x: openstring;
begin
  test(x);
end.
