{ Source provided for Free Pascal Bug Report 2975 }
{ Submitted by "Michalis Kamburelis" on  2004-02-15 }
{ e-mail: michalis@camelot.homedns.org }
procedure p;
const
  t:array[0..0]of AnsiString = ('blah');
begin
 Writeln('''', t[0], '''');
  if t[0]='' then
    halt(1);
end;

begin
 p;
 p;
end.
