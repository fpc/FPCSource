{ Source provided for Free Pascal Bug Report 3174 }
{ Submitted by "C Western" on  2004-06-19 }
{ e-mail: mftq75@dsl.pipex.com }
program test;

procedure Mumble(s: PChar);
begin
  WriteLn(s^);
  if s^<>'A' then
    halt(1);
end;

begin
  Mumble('A');
end.
