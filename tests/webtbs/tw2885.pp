{ Source provided for Free Pascal Bug Report 2885 }
{ Submitted by "Michalis Kamburelis" on  2004-01-07 }
{ e-mail: michalis@camelot.homedns.org }
procedure p(const b:Single); overload;
begin
  Writeln('single');
end;

procedure p(const b:Double); overload;
begin
  Writeln('double');
  halt(1);
end;

begin
  p(single(1.0));
end.
