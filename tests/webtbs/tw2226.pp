{ Source provided for Free Pascal Bug Report 2226 }
{ Submitted by "" on  2002-11-12 }
{ e-mail:  }
Program compcrash;

const mindouble: double = -5.0E324;

var
  s : string;

begin
  str(mindouble,s);
  if s<>'                   -Inf' then
    begin
      writeln('error');
      halt(1);
    end;
end.
