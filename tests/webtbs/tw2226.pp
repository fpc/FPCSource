{ Source provided for Free Pascal Bug Report 2226 }
{ Submitted by "" on  2002-11-12 }
{ e-mail:  }
Program compcrash;

const mindouble: double = -5.0E324;

var
  s : string;
  correct : string;
begin
  case sizeof(extended) of
    10: correct := '                   -Inf';
    8: correct := '                  -Inf';
  end;
  str(mindouble,s);
  if s<>correct then
    begin
      writeln('error');
      halt(1);
    end;
end.
