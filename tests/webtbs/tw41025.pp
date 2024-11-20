{ %OPT=-Sew }
program a;
uses types;

var b:longbool;

begin
  b:=false;
  case b of
  true: writeln('True');
  false: writeln('False');
  end;
end.
