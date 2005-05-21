{ Old file: tbs0272.pp }
{ No error issued if wrong parameter in function inside a second function OK 0.99.13 (PFV) }

program test_const_string;


function astring(s :string) : string;

begin
  astring:='Test string'+s;
end;

procedure testvar(var s : string);
begin
  writeln('testvar s is "',s,'"');
end;

procedure testconst(const s : string);
begin
  writeln('testconst s is "',s,'"');
end;

procedure testvalue(s : string);
begin
  writeln('testvalue s is "',s,'"');
end;

const
  s : string = 'test';
  conststr = 'Const test';
begin
  testvalue(astring('e'));
  testconst(astring(s));
  testconst(conststr);
end.
