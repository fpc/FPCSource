{ constant string in first branch must not truncate a longer string in a later branch }
{$Mode Delphi}

procedure Check(const Actual, Expected: string; Code: integer);
begin
  if Actual<>Expected then
  begin
    writeln('Error ',Code,': expected "',Expected,'", got "',Actual,'"');
    Halt(Code);
  end;
end;

var
  b: Boolean;
  s: string;
  ss: shortstring;
  a: ansistring;
begin
  b:=false;
  ss:='2233';
  a:='2233';

  s:=if b then '11' else '22'+'33';
  Check(s,'2233',1);
  s:=if not b then '11' else '22'+'33';
  Check(s,'11',2);
  s:=if b then '11' else ss;
  Check(s,'2233',3);
  s:=if b then '11' else a;
  Check(s,'2233',4);
  s:=if b then '11' else string('22'+'33');
  Check(s,'2233',5);
  s:=if not b then a else '11';
  Check(s,'2233',6);
  s:=if not b then ss else '11';
  Check(s,'2233',7);
  s:=case b of true: '11'; else ss end;
  Check(s,'2233',8);
  s:=case b of true: '11'; else '22'+'33' end;
  Check(s,'2233',9);
  writeln('ok');
end.
