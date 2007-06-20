{%opt=-OoSTACKFRAME}

procedure Proc;
var
  s:shortstring;
begin
  s:='test';
  if Copy(s,1,4)<>'test' then begin
    writeln('Test failed!');
    Halt(1);
  end
  else
    writeln('Test OK.');
end;

begin
  Proc;
end.
