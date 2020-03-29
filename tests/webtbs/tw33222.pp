var
  hl : record
    hashnext : pqword;
  end;
  fs : dword;
  sr : record
    slot : dword;
  end;

procedure p;
begin
  hl.hashnext[sr.slot] := (qword(fs) shl 32) or dword(hl.hashnext[sr.slot]);
end;

begin
  fs:=$12341234;
  sr.slot:=0;
  getmem(hl.hashnext,sizeof(qword));
  hl.hashnext[sr.slot]:=$1eadbeef00000000;
  p;
  if hl.hashnext[sr.slot]<>$1234123400000000 then
    halt(1);
  freemem(hl.hashnext);
  writeln('ok');
end.
