type
  TRec = record
    bbb: array[1..8] of byte;
    w: word;
  end;

  TRec2 = packed record
    a: array[1..9] of char;
  end;

procedure dotest(p: TRec);
var
  i: longint;
begin
  for i:=1 to 8 do
    write(p.bbb[i], ' ');
  writeln;
  if qword(p.bbb)<>$0102030405060708 then begin
    writeln('Test FAILED.');
    Halt(1);
  end;
end;

procedure dotest2(p: TRec);
var
  rr: TRec2;
  pp: TRec;
begin
  pp:=p;
  dotest(pp);
end;

var
  b: byte;
  p: TRec;
  i: longint;

begin
  qword(p.bbb):=$0102030405060708;
  dotest2(p);
  writeln('Test OK.');
end.