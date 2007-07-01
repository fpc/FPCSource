var
  wstr1: widestring;
  i: longint;
  w2,w3: widestring;

procedure testproc2(w: widestring);
begin
  wstr1:=w;
end;

procedure testproc1(const w: widestring);
begin
  w2:='';
  testproc2(w);
  if pointer(w)<>pointer(wstr1) then begin
    writeln('Test failed!');
    Halt(1);
  end;
  if w<>w3 then begin
    writeln('Test failed!');
    Halt(1);
  end;
end;

begin
  setlength(w2, 100000);
  for i:=1 to length(w2) do
    w2[i]:=WideChar(Chr(i mod $60 + $20));
  w3:=w2;
  wstr1:=w2;
  testproc1(wstr1);
end.
