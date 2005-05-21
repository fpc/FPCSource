program inline01;

var
  starti: longint;
  i:longint;


{$INLINE ON}

procedure kkainl(var c: longint); inline;
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  writeln('kka ',c);
  c:=c+1;
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
end;

procedure kka(var c:longint);
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  writeln('kka ',c);
  c:=c+1;
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
end;

procedure kkb(var c:longint);inline;
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  kka(c);
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
  writeln('kkb ',c);
end;

procedure kkb2(var c:longint);inline;
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  kkainl(c);
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
  writeln('kkb ',c);
end;

procedure kkc(var c: longint);
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  kkb(c);
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
end;

procedure kkcinl(var c: longint); inline;
begin
  if c <> starti then
    begin
      writeln('bug');
      halt(1);
    end;
  kkb2(c);
  if i <> starti+1 then
    begin
      writeln('bug');
      halt(1);
    end;
end;

begin
  i:=5;
  starti := 5;
  kkc(i);
  starti := i;
  kkc(i);
  starti := i;
  kkb(i);
  starti := i;
  kkb(i);
  starti := i;
  kka(i);
  starti := i;
  kkcinl(i);
  starti := i;
  kkb2(i);
end.
