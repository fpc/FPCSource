{ %cpu=powerpc }
{ %target=darwin,macos}

type
  tr1 = record
    b: byte;
  end;

  tr2 = record
    l: longint;
  end;

  tr3 = record
    i: int64;
  end;

  tr4 = record
    s: string;
  end;

var
  r1: tr1;
  r2: tr2;
  r3: tr3;
  r4: tr4;

procedure p1normal(const r: tr1);
begin
  if @r = @r1 then
    halt(1);
end;


procedure p2normal(const r: tr2);
begin
  if @r = @r2 then
    halt(1);
end;


procedure p3normal(const r: tr3);
begin
  if @r = @r3 then
    halt(1);
end;


procedure p4normal(const r: tr4);
begin
  if @r <> @r4 then
    halt(1);
end;

procedure p1normal2(r: tr1);
begin
  if @r = @r1 then
    halt(1);
end;


procedure p2normal2(r: tr2);
begin
  if @r = @r2 then
    halt(1);
end;


procedure p3normal2(r: tr3);
begin
  if @r = @r3 then
    halt(1);
end;


procedure p4normal2(r: tr4);
begin
  if @r = @r4 then
    halt(1);
end;


procedure p1mw(const r: tr1); mwpascal;
begin
  if @r <> @r1 then
    halt(1);
end;


procedure p2mw(const r: tr2);mwpascal;
begin
  if @r <> @r2 then
    halt(1);
end;


procedure p3mw(const r: tr3);mwpascal;
begin
  if @r <> @r3 then
    halt(1);
end;


procedure p4mw(const r: tr4);mwpascal;
begin
  if @r <> @r4 then
    halt(1);
end;


procedure p1mw2(r: tr1); mwpascal;
begin
  if @r = @r1 then
    halt(1);
end;


procedure p2mw2(r: tr2);mwpascal;
begin
  if @r = @r2 then
    halt(1);
end;


procedure p3mw2(r: tr3);mwpascal;
begin
  if @r = @r3 then
    halt(1);
end;


procedure p4mw2(r: tr4);mwpascal;
begin
  if @r = @r4 then
    halt(1);
end;


begin
  p1normal(r1);
  p2normal(r2);
  p3normal(r3);
  p4normal(r4);

  p1normal2(r1);
  p2normal2(r2);
  p3normal2(r3);
  p4normal2(r4);

  p1mw(r1);
  p2mw(r2);
  p3mw(r3);
  p4mw(r4);

  p1mw2(r1);
  p2mw2(r2);
  p3mw2(r3);
  p4mw2(r4);
end.
