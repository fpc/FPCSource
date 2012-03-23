
{ This test requires overflow and range check to be off }
{$R-}
{$Q-}

var
  error: boolean;

procedure test;
var
  b: byte;
  l: longint;
begin
  b := 254;
  inc(b,2);
  l := b;
  if (l <> 0) then
    begin
      writeln('overflow error with byte');
      error := true;
    end;

  b :=1;
  dec(b,2);
  l := b;
  if (l <> 255) then
    begin
      writeln('underflow error with byte');
      error := true;
    end;
end;


procedure test2;
var
  b: shortint;
  l: longint;
begin
  b := -127;
  dec(b,2);
  l := b;
  if (l <> 127) then
    begin
      writeln('neg error with shortint');
      error := true;
    end;

  b := 126;
  inc(b,2);
  l := b;
  if (l <> -128) then
    begin
      writeln('pos error with shortint');
      error := true;
    end;
end;


procedure test3;
var
  b: longint;
  l: int64;
begin
  b := low(longint)+1;
  dec(b,2);
  l := b;
  if (l <> high(longint)) then
    begin
      writeln('neg error with shortint');
      error := true;
    end;

  b := high(longint)-1;
  inc(b,2);
  l := b;
  if (l <> low(longint)) then
    begin
      writeln('pos error with shortint');
      error := true;
    end;
end;

begin
  error := false;
  test;
  test2;
  test3;
  halt(ord(error));
end.

