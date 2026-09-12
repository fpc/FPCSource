{ Minimal PS1 file/text hooks. There is no filesystem here.
  Stdout/stderr are sent to the PCSX-Redux debug port. }

procedure do_close(handle: THandle); inline;
begin
end;

procedure do_erase(p: PAnsiChar; pchangeable: Boolean); inline;
begin
end;

procedure do_rename(p1, p2: PAnsiChar; p1changeable, p2changeable: Boolean); inline;
begin
end;

function do_write(h: THandle; addr: Pointer; len: LongInt): LongInt;
var
  i: LongInt;
begin
  if (h = StdOutputHandle) or (h = StdErrorHandle) then
    for i := 0 to len - 1 do
      _putchar(PChar(addr)[i]);
  Result := len;
end;

function do_read(h: THandle; addr: Pointer; len: LongInt): LongInt;
begin
  Result := 0;
end;

function do_filepos(handle: THandle): LongInt; inline;
begin
  Result := 0;
end;

procedure do_seek(handle: THandle; pos: LongInt); inline;
begin
end;

function do_seekend(handle: THandle): LongInt; inline;
begin
  Result := 0;
end;

function do_filesize(handle: THandle): LongInt; inline;
begin
  Result := 0;
end;

procedure do_truncate(handle: THandle; pos: LongInt); inline;
begin
end;

procedure do_open(var f; p: PAnsiChar; flags: LongInt; pchangeable: Boolean); inline;
begin
end;
