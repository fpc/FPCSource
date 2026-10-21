{ %target=win32,win64 }

{ a Widestring the memory manager allocates (winwidestringalloc false) has
  room for its terminating #0, as an AnsiString or a UnicodeString has: a new
  one, and one made longer }
program tb0753;

{$warn symbol_deprecated off}

var
  W: WideString;
  n: longint;

procedure Check(const how: shortstring);
begin
  if MemSize(Pointer(W)-sizeof(DWord))<sizeof(DWord)+(n+1)*sizeof(WideChar) then
    begin
      writeln(how,' Widestring of ',n,' characters has no room for its terminating #0');
      halt(1);
    end;
end;

begin
  winwidestringalloc:=false;
  for n:=1 to 300 do
    begin
      W:='';
      SetLength(W,n);
      Check('a new');
    end;
  W:='';
  for n:=1 to 300 do
    begin
      SetLength(W,n);
      Check('a longer');
    end;
  W:='';
  writeln('ok');
end.
