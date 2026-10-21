{ %target=win32,win64 }

{ a Widestring variable does not hold its allocation's address while the
  memory manager frees or reallocates it: code a hook of the memory manager
  runs sees the variable holding the string it held while that is sized and
  reallocated, and empty while its string is freed -- and what such a hook
  assigns to it then is kept -- as for an AnsiString. The reallocation hook
  moves the string to a new block, which the variable must then hold, with
  the characters it had and the terminator after the new length }
program tb0752;

{$warn symbol_deprecated off}

var
  W: WideString;
  OldMM, HookMM: TMemoryManager;
  { 1 while the string is sized and reallocated, 2 while it is freed }
  Phase: longint = 0;
  { the first check that failed }
  Seen: longint = 0;
  Held: Pointer;
  Reallocated: boolean = false;

procedure Failed(code: longint);
begin
  if Seen=0 then
    Seen:=code;
end;

function HookFreeMem(p: Pointer): PtrUInt;
begin
  if Phase=2 then
    begin
      { once: W is empty then, so the assignment frees nothing }
      Phase:=0;
      if Pointer(W)<>nil then
        Failed(1)
      else
        W:='kept';
    end;
  HookFreeMem:=OldMM.FreeMem(p);
end;

function HookMemSize(p: Pointer): PtrUInt;
begin
  if (Phase=1) and (Pointer(W)<>Held) then
    Failed(5);
  HookMemSize:=OldMM.MemSize(p);
end;

function HookReAllocMem(var p: Pointer; Size: PtrUInt): Pointer;
var
  n: Pointer;
  keep: PtrUInt;
begin
  if Phase<>1 then
    begin
      HookReAllocMem:=OldMM.ReAllocMem(p,Size);
      exit;
    end;
  Reallocated:=true;
  if Pointer(W)<>Held then
    Failed(2);
  { a new block every time, so that the string moves }
  n:=OldMM.GetMem(Size);
  keep:=OldMM.MemSize(p);
  if keep>Size then
    keep:=Size;
  Move(p^,n^,keep);
  OldMM.FreeMem(p);
  p:=n;
  HookReAllocMem:=n;
end;

begin
  { the memory manager allocates Widestrings }
  winwidestringalloc:=false;
  W:='abc';
  GetMemoryManager(OldMM);
  HookMM:=OldMM;
  HookMM.FreeMem:=@HookFreeMem;
  HookMM.MemSize:=@HookMemSize;
  HookMM.ReAllocMem:=@HookReAllocMem;
  SetMemoryManager(HookMM);

  { a longer string than the allocation holds: reallocated }
  Held:=Pointer(W);
  Phase:=1;
  SetLength(W,1000);
  Phase:=0;
  if Seen<>0 then
    begin
      SetMemoryManager(OldMM);
      writeln('the variable did not hold its string while it was sized or reallocated');
      halt(Seen);
    end;
  if (Length(W)<>1000) or not Reallocated then
    begin
      SetMemoryManager(OldMM);
      halt(3);
    end;
  if Pointer(W)=Held then
    begin
      SetMemoryManager(OldMM);
      writeln('the variable does not hold the string at its new place');
      halt(6);
    end;
  if (W[1]<>'a') or (W[2]<>'b') or (W[3]<>'c') or
     (PWord(PByte(Pointer(W))+1000*sizeof(WideChar))^<>0) then
    begin
      SetMemoryManager(OldMM);
      writeln('the reallocated string lost its characters or its terminator');
      halt(7);
    end;

  { the string freed }
  Phase:=2;
  W:='';
  Phase:=0;
  SetMemoryManager(OldMM);
  if Seen<>0 then
    begin
      writeln('the variable was not empty while its string was freed');
      halt(Seen);
    end;
  if W<>'kept' then
    begin
      writeln('what a hook assigned while the string was freed was lost');
      halt(4);
    end;
  W:='';
  writeln('ok');
end.
