{ %target=win32,win64 }
{ %result=204 }

{ as tb0761, for an empty Widestring: making it High(SizeInt)-2 characters
  long is run time error 204 before any size is computed -- with the
  terminator's room that size wraps to 0, so the memory manager would be
  asked for a block of no characters. A memory manager stops the test when it
  is asked for a block: 3, 1 when SetLength returned }
program tb0762;

{$mode objfpc}
{$warn symbol_deprecated off}

var
  W: WideString;
  OldMM, GuardMM: TMemoryManager;
  Armed: boolean = false;
  OldExit: CodePointer;

procedure Disarm;
begin
  { the exit of the run time error: what it allocates is not SetLength's }
  Armed:=false;
  ExitProc:=OldExit;
end;

function GuardGetMem(size: PtrUInt): pointer;
begin
  if Armed then
    begin
      Armed:=false;
      SetMemoryManager(OldMM);
      writeln('a block was asked for');
      halt(3);
    end;
  GuardGetMem:=OldMM.GetMem(size);
end;

begin
  { the memory manager allocates Widestrings }
  winwidestringalloc:=false;
  W:='';
  GetMemoryManager(OldMM);
  GuardMM:=OldMM;
  GuardMM.GetMem:=@GuardGetMem;
  SetMemoryManager(GuardMM);
  OldExit:=ExitProc;
  ExitProc:=@Disarm;
  Armed:=true;
  SetLength(W,High(SizeInt)-2);
  Armed:=false;
  SetMemoryManager(OldMM);
  writeln('SetLength returned');
  halt(1);
end.
