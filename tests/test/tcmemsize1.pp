{ MemSize, FreeMemSize and ReAllocMem with the cmem memory manager }
program tcmemsize1;

{$mode objfpc}{$H+}

uses
  cmem;

{$if defined(win32) or defined(win64) or defined(linux) or defined(android) or defined(freebsd) or defined(darwin)}
  {$define USABLE_SIZE}
{$endif}

var
  gFailed: Boolean = False;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


// Checks MemSize of aP, allocated with aSize bytes
procedure CheckSize(aP: Pointer; aSize: PtrUInt; const aWhat: string);

var
  lSize: PtrUInt;

begin
  lSize:=MemSize(aP);
{$ifdef USABLE_SIZE}
  Check(lSize>=aSize,aWhat+': MemSize '+HexStr(lSize,8)+' is at least the requested size');
{$else}
  Check(lSize=aSize,aWhat+': MemSize '+HexStr(lSize,8)+' is the requested size');
{$endif}
end;


var
  lP: PByte;
  lS: AnsiString;
  lMM: TMemoryManager;
  i: Integer;
  lOk: Boolean;

begin
  GetMemoryManager(lMM);
  GetMem(lP,100);
  CheckSize(lP,100,'GetMem(100)');
  Check(lMM.MemSize(lP)=MemSize(lP),'the memory manager MemSize is the cmem one');
  for i:=0 to 99 do
    lP[i]:=i;
  ReAllocMem(lP,1000);
  CheckSize(lP,1000,'ReAllocMem(1000)');
  lOk:=True;
  for i:=0 to 99 do
    lOk:=lOk and (lP[i]=i);
  Check(lOk,'growing ReAllocMem keeps the contents');
  ReAllocMem(lP,10);
  CheckSize(lP,10,'ReAllocMem(10)');
  lOk:=True;
  for i:=0 to 9 do
    lOk:=lOk and (lP[i]=i);
  Check(lOk,'shrinking ReAllocMem keeps the contents');
  FreeMem(lP,10);

  lP:=AllocMem(64);
  CheckSize(lP,64,'AllocMem(64)');
  lOk:=True;
  for i:=0 to 63 do
    lOk:=lOk and (lP[i]=0);
  Check(lOk,'AllocMem clears the block');
  FreeMem(lP,MemSize(lP));

  GetMem(lP,1);
  ReAllocMem(lP,0);
  Check(lP=nil,'ReAllocMem(0) frees the block');
  ReAllocMem(lP,32);
  CheckSize(lP,32,'ReAllocMem of nil');
  FreeMem(lP);

  lS:='';
  for i:=1 to 10000 do
    lS:=lS+Chr(Ord('a')+i mod 26);
  lOk:=Length(lS)=10000;
  for i:=1 to Length(lS) do
    lOk:=lOk and (lS[i]=Chr(Ord('a')+i mod 26));
  Check(lOk,'a string grown one character at a time is intact');

  if gFailed then
    Halt(1);
  Writeln('ok');
end.
