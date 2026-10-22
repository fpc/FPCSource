{ %target=linux }
{ Linux.clone: argument passing, exit status, shared and separate memory, error results }
program tclone1;

{$mode objfpc}

uses
  BaseUnix, Linux;

const
  cStackSize = 256*1024;

var
  gErrors: Integer = 0;
  gShared: LongInt = 0;


procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;


function ChildFunc(aArgs: pointer): longint; cdecl;

begin
  gShared:=PLongInt(aArgs)^;
  Result:=PLongInt(aArgs)^ div 1000;
end;


// Clones ChildFunc with aValue as argument and returns the child's exit status, or -1.
function RunChild(aFlags: longint; aValue: LongInt): LongInt;

var
  lStack: pointer;
  lPid: TPid;
  lStatus: cint;

begin
  Result:=-1;
  GetMem(lStack,cStackSize);
  try
    lPid:=clone(@ChildFunc,PByte(lStack)+cStackSize,aFlags,@aValue);
    if lPid<=0 then
      Exit;
    if fpWaitPid(lPid,@lStatus,0)<>lPid then
      Exit;
    if wifexited(lStatus) then
      Result:=wexitstatus(lStatus);
  finally
    FreeMem(lStack);
  end;
end;


var
  lStack: array[0..255] of Byte;
  lStackTop: pointer;

begin
  lStackTop:=PByte(@lStack)+SizeOf(lStack);
  gShared:=0;
  CheckTrue('CLONE_VM exit status',RunChild(CLONE_VM or SIGCHLD,42000)=42);
  CheckTrue('CLONE_VM child writes shared memory',gShared=42000);
  gShared:=0;
  CheckTrue('Separate memory exit status',RunChild(SIGCHLD,17000)=17);
  CheckTrue('Separate memory not changed by child',gShared=0);
  CheckTrue('Nil function returns -1',clone(nil,lStackTop,SIGCHLD,nil)=-1);
  CheckTrue('Nil function sets EINVAL',fpgeterrno=ESysEINVAL);
  CheckTrue('Nil stack returns -1',clone(@ChildFunc,nil,SIGCHLD,nil)=-1);
  CheckTrue('CLONE_SIGHAND without CLONE_VM returns -1',clone(@ChildFunc,lStackTop,CLONE_SIGHAND or SIGCHLD,nil)=-1);
  CheckTrue('CLONE_SIGHAND without CLONE_VM sets EINVAL',fpgeterrno=ESysEINVAL);
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
