{ %TARGET=win32,win64 }
{ SwitchToThread and YieldProcessor in a spin-wait loop, issue #41780 }
program tyieldprocessor;

{$mode objfpc}{$h+}

uses
  Windows;

var
  gFlag: LongInt = 0;


// Sets gFlag after a short delay
function SetFlag(aParam: Pointer): DWORD; stdcall;

begin
  Sleep(50);
  InterlockedExchange(gFlag,1);
  Result:=0;
end;


var
  lThread: THandle;
  lId: DWORD;
  lStart: DWORD;
  lSpins: LongInt;

begin
  lSpins:=0;
  YieldProcessor;
  SwitchToThread;
  lThread:=CreateThread(nil,0,@SetFlag,nil,0,lId);
  if lThread=0 then
    halt(1);
  lStart:=GetTickCount;
  while InterlockedCompareExchange(gFlag,0,0)=0 do
    begin
    YieldProcessor;
    Inc(lSpins);
    if (lSpins and $FFF)=0 then
      SwitchToThread;
    if GetTickCount-lStart>10000 then
      halt(2);
    end;
  WaitForSingleObject(lThread,INFINITE);
  CloseHandle(lThread);
  if lSpins=0 then
    halt(3);
  writeln('ok');
end.
