{ %TARGET=linux,win32,win64 }
{ A thread created by the OS can use threadvars, the heap and exceptions after InitMultiThreading, issue #41793 }
program tinitmtforeign;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
{$ifdef windows}
  Windows,
{$endif}
  SysUtils;

{$ifdef unix}
function pthread_create(aThread: PPtrUInt; aAttr: Pointer; aStart: Pointer; aArg: Pointer): LongInt; cdecl; external 'c';
function pthread_join(aThread: PtrUInt; aResult: PPointer): LongInt; cdecl; external 'c';
{$endif}

threadvar
  gValue: LongInt;

var
  gResult: LongInt;


// Uses a threadvar, a managed string and an exception; returns 0 or an error code
function ForeignWork: LongInt;

var
  lText: string;

begin
  gValue:=11;
  lText:=IntToStr(gValue)+'-foreign';
  if lText<>'11-foreign' then
    exit(1);
  try
    raise Exception.Create(lText);
  except
    on E: Exception do
      if E.Message<>'11-foreign' then
        exit(2);
  end;
  if gValue<>11 then
    exit(3);
  Result:=0;
end;


{$ifdef unix}
// Thread entry point for pthread_create
function ForeignMain(aArg: Pointer): Pointer; cdecl;

begin
  gResult:=ForeignWork;
  Result:=nil;
end;
{$endif}

{$ifdef windows}
// Thread entry point for CreateThread
function ForeignMain(aArg: Pointer): DWORD; stdcall;

begin
  gResult:=ForeignWork;
  Result:=0;
end;
{$endif}


var
{$ifdef unix}
  lThread: PtrUInt;
{$endif}
{$ifdef windows}
  lThread: THandle;
  lTid: DWORD;
{$endif}

begin
  gValue:=5;
  gResult:=-1;
  InitMultiThreading;
{$ifdef unix}
  if pthread_create(@lThread,nil,@ForeignMain,nil)<>0 then
    halt(1);
  pthread_join(lThread,nil);
{$endif}
{$ifdef windows}
  lThread:=CreateThread(nil,0,@ForeignMain,nil,0,lTid);
  if lThread=0 then
    halt(1);
  WaitForSingleObject(lThread,INFINITE);
  CloseHandle(lThread);
{$endif}
  if gResult<>0 then
    begin
    writeln('foreign thread failed with code ',gResult);
    halt(2);
    end;
  if gValue<>5 then
    begin
    writeln('main thread threadvar changed to ',gValue,', expected 5');
    halt(3);
    end;
  writeln('ok');
end.
