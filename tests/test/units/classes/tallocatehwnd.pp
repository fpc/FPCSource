{ %target=win32,win64 }
program tallocatehwnd;

{$mode objfpc}{$H+}
{$apptype console}

uses
  windows, messages, sysutils, classes;

const
  InstanceCount = 500;
  WM_TEST_SEND = WM_USER+1;
  WM_TEST_POST = WM_USER+2;

type
  TReceiver = class
    FId : Integer;
    FLastMsg : UINT;
    FLastWParam : WPARAM;
    FLastLParam : LPARAM;
    FPosted : Boolean;
    FDestroyed : Boolean;
    FText : string;
    procedure WndProc(var aMsg: TMessage);
  end;


procedure Fail(aCode: Integer; const aMsg: string);

begin
  Writeln('Failed: ',aMsg);
  Halt(aCode);
end;


procedure TReceiver.WndProc(var aMsg: TMessage);

begin
  case aMsg.Msg of
    WM_TEST_SEND:
      begin
        FLastMsg:=aMsg.Msg;
        FLastWParam:=aMsg.WParam;
        FLastLParam:=aMsg.LParam;
        FText:=Format('%d:%.3f',[FId,FId/4]);
        aMsg.Result:=LRESULT(aMsg.WParam)+aMsg.LParam+FId;
      end;
    WM_TEST_POST:
      begin
        FPosted:=True;
        aMsg.Result:=0;
      end;
    WM_DESTROY:
      FDestroyed:=True;
  else
    aMsg.Result:=DefWindowProcW(0,aMsg.Msg,aMsg.WParam,aMsg.LParam);
  end;
end;


procedure TestInstances;

var
  lReceivers : array[0..InstanceCount-1] of TReceiver;
  lProcs : array[0..InstanceCount-1] of Pointer;
  i : Integer;
  lRes : LRESULT;
  lWParam : WPARAM;
  lLParam : LPARAM;
begin
  for i:=0 to InstanceCount-1 do
    begin
      lReceivers[i]:=TReceiver.Create;
      lReceivers[i].FId:=i;
      lProcs[i]:=MakeObjectInstance(@lReceivers[i].WndProc);
      if lProcs[i]=nil then
        Fail(1,'MakeObjectInstance returned nil');
    end;
{$ifdef CPU64}
  lWParam:=WPARAM($123456789A);
  lLParam:=LPARAM($200000000);
{$else}
  lWParam:=$12345;
  lLParam:=$20000;
{$endif}
  for i:=0 to InstanceCount-1 do
    begin
      lRes:=CallWindowProcW(WNDPROC(lProcs[i]),0,WM_TEST_SEND,lWParam,lLParam);
      if lRes<>LRESULT(lWParam)+lLParam+i then
        Fail(2,'Wrong result for instance '+IntToStr(i));
      if (lReceivers[i].FLastMsg<>WM_TEST_SEND) or (lReceivers[i].FLastWParam<>lWParam) or (lReceivers[i].FLastLParam<>lLParam) then
        Fail(3,'Wrong message received by instance '+IntToStr(i));
      if lReceivers[i].FText<>Format('%d:%.3f',[i,i/4]) then
        Fail(4,'Wrong text in instance '+IntToStr(i));
    end;
  for i:=0 to InstanceCount-1 do
    FreeObjectInstance(lProcs[i]);
  FreeObjectInstance(nil);
  lProcs[0]:=MakeObjectInstance(@lReceivers[7].WndProc);
  if CallWindowProcW(WNDPROC(lProcs[0]),0,WM_TEST_SEND,1,2)<>1+2+7 then
    Fail(5,'Reused instance calls wrong method');
  FreeObjectInstance(lProcs[0]);
  for i:=0 to InstanceCount-1 do
    lReceivers[i].Free;
end;


procedure TestAllocateHWnd;

var
  lReceiver : TReceiver;
  lWnd : HWND;
  lMsg : TMsg;
  lTries : Integer;
begin
  lReceiver:=TReceiver.Create;
  try
    lReceiver.FId:=42;
    lWnd:=AllocateHWnd(@lReceiver.WndProc);
    if lWnd=0 then
      Fail(10,'AllocateHWnd returned 0');
    if not IsWindow(lWnd) then
      Fail(11,'AllocateHWnd did not return a window');
    if SendMessageW(lWnd,WM_TEST_SEND,3,4)<>3+4+42 then
      Fail(12,'SendMessage to allocated window returned wrong result');
    if not PostMessageW(lWnd,WM_TEST_POST,0,0) then
      Fail(13,'PostMessage failed');
    lTries:=0;
    while not lReceiver.FPosted and (lTries<1000) do
      begin
        while PeekMessageW(@lMsg,0,0,0,PM_REMOVE) do
          begin
            TranslateMessage(@lMsg);
            DispatchMessageW(@lMsg);
          end;
        if not lReceiver.FPosted then
          Sleep(1);
        Inc(lTries);
      end;
    if not lReceiver.FPosted then
      Fail(14,'Posted message not received through the message loop');
    DeallocateHWnd(lWnd);
    if not lReceiver.FDestroyed then
      Fail(15,'WM_DESTROY not received');
    if IsWindow(lWnd) then
      Fail(16,'DeallocateHWnd did not destroy the window');
    lWnd:=AllocateHWnd(@lReceiver.WndProc);
    if SendMessageW(lWnd,WM_TEST_SEND,1,1)<>1+1+42 then
      Fail(17,'Second allocated window returned wrong result');
    DeallocateHWnd(lWnd);
  finally
    lReceiver.Free;
  end;
  lWnd:=AllocateHWnd(nil);
  if lWnd=0 then
    Fail(18,'AllocateHWnd without method returned 0');
  if SendMessageW(lWnd,WM_TEST_SEND,1,1)<>0 then
    Fail(19,'Window without method did not use DefWindowProc');
  DeallocateHWnd(lWnd);
  if IsWindow(lWnd) then
    Fail(20,'DeallocateHWnd did not destroy the window without method');
end;


begin
  TestInstances;
  TestAllocateHWnd;
  Writeln('ok');
end.
