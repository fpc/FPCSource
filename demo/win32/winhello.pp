{
  $Id: winhello.pp,v 1.4 2002/09/07 15:06:35 peter Exp $
  Copyright (c) 1996 by Charlie Calvert
  Modifications by Florian Klaempfl

  Standard Windows API application written in Object Pascal.
  No VCL code included. This is all done on the Windows API
  level.
}

{$APPTYPE GUI}
{$MODE DELPHI}
program WinHello;

uses
  Strings, Windows;

const
  AppName = 'WinHello';

function WindowProc(Window: HWnd; AMessage: UINT; WParam : WPARAM;
                    LParam: LPARAM): LRESULT; stdcall; export;

  var
     dc : hdc;
     ps : paintstruct;
     r : rect;

begin
  WindowProc := 0;

  case AMessage of
    wm_paint:
      begin
         dc:=BeginPaint(Window,@ps);
         GetClientRect(Window,@r);
         DrawText(dc,'Hello world by Free Pascal',-1,@r,
           DT_SINGLELINE or DT_CENTER or DT_VCENTER);
         EndPaint(Window,ps);
         Exit;
      end;
    wm_Destroy:
      begin
         PostQuitMessage(0);
         Exit;
      end;
  end;

  WindowProc := DefWindowProc(Window, AMessage, WParam, LParam);
end;

 { Register the Window Class }
function WinRegister: Boolean;
var
  WindowClass: WndClass;
begin
  WindowClass.Style := cs_hRedraw or cs_vRedraw;
  WindowClass.lpfnWndProc := WndProc(@WindowProc);
  WindowClass.cbClsExtra := 0;
  WindowClass.cbWndExtra := 0;
  WindowClass.hInstance := system.MainInstance;
  WindowClass.hIcon := LoadIcon(0, idi_Application);
  WindowClass.hCursor := LoadCursor(0, idc_Arrow);
  WindowClass.hbrBackground := GetStockObject(WHITE_BRUSH);
  WindowClass.lpszMenuName := nil;
  WindowClass.lpszClassName := AppName;

  Result := RegisterClass(WindowClass) <> 0;
end;

 { Create the Window Class }
function WinCreate: HWnd;
var
  hWindow: HWnd;
begin
  hWindow := CreateWindow(AppName, 'Hello world program',
              ws_OverlappedWindow, cw_UseDefault, cw_UseDefault,
              cw_UseDefault, cw_UseDefault, 0, 0, system.MainInstance, nil);

  if hWindow <> 0 then begin
    ShowWindow(hWindow, CmdShow);
    ShowWindow(hWindow, SW_SHOW);
    UpdateWindow(hWindow);
  end;

  Result := hWindow;
end;


var
  AMessage: Msg;
  hWindow: HWnd;

begin
  if not WinRegister then begin
    MessageBox(0, 'Register failed', nil, mb_Ok);
    Exit;
  end;
  hWindow := WinCreate;
  if longint(hWindow) = 0 then begin
    MessageBox(0, 'WinCreate failed', nil, mb_Ok);
    Exit;
  end;

  while GetMessage(@AMessage, 0, 0, 0) do begin
    TranslateMessage(AMessage);
    DispatchMessage(AMessage);
  end;
  Halt(AMessage.wParam);
end.

{
  $Log: winhello.pp,v $
  Revision 1.4  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/02/22 13:37:49  pierre
   * fix problem if started through cygwin bash

}
