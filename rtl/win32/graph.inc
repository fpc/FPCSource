{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by Florian Klaempfl

    This file implements the win32 gui support for the graph unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
   Remarks:
      Colors in 16 color mode:
      ------------------------
        - the behavior of xor/or/and put isn't 100%:
          it is done using the RGB color getting from windows
          instead of the palette index!
        - palette operations aren't supported
      To solve these drawbacks, setpalette must be implemented
      by exchanging the colors in the DCs, further GetPaletteEntry
      must be used when doing xor/or/and operations
}

const
   InternalDriverName = 'WIN32GUI';

var
   savedscreen : hbitmap;
   graphrunning : boolean;
   graphdrawing : tcriticalsection;
   bitmapdc : hdc;
   oldbitmap : hgdiobj;
   pal : ^rgbrec;
   SavePtr : pointer; { we don't use that pointer }
   MessageThreadHandle : Handle;
   MessageThreadID : DWord;
   windc : hdc;

function GetPaletteEntry(r,g,b : word) : word;

  var
     dist,i,index,currentdist : longint;

  begin
     dist:=$7fffffff;
     index:=0;
     for i:=0 to maxcolors-1 do
       begin
          currentdist:=abs(r-pal[i].red)+abs(g-pal[i].green)+
            abs(b-pal[i].blue);
          if currentdist<dist then
            begin
               index:=i;
               dist:=currentdist;
            end;
       end;
     GetPaletteEntry:=index;
  end;

procedure PutPixel16Win32GUI(x,y : integer;pixel : word);

  var
     c : colorref;

  begin
    x:=x+startxviewport;
    y:=y+startyviewport;
    { convert to absolute coordinates and then verify clipping...}
    if clippixels then
      begin
         if (x<startxviewport) or (x>(startxviewport+viewwidth)) or
           (y<StartyViewPort) or (y>(startyviewport+viewheight)) then
           exit;
      end;
    if graphrunning then
      begin
         EnterCriticalSection(graphdrawing);
         c:=RGB(pal[pixel].red,pal[pixel].green,pal[pixel].blue);
         SetPixel(bitmapdc,x,y,c);
         SetPixel(windc,x,y,c);
         LeaveCriticalSection(graphdrawing);
      end;
  end;

function GetPixel16Win32GUI(x,y : integer) : word;

  var
     c : COLORREF;

  begin
    x:=x+startxviewport;
    y:=y+startyviewport;
    { convert to absolute coordinates and then verify clipping...}
    if clippixels then
      begin
         if (x<startxviewport) or (x>(startxviewport+viewwidth)) or
           (y<StartyViewPort) or (y>(startyviewport+viewheight)) then
           exit;
      end;
    if graphrunning then
      begin
         EnterCriticalSection(graphdrawing);
         c:=Windows.GetPixel(bitmapdc,x,y);
         GetPixel16Win32GUI:=GetPaletteEntry(GetRValue(c),GetGValue(c),GetBValue(c));
         LeaveCriticalSection(graphdrawing);
      end
    else
      begin
        _graphresult:=grerror;
        exit;
      end;
  end;

procedure DirectPutPixel16Win32GUI(x,y : integer);

  var
     col : longint;
     c,c2 : COLORREF;

  begin
    if graphrunning then
      begin
         EnterCriticalSection(graphdrawing);
         case currentwritemode of
           XorPut:
             Begin
                c2:=Windows.GetPixel(bitmapdc,x,y);
                c:=RGB(pal[col].red,pal[col].green,pal[col].blue) xor c2;
                SetPixel(bitmapdc,x,y,c);
                SetPixel(windc,x,y,c);
             End;
           AndPut:
             Begin
                c2:=Windows.GetPixel(bitmapdc,x,y);
                c:=RGB(pal[col].red,pal[col].green,pal[col].blue) and c2;
                SetPixel(bitmapdc,x,y,c);
                SetPixel(windc,x,y,c);
             End;
           OrPut:
             Begin
                c2:=Windows.GetPixel(bitmapdc,x,y);
                c:=RGB(pal[col].red,pal[col].green,pal[col].blue) or c2;
                SetPixel(bitmapdc,x,y,c);
                SetPixel(windc,x,y,c);
             End
           else
             Begin
               If CurrentWriteMode<>NotPut Then
                 col:=CurrentColor
               Else col := Not(CurrentColor);
               c:=RGB(pal[col].red,pal[col].green,pal[col].blue);
               SetPixel(bitmapdc,x,y,c);
               SetPixel(windc,x,y,c);
             End
         end;
         LeaveCriticalSection(graphdrawing);
      end;
  end;

procedure HLine16Win32GUI(x,x2,y: integer);

   var
      c,c2 : COLORREF;
      col,i : longint;
      oldpen,pen : HPEN;

   Begin
      if graphrunning then
        begin
           { must we swap the values? }
           if x>x2 then
             Begin
               x:=x xor x2;
               x2:=x xor x2;
               x:=x xor x2;
             end;
           { First convert to global coordinates }
           X:=X+StartXViewPort;
           X2:=X2+StartXViewPort;
           Y:=Y+StartYViewPort;
           if ClipPixels then
             Begin
                if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                       StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
                   exit;
             end;
           Case CurrentWriteMode of
             AndPut:
               Begin
                  EnterCriticalSection(graphdrawing);
                  for i:=x to x2 do
                    begin
                       c2:=Windows.GetPixel(bitmapdc,i,y);
                       c:=RGB(pal[col].red,pal[col].green,pal[col].blue) and c2;
                       SetPixel(bitmapdc,i,y,c);
                       SetPixel(windc,i,y,c);
                    end;
                  LeaveCriticalSection(graphdrawing);
               End;
             XorPut:
               Begin
                  EnterCriticalSection(graphdrawing);
                  for i:=x to x2 do
                    begin
                       c2:=Windows.GetPixel(bitmapdc,i,y);
                       c:=RGB(pal[col].red,pal[col].green,pal[col].blue) xor c2;
                       SetPixel(bitmapdc,i,y,c);
                       SetPixel(windc,i,y,c);
                    end;
                  LeaveCriticalSection(graphdrawing);
               End;
             OrPut:
               Begin
                  EnterCriticalSection(graphdrawing);
                  for i:=x to x2 do
                    begin
                       c2:=Windows.GetPixel(bitmapdc,i,y);
                       c:=RGB(pal[col].red,pal[col].green,pal[col].blue) or c2;
                       SetPixel(bitmapdc,i,y,c);
                       SetPixel(windc,i,y,c);
                    end;
                  LeaveCriticalSection(graphdrawing);
               End
             Else
               Begin
                  If CurrentWriteMode<>NotPut Then
                    col:=CurrentColor
                  Else col:=Not(CurrentColor);
                  EnterCriticalSection(graphdrawing);
                  c:=RGB(pal[col].red,pal[col].green,pal[col].blue);
                  pen:=CreatePen(PS_SOLID,1,c);
                  oldpen:=SelectObject(bitmapdc,pen);
                  Windows.MoveToEx(bitmapdc,x,y,nil);
                  Windows.LineTo(bitmapdc,x2,y);
                  SelectObject(bitmapdc,oldpen);

                  oldpen:=SelectObject(windc,pen);
                  Windows.MoveToEx(windc,x,y,nil);
                  Windows.LineTo(windc,x2,y);
                  SelectObject(windc,oldpen);

                  DeleteObject(pen);
                  LeaveCriticalSection(graphdrawing);
               End;
           End;
        end;
   end;

procedure SetRGBPaletteWin32GUI(colorNum,redValue,greenvalue,
      bluevalue : integer);

  begin
     if directcolor or (colornum<0) or (colornum>=maxcolor) then
       begin
         _graphresult:=grerror;
         exit;
       end;
     pal[colorNum].red:=redValue;
     pal[colorNum].green:=greenValue;
     pal[colorNum].blue:=blueValue;
  end;

procedure GetRGBPaletteWin32GUI(colorNum : integer;
      var redValue,greenvalue,bluevalue : integer);

  begin
     if directcolor or (colornum<0) or (colornum>=maxcolor) then
       begin
         _graphresult:=grerror;
         exit;
       end;
     redValue:=pal[colorNum].red;
     greenValue:=pal[colorNum].green;
     blueValue:=pal[colorNum].blue;
  end;

procedure savestate;

  begin
  end;


procedure restorestate;

  begin
  end;

function WindowProc(Window: HWnd; AMessage, WParam,
                    LParam: Longint): Longint; stdcall; export;

  var
     dc : hdc;
     ps : paintstruct;
     r : rect;

begin
  WindowProc := 0;

  case AMessage of
    wm_lbuttondown,
    wm_rbuttondown,
    wm_mbuttondown,
    wm_lbuttonup,
    wm_rbuttonup,
    wm_mbuttonup,
    wm_lbuttondblclk,
    wm_rbuttondblclk,
    wm_mbuttondblclk:
    {
    This leads to problem, i.e. the menu etc doesn't work any longer 
    wm_nclbuttondown,
    wm_ncrbuttondown,
    wm_ncmbuttondown,
    wm_nclbuttonup,
    wm_ncrbuttonup,
    wm_ncmbuttonup,
    wm_nclbuttondblclk,
    wm_ncrbuttondblclk,
    wm_ncmbuttondblclk:
    }
      if assigned(mousemessagehandler) then
        WindowProc:=mousemessagehandler(window,amessage,wparam,lparam);
    wm_keydown,
    wm_keyup,
    wm_char:
      if assigned(charmessagehandler) then
        WindowProc:=charmessagehandler(window,amessage,wparam,lparam);
    wm_paint:
      begin
         EnterCriticalSection(graphdrawing);
         graphrunning:=true;
         dc:=BeginPaint(Window,@ps);
         GetClientRect(Window,@r);

         if graphrunning then
           BitBlt(dc,0,0,maxx+1,maxy+1,bitmapdc,0,0,SRCCOPY);

         EndPaint(Window,ps);
         LeaveCriticalSection(graphdrawing);
         Exit;
      end;
    wm_create:
      begin
         EnterCriticalSection(graphdrawing);
         dc:=GetDC(window);
         bitmapdc:=CreateCompatibleDC(dc);
         savedscreen:=CreateCompatibleBitmap(dc,maxx+1,maxy+1);
         ReleaseDC(window,dc);
         oldbitmap:=SelectObject(bitmapdc,savedscreen);
         windc:=GetDC(window);
         LeaveCriticalSection(graphdrawing);
      end;
    wm_Destroy:
      begin
         EnterCriticalSection(graphdrawing);
         graphrunning:=false;
         ReleaseDC(mainwindow,windc);
         SelectObject(bitmapdc,oldbitmap);
         DeleteObject(savedscreen);
         DeleteDC(bitmapdc);
         LeaveCriticalSection(graphdrawing);
         PostQuitMessage(0);
         Exit;
      end
    else
      WindowProc := DefWindowProc(Window, AMessage, WParam, LParam);
  end;
end;

function WinRegister: Boolean;
var
  WindowClass: WndClass;
begin
  WindowClass.Style := graphwindowstyle;
  WindowClass.lpfnWndProc := WndProc(@WindowProc);
  WindowClass.cbClsExtra := 0;
  WindowClass.cbWndExtra := 0;
  WindowClass.hInstance := system.MainInstance;
  WindowClass.hIcon := LoadIcon(0, idi_Application);
  WindowClass.hCursor := LoadCursor(0, idc_Arrow);
  WindowClass.hbrBackground := GetStockObject(BLACK_BRUSH);
  WindowClass.lpszMenuName := nil;
  WindowClass.lpszClassName := 'MyWindow';

  winregister:=RegisterClass(WindowClass) <> 0;
end;

 { Create the Window Class }
function WinCreate: HWnd;
var
  hWindow: HWnd;
begin

  hWindow := CreateWindow('MyWindow', 'Graph window application',
              ws_OverlappedWindow, 100, 100,
              maxx+20, maxy+40, 0, 0, system.MainInstance, nil);

  if hWindow <> 0 then begin
    ShowWindow(hWindow, SW_SHOW);
    UpdateWindow(hWindow);
  end;

  wincreate:=hWindow;
end;

function MessageHandleThread(p : pointer) : DWord;StdCall;

  var
     AMessage: Msg;

  begin
     if not WinRegister then begin
       MessageBox(0, 'Register failed', nil, mb_Ok);
       Exit;
     end;
     MainWindow := WinCreate;
     if longint(mainwindow) = 0 then begin
       MessageBox(0, 'WinCreate failed', nil, mb_Ok);
       Exit;
     end;
     while GetMessage(@AMessage, 0, 0, 0) do
       begin
          TranslateMessage(AMessage);
          DispatchMessage(AMessage);
       end;
     MessageHandleThread:=0;
  end;

procedure InitWin32GUI640x480x16;

  begin
     getmem(pal,sizeof(RGBrec)*maxcolor);
     move(DefaultColors,pal^,sizeof(RGBrec)*maxcolor);
     { start graph subsystem }
     InitializeCriticalSection(graphdrawing);
     graphrunning:=false;
     MessageThreadHandle:=CreateThread(nil,0,@MessageHandleThread,
       nil,0,MessageThreadID);
     repeat until graphrunning;
  end;

procedure CloseGraph;

  begin
     If not isgraphmode then
       begin
         _graphresult := grnoinitgraph;
         exit
       end;
     WaitForSingleObject(MessageThreadHandle,Infinite);
     CloseHandle(MessageThreadHandle);
     DeleteCriticalSection(graphdrawing);
     freemem(pal,sizeof(RGBrec)*maxcolor);
  end;

{
procedure line(x1,y1,x2,y2 : longint);

  var
     pen,oldpen : hpen;
     windc : hdc;

  begin
     if graphrunning then
       begin
          EnterCriticalSection(graphdrawing);
          pen:=CreatePen(PS_SOLID,4,RGB($ff,0,0));

          oldpen:=SelectObject(bitmapdc,pen);
          MoveToEx(bitmapdc,x1,y1,nil);
          LineTo(bitmapdc,x2,y2);
          SelectObject(bitmapdc,oldpen);

          windc:=GetDC(mainwindow);
          oldpen:=SelectObject(windc,pen);
          MoveToEx(windc,x1,y1,nil);
          LineTo(windc,x2,y2);
          SelectObject(windc,oldpen);
          ReleaseDC(mainwindow,windc);

          DeleteObject(pen);
          LeaveCriticalSection(graphdrawing);
       end;
  end;

}

{ multipage support could be done by using more than one background bitmap }
procedure SetVisualWin32GUI(page: word);

  begin
  end;

procedure SetActiveWin32GUI(page: word);
  begin
  end;

function queryadapterinfo : pmodeinfo;

  var
     mode: TModeInfo;

  begin
     SaveVideoState:=savestate;
     RestoreVideoState:=restorestate;
     QueryAdapterInfo := ModeList;
     { If the mode listing already exists... }
     { simply return it, without changing    }
     { anything...                           }
     if assigned(ModeList) then
       exit;
     InitMode(mode);
     { now add all standard VGA modes...       }
     mode.DriverNumber:= VGA;
     mode.HardwarePages:= 0;
     mode.ModeNumber:=0;
     mode.ModeName:='640 x 480 Win32GUI';
     mode.MaxColor := 16;
     mode.PaletteSize := mode.MaxColor;
     mode.DirectColor := FALSE;
     mode.MaxX := 639;
     mode.MaxY := 479;
     mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
     mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
     mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
     mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
     mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
     mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
     mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
     mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
     mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI640x480x16;
     mode.XAspect := 10000;
     mode.YAspect := 10000;
     AddMode(mode);
  end;

{
  $Log$
  Revision 1.3  1999-11-30 22:36:53  florian
    * the wm_nc... messages aren't handled anymore it leads to too mch problems ...

  Revision 1.2  1999/11/29 22:03:39  florian
    * first implementation of winmouse unit

  Revision 1.1  1999/11/08 11:15:22  peter
    * move graph.inc to the target dir

  Revision 1.1  1999/11/03 20:23:02  florian
    + first release of win32 gui support

}
