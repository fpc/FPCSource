{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This file implements the win32 gui support for the graph unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Graph;
interface

uses
  windows;

{$i graphh.inc}

  var
    { this procedure allows to hook keyboard messages }
    charmessagehandler : function(Window: hwnd; AMessage, WParam,
                                  LParam: Longint): Longint;
    { this procedure allows to hook mouse messages }
    mousemessagehandler : function(Window: hwnd; AMessage, WParam,
                                   LParam: Longint): Longint;
   mainwindow : HWnd;

  const
    { predefined window style }
    { we shouldn't set CS_DBLCLKS here }
    { because most dos applications    }
    { handle double clicks on it's own }
    graphwindowstyle : DWord = cs_hRedraw or cs_vRedraw;

    windowtitle : pchar = 'Graph window application';

CONST

  m640x200x16       = VGALo;
  m640x400x16       = VGAMed;
  m640x480x16       = VGAHi;

  { VESA Specific video modes. }
  m320x200x32k      = $10D;
  m320x200x64k      = $10E;

  m640x400x256      = $100;

  m640x480x256      = $101;
  m640x480x32k      = $110;
  m640x480x64k      = $111;

  m800x600x16       = $102;
  m800x600x256      = $103;
  m800x600x32k      = $113;
  m800x600x64k      = $114;

  m1024x768x16      = $104;
  m1024x768x256     = $105;
  m1024x768x32k     = $116;
  m1024x768x64k     = $117;

  m1280x1024x16     = $106;
  m1280x1024x256    = $107;
  m1280x1024x32k    = $119;
  m1280x1024x64k    = $11A;

  { some extra modes which applies only to GUI }
  mLargestWindow16  = $f0;
  mLargestWindow256 = $f1;
  mLargestWindow32k = $f2;
  mLargestWindow64k = $f3;
  mLargestWindow16M = $f4;
  mMaximizedWindow16 = $f5;
  mMaximizedWindow256 = $f6;
  mMaximizedWindow32k = $f7;
  mMaximizedWindow64k = $f8;
  mMaximizedWindow16M = $f9;


implementation

uses
  strings;

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

{$i graph.inc}


{ used to create a file containing all calls to WM_PAINT
  WARNING this probably creates HUGE files PM }
{ $define DEBUG_WM_PAINT}
var
   savedscreen : hbitmap;
   graphrunning : boolean;
   graphdrawing : tcriticalsection;
{$ifdef DEBUG_WM_PAINT}
   graphdebug : text;
const
   wm_paint_count : longint = 0;
var
{$endif DEBUG_WM_PAINT}
   bitmapdc : hdc;
   oldbitmap : hgdiobj;
   pal : ^rgbrec;
//   SavePtr : pointer; { we don't use that pointer }
   MessageThreadHandle : Handle;
   MessageThreadID : DWord;
   windc : hdc;

function GetPaletteEntry(r,g,b : word) : word;

  var
     dist,i,index,currentdist : longint;

  begin
     dist:=$7fffffff;
     index:=0;
     for i:=0 to maxcolors do
       begin
          currentdist:=abs(r-pal[i].red)+abs(g-pal[i].green)+
            abs(b-pal[i].blue);
          if currentdist<dist then
            begin
               index:=i;
               dist:=currentdist;
               if dist=0 then
                 break;
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
         c:=RGB(pal[pixel].red,pal[pixel].green,pal[pixel].blue);
         EnterCriticalSection(graphdrawing);
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
         LeaveCriticalSection(graphdrawing);
         GetPixel16Win32GUI:=GetPaletteEntry(GetRValue(c),GetGValue(c),GetBValue(c));
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
         col:=CurrentColor;
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

var
   bitmapfontcache : array[0..255] of HBITMAP;

procedure DrawBitmapCharHorizWin32GUI(x,y : longint;charsize : word;const s : string);

  var
     cnt1,cnt2,cnt3,cnt4,j,k,c,xpos,i : longint;
     fontbitmap    : TBitmapChar;
     charbitmap,oldcharbitmap : HBITMAP;
     chardc : HDC;
     color : longint;
     brushwin,oldbrushwin,brushbitmap,oldbrushbitmap : HBRUSH;
     bitmaprgn,winrgn : HRGN;

  begin
     EnterCriticalSection(graphdrawing);
     c:=length(s);
     chardc:=CreateCompatibleDC(windc);
     if currentcolor<>white then
       begin
          color:=RGB(pal[currentcolor].red,pal[currentcolor].green,
            pal[currentcolor].blue);

          brushwin:=CreateSolidBrush(color);
          oldbrushwin:=SelectObject(windc,brushwin);

          brushbitmap:=CreateSolidBrush(color);
          oldbrushbitmap:=SelectObject(bitmapdc,brushbitmap);
       end;
     inc(x,startxviewport);
     inc(y,startyviewport);
     { let windows do the clipping }
     bitmaprgn:=CreateRectRgn(startxviewport,startyviewport,
       startxviewport+viewwidth+1,startyviewport+viewheight+1);
     winrgn:=CreateRectRgn(startxviewport,startyviewport,
       startxviewport+viewwidth+1,startyviewport+viewheight+1);
     SelectClipRgn(bitmapdc,bitmaprgn);
     SelectClipRgn(windc,winrgn);
     for i:=0 to c-1 do
       begin
          xpos:=x+(i*8)*Charsize;
          if bitmapfontcache[byte(s[i+1])]=0 then
            begin
               charbitmap:=CreateCompatibleBitmap(windc,8,8);
               oldcharbitmap:=SelectObject(chardc,charbitmap);
               Fontbitmap:=TBitmapChar(DefaultFontData[s[i+1]]);

               for j:=0 to 7 do
                  for k:=0 to 7 do
                    if Fontbitmap[j,k]<>0 then
                      SetPixel(chardc,k,j,$ffffff)
                    else
                      SetPixel(chardc,k,j,0);
               bitmapfontcache[byte(s[i+1])]:=charbitmap;
               SelectObject(chardc,oldcharbitmap);
            end;
          oldcharbitmap:=SelectObject(chardc,bitmapfontcache[byte(s[i+1])]);
          if CharSize=1 then
            begin
               if currentcolor=white then
                 begin
                    BitBlt(windc,xpos,y,8,8,chardc,0,0,SRCPAINT);
                    BitBlt(bitmapdc,xpos,y,8,8,chardc,0,0,SRCPAINT);
                 end
               else
                 begin
                    { could we do this with one pattern operation ?? }
                    { we would need something like DSnaSPao }
                    // ROP $00220326=DSna
                    BitBlt(windc,xpos,y,8,8,chardc,0,0,$00220326);
                    BitBlt(bitmapdc,xpos,y,8,8,chardc,0,0,$00220326);
                    // ROP $00EA02E9 = DPSao
                    BitBlt(windc,xpos,y,8,8,chardc,0,0,$00EA02E9);
                    BitBlt(bitmapdc,xpos,y,8,8,chardc,0,0,$00EA02E9);
                 end;
            end
          else
            begin
               if currentcolor=white then
                 begin
                    StretchBlt(windc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,SRCPAINT);
                    StretchBlt(bitmapdc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,SRCPAINT);
                 end
               else
                 begin
                    { could we do this with one pattern operation ?? }
                    { we would need something like DSnaSPao }
                    // ROP $00220326=DSna
                    StretchBlt(windc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,$00220326);
                    StretchBlt(bitmapdc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,$00220326);
                    // ROP $00EA02E9 = DPSao
                    StretchBlt(windc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,$00EA02E9);
                    StretchBlt(bitmapdc,xpos,y,8*charsize,8*charsize,chardc,0,0,8,8,$00EA02E9);
                 end;
            end;
          SelectObject(chardc,oldcharbitmap);
       end;
    if currentcolor<>white then
      begin
         SelectObject(windc,oldbrushwin);
         DeleteObject(brushwin);

         SelectObject(bitmapdc,oldbrushbitmap);
         DeleteObject(brushbitmap);
      end;
    { release clip regions }
    SelectClipRgn(bitmapdc,0);
    SelectClipRgn(windc,0);
    DeleteObject(bitmaprgn);
    DeleteObject(winrgn);
    DeleteDC(chardc);
    LeaveCriticalSection(graphdrawing);
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
                  col:=CurrentColor;
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
                  col:=CurrentColor;
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
                  col:=CurrentColor;
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
                  c:=RGB(pal[col].red,pal[col].green,pal[col].blue);
                  pen:=CreatePen(PS_SOLID,1,c);
                  EnterCriticalSection(graphdrawing);
                  oldpen:=SelectObject(bitmapdc,pen);
                  Windows.MoveToEx(bitmapdc,x,y,nil);
                  Windows.LineTo(bitmapdc,x2+1,y);
                  SelectObject(bitmapdc,oldpen);

                  oldpen:=SelectObject(windc,pen);
                  Windows.MoveToEx(windc,x,y,nil);
                  Windows.LineTo(windc,x2+1,y);
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
     oldbrush : hbrush;
     oldpen : hpen;
     i : longint;

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
{$ifdef DEBUG_WM_PAINT}
         inc(wm_paint_count);
{$endif DEBUG_WM_PAINT}
         if not GetUpdateRect(Window,@r,false) then
           exit;
         EnterCriticalSection(graphdrawing);
         graphrunning:=true;
         dc:=BeginPaint(Window,@ps);
{$ifdef DEBUG_WM_PAINT}
         Writeln(graphdebug,'WM_PAINT in ((',r.left,',',r.top,
           '),(',r.right,',',r.bottom,'))');
{$endif def DEBUG_WM_PAINT}
         if graphrunning then
           {BitBlt(dc,0,0,maxx+1,maxy+1,bitmapdc,0,0,SRCCOPY);}
           BitBlt(dc,r.left,r.top,r.right-r.left+1,r.bottom-r.top+1,bitmapdc,r.left,r.top,SRCCOPY);
         EndPaint(Window,ps);
         LeaveCriticalSection(graphdrawing);
         Exit;
      end;
    wm_create:
      begin
{$ifdef DEBUG_WM_PAINT}
         assign(graphdebug,'wingraph.log');
         rewrite(graphdebug);
{$endif DEBUG_WM_PAINT}
         EnterCriticalSection(graphdrawing);
         dc:=GetDC(window);
         bitmapdc:=CreateCompatibleDC(dc);
         savedscreen:=CreateCompatibleBitmap(dc,maxx+1,maxy+1);
         ReleaseDC(window,dc);
         oldbitmap:=SelectObject(bitmapdc,savedscreen);
         windc:=GetDC(window);
         // clear everything
         oldpen:=SelectObject(bitmapdc,GetStockObject(BLACK_PEN));
         oldbrush:=SelectObject(bitmapdc,GetStockObject(BLACK_BRUSH));
         Windows.Rectangle(bitmapdc,0,0,maxx,maxy);
         SelectObject(bitmapdc,oldpen);
         SelectObject(bitmapdc,oldbrush);
         // ... the window too
         oldpen:=SelectObject(windc,GetStockObject(BLACK_PEN));
         oldbrush:=SelectObject(windc,GetStockObject(BLACK_BRUSH));
         Windows.Rectangle(windc,0,0,maxx,maxy);
         SelectObject(windc,oldpen);
         SelectObject(windc,oldbrush);
         // clear font cache
         fillchar(bitmapfontcache,sizeof(bitmapfontcache),0);
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
         // release font cache
         for i:=0 to 255 do
           if bitmapfontcache[i]<>0 then
             DeleteObject(bitmapfontcache[i]);

         LeaveCriticalSection(graphdrawing);
{$ifdef DEBUG_WM_PAINT}
         close(graphdebug);
{$endif DEBUG_WM_PAINT}
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
  WindowClass.lpszClassName := 'FPCGraphWindow';

  winregister:=RegisterClass(WindowClass) <> 0;
end;

var
   // here we can force the creation of a maximized window }
   extrastyle : longint;

 { Create the Window Class }
function WinCreate : HWnd;
var
  hWindow: HWnd;
begin

  hWindow := CreateWindow('FPCGraphWindow', windowtitle,
              ws_OverlappedWindow or extrastyle, CW_USEDEFAULT, 0,
              maxx+1+2*GetSystemMetrics(SM_CXFRAME),
              maxy+1+2*GetSystemMetrics(SM_CYFRAME)+
                GetSystemMetrics(SM_CYCAPTION),
              0, 0, system.MainInstance, nil);

  if hWindow <> 0 then begin
    ShowWindow(hWindow, SW_SHOW);
    UpdateWindow(hWindow);
  end;

  wincreate:=hWindow;
end;

const
   winregistered : boolean = false;

function MessageHandleThread(p : pointer) : DWord;StdCall;

  var
     AMessage: Msg;

  begin
     if not(winregistered) then
       begin
          if not WinRegister then
            begin
               MessageBox(0, 'Window registration failed', nil, mb_Ok);
               ExitThread(1);
            end;
          winregistered:=true;
       end;
     MainWindow := WinCreate;
     if longint(mainwindow) = 0 then begin
       MessageBox(0, 'Window creation failed', nil, mb_Ok);
       ExitThread(1);
     end;
     while longint(GetMessage(@AMessage, 0, 0, 0))=longint(true) do
       begin
          TranslateMessage(AMessage);
          DispatchMessage(AMessage);
       end;
     MessageHandleThread:=0;
  end;

procedure InitWin32GUI16colors;

  var
     threadexitcode : longint;
  begin
     getmem(pal,sizeof(RGBrec)*maxcolor);
     move(DefaultColors,pal^,sizeof(RGBrec)*maxcolor);
     if (IntCurrentMode=mMaximizedWindow16) or
       (IntCurrentMode=mMaximizedWindow256) or
       (IntCurrentMode=mMaximizedWindow32k) or
       (IntCurrentMode=mMaximizedWindow64k) or
       (IntCurrentMode=mMaximizedWindow16M) then
       extrastyle:=ws_maximize
     else
       extrastyle:=0;
     { start graph subsystem }
     InitializeCriticalSection(graphdrawing);
     graphrunning:=false;
     MessageThreadHandle:=CreateThread(nil,0,@MessageHandleThread,
       nil,0,MessageThreadID);
     repeat
       GetExitCodeThread(MessageThreadHandle,@threadexitcode);
     until graphrunning or (threadexitcode<>STILL_ACTIVE);
     if threadexitcode<>STILL_ACTIVE then
        _graphresult := grerror;
  end;

procedure CloseGraph;

  begin
     If not isgraphmode then
       begin
         _graphresult := grnoinitgraph;
         exit
       end;
     PostMessage(MainWindow,wm_destroy,0,0);
     PostThreadMessage(MessageThreadHandle,wm_quit,0,0);
     WaitForSingleObject(MessageThreadHandle,Infinite);
     CloseHandle(MessageThreadHandle);
     DeleteCriticalSection(graphdrawing);
     freemem(pal,sizeof(RGBrec)*maxcolor);
  end;

procedure LineWin32GUI(X1, Y1, X2, Y2: smallint); {$ifndef fpc}far;{$endif fpc}

  var X, Y :           smallint;
      deltax, deltay : smallint;
      d, dinc1, dinc2: smallint;
      xinc1          : smallint;
      xinc2          : smallint;
      yinc1          : smallint;
      yinc2          : smallint;
      i              : smallint;
      Flag           : Boolean; { determines pixel direction in thick lines }
      NumPixels      : smallint;
      PixelCount     : smallint;
      OldCurrentColor: Word;
      swtmp          : smallint;
      TmpNumPixels   : smallint;
      col : longint;
      pen,oldpen : hpen;

 begin
    if graphrunning then
      begin
         {******************************************}
         {  SOLID LINES                             }
         {******************************************}
         if lineinfo.LineStyle = SolidLn then
           Begin
              { Convert to global coordinates. }
              x1 := x1 + StartXViewPort;
              x2 := x2 + StartXViewPort;
              y1 := y1 + StartYViewPort;
              y2 := y2 + StartYViewPort;
              { if fully clipped then exit... }
              if ClipPixels then
                begin
                   if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
                     StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
                       exit;
                  If LineInfo.Thickness=NormWidth then
                   Begin
                      EnterCriticalSection(graphdrawing);
                      col:=RGB(pal[CurrentColor].red,pal[CurrentColor].green,pal[CurrentColor].blue);
                      pen:=CreatePen(PS_SOLID,1,col);
                      OldCurrentColor:=CurrentColor;

                      oldpen:=SelectObject(windc,pen);
                      MoveToEx(windc,x1,y1,nil);
                      Windows.LineTo(windc,x2,y2);
                      SetPixel(windc,x2,y2,col);
                      SelectObject(windc,oldpen);

                      oldpen:=SelectObject(bitmapdc,pen);
                      MoveToEx(bitmapdc,x1,y1,nil);
                      Windows.LineTo(bitmapdc,x2,y2);
                      SetPixel(bitmapdc,x2,y2,col);
                      SelectObject(bitmapdc,oldpen);

                      DeleteObject(pen);
                      CurrentColor:=OldCurrentColor;
                      LeaveCriticalSection(graphdrawing);
                   end
                 else
                  { Thick width lines }
                   begin
                     { Draw the pixels }
                      for i := 1 to numpixels do
                        begin
                         { all depending on the slope, we can determine         }
                         { in what direction the extra width pixels will be put }
                         If Flag then
                           Begin
                             DirectPutPixelClip(x-1,y);
                             DirectPutPixelClip(x,y);
                             DirectPutPixelClip(x+1,y);
                           end
                         else
                           Begin
                             DirectPutPixelClip(x, y-1);
                             DirectPutPixelClip(x, y);
                             DirectPutPixelClip(x, y+1);
                           end;
                         if d < 0 then
                           begin
                             d := d + dinc1;
                             x := x + xinc1;
                             y := y + yinc1;
                           end
                         else
                           begin
                             d := d + dinc2;
                             x := x + xinc2;
                             y := y + yinc2;
                           end;
                        end;
                   end;
                 end;
           end
          else
       {******************************************}
       {  begin patterned lines                   }
       {******************************************}
           Begin
             { Convert to global coordinates. }
             x1 := x1 + StartXViewPort;
             x2 := x2 + StartXViewPort;
             y1 := y1 + StartYViewPort;
             y2 := y2 + StartYViewPort;
             { if fully clipped then exit... }
             if ClipPixels then
              begin
              if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
                  StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
                     exit;
              end;

             OldCurrentColor := CurrentColor;
             PixelCount:=0;
             if y1 = y2 then
                   Begin
                    { Check if we must swap }
                if x1 >= x2 then
                      Begin
                        swtmp := x1;
                        x1 := x2;
                        x2 := swtmp;
                      end;
                if LineInfo.Thickness = NormWidth then
                     Begin
                      for PixelCount:=x1 to x2 do
                            { optimization: PixelCount mod 16 }
                            if LinePatterns[PixelCount and 15] = TRUE then
                             begin
                               DirectPutPixel(PixelCount,y2);
                             end;
                     end
                    else
                     Begin
                      for i:=-1 to 1 do
                            Begin
                              for PixelCount:=x1 to x2 do
                                { Optimization from Thomas - mod 16 = and 15 }
                                {this optimization has been performed by the compiler
                                 for while as well (JM)}
                                if LinePatterns[PixelCount and 15] = TRUE then
                                  begin
                                        DirectPutPixelClip(PixelCount,y2+i);
                                  end;
                            end;
                     end;
               end
             else
             if x1 = x2 then
                  Begin
                   { Check if we must swap }
                   if y1 >= y2 then
                     Begin
                       swtmp := y1;
                       y1 := y2;
                       y2 := swtmp;
                     end;
                   if LineInfo.Thickness = NormWidth then
                     Begin
                       for PixelCount:=y1 to y2 do
                           { compare if we should plot a pixel here , compare }
                           { with predefined line patterns...                 }
                           if LinePatterns[PixelCount and 15] = TRUE then
                             begin
                           DirectPutPixel(x1,PixelCount);
                             end;
                     end
                   else
                     Begin
                       for i:=-1 to 1 do
                            Begin
                              for PixelCount:=y1 to y2 do
                              { compare if we should plot a pixel here , compare }
                              { with predefined line patterns...                 }
                                if LinePatterns[PixelCount and 15] = TRUE then
                                  begin
                                    DirectPutPixelClip(x1+i,PixelCount);
                                  end;
                            end;
                     end;
                  end
             else
                  Begin
                    oldCurrentColor := CurrentColor;
                    { Calculate deltax and deltay for initialisation }
                    deltax := abs(x2 - x1);
                    deltay := abs(y2 - y1);

                    { Initialize all vars based on which is the independent variable }
                    if deltax >= deltay then
                      begin

                        Flag := FALSE;
                        { x is independent variable }
                        numpixels := deltax + 1;
                        d := (2 * deltay) - deltax;
                        dinc1 := deltay Shl 1;
                        dinc2 := (deltay - deltax) shl 1;
                        xinc1 := 1;
                        xinc2 := 1;
                        yinc1 := 0;
                        yinc2 := 1;
                     end
                   else
                     begin

                       Flag := TRUE;
                       { y is independent variable }
                       numpixels := deltay + 1;
                       d := (2 * deltax) - deltay;
                       dinc1 := deltax Shl 1;
                       dinc2 := (deltax - deltay) shl 1;
                       xinc1 := 0;
                       xinc2 := 1;
                       yinc1 := 1;
                       yinc2 := 1;
                     end;

                   { Make sure x and y move in the right directions }
                   if x1 > x2 then
                     begin
                       xinc1 := - xinc1;
                       xinc2 := - xinc2;
                     end;
                   if y1 > y2 then
                     begin
                       yinc1 := - yinc1;
                       yinc2 := - yinc2;
                     end;

                   { Start drawing at <x1, y1> }
                   x := x1;
                   y := y1;

                   If LineInfo.Thickness=ThickWidth then

                    Begin
                      TmpNumPixels := NumPixels-1;
                      { Draw the pixels }
                      for i := 0 to TmpNumPixels do
                        begin
                            { all depending on the slope, we can determine         }
                            { in what direction the extra width pixels will be put }
                              If Flag then
                                 Begin
                                   { compare if we should plot a pixel here , compare }
                                   { with predefined line patterns...                 }
                                   if LinePatterns[i and 15] = TRUE then
                                     begin
                                       DirectPutPixelClip(x-1,y);
                                       DirectPutPixelClip(x,y);
                                       DirectPutPixelClip(x+1,y);
                                     end;
                                 end
                              else
                                 Begin
                                   { compare if we should plot a pixel here , compare }
                                   { with predefined line patterns...                 }
                                   if LinePatterns[i and 15] = TRUE then
                                    begin
                                      DirectPutPixelClip(x,y-1);
                                      DirectPutPixelClip(x,y);
                                      DirectPutPixelClip(x,y+1);
                                    end;
                                 end;
                          if d < 0 then
                                begin
                                  d := d + dinc1;
                                  x := x + xinc1;
                                  y := y + yinc1;
                                end
                          else
                                begin
                          d := d + dinc2;
                          x := x + xinc2;
                          y := y + yinc2;
                                end;
                       end;
                   end
                  else
                   Begin
                    { instead of putting in loop , substract by one now }
                    TmpNumPixels := NumPixels-1;
                   { NormWidth }
                    for i := 0 to TmpNumPixels do
                    begin
                         if LinePatterns[i and 15] = TRUE then
                           begin
                                 DirectPutPixel(x,y);
                           end;
                    if d < 0 then
                        begin
                          d := d + dinc1;
                          x := x + xinc1;
                          y := y + yinc1;
                        end
                    else
                        begin
                          d := d + dinc2;
                          x := x + xinc2;
                          y := y + yinc2;
                        end;
                    end;
                   end
               end;
       {******************************************}
       {  end patterned lines                     }
       {******************************************}
              { restore color }
              CurrentColor:=OldCurrentColor;
          end;
    end;
 end;  { Line }

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
     ScreenWidth,ScreenHeight : longint;
     ScreenWidthMaximized,ScreenHeightMaximized : longint;

  begin
     SaveVideoState:=savestate;
     RestoreVideoState:=restorestate;
     { we must take care of the border and caption }
     ScreenWidth:=GetSystemMetrics(SM_CXSCREEN)-
       2*GetSystemMetrics(SM_CXFRAME);
     ScreenHeight:=GetSystemMetrics(SM_CYSCREEN)-
       2*GetSystemMetrics(SM_CYFRAME)-
       GetSystemMetrics(SM_CYCAPTION);
     { for maximozed windows it's again different }
     { here we've only a caption }
     ScreenWidthMaximized:=GetSystemMetrics(SM_CXFULLSCREEN);
     { neither GetSystemMetrics(SM_CYFULLSCREEN nor     }
     { SystemParametersInfo(SPI_GETWORKAREA)            }
     { takes a hidden try into account :( FK            }
     ScreenHeightMaximized:=GetSystemMetrics(SM_CYFULLSCREEN);

     QueryAdapterInfo := ModeList;
     { If the mode listing already exists... }
     { simply return it, without changing    }
     { anything...                           }
     if assigned(ModeList) then
       exit;
     { the first one becomes the standard mode }
     if (ScreenWidth>=640) and (ScreenHeight>=480) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VGA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=VGAHi;
          mode.ModeName:='640 x 480 x 16 Win32GUI';
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
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          // mode.Line:={$ifdef fpc}@{$endif}LineWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     if (ScreenWidth>=640) and (ScreenHeight>=200) then
       begin
          InitMode(mode);
          { now add all standard VGA modes...       }
          mode.DriverNumber:= VGA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=VGALo;
          mode.ModeName:='640 x 200 x 16 Win32GUI';
          mode.MaxColor := 16;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 639;
          mode.MaxY := 199;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     if (ScreenWidth>=640) and (ScreenHeight>=350) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VGA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=VGAMed;
          mode.ModeName:='640 x 350 x 16 Win32GUI';
          mode.MaxColor := 16;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 639;
          mode.MaxY := 349;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     if (ScreenWidth>=640) and (ScreenHeight>=400) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m640x400x256;
          mode.ModeName:='640 x 400 x 256 Win32GUI';
          mode.MaxColor := 256;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 639;
          mode.MaxY := 399;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     if (ScreenWidth>=640) and (ScreenHeight>=480) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m640x480x256;
          mode.ModeName:='640 x 480 x 256 Win32GUI';
          mode.MaxColor := 256;
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
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     { add 800x600 only if screen is large enough }
     If (ScreenWidth>=800) and (ScreenHeight>=600) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m800x600x16;
          mode.ModeName:='800 x 600 x 16 Win32GUI';
          mode.MaxColor := 16;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 799;
          mode.MaxY := 599;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m800x600x256;
          mode.ModeName:='800 x 600 x 256 Win32GUI';
          mode.MaxColor := 256;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 799;
          mode.MaxY := 599;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     { add 1024x768 only if screen is large enough }
     If (ScreenWidth>=1024) and (ScreenHeight>=768) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m1024x768x16;
          mode.ModeName:='1024 x 768 x 16 Win32GUI';
          mode.MaxColor := 16;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 1023;
          mode.MaxY := 767;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m1024x768x256;
          mode.ModeName:='1024 x 768 x 256 Win32GUI';
          mode.MaxColor := 256;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 1023;
          mode.MaxY := 768;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     { add 1280x1024 only if screen is large enough }
     If (ScreenWidth>=1280) and (ScreenHeight>=1024) then
       begin
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m1280x1024x16;
          mode.ModeName:='1280 x 1024 x 16 Win32GUI';
          mode.MaxColor := 16;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 1279;
          mode.MaxY := 1023;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
          InitMode(mode);
          mode.DriverNumber:= VESA;
          mode.HardwarePages:= 0;
          mode.ModeNumber:=m1280x1024x256;
          mode.ModeName:='1280 x 1024 x 256 Win32GUI';
          mode.MaxColor := 256;
          mode.PaletteSize := mode.MaxColor;
          mode.DirectColor := FALSE;
          mode.MaxX := 1279;
          mode.MaxY := 1023;
          mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
          mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
          mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
          mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
          mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
          mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
          mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
          mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
          mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
          mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
          mode.XAspect := 10000;
          mode.YAspect := 10000;
          AddMode(mode);
       end;
     { at least we add a mode with the largest possible window }
      InitMode(mode);
      mode.DriverNumber:= VESA;
      mode.HardwarePages:= 0;
      mode.ModeNumber:=mLargestWindow16;
      mode.ModeName:='Largest Window x 16';
      mode.MaxColor := 16;
      mode.PaletteSize := mode.MaxColor;
      mode.DirectColor := FALSE;
      mode.MaxX := ScreenWidth-1;
      mode.MaxY := ScreenHeight-1;
      mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
      mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
      mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
      mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
      mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
      mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
      mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
      mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
      mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
      mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
      mode.XAspect := 10000;
      mode.YAspect := 10000;
      AddMode(mode);
      InitMode(mode);
      mode.DriverNumber:= VESA;
      mode.HardwarePages:= 0;
      mode.ModeNumber:=mLargestWindow256;
      mode.ModeName:='Largest Window x 256';
      mode.MaxColor := 256;
      mode.PaletteSize := mode.MaxColor;
      mode.DirectColor := FALSE;
      mode.MaxX := ScreenWidth-1;
      mode.MaxY := ScreenHeight-1;
      mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
      mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
      mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
      mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
      mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
      mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
      mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
      mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
      mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
      mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
      mode.XAspect := 10000;
      mode.YAspect := 10000;
      AddMode(mode);
      { .. and a maximized window }
      InitMode(mode);
      mode.DriverNumber:= VESA;
      mode.HardwarePages:= 0;
      mode.ModeNumber:=mMaximizedWindow16;
      mode.ModeName:='Maximized Window x 16';
      mode.MaxColor := 16;
      mode.PaletteSize := mode.MaxColor;
      mode.DirectColor := FALSE;
      mode.MaxX := ScreenWidthMaximized-1;
      mode.MaxY := ScreenHeightMaximized-1;
      mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
      mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
      mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
      mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
      mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
      mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
      mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
      mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
      mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
      mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
      mode.XAspect := 10000;
      mode.YAspect := 10000;
      AddMode(mode);
      InitMode(mode);
      mode.DriverNumber:= VESA;
      mode.HardwarePages:= 0;
      mode.ModeNumber:=mMaximizedWindow256;
      mode.ModeName:='Maximized Window x 256';
      mode.MaxColor := 256;
      mode.PaletteSize := mode.MaxColor;
      mode.DirectColor := FALSE;
      mode.MaxX := ScreenWidthMaximized-1;
      mode.MaxY := ScreenHeightMaximized-1;
      mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16Win32GUI;
      mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16Win32GUI;
      mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16Win32GUI;
      mode.HLine := {$ifdef fpc}@{$endif}HLine16Win32GUI;
      mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetRGBPaletteWin32GUI;
      mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetRGBPaletteWin32GUI;
      mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualWin32GUI;
      mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveWin32GUI;
      mode.InitMode := {$ifdef fpc}@{$endif}InitWin32GUI16colors;
      mode.DrawBitmapCharHoriz:={$ifdef fpc}@{$endif}DrawBitmapCharHorizWin32GUI;
      mode.XAspect := 10000;
      mode.YAspect := 10000;
      AddMode(mode);
  end;

begin
  InitializeGraph;
end.
{
  $Log$
  Revision 1.6  2000-03-27 12:57:30  florian
    * some "resource leaks" fixed

  Revision 1.5  2000/03/25 19:10:11  florian
    * colored bitmap font drawing fixed: the color brush
      was selected for the recovery bitmap

  Revision 1.4  2000/03/24 18:18:15  florian
    * accelerated output of bitmap fonts

  Revision 1.3  2000/03/24 12:57:41  florian
    * the window is now cleared by wm_create
    * default mode is again 640x480x16

  Revision 1.2  2000/03/24 10:49:17  florian
    * the mode detection takes now care of window caption and border
    + 1024x768 and 1280x1024 modes added
    + special gui modes added: largest window and maximized window to
      use the desktop as much as possible
    * Hline fixed: the windows function LineTo doesn't draw the last pixel!

  Revision 1.1  2000/03/19 11:20:14  peter
    * graph unit include is now independent and the dependent part
      is now in graph.pp
    * ggigraph unit for linux added

  Revision 1.8  2000/03/17 22:53:20  florian
    * window class is registered only once => multible init/closegraphs are possible
    * calling cleardevice when creating the window

  Revision 1.7  2000/03/05 13:06:32  florian
    * the title can be user defined

  Revision 1.6  2000/01/07 16:41:52  daniel
    * copyright 2000

  Revision 1.5  1999/12/08 09:09:34  pierre
   + add VESA compatible mode in 16 and 256 colors

  Revision 1.4  1999/12/02 00:24:36  pierre
    * local var col was undefined
    + 640x200 and 640x350 modes added (VGALo and VGAMed)
    * WM_PAINT better handled (only requested region written)

  Revision 1.3  1999/11/30 22:36:53  florian
    * the wm_nc... messages aren't handled anymore it leads to too mch problems ...

  Revision 1.2  1999/11/29 22:03:39  florian
    * first implementation of winmouse unit

  Revision 1.1  1999/11/08 11:15:22  peter
    * move graph.inc to the target dir

  Revision 1.1  1999/11/03 20:23:02  florian
    + first release of win32 gui support
}