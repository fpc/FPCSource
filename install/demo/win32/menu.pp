{
  $Id$
  Copyright (c) 1999 by Michael van Canneyt

  Win32 menu creation example.
}
program menudemo;
{$APPTYPE GUI}
{$MODE DELPHI}

uses Strings,Windows;

const AppName='MenuDemo';

var AMessage:Msg;
    hWindow,hStatus,Hedit : HWnd;
    WindowClass:WndClass;
    menu:hMenu;

Const FileMenus : Array[101..104] of pchar = ('New','Open','Save','Exit');
      EditMenus : Array[201..203] of pchar = ('Cut','copy','paste');

function WindowProc (Window:HWnd;AMessage,WParam,LParam:Longint):Longint;
stdcall; export;
var dc:hdc;
    ps:paintstruct;
    r:rect;
    nrmenu : longint;

 begin
 WindowProc:=0;
 case AMessage of
  wm_paint:
   begin
   dc:=BeginPaint (Window,@ps);
   GetClientRect (Window,@r);
   EndPaint (Window,ps);
   Exit;
   end;
  wm_Destroy:
   begin
   PostQuitMessage (0);
   Exit;
   end;
  wm_Size:
   begin
   GetClientRect (Window,@r);
   MoveWindow (hStatus,r.left,r.bottom-16,r.right,r.bottom,true);
   MoveWindow (HEdit,0,0,r.right-r.left,r.bottom-r.top-16,true);
   end;
  wm_command:
   begin
   NrMenu:=WParam and $FFFF;
   Case NrMenu of
     104 : Halt(0);
     101..103: MessageBox(Window,FileMenus[NrMenu],'File Menu click received',MB_OK or MB_ICONINFORMATION);
     201..203: MessageBox(Window,EditMenus[NrMenu],'Edit operation not implemented',MB_OK or MB_ICONINFORMATION);
   end;
   end;
  end;
 WindowProc:=DefWindowProc (Window,AMessage,WParam,LParam);
 end;

function WinRegister:Boolean;
 begin
 WindowClass.Style:=cs_hRedraw or cs_vRedraw;
 WindowClass.lpfnWndProc:=WndProc(@WindowProc);
 WindowClass.cbClsExtra:=0;
 WindowClass.cbWndExtra:=0;
 WindowClass.hInstance:=system.MainInstance;
 WindowClass.hIcon:=LoadIcon (0,idi_Application);
 WindowClass.hCursor:=LoadCursor (0,idc_Arrow);
 WindowClass.hbrBackground:=GetStockObject(GRAY_BRUSH);
 WindowClass.lpszMenuName:='Files';
 WindowClass.lpszClassName:=AppName;
 Result:=RegisterClass (WindowClass)<>0;
 end;

Const
  CS_Start = ES_AUTOHSCROLL OR ES_AUTOVSCROLL OR ES_MULTILINE;
  CS_OFF = CS_OWNDC or CS_CLASSDC or CS_GLOBALCLASS;
  CS_ON = CS_VREDRAW or CS_HREDRAW or CS_PARENTDC or WS_CHILD;

  EdiTText : Pchar = 'This is an edit text...';

Function EditCreate (ParentWindow : HWnd) : HWnd;

Var
    hedit : HWND;
    R : TRect;
    DC : HDC;

begin
  GetClientRect(ParentWindow,@r);
 HEdit:=CreateWindow ('EDIT',EditText, CS_START AND NOT CS_OFF OR CS_ON,
        0,0,R.RIght-R.Left,R.Bottom-R.top-16,ParentWindow,0,system.MainInstance,nil);
 If Hedit<>0 then begin
   showwindow(Hedit,cmdShow);
   updateWindow(HEdit);
   end;
 Result:=HEdit;
 dc:=getwindowdc(Hedit);
 settextalign (dc,DT_Left);
end;

function WinCreate:HWnd;

var hWindow:HWnd;
    submenu : hmenu;


 begin
 hWindow:=CreateWindow (AppName,'MenuDemo',
                        ws_OverlappedWindow,cw_UseDefault,cw_UseDefault,
                        cw_UseDefault,cw_UseDefault,0,0,system.MainInstance,nil);
 menu:=CreateMenu;
 submenu:=createmenu;
 AppendMenu (submenu,MF_STRING,101,'&New...');
 AppendMenu (submenu,MF_STRING,102,'&Open...');
 AppendMenu (Submenu,MF_STRING,103,'&Save...');
 AppendMenu (Submenu,MF_SEPARATOR,0,Nil);
 AppendMenu (SubMenu,MF_String,104,'E&xit');
 AppendMenu (Menu,MF_POPUP,SubMenu,'&Files');
 submenu:=createmenu;
  AppendMenu (SubMenu,MF_String,201,'&Cut');
 AppendMenu (SubMenu,MF_String,202,'&Copy');
 AppendMenu (SubMenu,MF_STRING,203,'&Paste');
 AppendMenu (Menu,MF_POPUP,SubMenu,'&Edit');
 SubMenu:=CreateMenu;
 AppendMenu (SubMenu,MF_STring,301,'&Settings');
 AppendMenu (menu,MF_STRING,0,'&Options');
 AppendMenu (menu,MF_STRING,0,'&Help');
 SetMenu (hWindow,menu);
 if hWindow<>0 then
   begin
   ShowWindow (hWindow,CmdShow);
   UpdateWindow(hWindow);
   end;
 Result:=hWindow;
 end;

function StatusCreate (parent:hwnd):HWnd;

begin
 StatusCreate:=CreateStatusWindow (WS_CHILD or
  WS_VISIBLE,'Ready...',parent,$7712);
end;

begin
if not WinRegister then
 begin MessageBox (0,'Register failed',nil, mb_Ok); Exit; end;
hWindow:=WinCreate;
if longint(hWindow)=0 then
  begin
  MessageBox (0,'WinCreate failed',nil,mb_Ok);
  Exit;
  end
Else
  Hedit:=EditCreate(HWindow);
hstatus:=statuscreate (hwindow);
while GetMessage (@AMessage,0,0,0) do
 begin
 TranslateMessage (AMessage);
 DispatchMessage (AMessage);
 end;
Halt (AMessage.wParam);

end.
{
  $Log$
  Revision 1.3  1999-06-28 16:15:11  peter
    * fixed dup id

  Revision 1.2  1999/05/03 18:04:39  peter
    * updates

}

