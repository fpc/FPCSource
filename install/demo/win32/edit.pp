{
  $Id$
  Copyright (c) 1999 by Michael van Canneyt and Goran Andersson

  Win32 editor example.
}

{ Derived from menu.pp

  Changes by Goeran Andersson:

  2000.02.20
    Sends focus to editor

  2000.02.19
    Client edge added to editor
    Changes to also compile in FPC mode
    Handles Edit modify flag
    Undo menu item added
    Key codes added to edit menu
    Undo, Cut, Copy & Paste implemented
    WM_Paint sections commented


  1999.08.10
    LoadText() added
    NewText() added
    File selector added
    Asks to save file
    Empty files works
    EditCreate styles corrected

2do:
  reduce flickering when resizing
  use the status bar for something
}

Program editdemo;
{$APPTYPE GUI}
{$MODE DELPHI}

Uses Strings,Windows;

Const AppName = 'EditDemo';

Var AMessage: Msg;
    HWindow,HStatus,HEdit: HWnd;

Type
  TFileName = Array[0..Max_Path] Of Char;

Function SelectFile(Var FName:TFileName; Open:Boolean): Boolean;

Const
  Filter: PChar = 'Text files (*.txt)'#0'*.txt'#0'All files (*.*)'#0'*.*'#0;
  Ext: PChar = 'txt';

Var
  NameRec: OpenFileName;
Begin
  FillChar(NameRec,SizeOf(NameRec),0);
  FName[0] := #0;
  With NameRec Do
    Begin
      LStructSize := SizeOf(NameRec);
      HWndOwner := HWindow;
      LpStrFilter := Filter;
      LpStrFile := @FName;
      NMaxFile := Max_Path;
      Flags := OFN_Explorer Or OFN_HideReadOnly;
      If Open Then
        Begin
          Flags := Flags Or OFN_FileMustExist;
        End;
      LpStrDefExt := Ext;
    End;
  If Open Then
      SelectFile := GetOpenFileName(@NameRec)
  Else
      SelectFile := GetSaveFileName(@NameRec);
End;

Procedure SaveText;

Var Len: Longint;
    P: PChar;
    F: File;
    FName: TFileName;
Begin
  If SelectFile(FName,False) Then
    Begin
      Assign(F,@FName);
      Rewrite(F,1);
      Len := GetWindowTextLength(HEdit);
      GetMem(P,Len+1);
      P[Len] := #0;
      If Len>0 Then
        Begin
          GetWindowText(HEdit,P,Len+1);
          BlockWrite(F,P^,Len);
        End;
      Close(F);
      FreeMem(P,Len+1);
    End;
End;

Procedure AskSave;

Const
  BoxType=MB_IconQuestion Or MB_YesNo;
Begin
  If SendMessage(HEdit,EM_GetModify,0,0)<>0 Then
    Begin
      If MessageBox(HWindow,'Save text?','Edited',BoxType)=IdYes Then
        Begin
          SaveText;
        End;
    End;
End;

Procedure LoadText;

Var
  FName: TFileName;
  F: File;
  Len: LongInt;
  P: PChar;
Begin
  AskSave;
  If SelectFile(FName,True) Then
    Begin
      Assign(F,@FName);
      Reset(F,1);
      Len := FileSize(F);
      GetMem(P,Len+1);
      P[Len] := #0;
      If Len>0 Then BlockRead(F,P^,Len);
      Close(F);
      SetWindowText(HEdit,P);
      SendMessage(HEdit,EM_SetModify,0,0);
      FreeMem(P,Len+1);
    End;
End;

Procedure NewText;

Const
  Empty: PChar = '';
Begin
  AskSave;
  SendMessage(HEdit,WM_SetText,1,LongInt(Empty));
  SendMessage(HEdit,EM_SetModify,0,0);
End;

Function WindowProc (Window:HWnd;AMessage,WParam,LParam:Longint): Longint;
stdcall;
export;

Var
    R: rect;
    StatH: Word;
    NrMenu : Longint;
//  ps: paintstruct;
Begin
  WindowProc := 0;
  Case AMessage Of
{
    wm_Paint:
              Begin
                GetClientRect(Window,@R);
                BeginPaint(Window,@ps);
                ...
                EndPaint(Window,@ps);
                Exit;
              End;
}
    wm_Close:
              Begin
                AskSave;
              End;
    wm_Destroy:
                Begin
                  PostQuitMessage (0);
                  Exit;
                End;
    wm_SetFocus:
                 Begin
                   SetFocus(HEdit);
                 End;
    wm_Size:
             Begin
               GetClientRect(HStatus,@R);
               StatH := R.Bottom-R.Top;
               GetClientRect(Window,@R);
               MoveWindow (HEdit,0,0,R.Right,R.Bottom-StatH,False);
               MoveWindow (HStatus,0,R.Bottom-StatH,R.Right,R.Bottom,False);
             End;
    wm_Command:
                Begin
                  NrMenu := LoWord(WParam);
                  Case NrMenu Of
                    101 : NewText;
                    102 : LoadText;
                    103 : SaveText;
                    104 : PostMessage(Window,WM_Close,0,0);
                    201 : SendMessage(HEdit,WM_Undo,0,0);
                    202 : SendMessage(HEdit,WM_Cut,0,0);
                    203 : SendMessage(HEdit,WM_Copy,0,0);
                    204 : SendMessage(HEdit,WM_Paste,0,0);
                    301 : MessageBox(Window,'Options','Not implemented',
                                     MB_OK Or MB_IconInformation);
                    401 : MessageBox(Window,'Help','Not implemented',
                                     MB_OK Or MB_IconInformation);
                  End;
                End;
  End;
  WindowProc := DefWindowProc(Window,AMessage,WParam,LParam);
End;

Function WinRegister: Boolean;

Var
  WindowClass: WndClass;
Begin
  With WindowClass Do
    Begin
      Style := cs_hRedraw Or cs_vRedraw;
      lpfnWndProc := WndProc(@WindowProc);
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := system.MainInstance;
      hIcon := LoadIcon (0,idi_Application);
      hCursor := LoadCursor (0,idc_Arrow);
      hbrBackground := GetStockObject(GRAY_BRUSH);
      lpszMenuName := 'Files';
      lpszClassName := AppName;
    End;
  WinRegister := RegisterClass (WindowClass)<>0;
End;

Function EditCreate(ParentWindow,Status:HWnd): HWnd;

Const
  CS_Start = WS_Child or WS_HScroll or WS_VScroll or ES_MultiLine or ES_Left;
  CS_Ex = WS_EX_ClientEdge;
  EdiTText: PChar = '';

Var
  HEdit: HWND;
  R: TRect;
  StatH: Word;
{                                             rev 1.5 : comment out
  Procedure AddText (S:String);
  begin
    S:=S+#0;
    SendMessage(HEdit,em_replacesel,0,longint(pchar(@S[1])));
  end;
}
Begin
  GetClientRect(Status,@R);
  StatH := R.Bottom-R.Top;
  GetClientRect(ParentWindow,@R);
  HEdit := CreateWindowEx (CS_Ex,'EDIT',EditText,CS_Start,0,0,
                           R.Right-R.Left,R.Bottom-R.Top-StatH,ParentWindow,0,
                           MainInstance,Nil);
  If HEdit<>0 Then
    Begin
      ShowWindow(Hedit,SW_Show);
      UpdateWindow(HEdit);
    End;
  EditCreate := HEdit;
End;

Function WinCreate: HWnd;

Var hWindow: HWnd;
    Menu: hMenu;
    SubMenu: hMenu;
Begin
  hWindow := CreateWindow (AppName,'EditDemo',ws_OverlappedWindow,
                           cw_UseDefault,cw_UseDefault,cw_UseDefault,
                           cw_UseDefault,0,0,MainInstance,Nil);
  If hWindow<>0 Then
    Begin
      Menu := CreateMenu;
      SubMenu := CreateMenu;
      AppendMenu(Submenu,MF_STRING,101,'&New...');
      AppendMenu(Submenu,MF_STRING,102,'&Open...');
      AppendMenu(Submenu,MF_STRING,103,'&Save...');
      AppendMenu(Submenu,MF_SEPARATOR,0,Nil);
      AppendMenu(SubMenu,MF_String,104,'E&xit');
      AppendMenu(Menu,MF_POPUP,SubMenu,'&Files');
      SubMenu := CreateMenu;
      AppendMenu(SubMenu,MF_String,201,'&Undo'#8'Ctrl+Z');
      AppendMenu(Submenu,MF_SEPARATOR,0,Nil);
      AppendMenu(SubMenu,MF_String,202,'&Cut'#8'Ctrl+X');
      AppendMenu(SubMenu,MF_String,203,'&Copy'#8'Ctrl+C');
      AppendMenu(SubMenu,MF_STRING,204,'&Paste'#8'Ctrl+V');
      AppendMenu(Menu,MF_POPUP,SubMenu,'&Edit');
      SubMenu := CreateMenu;
      AppendMenu(SubMenu,MF_String,301,'&Settings');
      AppendMenu(Menu,MF_POPUP,SubMenu,'&Options');
      AppendMenu(Menu,MF_STRING,401,'&Help');
      SetMenu(hWindow,menu);
      ShowWindow(hWindow,SW_Show);
      UpdateWindow(hWindow);
    End;
  WinCreate := hWindow;
End;

Function StatusCreate (parent:hwnd): HWnd;
Begin
  StatusCreate := CreateStatusWindow (WS_CHILD Or WS_VISIBLE,'Ready...',parent,$7712);
End;

Begin
  If Not WinRegister Then
    Begin
      MessageBox (0,'Register failed',Nil, mb_Ok);
    End
  Else
    Begin
      hWindow := WinCreate;
      If longint(hWindow)=0 Then
        Begin
          MessageBox (0,'WinCreate failed',Nil,MB_OK);
        End
      Else
        Begin
          HStatus := statuscreate(hwindow);
          HEdit := EditCreate(HWindow,HStatus);
          SetFocus(HEdit);
          While GetMessage(@AMessage,0,0,0) Do
          Begin
            TranslateMessage(AMessage);
            DispatchMessage(AMessage);
          End;
          Halt(AMessage.wParam);
        End;
    End;
End.

{
  $Log$
  Revision 1.1  2000-02-20 20:33:37  florian
    * Initial revision

}
