{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Window menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.CloseAll;

  procedure SendClose(P: PView); {$ifndef FPC}far;{$endif}
  begin
    Message(P,evCommand,cmClose,nil);
  end;

begin
  Desktop^.ForEach(@SendClose);
end;

procedure TIDEApp.ResizeApplication(x, y : longint);
var
  OldR, R : TRect;
  Mode: TVideoMode;
begin
  GetBounds(OldR);
  { adapt to new size }
  if (OldR.B.Y-OldR.A.Y<>y) or
     (OldR.B.X-OldR.A.X<>x) then
    begin
      Mode.color:=ScreenMode.Color;
      Mode.col:=x;
      Mode.row:=y;
      SetScreenVideoMode(Mode);
      Redraw;
    end;
end;

type
    PWindowListBox = ^TWindowListBox;
    TWindowListBox = object(TAdvancedListBox)
      constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
    end;

    PWindowListDialog = ^TWindowListDialog;
    TWindowListDialog = object(TCenterDialog)
      constructor Init;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      LB: PWindowListBox;
      C : PCollection;
      BtnShow,BtnHide: PNoUpdateButton;
      procedure  UpdateList;
      procedure  UpdateButtons;
    end;

constructor TWindowListBox.Init(var Bounds: TRect; AScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AScrollBar);
end;

function TWindowListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PView;
    S: string;
begin
  P:=List^.At(Item);
  case P^.HelpCtx of
    hcSourceWindow : S:=PSourceWindow(P)^.GetTitle(MaxLen);
    hcInfoWindow   : S:=PProgramInfoWindow(P)^.GetTitle(MaxLen);
    hcHelpWindow   : S:=PHelpWindow(P)^.GetTitle(MaxLen);
    hcCalcWindow   : S:=PCalculator(P)^.GetTitle(MaxLen);
    hcBrowserWindow: S:=PBrowserWindow(P)^.GetTitle(MaxLen);
    hcCompilerMessagesWindow,
    hcMessagesWindow:S:=PMessagesWindow(P)^.GetTitle(MaxLen);
    hcGDBWindow,
    hcDisassemblyWindow,
    hcWatchesWindow,
    hcStackWindow,
    hcRegistersWindow,
    hcFPURegisters,
    hcClipboardWindow,
    hcASCIITableWindow,
    hcUserScreenWindow,
    hcBreakpointListWindow :
      S:=PWindow(P)^.GetTitle(MaxLen);
  else S:='???? - '+PWindow(P)^.GetTitle(MaxLen);
  end;
  if PWindow(P)^.Number<>0 then
    S:=S+'('+IntToStr(PWindow(P)^.Number)+')';
  if P^.GetState(sfVisible) then S:=' '+S else
    begin
      S:='*'+S+' - '+msg_windowlist_hidden;
    end;
  GetText:=copy(S,1,MaxLen);
end;

constructor TWindowListDialog.Init;
var R,R2: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,round(ScreenWidth*5/8),15);
  inherited Init(R, dialog_windowlist);
  HelpCtx:=hcWindowList;
  New(C, Init(20,10));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.X:=R.B.X-13;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(LB, Init(R, SB));
  LB^.Default:=true;
  LB^.NewList(C);
  UpdateList;
  if C^.Count>=2 then
   if PWindow(C^.At(1))^.GetState(sfVisible) then
    LB^.FocusItem(1); { focus the 2nd one }
  Insert(LB);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_wndlist_windows, LB)));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.A.X:=R.B.X-13+1; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, button_OK, cmOK, bfDefault)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Delete, cmDeleteItem, bfNormal)));
  R.Move(0,2);
  New(BtnShow, Init(R, button_Show, cmShowItem, bfNormal));
  Insert(BtnShow);
  R.Move(0,2);
  New(BtnHide, Init(R, button_Hide, cmHideItem, bfNormal));
  Insert(BtnHide);
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));

  LB^.Select;
  PutCommand(@Self,evBroadcast,cmListFocusChanged,LB);
end;

procedure TWindowListDialog.UpdateList;
var VisState: boolean;
procedure AddIt(P: PView); {$ifndef FPC}far;{$endif}
begin
  if (P<>pointer(Desktop^.Background)) and
     (P^.GetState(sfDisabled)=false) and
     ((P^.Options and ofSelectable)<>0) and
     (P^.GetState(sfVisible)=VisState) then
     C^.Insert(P);
end;
begin
  C^.DeleteAll;
  VisState:=true; Desktop^.ForEach(@AddIt); { add visible windows to list }
  VisState:=false; Desktop^.ForEach(@AddIt); { add hidden windows }
  LB^.SetRange(C^.Count);
  UpdateButtons;
  ReDraw;
end;

procedure TWindowListDialog.UpdateButtons;
var W: PView;
begin
  if LB^.Range>0 then
    begin
      W:=LB^.List^.At(LB^.Focused);
      if Assigned(BtnShow) then
        BtnShow^.SetState(sfDisabled,W^.GetState(sfVisible));
      if Assigned(BtnHide) then
        BtnHide^.SetState(sfDisabled,not W^.GetState(sfVisible));
    end
  else
    begin
      BtnShow^.SetState(sfDisabled,true);
      BtnHide^.SetState(sfDisabled,true);
    end;
  ReDraw;
end;

procedure TWindowListDialog.HandleEvent(var Event: TEvent);
var W: PWindow;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbDel :
          begin
            Message(@Self,evCommand,cmDeleteItem,nil);
            ClearEvent(Event);
          end;
      end;
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
          if Event.InfoPtr=LB then
            UpdateButtons;
      end;
    evCommand :
      case Event.Command of
        cmDeleteItem :
          if C^.Count>0 then
          begin
            Message(C^.At(LB^.Focused),evCommand,cmClose,nil);
            UpdateList;
            ClearEvent(Event);
          end;
        cmShowItem :
          if C^.Count>0 then
          begin
            PWindow(C^.At(LB^.Focused))^.Show;
            UpdateList;
            ClearEvent(Event);
          end;
        cmHideItem :
          if C^.Count>0 then
          begin
            PWindow(C^.At(LB^.Focused))^.Hide;
            UpdateList;
            ClearEvent(Event);
          end;
        cmOK :
          if C^.Count>0 then
          begin
            W:=PWindow(C^.At(LB^.Focused));
            if W^.GetState(sfVisible)=false then
              W^.Show;
            W^.MakeFirst;
          end;
      end;
  end;
  inherited HandleEvent(Event);
end;

destructor TWindowListDialog.Done;
begin
  if C<>nil then begin C^.DeleteAll; Dispose(C, Done); end;
  inherited Done;
end;

procedure TIDEApp.WindowList;
var W: PWindowListDialog;
begin
  New(W,Init);
  ExecView(W);
  Dispose(W,Done);
  if assigned(Desktop^.Current) then
    begin
      Desktop^.Lock;
      { force correct commands to be enabled }
      Desktop^.Current^.SetState(sfActive,false);
      Desktop^.Current^.SetState(sfActive,true);
      Desktop^.UnLock;
    end;
end;

{
  $Log$
  Revision 1.3  2002-06-07 14:09:56  pierre
   * try to get resizing to work

  Revision 1.2  2002/05/30 15:03:24  pierre
   + ResizeApplication pethod for fvision

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.8  2001/03/12 17:34:56  pierre
   + Disassembly window started

  Revision 1.1.2.7  2001/03/09 15:04:30  pierre
   + recognize FPU window

  Revision 1.1.2.6  2000/11/29 11:26:00  pierre
   + TFPDlgWindow that handles cmSearchWindow

  Revision 1.1.2.5  2000/11/29 00:54:45  pierre
   + preserve window number and save special windows

  Revision 1.1.2.4  2000/11/23 16:33:31  pierre
   * fix Alt-X problem and set HelpCtx for most dialogs

  Revision 1.1.2.3  2000/10/31 07:44:33  pierre
   * Window List problem solved in a different way

  Revision 1.1.2.2  2000/10/24 00:21:59  pierre
   * fix the greyed save after window list box

  Revision 1.1.2.1  2000/08/15 03:40:53  peter
   [*] no more fatal exits when the IDE can't find the error file (containing
       the redirected assembler/linker output) after compilation
   [*] hidden windows are now added always at the end of the Window List
   [*] TINIFile parsed entries encapsulated in string delimiters incorrectly
   [*] selection was incorrectly adjusted when typing in overwrite mode
   [*] the line wasn't expanded when it's end was reached in overw. mode
   [*] the IDE now tries to locate source files also in the user specified
       unit dirs (for ex. as a response to 'Open at cursor' (Ctrl+Enter) )
   [*] 'Open at cursor' is now aware of the extension (if specified)

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.18  2000/06/16 08:50:41  pierre
   + new bunch of Gabor's changes

  Revision 1.17  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.16  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.15  2000/03/07 21:51:52  pierre
   * Reconginze UserScreenWindow

  Revision 1.14  2000/01/28 22:36:46  pierre
   * avoid unknown type in alt+0 dialog for Ascii Chart and Clipboard

  Revision 1.13  2000/01/10 00:24:18  pierre
   * register window type added

  Revision 1.12  1999/09/09 14:09:58  pierre
   + StackWindow is a known window type

  Revision 1.11  1999/07/10 01:24:20  pierre
   + First implementation of watches window

  Revision 1.10  1999/06/30 23:58:18  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.9  1999/06/29 22:50:15  peter
    * more fixes from gabor

  Revision 1.8  1999/06/28 19:32:23  peter
    * fixes from gabor

  Revision 1.7  1999/03/01 15:42:00  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.6  1999/01/21 11:54:22  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.5  1999/01/12 14:29:37  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.4  1999/01/04 11:49:49  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.3  1998/12/28 15:47:50  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.2  1998/12/23 22:58:19  peter
    * fixed windowlist for fpc

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:50  peter
    + options are now written/read
    + find and replace routines

}
