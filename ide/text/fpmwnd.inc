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
      procedure  UpdateList;
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
    hcMessagesWindow:S:=PMessagesWindow(P)^.GetTitle(MaxLen);
    hcGDBWindow,
    hcWatches,
    hcStack,
    hcRegisters,
    hcClipboardWindow,
    hcASCIITableWindow,
    hcBreakpointListWindow :
      S:=PWindow(P)^.GetTitle(MaxLen);
  else S:='???? - '+PWindow(P)^.GetTitle(MaxLen);
  end;
  if PWindow(P)^.Number<>0 then
    S:=S+'('+IntToStr(PWindow(P)^.Number)+')';
  GetText:=copy(S,1,MaxLen);
end;

constructor TWindowListDialog.Init;
var R,R2: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,50,15);
  inherited Init(R, 'Window List');

  New(C, Init(20,10));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.X:=37;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(LB, Init(R, SB));
  LB^.Default:=true;
  LB^.NewList(C);
  UpdateList;
  if C^.Count>=2 then LB^.FocusItem(1); { focus the 2nd one }
  Insert(LB);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, '~W~indows', LB)));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.A.X:=38; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
  R.Move(0,3);
  Insert(New(PButton, Init(R, '~D~elete', cmDeleteItem, bfNormal)));
  R.Move(0,3);
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

  LB^.Select;
end;

procedure TWindowListDialog.UpdateList;
procedure AddIt(P: PView); {$ifndef FPC}far;{$endif}
begin
  if (P<>pointer(Desktop^.Background)) and (P^.GetState(sfVisible)) then
     C^.Insert(P);
end;
begin
  C^.DeleteAll;
  Desktop^.ForEach(@AddIt);
  LB^.SetRange(C^.Count);
  ReDraw;
end;

procedure TWindowListDialog.HandleEvent(var Event: TEvent);
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
    evCommand :
      case Event.Command of
        cmDeleteItem :
          if C^.Count>0 then
          begin
            Message(C^.At(LB^.Focused),evCommand,cmClose,nil);
            UpdateList;
            ClearEvent(Event);
          end;
        cmOK :
          if C^.Count>0 then
            PView(C^.At(LB^.Focused))^.MakeFirst;
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
end;

{
  $Log$
  Revision 1.14  2000-01-28 22:36:46  pierre
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