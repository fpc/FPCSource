{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Debug menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.DoUserScreenWindow;
begin
  if UserScreenWindow=nil then
    begin
     New(UserScreenWindow, Init(UserScreen, SearchFreeWindowNo));
     Desktop^.Insert(UserScreenWindow);
    end;
  UserScreenWindow^.MakeFirst;
end;

procedure TIDEApp.DoCloseUserScreenWindow;
begin
  if Assigned(UserScreenWindow) then
    Message(UserScreenWindow,evCommand,cmClose,nil);
end;

procedure TIDEApp.DoUserScreen;
var Event : TEvent;
    Clear : Boolean;
begin
  if UserScreen=nil then
   begin
     ErrorBox(msg_userscreennotavailable,nil);
     Exit;
   end;

  ShowUserScreen;

  InitKeyBoard;
  repeat
    repeat
      Drivers.GetKeyEvent(Event);
      if Event.What=evNothing then
        GiveUpTimeSlice;
    until Event.What=evKeyboard;
    Clear:=true;
    case Event.keycode of
      kbPgUp : UserScreen^.Scroll(-20);
      kbPgDn : UserScreen^.Scroll(20);
      kbUp : UserScreen^.Scroll(-1);
      kbDown : UserScreen^.Scroll(1);
      kbHome : UserScreen^.Scroll(-1024);
      kbEnd : UserScreen^.Scroll(+1024);
      else
        Clear:=false;
    end;
    if Clear then
      ClearEvent(Event);
  until Event.what=evKeyboard;
  while (Keyboard.PollKeyEvent<>0) do
   Keyboard.GetKeyEvent;
  DoneKeyboard;

  ShowIDEScreen;
end;

procedure TIDEApp.DoShowCallStack;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(StackWindow) then
    InitStackWindow
  else
    StackWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowRegisters;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(RegistersWindow) then
    InitRegistersWindow
  else
    RegistersWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowFPU;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(FPUWindow) then
    InitFPUWindow
  else
    FPUWindow^.MakeFirst;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowBreakpointList;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If assigned(BreakpointsWindow) then
    begin
      BreakpointsWindow^.Update;
      BreakpointsWindow^.Show;
      BreakpointsWindow^.MakeFirst;
    end
  else
    begin
      New(BreakpointsWindow,Init);
      Desktop^.Insert(BreakpointsWindow);
    end;
{$endif NODEBUG}
end;

procedure TIDEApp.DoShowWatches;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If assigned(WatchesWindow) then
    begin
      WatchesWindow^.Update;
      WatchesWindow^.MakeFirst;
    end
  else
    begin
      New(WatchesWindow,Init);
      Desktop^.Insert(WatchesWindow);
    end;
{$endif NODEBUG}
end;

procedure TIDEApp.DoAddWatch;
{$ifdef NODEBUG}
begin
  NoDebugger;
end;
{$else}
var
  P: PWatch;
  EditorWindow : PSourceWindow;
  EditorWasFirst : boolean;
  S : string;
begin
  EditorWindow:=FirstEditorWindow;
  { Leave the editor first, but only if there was already an WatchesWindow }
  EditorWasFirst:=(PWindow(Desktop^.First)=PWindow(EditorWindow)) and
                  assigned(WatchesWindow);
  If assigned(EditorWindow) then
    S:={LowerCaseStr(}EditorWindow^.Editor^.GetCurrentWord
  else
    S:='';
  P:=New(PWatch,Init(S));
  if ExecuteDialog(New(PWatchItemDialog,Init(P)),nil)<>cmCancel then
    begin
      WatchesCollection^.Insert(P);
      WatchesCollection^.Update;
      DoShowWatches;
      if EditorWasFirst then
       EditorWindow^.MakeFirst;
    end
  else
    dispose(P,Done);
end;
{$endif NODEBUG}

{
  $Log$
  Revision 1.3  2002-09-03 13:59:09  pierre
   * don't use LowerCaseStr for AddWatch as it confuses line completion

  Revision 1.2  2002/09/02 09:27:35  pierre
   * avoid 100 CPU usage when AltF5 is used

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.3  2001/06/13 16:15:36  pierre
   * avoid crash if GDB gives an error while run is executed

  Revision 1.1.2.2  2001/03/09 15:06:27  pierre
   + DoShowFPU procedure

  Revision 1.1.2.1  2001/01/07 22:36:56  peter
    * Ctrl-F7 will stay in the editor if the watch window was already
      open

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.13  2000/05/17 10:17:49  pierre
   * Reinit the keyboard in ShowUserScreen

  Revision 1.12  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.11  2000/02/04 00:13:59  pierre
   * AddWatch calls DoShowWatches

  Revision 1.10  2000/01/08 18:26:20  florian
    + added a register window, doesn't work yet

  Revision 1.9  1999/09/22 16:18:19  pierre
   + TIDEApp.DoCloseUserScreenWindow

  Revision 1.8  1999/09/09 14:20:05  pierre
   + Stack Window

  Revision 1.7  1999/07/28 23:11:19  peter
    * fixes from gabor

  Revision 1.6  1999/07/10 01:24:19  pierre
   + First implementation of watches window

  Revision 1.5  1999/06/30 23:58:17  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.4  1999/06/25 00:36:51  pierre
   + missing Debug menu added (not implemented yet)

  Revision 1.3  1999/02/02 16:41:40  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.2  1999/01/21 11:54:16  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.1  1998/12/28 15:47:47  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
