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
  Revision 1.4  2002-09-07 15:40:43  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/09/03 13:59:09  pierre
   * don't use LowerCaseStr for AddWatch as it confuses line completion

  Revision 1.2  2002/09/02 09:27:35  pierre
   * avoid 100 CPU usage when AltF5 is used

}
