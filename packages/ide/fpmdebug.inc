{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Debug menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef SUPPORT_REMOTE}
procedure TIDEApp.TransferRemote;
var
  DoSendCommand : string;
  Executed : boolean;
begin
  DoSendCommand:=TransformRemoteString(RemoteSendCommand);
  if DoSendCommand<>'' then
    begin
      Executed:=DoExecute(DoSendCommand,'','','send___.out','send___.err',exNormal);
      if Executed then
        begin
         if (DosError<>0) or (DosExitCode<>0) then
          ErrorBox(#3'Execution of'#13#3+DoSendCommand+#13#3+
            'returned ('+inttostr(DosError)+','+inttostr(DosExitCode)+')',nil);
         ProcessMessageFile('send___.out');
         ProcessMessageFile('send___.err');
         UpdateToolMessages;
        end
      else
        ErrorBox(#3'Unable to execute'#13#3+DoSendCommand,nil);

    end
  else
    ErrorBox(#3'Unable to transfer executable',nil);
end;
{$endif SUPPORT_REMOTE}

procedure TIDEApp.DoUserScreenWindow;
begin
  {$ifdef HASAMIGA}
  Exit; // Do not open the Userscreen on AMIGA systems, its not closeable
  {$endif}
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
    ev    : TMouseEvent;
    Clear : Boolean;
begin
  if UserScreen=nil then
   begin
     ErrorBox(msg_userscreennotavailable,nil);
     Exit;
   end;

  ShowUserScreen;

  InitKeyBoard;
  { closing the user screen on mouse events makes copy paste impossible }
  repeat
    repeat
      GiveUpTimeSlice;
      Drivers.GetKeyEvent(Event);
    until Event.What=evKeyboard;
    Clear:=true;
    if not UserScreen^.CanScroll then
      Clear:=false
    else
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


procedure TIDEApp.DoShowDisassembly;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(DisassemblyWindow) then
    InitDisassemblyWindow
  else
    DisassemblyWindow^.MakeFirst;
  DisassemblyWindow^.LoadFunction('');
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


procedure TIDEApp.DoShowVector;
begin
{$ifdef NODEBUG}
  NoDebugger;
{$else}
  If not assigned(VectorWindow) then
    InitVectorWindow
  else
    VectorWindow^.MakeFirst;
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

{$ifdef NODEBUG}
procedure TIDEapp.do_evaluate;

begin
  nodebugger;
end;
{$else}
procedure TIDEapp.do_evaluate;

var d:Pevaluate_dialog;
    r:Trect;

begin
  desktop^.getextent(r);
  r.b.x:=r.b.x*3 div 4;
  r.b.y:=12;
  new(d,init(r));
  desktop^.execview(d);
  dispose(d,done);
end;
{$endif}

