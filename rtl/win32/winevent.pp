{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Event Handling unit for setting Keyboard and Mouse Handlers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WinEvent;
interface

{
   We need this unit to implement keyboard and mouse,
   because win32 uses only one message queue for mouse and key events
}

    type
       TEventProcedure = Procedure;

    { these procedures must be used to set the event handlers }
    { these doesn't do something, they signal only the        }
    { the upper layer that an event occured, this event       }
    { must be handled with Win32-API function by the upper    }
    { layer                                                   }
    Procedure SetMouseEventHandler(p : TEventProcedure);
    Procedure SetKeyboardEventHandler(p : TEventProcedure);
    Procedure SetFocusEventHandler(p : TEventProcedure);
    Procedure SetMenuEventHandler(p : TEventProcedure);
    Procedure SetResizeEventHandler(p : TEventProcedure);
    Procedure SetUnknownEventHandler(p : TEventProcedure);

    { these procedures must be used to get the event handlers }
    Function GetMouseEventHandler : TEventProcedure;
    Function GetKeyboardEventHandler : TEventProcedure;
    Function GetFocusEventHandler : TEventProcedure;
    Function GetMenuEventHandler : TEventProcedure;
    Function GetResizeEventHandler : TEventProcedure;
    Function GetUnknownEventHandler : TEventProcedure;

  implementation

    uses
       windows, dos;

    const
       { these procedures are called if an event occurs }
       MouseEventHandler : procedure = nil;
       KeyboardEventHandler : procedure = nil;
       FocusEventHandler : procedure = nil;
       MenuEventHandler : procedure = nil;
       ResizeEventHandler : procedure = nil;
       UnknownEventHandler  : procedure = nil;

       { if this counter is zero, the event handler thread is killed }
       InstalledHandlers : Byte = 0;

    var
       HandlerChanging : TCriticalSection;
       OldExitProc : Pointer;
       EventThreadHandle : Handle;
       EventThreadID : DWord;

       { true, if the event handler should be stoped }
       ExitEventHandleThread : boolean;

    Function GetMouseEventHandler : TEventProcedure;
      begin
         GetMouseEventHandler:=MouseEventHandler;
      end;


    Function GetKeyboardEventHandler : TEventProcedure;
      begin
         GetKeyboardEventHandler:=KeyboardEventHandler;
      end;


    Function GetFocusEventHandler : TEventProcedure;
      begin
         GetFocusEventHandler:=FocusEventHandler;
      end;


    Function GetMenuEventHandler : TEventProcedure;
      begin
         GetMenuEventHandler:=MenuEventHandler;
      end;


    Function GetResizeEventHandler : TEventProcedure;
      begin
         GetResizeEventHandler:=ResizeEventHandler;
      end;


    Function GetUnknownEventHandler : TEventProcedure;
      begin
         GetUnknownEventHandler:=UnknownEventHandler;
      end;

    { removes an event from the event queue }
    { necessary, if no handler is installed }
    Procedure DestroyOneEvent;
      var
         ir : TInputRecord;
         dwRead : DWord;
      begin
         ReadConsoleInput(TextRec(Input).Handle,ir,1,dwRead);
      end;

    Function EventHandleThread(p : pointer) : DWord;StdCall;
      var
         ir : TInputRecord;
         dwRead : DWord;
      begin
         while not(ExitEventHandleThread) do
           begin
              { wait for an event }
              WaitForSingleObject(TextRec(Input).Handle,INFINITE);
              { guard this code, else it is doomed to crash, if the
                thread is switched between the assigned test and
                the call and the handler is removed
              }
              if not(ExitEventHandleThread) then
                begin
                   EnterCriticalSection(HandlerChanging);
                   { read, but don't remove the event }
                   if (PeekConsoleInput(TextRec(Input).Handle,ir,1,dwRead)) and
                     (dwRead>0) then
                     { call the handler }
                     case ir.EventType of
                        KEY_EVENT:
                          begin
                             if assigned(KeyboardEventHandler) then
                               KeyboardEventHandler
                             else
                               DestroyOneEvent;
                          end;

                        _MOUSE_EVENT:
                          begin
                             if assigned(MouseEventHandler) then
                               MouseEventHandler
                             else
                               DestroyOneEvent;
                          end;

                        WINDOW_BUFFER_SIZE_EVENT:
                          begin
                             if assigned(ResizeEventHandler) then
                               ResizeEventHandler
                             else
                               DestroyOneEvent;
                          end;

                        MENU_EVENT:
                          begin
                             if assigned(MenuEventHandler) then
                               MenuEventHandler
                             else
                               DestroyOneEvent;
                          end;

                        FOCUS_EVENT:
                          begin
                             if assigned(FocusEventHandler) then
                               FocusEventHandler
                             else
                               DestroyOneEvent;
                          end;

                        else
                          begin
                             if assigned(UnknownEventHandler) then
                               UnknownEventHandler
                             else
                               DestroyOneEvent;
                          end;
                     end;
                   LeaveCriticalSection(HandlerChanging);
                end;
           end;
      end;

    Procedure NewEventHandlerInstalled(p,oldp : TEventProcedure);
      var
         oldcount : Byte;
         ir : TInputRecord;
         written : DWord;
      begin
         oldcount:=InstalledHandlers;
         if Pointer(oldp)<>nil then
           dec(InstalledHandlers);
         if Pointer(p)<>nil then
           inc(InstalledHandlers);
         { start event handler thread }
         if (oldcount=0) and (InstalledHandlers=1) then
           begin
              ExitEventHandleThread:=false;
              EventThreadHandle:=CreateThread(nil,0,@EventHandleThread,
                nil,0,EventThreadID);
           end
         { stop and destroy event handler thread }
         else if (oldcount=1) and (InstalledHandlers=0) then
           begin
              ExitEventHandleThread:=true;
              { create a dummy event and sent it to the thread, so
                we can leave WatiForSingleObject }
              ir.EventType:=KEY_EVENT;
              { mouse event can be disabled by mouse.inc code
                in DoneMouse
                so use a key event instead PM }
              WriteConsoleInput(TextRec(Input).Handle,ir,1,written);
              { wait, til the thread is ready }
              WaitForSingleObject(EventThreadHandle,INFINITE);
              CloseHandle(EventThreadHandle);
           end;
      end;


    Procedure SetMouseEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=MouseEventHandler;
         MouseEventHandler:=p;
         NewEventHandlerInstalled(MouseEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


    Procedure SetKeyboardEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=KeyboardEventHandler;
         KeyboardEventHandler:=p;
         NewEventHandlerInstalled(KeyboardEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


    Procedure SetFocusEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=FocusEventHandler;
         FocusEventHandler:=p;
         NewEventHandlerInstalled(FocusEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


    Procedure SetMenuEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=MenuEventHandler;
         MenuEventHandler:=p;
         NewEventHandlerInstalled(MenuEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


    Procedure SetResizeEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=ResizeEventHandler;
         ResizeEventHandler:=p;
         NewEventHandlerInstalled(ResizeEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


    Procedure SetUnknownEventHandler(p : TEventProcedure);
      var
         oldp : TEventProcedure;
      begin
         EnterCriticalSection(HandlerChanging);
         oldp:=UnknownEventHandler;
         UnknownEventHandler:=p;
         NewEventHandlerInstalled(UnknownEventHandler,oldp);
         LeaveCriticalSection(HandlerChanging);
      end;


initialization
   InitializeCriticalSection(HandlerChanging);

finalization
  { Uninstall all handlers                   }
  { this stops also the event handler thread }
  SetMouseEventHandler(nil);
  SetKeyboardEventHandler(nil);
  SetFocusEventHandler(nil);
  SetMenuEventHandler(nil);
  SetResizeEventHandler(nil);
  SetUnknownEventHandler(nil);
  { delete the critical section object }
  DeleteCriticalSection(HandlerChanging);
end.

{
  $Log$
  Revision 1.1  2001-01-13 11:03:59  peter
    * API 2 RTL commit

}
