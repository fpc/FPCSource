{
    $Id$

    fpEvents: Asynchronous event management for Free Pascal
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Test program 2: File handles

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program Async2;

{$MODE objfpc}
{$H+}

uses CRT, Classes, fpAsync;

type

  TMyApplication = class
    EventLoop: TEventLoop;
    procedure TimerEvent(Sender: TObject);
    procedure KeyboardEvent(Sender: TObject);
  protected
    StartTimerTicks: Int64;
    Keyboard: THandleStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;


constructor TMyApplication.Create;
begin
  Keyboard := THandleStream.Create(StdInputHandle);
  EventLoop := TEventLoop.Create;
  EventLoop.AddTimerNotify(1000, True, @TimerEvent, nil);
  EventLoop.SetIONotify(StdInputHandle, @KeyboardEvent, nil);
end;

destructor TMyApplication.Destroy;
begin
  EventLoop.Free;
  Keyboard.Free;
  inherited Destroy;
end;

procedure TMyApplication.Run;
begin
  WriteLn('Break with Ctrl-C...');
  StartTimerTicks := EventLoop.TimerTicks;
  EventLoop.Run;
end;

procedure TMyApplication.TimerEvent(Sender: TObject);
begin
  WriteLn('Timer tick after ', EventLoop.TimerTicks - StartTimerTicks, ' ms');
end;

procedure TMyApplication.KeyboardEvent(Sender: TObject);
var
  b: Byte;
begin
  b := Keyboard.ReadByte;
  WriteLn('Keyboard data available: #', b, '  ', Chr(b));
  if b = 3 then		// Ctrl-C pressed?
  begin
    WriteLn('Breaking...');
    EventLoop.Break;
  end;
end;


var
  App: TMyApplication;
begin
  App := TMyApplication.Create;
  try
    App.Run;
  finally
    App.Free;
  end;
end.


{
  $Log$
  Revision 1.1.2.1  2001-09-08 15:43:24  sg
  * First public version

}
