{
    $Id$

    fpAsync: Asynchronous event management for Free Pascal
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Test program 1: Timers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program Async1;

{$MODE objfpc}
{$H+}

uses fpAsync;

type

  TMyApplication = class
    EventLoop: TEventLoop;
    procedure Timer1Event(Sender: TObject);
    procedure Timer2Event(Sender: TObject);
    procedure Timer3Event(Sender: TObject);
    procedure TerminateTimerEvent(Sender: TObject);
  protected
    StartTimerTicks: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;


constructor TMyApplication.Create;
begin
  EventLoop := TEventLoop.Create;
  EventLoop.AddTimerNotify(500, True, @Timer1Event, nil);
  EventLoop.AddTimerNotify(2000, True, @Timer2Event, nil);
  EventLoop.AddTimerNotify(1200, True, @Timer3Event, nil);
end;

destructor TMyApplication.Destroy;
begin
  EventLoop.Free;
  inherited Destroy;
end;

procedure TMyApplication.Run;
begin
  WriteLn('Will terminate in 10 seconds...');
  EventLoop.AddTimerNotify(10000, False, @TerminateTimerEvent, nil);
  StartTimerTicks := EventLoop.TimerTicks;
  EventLoop.Run;
end;

procedure TMyApplication.Timer1Event(Sender: TObject);
begin
  WriteLn('Timer 1 after ', EventLoop.TimerTicks - StartTimerTicks, ' ms');
end;

procedure TMyApplication.Timer2Event(Sender: TObject);
begin
  WriteLn('Timer 2 after ', EventLoop.TimerTicks - StartTimerTicks, ' ms');
end;

procedure TMyApplication.Timer3Event(Sender: TObject);
begin
  WriteLn('Timer 3 after ', EventLoop.TimerTicks - StartTimerTicks, ' ms');
end;

procedure TMyApplication.TerminateTimerEvent(Sender: TObject);
begin
  WriteLn('Terminating');
  EventLoop.Break;
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
