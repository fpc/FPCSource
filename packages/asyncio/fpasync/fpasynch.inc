{
    $Id$

    fpAsync: Asynchronous event management for Free Pascal
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Common interface

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

type

  TEventLoop = class
  private
    FData: TAsyncData;
    FFirstNotifyData: Pointer;
    function GetIsRunning: Boolean;
    procedure SetIsRunning(AIsRunning: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Handle: TAsyncHandle;

    // Main loop control
    procedure Run;
    procedure Break;

    // Timer support
    function AddTimerCallback(AMSec: LongInt; APeriodic: Boolean;
      ACallback: TAsyncCallback; AUserData: Pointer): TAsyncTimer;
    procedure RemoveTimerCallback(ATimer: TAsyncTimer);
    function AddTimerNotify(AMSec: LongInt; APeriodic: Boolean;
      ANotify: TNotifyEvent; ASender: TObject): Pointer;
    procedure RemoveTimerNotify(AHandle: Pointer);

    // I/O notification support (for files, sockets etc.)
    procedure SetIOCallback(AHandle: Integer; ACallback: TAsyncCallback;
      AUserData: Pointer);
    procedure ClearIOCallback(AHandle: Integer);
    function SetIONotify(AHandle: Integer; ANotify: TNotifyEvent;
      ASender: TObject): Pointer;
    procedure ClearIONotify(AHandle: Pointer);

    class function TimerTicks: Int64;

    // Properties
    property IsRunning: Boolean read GetIsRunning write SetIsRunning;
  end;


{
  $Log$
  Revision 1.1.2.1  2001-09-08 15:43:24  sg
  * First public version

}
