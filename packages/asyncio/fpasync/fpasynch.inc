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

    procedure SetDataAvailableCallback(AHandle: Integer;
      ACallback: TAsyncCallback; AUserData: Pointer);
    procedure ClearDataAvailableCallback(AHandle: Integer);
    function SetDataAvailableNotify(AHandle: Integer; ANotify: TNotifyEvent;
      ASender: TObject): Pointer;
    procedure ClearDataAvailableNotify(AHandle: Pointer);

    procedure SetCanWriteCallback(AHandle: Integer; ACallback: TAsyncCallback;
      AUserData: Pointer);
    procedure ClearCanWriteCallback(AHandle: Integer);
    function SetCanWriteNotify(AHandle: Integer; ANotify: TNotifyEvent;
      ASender: TObject): Pointer;
    procedure ClearCanWriteNotify(AHandle: Pointer);


    class function TimerTicks: Int64;

    // Properties
    property IsRunning: Boolean read GetIsRunning write SetIsRunning;
  end;


// -------------------------------------------------------------------
//   Asynchronous write buffers
// -------------------------------------------------------------------

  TWriteBuffer = class(TStream)
  protected
    FBuffer: PChar;
    FBytesInBuffer: Integer;
    FOnBufferEmpty: TNotifyEvent;

    function  Seek(Offset: LongInt; Origin: Word): LongInt; override;
    function  Write(const ABuffer; Count: LongInt): LongInt; override;
    function  DoRealWrite(const ABuffer; Count: Integer): Integer; virtual; abstract;
    procedure WritingFailed; virtual; abstract;
    procedure WantWrite; virtual; abstract;
    procedure BufferEmpty; virtual;
  public
    EndOfLineMarker: String;

    constructor Create;
    destructor Destroy; override;
    procedure WriteLine(const line: String);
    procedure Run;		// Write as many data as possible

    property BytesInBuffer: Integer read FBytesInBuffer;
    property OnBufferEmpty: TNotifyEvent read FOnBufferEmpty write FOnBufferEmpty;
  end;


  TAsyncWriteStream = class(TWriteBuffer)
  protected
    FManager: TEventLoop;
    FDataStream: TStream;
    FBlockingStream: THandleStream;
    FNotifyHandle: Pointer;

    function  DoRealWrite(const ABuffer; Count: Integer): Integer; override;
    procedure WritingFailed; override;
    procedure WantWrite; override;
    procedure BufferEmpty; override;
    procedure CanWrite(UserData: TObject);
  public
    constructor Create(AManager: TEventLoop; AStream: THandleStream);
    constructor Create(AManager: TEventLoop;
      ADataStream: TStream; ABlockingStream: THandleStream);
    destructor Destroy; override;

    property DataStream: TStream read FDataStream;
    property BlockingStream: THandleStream read FBlockingStream;
  end;


{
  $Log$
  Revision 1.2  2001-12-11 17:45:28  marco
   * was only commited to fixes.

  Revision 1.1.2.2  2001/11/16 12:51:41  sg
  * Now different handlers for available data and space in write buffer can
    be set
  * LOTS of bugfixes in the implementation
  * fpAsync now has a write buffer class (a read buffer class for reading
    line by line will be included in the next release)

  Revision 1.1.2.1  2001/09/08 15:43:24  sg
  * First public version

}
