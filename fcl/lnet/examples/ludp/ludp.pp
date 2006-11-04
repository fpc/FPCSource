program ludp;

{$mode objfpc}{$H+}

uses
  SysUtils, lNet, Crt;
  
type

  { TLUDPTest
    - This class serves as an example on how to use basic UDP.
      It merely acts as server or client.
      Clients send messages and server is simple echo, no checks. }

  TLUDPTest = class
   private
    FCon: TLUDP; // the lNet connection to use
    FServer: Boolean; // are we the server?
    FQuit: Boolean; // the main loop control
    procedure OnEr(const msg: string; aSocket: TLSocket); // OnError callback
    procedure OnRe(aSocket: TLSocket); // OnReceive callback
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run; // this is the main loop
  end;

{ TLUDPTest }

constructor TLUDPTest.Create;
begin
  FCon:=TLUdp.Create(nil); // create a new TLUDP component with no parent coponent
  FCon.OnError:=@OnEr;     // assign callbacks
  FCon.OnReceive:=@OnRe;
end;

destructor TLUDPTest.Destroy;
begin
  FCon.Free; // free the UDP connection
  inherited Destroy;
end;

procedure TLUDPTest.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg); // write the error message
  FQuit:=true;  // and quit ASAP
end;

procedure TLUDPTest.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin // if we received anything (will be in s)
    Writeln(s);                           // write the message received
    Writeln('Host at: ', aSocket.PeerAddress); // and the address of sender
    if FServer then // if we act as server
      FCon.SendMessage('Welcome'); // echo the message "Welcome" back to the client
  end;
end;

procedure TLUDPTest.Run;
var
  Result: Boolean; // result of connect or listen functions
  Port: Word; // port to connect to
  Address: string; // address to connect to
begin
  if ParamCount > 1 then begin // we need atleast one argument
    Result:=False;
    FQuit:=False;  // initialize loop variables

    try
      Port:=Word(StrToInt(ParamStr(2))); // try to get port from argument if possible
    except
      on e: Exception do begin // write error and quit if not
        Writeln(e.message);
        Halt;
      end;
    end;

    if ParamCount > 2 then // if we got additional argument, then parse it as address
      Address:=ParamStr(3)
    else
      Address:=LADDR_BR;   // else use broadcast address

    if ParamStr(1) = '-s' then begin // if we're supposed to be the server
      Result:=FCon.Listen(port); // start listening
      Writeln('Starting server...');
      FServer:=True;             // and remember the descision
    end else begin
      Result:=FCon.Connect(Address, port); // otherwise connect
      Writeln('Starting client...');
    end;

    if Result then repeat // if listen/connect was successful
      FCon.CallAction;     // "eventize" the event loop in lNet
      if KeyPressed then  // if user provided input
        if ReadKey <> #27 then  // and he didn't pres "escape"
          FCon.SendMessage('Hello') // send the "Hello" message to other side
        else
          FQuit:=true; // otherwise (if he pressed "escape") quit
      Sleep(20); // 20 ms delay to not hog cpu (normaly, 1ms is used)
    until FQuit; // repeat this cycle until FQuit = true, due to error or user input

    Writeln; // write additional line to clarify stuff
  end else Writeln('Usage: ', ParamStr(0), ' <-s/-c> <port> [address]');
end;

var
  UDP: TLUDPTest;
begin
  UDP:=TLUDPTest.Create;
  UDP.Run;
  UDP.Free;
end.

