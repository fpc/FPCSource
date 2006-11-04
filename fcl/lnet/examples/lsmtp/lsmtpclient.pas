program lSMTPClient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lNet, lSMTP;
  
type

  { TDoer }

  { TLSMTPClientTest }

  TLSMTPClientTest = class
   private
    FSMTP: TLSMTPClient;
    FQuit: Boolean;
    function GetAnswer(const s: string): string;
    procedure PrintUsage(const Msg: string);
    procedure OnReceive(Sender: TLSMTPClient);
    procedure OnConnect(Sender: TLSMTPClient);
    procedure OnDisconnect(Sender: TLSMTPClient);
    procedure OnError(const msg: string; Sender: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TLSMTPClientTest }

function TLSMTPClientTest.GetAnswer(const s: string): string;
var
  c: Char;
begin
  Result:='';
  Write(s, ': ');
  while True do begin
    FSMTP.CallAction;
    if KeyPressed then begin
      c:=ReadKey;
      case c of
        #13, #27 : begin
                     Writeln;
                     Exit;
                   end;
        #8       : if Length(Result) > 0 then begin
                     SetLength(Result, Length(Result)-1);
                     GotoXY(WhereX-1, WhereY);
                     Write(' ');
                     GotoXY(WhereX-1, WhereY);
                   end;
        else begin
          Result:=Result + c;
          Write(c);
        end;
      end;
    end;
    Sleep(1);
  end;
end;

procedure TLSMTPClientTest.PrintUsage(const Msg: string);
begin
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' ', Msg);
end;

procedure TLSMTPClientTest.OnReceive(Sender: TLSMTPClient);
var
  s: string;
begin
  Sender.GetMessage(s);
  Write(s);
end;

procedure TLSMTPClientTest.OnConnect(Sender: TLSMTPClient);
begin
  Writeln('Connected');
end;

procedure TLSMTPClientTest.OnDisconnect(Sender: TLSMTPClient);
begin
  Writeln('Lost connection');
  FQuit:=True;
end;

procedure TLSMTPClientTest.OnError(const msg: string; Sender: TLSocket);
begin
  Writeln(msg);
end;

constructor TLSMTPClientTest.Create;
begin
  FQuit:=False;
  FSMTP:=TLSMTPClient.Create(nil);
  FSMTP.OnReceive:=@OnReceive;
  FSMTP.OnConnect:=@OnConnect;
  FSMTP.OnDisconnect:=@OnDisconnect;
  FSMTP.OnError:=@OnError;
end;

destructor TLSMTPClientTest.Destroy;
begin
  FSMTP.Free;
  inherited Destroy;
end;

procedure TLSMTPClientTest.Run;
const
  MAX_RECIPIENTS = 10;
var
  Addr, Subject, Sender, Recipients, Message: string;
  Port: Word = 25;
begin
  if ParamCount > 0 then begin
    Addr:=ParamStr(1);
    if ParamCount > 1 then
      Port:=Word(StrToInt(ParamStr(2)));

    Write('Connecting to ', Addr, '... ');
    if FSMTP.Connect(Addr, Port) then repeat  // try to connect
      FSMTP.CallAction;  // if inital connect went ok, wait for "acknowlidgment" or otherwise
      if KeyPressed then
        if ReadKey = #27 then
          FQuit:=True;  // if user doesn't wish to wait, quit
      Sleep(1);
    until FQuit or FSMTP.Connected; // if user quit, or we connected, then continue
    if not FQuit then begin // if we connected send HELO
      FSMTP.Helo;
      Writeln('Press escape to quit or any other key to compose an email');
    end;

    while not FQuit do begin // if we connected, do main loop
      SetLength(Recipients, MAX_RECIPIENTS);
      FSMTP.CallAction;
      if KeyPressed then
        if ReadKey = #27 then begin
          if FSMTP.Connected then
            FSMTP.Quit
          else
            FQuit:=True;
        end else begin
          Sender:=GetAnswer('From');
          Recipients:=GetAnswer('Recipients');
          Subject:=GetAnswer('Subject');
          Message:=GetAnswer('Data');

          FSMTP.SendMail(Sender, Recipients, Subject, Message);
        end;
      Sleep(1);
    end;
  end else PrintUsage('<SMTP server hostname/IP> [port]');
end;

var
  SMTP: TLSMTPClientTest;
begin
  SMTP:=TLSMTPClientTest.Create;
  SMTP.Run;
  SMTP.Free;
end.
