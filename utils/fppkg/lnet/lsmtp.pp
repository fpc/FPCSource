{ lNet SMTP unit

  CopyRight (C) 2005-2007 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lsmtp;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, Contnrs, lNet, lEvents, lCommon, lMimeWrapper, lMimeStreams;
  
type
  TLSMTP = class;
  TLSMTPClient = class;
  
  TLSMTPStatus = (ssNone, ssCon, ssHelo, ssEhlo, ssMail,
                  ssRcpt, ssData, ssRset, ssQuit);

  TLSMTPStatusSet = set of TLSMTPStatus;

  TLSMTPStatusRec = record
    Status: TLSMTPStatus;
    Args: array[1..2] of string;
  end;
  
  { TLSMTPStatusFront }
  {$DEFINE __front_type__  :=  TLSMTPStatusRec}
  {$i lcontainersh.inc}
  TLSMTPStatusFront = TLFront;

  TLSMTPClientStatusEvent = procedure (aSocket: TLSocket;
                                       const aStatus: TLSMTPStatus) of object;
                                       
  { TMail }

  TMail = class
   protected
    FMailText: string;
    FMailStream: TMimeStream;
    FRecipients: string;
    FSender: string;
    FSubject: string;
    function GetCount: Integer;
    function GetSection(i: Integer): TMimeSection;
    procedure SetSection(i: Integer; const AValue: TMimeSection);
   public
    constructor Create;
    destructor Destroy; override;
    procedure AddTextSection(const aText: string; const aCharSet: string = 'UTF-8');
    procedure AddFileSection(const aFileName: string);
    procedure AddStreamSection(aStream: TStream; const FreeStream: Boolean = False);
    procedure DeleteSection(const i: Integer);
    procedure RemoveSection(aSection: TMimeSection);
   public
    property MailText: string read FMailText write FMailText; deprecated; // use sections!
    property Sender: string read FSender write FSender;
    property Recipients: string read FRecipients write FRecipients;
    property Subject: string read FSubject write FSubject;
    property Sections[i: Integer]: TMimeSection read GetSection write SetSection; default;
    property SectionCount: Integer read GetCount;
  end;

  TLSMTP = class(TLComponent)
   protected
    FConnection: TLTcp;
   protected
    function GetTimeout: Integer;
    procedure SetTimeout(const AValue: Integer);
    
    function GetConnected: Boolean;

    function GetSocketClass: TLSocketClass;
    procedure SetSocketClass(const AValue: TLSocketClass);
    
    function GetEventer: TLEventer;
    procedure SetEventer(Value: TLEventer);
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
   public
    property Connected: Boolean read GetConnected;
    property Connection: TLTcp read FConnection;

    property SocketClass: TLSocketClass read GetSocketClass write SetSocketClass;
    property Eventer: TLEventer read GetEventer write SetEventer;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;

  { TLSMTPClient }

  TLSMTPClient = class(TLSMTP, ILClient)
   protected
    FStatus: TLSMTPStatusFront;
    FCommandFront: TLSMTPStatusFront;
    FPipeLine: Boolean;

    FOnConnect: TLSocketEvent;
    FOnReceive: TLSocketEvent;
    FOnDisconnect: TLSocketEvent;
    FOnSuccess: TLSMTPClientStatusEvent;
    FOnFailure: TLSMTPClientStatusEvent;
    FOnError: TLSocketErrorEvent;
    FOnSent: TLSocketProgressEvent;

    FSL: TStringList;
    FStatusSet: TLSMTPStatusSet;
    FBuffer: string;
    FDataBuffer: string; // intermediate wait buffer on DATA command
    FCharCount: Integer; // count of chars from last CRLF
    FStream: TStream;
   protected
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnCo(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    procedure OnCs(aSocket: TLSocket);
   protected
    function CanContinue(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): Boolean;
    
    function CleanInput(var s: string): Integer;
    
    procedure EvaluateAnswer(const Ans: string);
    
    procedure ExecuteFrontCommand;
    
    procedure ClearCR_LF;
    procedure SendData(const FromStream: Boolean = False);
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    
    function Connect(const aHost: string; const aPort: Word = 25): Boolean; virtual; overload;
    function Connect: Boolean; virtual; overload;
    
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; virtual;

    procedure SendMail(From, Recipients, Subject, Msg: string);
    procedure SendMail(From, Recipients, Subject: string; aStream: TStream);
    procedure SendMail(aMail: TMail);
    
    procedure Helo(aHost: string = '');
    procedure Ehlo(aHost: string = '');
    procedure Mail(const From: string);
    procedure Rcpt(const RcptTo: string);
    procedure Data(const Msg: string);
    procedure Rset;
    procedure Quit;
    
    procedure Disconnect; override;
    
    procedure CallAction; override;
   public
    property PipeLine: Boolean read FPipeLine write FPipeLine;
    property StatusSet: TLSMTPStatusSet read FStatusSet write FStatusSet;
    property OnConnect: TLSocketEvent read FOnConnect write FOnConnect;
    property OnReceive: TLSocketEvent read FOnReceive write FOnReceive;
    property OnDisconnect: TLSocketEvent read FOnDisconnect write FOnDisconnect;
    property OnSuccess: TLSMTPClientStatusEvent read FOnSuccess write FOnSuccess;
    property OnFailure: TLSMTPClientStatusEvent read FOnFailure write FOnFailure;
    property OnError: TLSocketErrorEvent read FOnError write FOnError;
    property OnSent: TLSocketProgressEvent read FOnSent write FOnSent;
  end;

implementation

const
  EMPTY_REC: TLSMTPStatusRec = (Status: ssNone; Args: ('', ''));

{$i lcontainers.inc}

function StatusToStr(const aStatus: TLSMTPStatus): string;
const
  STATAR: array[ssNone..ssQuit] of string = ('ssNone', 'ssCon', 'ssHelo', 'ssEhlo', 'ssMail',
                                             'ssRcpt', 'ssData', 'ssRset', 'ssQuit');
begin
  Result := STATAR[aStatus];
end;

function MakeStatusRec(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): TLSMTPStatusRec;
begin
  Result.Status := aStatus;
  Result.Args[1] := Arg1;
  Result.Args[2] := Arg2;
end;

{ TLSMTP }

function TLSMTP.GetTimeout: Integer;
begin
  Result := FConnection.Timeout;
end;

procedure TLSMTP.SetTimeout(const AValue: Integer);
begin
  FConnection.Timeout := aValue;
end;

function TLSMTP.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TLSMTP.GetSocketClass: TLSocketClass;
begin
  Result := FConnection.SocketClass;
end;

procedure TLSMTP.SetSocketClass(const AValue: TLSocketClass);
begin
  FConnection.SocketClass := AValue;
end;

function TLSMTP.GetEventer: TLEventer;
begin
  Result := FConnection.Eventer;
end;

procedure TLSMTP.SetEventer(Value: TLEventer);
begin
  FConnection.Eventer := Value;
end;

constructor TLSMTP.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  FConnection := TLTcp.Create(nil);
end;

destructor TLSMTP.Destroy;
begin
  FConnection.Free;

  inherited Destroy;
end;

{ TLSMTPClient }

constructor TLSMTPClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPort := 25;
  FStatusSet := []; // empty set for "ok/not-ok" Event
  FSL := TStringList.Create;
//  {$warning TODO: fix pipelining support when server does it}
  FPipeLine := False;
  
  FConnection.OnError := @OnEr;
  FConnection.OnCanSend := @OnCs;
  FConnection.OnReceive := @OnRe;
  FConnection.OnConnect := @OnCo;
  FConnection.OnDisconnect := @OnDs;

  FStatus := TLSMTPStatusFront.Create(EMPTY_REC);
  FCommandFront := TLSMTPStatusFront.Create(EMPTY_REC);
end;

destructor TLSMTPClient.Destroy;
begin
  Quit;
  FSL.Free;
  FStatus.Free;
  FCommandFront.Free;

  inherited Destroy;
end;

procedure TLSMTPClient.OnEr(const msg: string; aSocket: TLSocket);
begin
  if Assigned(FOnError) then
    FOnError(msg, aSocket);
end;

procedure TLSMTPClient.OnRe(aSocket: TLSocket);
begin
  if Assigned(FOnReceive) then
    FOnReceive(aSocket);
end;

procedure TLSMTPClient.OnCo(aSocket: TLSocket);
begin
  if Assigned(FOnConnect) then
    FOnConnect(aSocket);
end;

procedure TLSMTPClient.OnDs(aSocket: TLSocket);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(aSocket);
end;

procedure TLSMTPClient.OnCs(aSocket: TLSocket);
begin
  SendData(FStatus.First.Status = ssData);
end;

function TLSMTPClient.CanContinue(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): Boolean;
begin
  Result := FPipeLine or FStatus.Empty;
  if not Result then
    FCommandFront.Insert(MakeStatusRec(aStatus, Arg1, Arg2));
end;

function TLSMTPClient.CleanInput(var s: string): Integer;
var
  i: Integer;
begin
  FSL.Text := s;
  if FSL.Count > 0 then
    for i := 0 to FSL.Count-1 do
      if Length(FSL[i]) > 0 then EvaluateAnswer(FSL[i]);
  s := StringReplace(s, CRLF, LineEnding, [rfReplaceAll]);
  i := Pos('PASS', s);
  if i > 0 then
    s := Copy(s, 1, i-1) + 'PASS';
  Result := Length(s);
end;

procedure TLSMTPClient.EvaluateAnswer(const Ans: string);

  function GetNum: Integer;
  begin
    try
      Result := StrToInt(Copy(Ans, 1, 3));
    except
      Result := -1;
    end;
  end;
  
  function ValidResponse(const Answer: string): Boolean; inline;
  begin
    Result := (Length(Ans) >= 3) and
            (Ans[1] in ['1'..'5']) and
            (Ans[2] in ['0'..'9']) and
            (Ans[3] in ['0'..'9']);

    if Result then
      Result := (Length(Ans) = 3) or ((Length(Ans) > 3) and (Ans[4] = ' '));
  end;
  
  procedure Eventize(const aStatus: TLSMTPStatus; const Res: Boolean);
  begin
    if Res then begin
      if Assigned(FOnSuccess) and (aStatus in FStatusSet) then
        FOnSuccess(FConnection.Iterator, aStatus);
    end else begin
      if Assigned(FOnFailure) and (aStatus in FStatusSet) then
        FOnFailure(FConnection.Iterator, aStatus);
    end;
  end;
  
var
  x: Integer;
begin
  x := GetNum;
  if ValidResponse(Ans) and not FStatus.Empty then
    case FStatus.First.Status of
      ssCon,
      ssHelo,
      ssEhlo: case x of
                200..299: begin
                            Eventize(FStatus.First.Status, True);
                            FStatus.Remove;
                          end;
              else        begin
                            Eventize(FStatus.First.Status, False);
                            Disconnect;
                          end;
              end;
               
      ssMail,
      ssRcpt: begin
                Eventize(FStatus.First.Status, (x >= 200) and (x < 299));
                FStatus.Remove;
              end;

      ssData: case x of
                200..299: begin
                            Eventize(FStatus.First.Status, True);
                            FStatus.Remove;
                          end;
                300..399: begin
                            FBuffer := FDataBuffer;
                            FDataBuffer := '';
                            SendData(True);
                          end;
              else        begin
                            FDataBuffer := '';
                            Eventize(FStatus.First.Status, False);
                            FStatus.Remove;
                          end;
              end;
              
      ssRset: begin
                Eventize(FStatus.First.Status, (x >= 200) and (x < 299));
                FStatus.Remove;
              end;
              
      ssQuit: begin
                Eventize(FStatus.First.Status, (x >= 200) and (x < 299));
                FStatus.Remove;
                if Assigned(FOnDisconnect) then
                  FOnDisconnect(FConnection.Iterator);
                Disconnect;
              end;
    end;
    
  if FStatus.Empty and not FCommandFront.Empty then
    ExecuteFrontCommand;
end;

procedure TLSMTPClient.ExecuteFrontCommand;
begin
  with FCommandFront.First do
    case Status of
      ssHelo: Helo(Args[1]);
      ssEhlo: Ehlo(Args[1]);
      ssMail: Mail(Args[1]);
      ssRcpt: Rcpt(Args[1]);
      ssData: Data(Args[1]);
      ssRset: Rset;
      ssQuit: Quit;
    end;
  FCommandFront.Remove;
end;

procedure TLSMTPClient.ClearCR_LF;
var
  i: Integer;
  Skip: Boolean = False;
begin
  for i := 1 to Length(FBuffer) do begin
    if Skip then begin
      Skip := False;
      Continue;
    end;
    
    if (FBuffer[i] = #13) or (FBuffer[i] = #10) then begin
      if FBuffer[i] = #13 then
        if (i < Length(FBuffer)) and (FBuffer[i + 1] = #10) then begin
          FCharCount := 0;
          Skip := True; // skip the crlf
        end else begin // insert LF to a standalone CR
          System.Insert(#10, FBuffer, i + 1);
          FCharCount := 0;
          Skip := True; // skip the new crlf
        end;
        
      if FBuffer[i] = #10 then begin
        System.Insert(#13, FBuffer, i);
        FCharCount := 0;
        Skip := True; // skip the new crlf
      end;
    end else if FCharCount >= 1000 then begin // line too long
      System.Insert(CRLF, FBuffer, i);
      FCharCount := 0;
      Skip := True;
    end else
      Inc(FCharCount);
  end;
end;

procedure TLSMTPClient.SendData(const FromStream: Boolean = False);
const
  SBUF_SIZE = 65535;
  
  procedure FillBuffer;
  var
    s: string;
  begin
    SetLength(s, SBUF_SIZE - Length(FBuffer));
    SetLength(s, FStream.Read(s[1], Length(s)));
    
    FBuffer := FBuffer + s;
    
    if FStream.Position = FStream.Size then begin // we finished the stream
      FBuffer := FBuffer + CRLF + '.' + CRLF;
      FStream := nil;
    end;
  end;

var
  n: Integer;
  Sent: Integer;
begin
  if FromStream and Assigned(FStream) then
    FillBuffer;

  n := 1;
  Sent := 0;
  while (Length(FBuffer) > 0) and (n > 0) do begin
    ClearCR_LF;
  
    n := FConnection.SendMessage(FBuffer);
    Sent := Sent + n;
    if n > 0 then
      Delete(FBuffer, 1, n);

    if FromStream and Assigned(FStream) and (Length(FBuffer) < SBUF_SIZE) then
      FillBuffer;
  end;
  
  if Assigned(FOnSent) and (FStatus.First.Status = ssData) then
    FOnSent(FConnection.Iterator, Sent);
end;

function TLSMTPClient.Connect(const aHost: string; const aPort: Word = 25): Boolean;
begin
  Result := False;
  Disconnect;
  if FConnection.Connect(aHost, aPort) then begin
    FHost := aHost;
    FPort := aPort;
    FStatus.Insert(MakeStatusRec(ssCon, '', ''));
    Result := True;
  end;
end;

function TLSMTPClient.Connect: Boolean;
begin
  Result := Connect(FHost, FPort);
end;

function TLSMTPClient.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
var
  s: string;
begin
  Result := FConnection.Get(aData, aSize, aSocket);
  if Result > 0 then begin
    SetLength(s, Result);
    Move(aData, PChar(s)^, Result);
    CleanInput(s);
  end;
end;

function TLSMTPClient.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result := FConnection.GetMessage(msg, aSocket);
  if Result > 0 then
    Result := CleanInput(msg);
end;

procedure TLSMTPClient.SendMail(From, Recipients, Subject, Msg: string);
var
  i: Integer;
begin
  FStream := nil;
  From := EncodeMimeHeaderText(From);
  Recipients := EncodeMimeHeaderText(Recipients);
  Subject := EncodeMimeHeaderText(Subject);
  
  if (Length(Recipients) > 0) and (Length(From) > 0) then begin
    Mail(From);
    FSL.CommaText := StringReplace(Recipients, ' ', ',', [rfReplaceAll]);
    for i := 0 to FSL.Count-1 do
      Rcpt(FSL[i]);
    Data('From: ' + From + CRLF + 'Subject: ' + Subject + CRLF + 'To: ' + FSL.CommaText + CRLF + Msg);
    Rset;
  end;
end;

procedure TLSMTPClient.SendMail(From, Recipients, Subject: string; aStream: TStream);
var
  i: Integer;
begin
  From := EncodeMimeHeaderText(From);
  Recipients := EncodeMimeHeaderText(Recipients);
  Subject := EncodeMimeHeaderText(Subject);
  
  FStream := aStream;

  if (Length(Recipients) > 0) and (Length(From) > 0) then begin
    Mail(From);
    FSL.CommaText := StringReplace(Recipients, ' ', ',', [rfReplaceAll]);
    for i := 0 to FSL.Count-1 do
      Rcpt(FSL[i]);
    Data('From: ' + From + CRLF + 'Subject: ' + Subject + CRLF + 'To: ' + FSL.CommaText + CRLF);
    Rset;
  end;
end;

procedure TLSMTPClient.SendMail(aMail: TMail);
begin
  if Length(aMail.FMailText) > 0 then
    SendMail(aMail.Sender, aMail.Recipients, aMail.Subject, aMail.FMailText)
  else if Assigned(aMail.FMailStream) then
    SendMail(aMail.Sender, aMail.Recipients, aMail.Subject, aMail.FMailStream);
end;

procedure TLSMTPClient.Helo(aHost: string = '');
begin
  if Length(Host) = 0 then
    aHost := FHost;
  if CanContinue(ssHelo, aHost, '') then begin
    FBuffer := FBuffer + 'HELO ' + aHost + CRLF;
    FStatus.Insert(MakeStatusRec(ssHelo, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Ehlo(aHost: string = '');
begin
  if Length(aHost) = 0 then
    aHost := FHost;
  if CanContinue(ssEhlo, aHost, '') then begin
    FBuffer := FBuffer + 'EHLO ' + aHost + CRLF;
    FStatus.Insert(MakeStatusRec(ssEhlo, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Mail(const From: string);
begin
  if CanContinue(ssMail, From, '') then begin
    FBuffer := FBuffer + 'MAIL FROM:' + '<' + From + '>' + CRLF;
    FStatus.Insert(MakeStatusRec(ssMail, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Rcpt(const RcptTo: string);
begin
  if CanContinue(ssRcpt, RcptTo, '') then begin
    FBuffer := FBuffer + 'RCPT TO:' + '<' + RcptTo + '>' + CRLF;
    FStatus.Insert(MakeStatusRec(ssRcpt, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Data(const Msg: string);
begin
  if CanContinue(ssData, Msg, '') then begin
    FBuffer := 'DATA ' + CRLF;
    FDataBuffer := '';

    if Assigned(FStream) then begin
      if Length(Msg) > 0 then
        FDataBuffer := Msg;
    end else
      FDataBuffer := Msg + CRLF + '.' + CRLF;

    FStatus.Insert(MakeStatusRec(ssData, '', ''));
    SendData(False);
  end;
end;

procedure TLSMTPClient.Rset;
begin
  if CanContinue(ssRset, '', '') then begin
    FBuffer := FBuffer + 'RSET' + CRLF;
    FStatus.Insert(MakeStatusRec(ssRset, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Quit;
begin
  if CanContinue(ssQuit, '', '') then begin
    FBuffer := FBuffer + 'QUIT' + CRLF;
    FStatus.Insert(MakeStatusRec(ssQuit, '', ''));
    SendData;
  end;
end;

procedure TLSMTPClient.Disconnect;
begin
  FConnection.Disconnect;
  FStatus.Clear;
  FCommandFront.Clear;
end;

procedure TLSMTPClient.CallAction;
begin
  FConnection.CallAction;
end;

{ TMail }

function TMail.GetCount: Integer;
begin
  Result := FMailStream.Count;
end;

function TMail.GetSection(i: Integer): TMimeSection;
begin
  Result := FMailStream.Sections[i];
end;

procedure TMail.SetSection(i: Integer; const AValue: TMimeSection);
begin
  FMailStream.Sections[i] := aValue;
end;

constructor TMail.Create;
begin
  FMailStream := TMimeStream.Create;
end;

destructor TMail.Destroy;
begin
  FMailStream.Free;
end;

procedure TMail.AddTextSection(const aText: string; const aCharSet: string);
begin
  FMailStream.AddTextSection(aText, aCharSet);
end;

procedure TMail.AddFileSection(const aFileName: string);
begin
  FMailStream.AddFileSection(aFileName);
end;

procedure TMail.AddStreamSection(aStream: TStream; const FreeStream: Boolean);
begin
  FMailStream.AddStreamSection(aStream, FreeStream);
end;

procedure TMail.DeleteSection(const i: Integer);
begin
  FMailStream.Delete(i);
end;

procedure TMail.RemoveSection(aSection: TMimeSection);
begin
  FMailStream.Remove(aSection);
end;


end.

