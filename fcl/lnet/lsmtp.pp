{ lNet SMTP unit

  CopyRight (C) 2005-2006 Ales Katona

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
  Classes, lNet, lEvents, lCommon;
  
type
  TLSMTPClient = class;
  
  TLSMTPStatus = (ssNone, ssCon, ssHelo, ssEhlo, ssMail,
                  ssRcpt, ssData, ssRset, ssQuit);

  TLSMTPStatusSet = set of TLSMTPStatus;

  TLSMTPStatusRec = record
    Status: TLSMTPStatus;
    Args: array[1..2] of string;
  end;
  
  { TLSMTPStatusFront }
  {$DEFINE __front_type__ := TLSMTPStatusRec}
  {$i lcontainersh.inc}
  TLSMTPStatusFront = TLFront;

  TLSMTPClientCallback = procedure (Sender: TLSMTPClient) of object;
  TLSMTPStatusCallback = procedure (Sender: TLSMTPClient;
                                    const aStatus: TLSMTPStatus) of object;

  { TLSMTPClient }

  TLSMTPClient = class(TComponent)
   protected
    FStatus: TLSMTPStatusFront;
    FCommandFront: TLSMTPStatusFront;
    FConnection: TLTcp;
    FPipeLine: Boolean;
    FOnError: TLErrorProc;
    FOnConnect: TLSMTPClientCallback;
    FOnReceive: TLSMTPClientCallback;
    FOnDisconnect: TLSMTPClientCallback;
    FOnSuccess: TLSMTPStatusCallback;
    FOnFailure: TLSMTPStatusCallback;
    FSL: TStringList;
    FStatusSet: TLSMTPStatusSet;
    FHost: string;
    FMessage: string;
    FPort: Word;
   protected
    function GetSocketClass: TLSocketClass;
    procedure SetSocketClass(const AValue: TLSocketClass);
   protected
    procedure OnRe(Sender: TLSocket);
    procedure OnCo(Sender: TLSocket);
    procedure OnDs(Sender: TLSocket);
   protected
    function CanContinue(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): Boolean;
    function CleanInput(var s: string): Integer;
    function GetConnected: Boolean;
    function GetEventer: TLEventer;
    procedure SetEventer(Value: TLEventer);
    procedure SetOnError(Value: TLErrorProc);
    procedure EvaluateAnswer(const Ans: string);
    procedure ExecuteFrontCommand;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(const aHost: string; const aPort: Word = 25): Boolean; virtual;
    function Connect: Boolean; virtual;
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;
    procedure SendMail(const From, Recipients, Subject, Msg: string);
    procedure Helo(aHost: string = '');
    procedure Ehlo(aHost: string = '');
    procedure Mail(const From: string);
    procedure Rcpt(const RcptTo: string);
    procedure Data(const Msg: string);
    procedure Rset;
    procedure Quit;
    procedure Disconnect;
    procedure CallAction; virtual;
   public
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property PipeLine: Boolean read FPipeLine write FPipeLine;
    property SocketClass: TLSocketClass read GetSocketClass write SetSocketClass;
    property Eventer: TLEventer read GetEventer write SetEventer;
    property StatusSet: TLSMTPStatusSet read FStatusSet write FStatusSet;
    property OnConnect: TLSMTPClientCallback read FOnConnect write FOnConnect;
    property OnReceive: TLSMTPClientCallback read FOnReceive write FOnReceive;
    property OnDisconnect: TLSMTPClientCallback read FOnDisconnect write FOnDisconnect;
    property OnSuccess: TLSMTPStatusCallback read FOnSuccess write FOnSuccess;
    property OnFailure: TLSMTPStatusCallback read FOnFailure write FOnFailure;
    property OnError: TLErrorProc read FOnError write SetOnError;
    property Connected: Boolean read GetConnected;
    property Connection: TLTcp read FConnection;
  end;

implementation

uses
  SysUtils;

const
  EMPTY_REC: TLSMTPStatusRec = (Status: ssNone; Args: ('', ''));
  SLE                        = #13#10;
  
{$i lcontainers.inc}

function StatusToStr(const aStatus: TLSMTPStatus): string;
const
  STATAR: array[ssNone..ssQuit] of string = ('ssNone', 'ssCon', 'ssHelo', 'ssEhlo', 'ssMail',
                                             'ssRcpt', 'ssData', 'ssRset', 'ssQuit');
begin
  Result:=STATAR[aStatus];
end;

function MakeStatusRec(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): TLSMTPStatusRec;
begin
  Result.Status:=aStatus;
  Result.Args[1]:=Arg1;
  Result.Args[2]:=Arg2;
end;

{ TLSMTPClient }

constructor TLSMTPClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPort:=25;
  FStatusSet:=[]; // empty set for "ok/not-ok" callback
  FSL:=TStringList.Create;
  FHost:='';
  FMessage:='';
//  {$warning TODO: fix pipelining support when server does it}
  FPipeLine:=False;
  FConnection:=TLTcp.Create(nil);
  FConnection.OnReceive:=@OnRe;
  FConnection.OnConnect:=@OnCo;
  FConnection.OnError:=nil;
  FOnReceive:=nil;
  FOnConnect:=nil;
  FOnError:=nil;
  FOnDisconnect:=nil;
  FStatus:=TLSMTPStatusFront.Create(EMPTY_REC);
  FCommandFront:=TLSMTPStatusFront.Create(EMPTY_REC);
end;

destructor TLSMTPClient.Destroy;
begin
  Quit;
  FConnection.Free;
  FSL.Free;
  FStatus.Free;
  FCommandFront.Free;
  inherited Destroy;
end;

function TLSMTPClient.GetSocketClass: TLSocketClass;
begin
  Result:=FConnection.SocketClass;
end;

procedure TLSMTPClient.SetSocketClass(const AValue: TLSocketClass);
begin
  FConnection.SocketClass:=AValue;
end;

procedure TLSMTPClient.OnRe(Sender: TLSocket);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self);
end;

procedure TLSMTPClient.OnCo(Sender: TLSocket);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TLSMTPClient.OnDs(Sender: TLSocket);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

function TLSMTPClient.CanContinue(const aStatus: TLSMTPStatus; const Arg1, Arg2: string): Boolean;
begin
  Result:=FPipeLine or FStatus.Empty;
  if not Result then
    FCommandFront.Insert(MakeStatusRec(aStatus, Arg1, Arg2));
end;

function TLSMTPClient.CleanInput(var s: string): Integer;
var
  i: Integer;
begin
  FSL.Text:=s;
  if FSL.Count > 0 then
    for i:=0 to FSL.Count-1 do
      if Length(FSL[i]) > 0 then EvaluateAnswer(FSL[i]);
  s:=StringReplace(s, SLE, LineEnding, [rfReplaceAll]);
  i:=Pos('PASS', s);
  if i > 0 then
    s:=Copy(s, 1, i-1) + 'PASS';
  Result:=Length(s);
end;

function TLSMTPClient.GetConnected: Boolean;
begin
  Result:=FConnection.Connected;
end;

function TLSMTPClient.GetEventer: TLEventer;
begin
  Result:=FConnection.Eventer;
end;

procedure TLSMTPClient.SetEventer(Value: TLEventer);
begin
  FConnection.Eventer:=Value;
end;

procedure TLSMTPClient.SetOnError(Value: TLErrorProc);
begin
  FConnection.OnError:=Value;
end;

procedure TLSMTPClient.EvaluateAnswer(const Ans: string);

  function GetNum: Integer;
  begin
    try
      Result:=StrToInt(Copy(Ans, 1, 3));
    except
      Result:=-1;
    end;
  end;
  
  function ValidResponse(const Answer: string): Boolean; inline;
  begin
    Result:=(Length(Ans) >= 3) and
            (Ans[1] in ['1'..'5']) and
            (Ans[2] in ['0'..'9']) and
            (Ans[3] in ['0'..'9']);

    if Result then
      Result:=(Length(Ans) = 3) or ((Length(Ans) > 3) and (Ans[4] = ' '));
  end;
  
  procedure Eventize(const aStatus: TLSMTPStatus; const Res: Boolean);
  begin
    if Res then begin
      if Assigned(FOnSuccess) and (aStatus in FStatusSet) then
        FOnSuccess(Self, aStatus);
    end else begin
      if Assigned(FOnFailure) and (aStatus in FStatusSet) then
        FOnFailure(Self, aStatus);
    end;
  end;
  
var
  x: Integer;
begin
  x:=GetNum;
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
                300..399: if Length(FMessage) > 0 then begin
                            FConnection.SendMessage(FMessage);
                            FMessage:='';
                          end;
              else        begin
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
                Disconnect;
                if Assigned(FOnDisconnect) then
                  FOnDisconnect(Self);
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

function TLSMTPClient.Connect(const aHost: string; const aPort: Word = 25): Boolean;
begin
  Result:=False;
  Disconnect;
  if FConnection.Connect(aHost, aPort) then begin
    FHost:=aHost;
    FPort:=aPort;
    FStatus.Insert(MakeStatusRec(ssCon, '', ''));
    Result:=True;
  end;
end;

function TLSMTPClient.Connect: Boolean;
begin
  Result:=Connect(FHost, FPort);
end;

function TLSMTPClient.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
var
  s: string;
begin
  Result:=FConnection.Get(aData, aSize, aSocket);
  if Result > 0 then begin
    SetLength(s, Result);
    Move(aData, PChar(s)^, Result);
    CleanInput(s);
  end;
end;

function TLSMTPClient.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=FConnection.GetMessage(msg, aSocket);
  if Result > 0 then
    Result:=CleanInput(msg);
end;

procedure TLSMTPClient.SendMail(const From, Recipients, Subject, Msg: string);
var
  i: Integer;
begin
  if (Length(Recipients) > 0) and (Length(From) > 0) then begin
    Mail(From);
    FSL.CommaText:=StringReplace(Recipients, ' ', ',', [rfReplaceAll]);
    for i:=0 to FSL.Count-1 do
      Rcpt(FSL[i]);
    Data('From: ' + From + SLE + 'Subject: ' + Subject + SLE + 'To: ' + FSL.CommaText + SLE + Msg);
    Rset;
  end;
end;

procedure TLSMTPClient.Helo(aHost: string = '');
begin
  if Length(Host) = 0 then
    aHost:=FHost;
  if CanContinue(ssHelo, aHost, '') then begin
    FConnection.SendMessage('HELO ' + aHost + SLE);
    FStatus.Insert(MakeStatusRec(ssHelo, '', ''));
  end;
end;

procedure TLSMTPClient.Ehlo(aHost: string = '');
begin
  if Length(aHost) = 0 then
    aHost:=FHost;
  if CanContinue(ssEhlo, aHost, '') then begin
    FConnection.SendMessage('EHLO ' + aHost + SLE);
    FStatus.Insert(MakeStatusRec(ssEhlo, '', ''));
  end;
end;

procedure TLSMTPClient.Mail(const From: string);
begin
  if CanContinue(ssMail, From, '') then begin
    FConnection.SendMessage('MAIL FROM:' + '<' + From + '>' + SLE);
    FStatus.Insert(MakeStatusRec(ssMail, '', ''));
  end;
end;

procedure TLSMTPClient.Rcpt(const RcptTo: string);
begin
  if CanContinue(ssRcpt, RcptTo, '') then begin
    FConnection.SendMessage('RCPT TO:' + '<' + RcptTo + '>' + SLE);
    FStatus.Insert(MakeStatusRec(ssRcpt, '', ''));
  end;
end;

procedure TLSMTPClient.Data(const Msg: string);
begin
  if CanContinue(ssData, Msg, '') then begin
    // TODO: clean SLEs and '.' on line starts
    FMessage:=Msg + SLE + '.' + SLE;
    FConnection.SendMessage('DATA' + SLE);
    FStatus.Insert(MakeStatusRec(ssData, '', ''));
  end;
end;

procedure TLSMTPClient.Rset;
begin
  if CanContinue(ssRset, '', '') then begin
    FConnection.SendMessage('RSET' + SLE);
    FStatus.Insert(MakeStatusRec(ssRset, '', ''));
  end;
end;

procedure TLSMTPClient.Quit;
begin
  if CanContinue(ssQuit, '', '') then begin
    FConnection.SendMessage('QUIT' + SLE);
    FStatus.Insert(MakeStatusRec(ssQuit, '', ''));
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

end.

