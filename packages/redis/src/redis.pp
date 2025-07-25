{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by the Free Pascal development team

    Simple Redis client, donated by Mario Ray Mahardhika

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit redis;
{$ENDIF}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

// Define REDIS_DEBUG to debug
{ $define REDIS_DEBUG}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Net.Ssockets;
  {$ELSE}
  SysUtils, Classes, ssockets;
  {$ENDIF}

const
  DefaultHost           = '127.0.0.1';
  DefaultPort           = 6379;
  DefaultConnectTimeout = 100;
  DefaultCanReadTimeout = 100;
  CRLF                  = #13#10;
  CR                    = #13;

type
  ERedis = Class(Exception);
  TRESPType = (rtError,rtString,rtInteger,rtArray);

  TRESP = record
  private
    FRESPType: TRESPType;
    FErrorType: AnsiString;
    FStrValue: AnsiString;
    FIntValue: Integer;
    FElements: array of TRESP;
    function GetElement(const i: Integer): TRESP;
    procedure SetElement(const i: Integer; const AValue: TRESP);
    function GetElementCount: Integer;
    procedure SetElementCount(const AValue: Integer);
  public
    property RESPType: TRESPType read FRESPType write FRESPType;
    property ErrorType: AnsiString read FErrorType write FErrorType;
    property StrValue: AnsiString read FStrValue write FStrValue;
    property IntValue: Integer read FIntValue write FIntValue;
    property Elements[const i: Integer]: TRESP read GetElement write SetElement;
    property ElementCount: Integer read GetElementCount write SetElementCount;
  end;

  TOnSendRedisMessage = Procedure(Sender : TObject; var aMessage : AnsiString) of object;
  TOnReceiveRedisMessage = Procedure(Sender : TObject; const aMessage : AnsiString) of object;

  { TAbstractTCPClient }

  TAbstractTCPClient = class abstract
  private
    FOnReceive: TOnReceiveRedisMessage;
    FOnSend: TOnSendRedisMessage;
    FHost: AnsiString;
    FOnTimeOut: TNotifyEvent;
    FPort: Word;
    FConnectTimeout : Integer;
    FCanReadTimeout : Integer;
  Protected
    Procedure DoOnSend(var aMsg : AnsiString); virtual;
    Procedure DoOnReceive(const aMsg : AnsiString); virtual;
    function DoSend(const AMsg: AnsiString): AnsiString; virtual; abstract;
  public
    constructor Create(const AHost: AnsiString; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer); virtual;
    function Send(AMsg: AnsiString): AnsiString;
    Property OnSend : TOnSendRedisMessage Read FOnSend Write FOnSend;
    Property OnReceive : TOnReceiveRedisMessage Read FOnReceive Write FOnReceive;
    Property Host : AnsiString Read FHost;
    Property Port : Word Read FPort;
    Property ConnectTimeout : Integer Read FConnectTimeout;
    Property CanReadTimeout : Integer Read FCanReadTimeout Write FCanReadTimeout;
    Property OnTimeOut : TNotifyEvent Read FOnTimeOut Write FOnTimeOut;
  end;
  TAbstractTCPClientClass = class of TAbstractTCPClient;

  { TSSocketsTCPClient }

  TSSocketsTCPClient = class(TAbstractTCPClient)
  private
    FConn: TInetSocket;
  protected
    function DoSend(const AMsg: AnsiString): AnsiString; override;
  public
    constructor Create(const AHost: AnsiString; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer); override;
    destructor Destroy; override;
  end;

  { TRedis }

  TRedis = class
  private
    FConn: TAbstractTCPClient;
  protected
    class function RESPStringToRESP(const ARespString: AnsiString): TRESP;
  public
    constructor Create(AConn: TAbstractTCPClient);
    function SendCommand(AParams: array of const): TRESP;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  {$ifdef REDIS_DEBUG} System.StrUtils, {$endif REDIS_DEBUG} System.Strings;
  {$ELSE}
  {$ifdef REDIS_DEBUG} StrUtils, {$endif REDIS_DEBUG} Strings;
  {$ENDIF}

resourcestring
  SErrUnsupportedType = '';

{ TRESP }

function TRESP.GetElement(const i: Integer): TRESP;
begin
  Result := FElements[i];
end;

procedure TRESP.SetElement(const i: Integer; const AValue: TRESP);
begin
  FElements[i] := AValue;
end;

function TRESP.GetElementCount: Integer;
begin
  Result := Length(FElements);
end;

procedure TRESP.SetElementCount(const AValue: Integer);
begin
  SetLength(FElements, AValue);
end;


{ TAbstractTCPClient }

procedure TAbstractTCPClient.DoOnSend(var aMsg: AnsiString);
begin
  if Assigned(FOnSend) then
    FOnSend(Self,aMsg);
end;

procedure TAbstractTCPClient.DoOnReceive(const aMsg: AnsiString);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self,aMsg);
end;

constructor TAbstractTCPClient.Create(const AHost: AnsiString; const APort: Word; AConnectTimeout, ACanReadTimeout: Integer);
begin
  FHost:=AHost;
  FPort:=APort;
  FConnectTimeout:=AConnectTimeout;
  FCanReadTimeout:=ACanReadTimeout;
end;

function TAbstractTCPClient.Send(AMsg: AnsiString): AnsiString;
var
  lRes : AnsiString;
begin
  DoOnSend(aMsg);
  lRes:=DoSend(aMsg);
  DoOnReceive(lRes);
  Result:=lRes;
end;

{ TSSocketsTCPClient }

constructor TSSocketsTCPClient.Create(const AHost: AnsiString; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer);
begin
  inherited;
  FConn := TInetSocket.Create(AHost, APort, AConnectTimeout);
end;

destructor TSSocketsTCPClient.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

function TSSocketsTCPClient.DoSend(const AMsg: AnsiString): AnsiString;
const
  ChunkSize = 255;
var
  LLengthSoFar,LRecvSize: Integer;
begin
  Result:='';
  {$ifdef REDIS_DEBUG}
  WriteLn('send: ' + StringsReplace(AMsg,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif REDIS_DEBUG}
  FConn.Write(AMsg[1],Length(AMsg));

  while not FConn.CanRead(CanReadTimeout) do
    begin
    Sleep(1); // better than no-op, will not hog CPU
    if Assigned(OnTimeOut) then
      OnTimeOut(Self);
    end;
  LLengthSoFar := 0;
  repeat
    SetLength(Result, LLengthSoFar + ChunkSize);
    LRecvSize := FConn.Read(Result[LLengthSoFar + 1], ChunkSize);
    Inc(LLengthSoFar, LRecvSize);
  until LRecvSize < ChunkSize;
  SetLength(Result, LLengthSoFar);
  {$ifdef REDIS_DEBUG}
  WriteLn('recv: ' + StringsReplace(Result,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif REDIS_DEBUG}
end;

{ TRedis }

constructor TRedis.Create(AConn: TAbstractTCPClient);
begin
  FConn := AConn;
end;

function ArrayOfConstToRESPString(AParams: array of const): AnsiString;
var
  LStrs: TStrings;
  i: Integer;
  LParam: TVarRec;
  LStr: AnsiString;
begin
  LStrs := TStringList.Create;
  LStrs.TextLineBreakStyle := tlbsCRLF;

  try
    LStrs.Add('*' + IntToStr(Length(AParams)));
    for i := Low(AParams) to High(AParams) do
      begin
      LParam := AParams[i];
      case LParam.vtype of
        vtWideString:
          begin
          LStr := UTF8Encode(AnsiString(LParam.VAnsiString));
          LStrs.Add('$' + IntToStr(Length(LStr)));
          LStrs.Add(LStr);
          end;
        vtAnsiString:
          begin
          LStr := AnsiString(LParam.VAnsiString);
          LStrs.Add('$' + IntToStr(Length(LStr)));
          LStrs.Add(LStr);
          end;
        vtChar:
          begin
          LStr := LParam.VChar;
          LStrs.Add('$1');
          LStrs.Add(LStr);
          end;
        vtWideChar:
          begin
          LStr :=UTF8Encode( LParam.VWideChar);
          LStrs.Add('$1');
          LStrs.Add(LStr);
          end;
        vtInteger:
          begin
          LStr := IntToStr(LParam.VInteger);
          LStrs.Add(':' + LStr);
          end;
        else
          raise ERedis.CreateFmt(SErrUnsupportedType,[LParam.vtype]);
      end;
      end;
    Result := LStrs.Text;
  finally
    LStrs.Free;
  end;
end;

class function TRedis.RESPStringToRESP(const ARespString: AnsiString): TRESP;

  function RESPPCharToRESP(var APC: PAnsiChar): TRESP;
  var
    LPos: PAnsiChar;
    LCount,i: Integer;
    LStr: AnsiString;
  begin
    LStr:='';
    case APC^ of
      '+':
        begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then
          begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := Default(TResp);
          Result.RESPType := rtString;
          Result.StrValue := LStr;
          end;
        APC := LPos + 2;
        end;
      '-':
        begin
        LPos := StrPos(APC, ' ');
        // the spec says space or newline, this is just to comply although when this is true that means the error has no StrValue at all
        if not Assigned(LPos) then
          LPos := StrPos(APC, CRLF);
        if Assigned(LPos) then
          begin
          LCount := LPos - APC - 1;
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := Default(TResp);
          Result.ErrorType := LStr;
          // current char not CR means it's an not empty error, get the StrValue
          if LPos^ <> CR then
            begin
            APC := LPos + 1;
            LPos := StrPos(APC, CRLF);
            LCount := LPos - APC;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result.RESPType := rtError;
            Result.StrValue := LStr;
            end;
          end;
        end;
      ':':
        begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then
          begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := Default(TResp);
          Result.RESPType := rtInteger;
          Result.IntValue := StrToInt(LStr);
          end;
        APC := LPos + 2;
        end;
      '$':
        begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then
          begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          LCount := StrToInt(LStr);
          end;
        Result := Default(TResp);
        Result.RESPType := rtString;
        case LCount of
          0:
            begin
            Result.StrValue := '';
            end;
          else
            APC := LPos + 2;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result.StrValue := LStr;
        end;
        Inc(APC, LCount + 2);
        end;
      '*':
        begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then
          begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          LCount := StrToInt(LStr);
          end;
        APC := LPos + 2;
        Result := Default(TResp);
        Result.RESPType := rtArray;
        Result.ElementCount := LCount;
        for i := 0 to LCount - 1 do
          begin
          Result.Elements[i] := RESPPCharToRESP(APC);
          end;
        end;
     end;
  end;

var
  LPC: PAnsiChar;
begin
  LPC := @ARespString[1];
  Result := RESPPCharToRESP(LPC);
end;

function TRedis.SendCommand(AParams: array of const): TRESP;
var
  LStr: AnsiString;
begin
  LStr := ArrayOfConstToRESPString(AParams);
  LStr := FConn.Send(LStr);
  {$IFDEF REDIS_DEBUG}
  Writeln('Received : ',LStr);
  {$ENDIF}
  Result := RESPStringToRESP(LStr);
end;

end.
