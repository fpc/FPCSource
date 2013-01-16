{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a basic logger interface

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_logger_intf;

interface

uses
  SysUtils; 

type

  TMessageType = ( mtInfo, mtError );

const
  MessageTypeNames : array[TMessageType] of string = ( 'Information', 'Error' );

type

  ILogger = interface
    ['{158C90B5-BAC3-40A1-B471-C9327692A3BF}']
    procedure Log(const AMsgType : TMessageType; const AMsg : string);overload;
    procedure Log(const AMsgType : TMessageType; const AMsg : string; const AArgs : array of const);overload;
    function GetMessageCount(const AMsgType : TMessageType) : Integer;
  end;

  { TSimpleConsoleLogger }

  TSimpleConsoleLogger = class(TInterfacedObject,ILogger)
  private
    FMessageCount : array[TMessageType] of Integer;
  protected
    procedure Log(const AMsgType : TMessageType; const AMsg : string);overload;
    procedure Log(const AMsgType : TMessageType; const AMsg : string; const AArgs : array of const);overload;
    function GetMessageCount(const AMsgType : TMessageType) : Integer;
  end;


  function HasLogger() : Boolean;
  function SetLogger(ALogger : ILogger) : ILogger;
  function GetLogger() : ILogger;
  
implementation

var FLogger : ILogger = nil;
function SetLogger(ALogger : ILogger) : ILogger;
begin
  Result := FLogger;
  FLogger := ALogger;
end;

function GetLogger() : ILogger;
begin
  Result := FLogger;
end;

function HasLogger() : Boolean;
begin
  Result := Assigned(FLogger);
end;

{ TSimpleConsoleLogger }

procedure TSimpleConsoleLogger.Log(const AMsgType: TMessageType; const AMsg: string);
begin
  Log(AMsgType,AMsg,[]);
end;

procedure TSimpleConsoleLogger.Log(
  const AMsgType : TMessageType;
  const AMsg     : string;
  const AArgs    : array of const
);
begin
  Inc(FMessageCount[AMsgType]);
  WriteLn(Format('%s : %s',[MessageTypeNames[AMsgType],Format(AMsg,AArgs)]));
end;

function TSimpleConsoleLogger.GetMessageCount(const AMsgType: TMessageType): Integer;
begin
  Result := FMessageCount[AMsgType];
end;

end.

