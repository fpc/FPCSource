{$MODE objfpc}
{$H+}
{$inline on}

unit u41384b;

interface

uses
  SysUtils, Classes, u41384a;

operator:=(const AStr:String):TNetworkAddress;

var
  DefaultAddress : TNetworkAddress;
  LocalHostAddress : TNetworkAddress;

implementation

operator:=(const AStr:String):TNetworkAddress;
begin
  Result := NetAddr(AStr);
end;

begin
  DefaultAddress:=Default(TNetworkAddress);
  LocalHostAddress:='127.0.0.1';
end.

