program test_intf_query;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

uses
  Classes;

type
  ISimple = interface
  ['{24811FF3-4F01-4601-AC5C-22A5B5D46928}']
    function ShowSomething: Integer; cdecl;
    function GetIsBlob: Boolean; cdecl;
  end;

  TSimple = class(TInterfacedObject, ISimple)
  protected
    { ISimple implementation }
    function ShowSomething: Integer; cdecl;
    function GetIsBlob: Boolean; cdecl;
  end;

function QuerySimple(const OnChange: TNotifyEvent = nil): ISimple; cdecl;
begin
  Result := TSimple.Create;
end;

function TSimple.ShowSomething: Integer; cdecl;
begin
  Writeln('Message from ISimple');
  Result := 0;
end;

function TSimple.GetIsBlob: Boolean; cdecl;
begin
  Result := true;
end;

{*** main program ***}

var
  FSimple: ISimple;

begin
  FSimple := QuerySimple;
  FSimple.ShowSomething;
end.