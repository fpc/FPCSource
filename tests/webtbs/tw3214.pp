{$mode objfpc}{$H+}{$r+}

uses
  Classes, SysUtils;

type
  TMyEvent = class(TObject)
  public
    procedure SaveToStream(aStream: TStream); virtual; abstract;
  end;

  TMyClass = class
  private
    function GetEvent(aIndex: integer): TMyEvent;
    function GetEventCount: integer;
  public
    property EventCount: integer read GetEventCount;
    property Events[aIndex: integer]: TMyEvent read GetEvent;
    procedure SaveToStream(aDest: TStream);
  end;

{ TMyClass }

function TMyClass.GetEvent(aIndex: integer): TMyEvent;
begin
  Result:=nil;
end;

function TMyClass.GetEventCount: integer;
begin
  Result:=0;
end;

procedure TMyClass.SaveToStream(aDest: TStream);
var
  cEvent: Integer;
  eCnt: Integer;
begin
  eCnt:=EventCount;
  aDest.Write(eCnt, sizeof(eCnt));
  for cEvent := 0 to eCnt -1 do
    Events[cEvent].SaveToStream(aDest);
end;

begin
end.
