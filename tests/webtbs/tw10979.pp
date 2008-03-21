{$ifdef fpc}
{$mode objfpc}
{$endif fpc}

uses Classes;

{$ifndef fpc}
type
  ptruint = cardinal;
{$endif}

type
  TMyStringList = class(TStringList)
  private
    function GetObjects(Index: Integer): TStringList;
    procedure SetObjects(Index: Integer; const Value: TStringList);  
  public
    property Objects[Index: Integer]: TStringList read GetObjects write SetObjects;
  end;

function TMyStringList.GetObjects(Index: Integer): TStringList;
begin
  Result := TStringList(inherited Objects[Index]);
end;

procedure TMyStringList.SetObjects(Index: Integer; const Value: TStringList);
begin
  writeln('setobjects called');
  inherited Objects[Index] := Value;
end;

              
var
  SL: TMyStringList;
begin
  SL := TMyStringList.Create;
  SL.AddObject('Hello',SL);
  WriteLn(SL[0],':',PtrUint(SL.Objects[0]),':',PtrUint(SL));
  if (sl[0]<>'Hello') or
     (PtrUint(SL.Objects[0])<>PtrUint(SL)) then
    halt(1);
end.

