unit TinyPTC;

{$MODE objfpc}

interface

function ptc_open(const ATitle: string; AWidth, AHeight: Integer): Boolean;
function ptc_update(ABuffer: Pointer): Boolean;
procedure ptc_close;

implementation

uses
  SysUtils, ptc;

var
  Console: TPTCConsole = nil;
  Format: TPTCFormat = nil;
  Palette: TPTCPalette = nil;
  Width, Height: Integer;

function ptc_open(const ATitle: string; AWidth, AHeight: Integer): Boolean;
begin
  try
    if Console = nil then
      Console := TPTCConsole.Create;
    if Format = nil then
      Format := TPTCFormat.Create(32, $FF0000, $FF00, $FF);
    if Palette = nil then
      Palette := TPTCPalette.Create;
    Console.Open(ATitle, AWidth, AHeight, Format);
    Width := AWidth;
    Height := AHeight;
    Result := true;
  except
    on error: TPTCError do
      Result := false;
  end;
end;

function ptc_update(ABuffer: Pointer): Boolean;
begin
  try
    Console.Load(ABuffer, Width, Height, Width*4, Format, Palette);
    Result := true;
  except
    on error: TPTCError do
      Result := false;
  end;
end;

procedure ptc_close;
begin
  if Assigned(Console) then
    Console.Close;
  FreeAndNil(Console);
  FreeAndNil(Format);
  FreeAndNil(Palette);
end;

finalization
  ptc_close;
end.
