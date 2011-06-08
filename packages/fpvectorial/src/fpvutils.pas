{
fpvutils.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit fpvutils;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Math,
  fpvectorial, fpimage;

type
  T10Strings = array[0..9] of shortstring;

// Color Conversion routines
function VColorToFPColor(AVColor: TvColor): TFPColor; inline;
function VColorToRGBHexString(AVColor: TvColor): string;
function RGBToVColor(AR, AG, AB: Byte): TvColor; inline;
function  SeparateString(AString: string; ASeparator: Char): T10Strings;

implementation

function VColorToFPColor(AVColor: TvColor): TFPColor; inline;
begin
  Result.Red := AVColor.Red;
  Result.Green := AVColor.Green;
  Result.Blue := AVColor.Blue;
  Result.Alpha := AVColor.Alpha;
end;

function VColorToRGBHexString(AVColor: TvColor): string;
begin
  Result := Format('%.2x%.2x%.2x', [AVColor.Red, AVColor.Green, AVColor.Blue]);
end;

function RGBToVColor(AR, AG, AB: Byte): TvColor; inline;
begin
  Result.Red := AR;
  Result.Green := AG;
  Result.Blue := AB;
  Result.Alpha := 255;
end;

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function SeparateString(AString: string; ASeparator: Char): T10Strings;
var
  i, CurrentPart: Integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

end.

