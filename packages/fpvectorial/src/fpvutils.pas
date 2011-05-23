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

// Color Conversion routines
function VColorToFPColor(AVColor: TvColor): TFPColor; inline;
function VColorToRGBHexString(AVColor: TvColor): string;
function RGBToVColor(AR, AG, AB: Byte): TvColor; inline;

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

end.

