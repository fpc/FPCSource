{
Writes an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit svgvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial;

type
  { TvSVGVectorialWriter }

  TvSVGVectorialWriter = class(TvCustomVectorialWriter)
  public
    { General reading methods }
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); virtual;
  end;

implementation

{ TvSVGVectorialWriter }

procedure TvSVGVectorialWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
begin

end;

initialization

  RegisterVectorialWriter(TvSVGVectorialWriter, vfSVG);

end.

