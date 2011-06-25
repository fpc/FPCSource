{
Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvmodifytest;

{$mode objfpc}{$H+}

uses
  fpvectorial, svgvectorialwriter, svgvectorialreader, fpvutils;

const
  cFormat = vfSVG;
  cExtension = '.svg';
var
  Vec: TvVectorialDocument;
  Path: TPath;
  i: Integer;
  Segment: TPathSegment;
  _2DSegment: T2DSegment;
  BezSegment: T2DBezierSegment;
begin
  Vec := TvVectorialDocument.Create;
  try
    // Read the file
    Vec.ReadFromFile('bezier_1.svg');

    // Now add 10 to the Y coordinate of all elements
    for i := 0 to Vec.GetPathCount() - 1 do
    begin
      Path := Vec.GetPath(i);
      Path.PrepareForSequentialReading();
      Path.Next();
      while Path.CurPoint <> nil do
      begin
        Segment := Path.CurPoint;

        if Segment is T2DBezierSegment then
        begin
          BezSegment := Segment as T2DBezierSegment;
          BezSegment.Y := BezSegment.Y + 10;
          BezSegment.Y2 := BezSegment.Y2 + 10;
          BezSegment.Y3 := BezSegment.Y3 + 10;
        end
        else if Segment is T2DSegment then
        begin
          _2DSegment := Segment as T2DSegment;
          _2DSegment.Y := _2DSegment.Y + 10;
        end;

        Path.Next();
      end;
    end;

    // Write the changed file to disk
    Vec.WriteToFile('bezier_1_mod' + cExtension, cFormat);
  finally
    Vec.Free;
  end;
end.

