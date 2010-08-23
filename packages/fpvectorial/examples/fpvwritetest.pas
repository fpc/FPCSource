{
FPVectorial example application for writing vectorial images
generated in code to disk. This program will generate the following
vectorial images:

single_line_1    One line from (0, 20) to (30, 30)
single_line_2    One line from (20, 30) to (30, 20)
polyline_1       One line from (0, 0) to (10, 10) to (20, 30) to (30, 20)
polyline_2       One line from (10, 10) to (20, 30) to (30, 20) to (40, 40)
bezier_1         One path starting in (0, 0) lining to (10, 10) then bezier to (20, 10) and then line to (30, 0)
bezier_2         One curve from (10, 10) to (20, 20)
text_ascii       One text written at (10, 10)
text_europen     One text testing european languages at (20, 20)
text_asian       One text testing asian languages at (30, 30)

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvwritetest;

{$mode objfpc}{$H+}

uses
  fpvectorial, svgvectorialwriter;

const
  cFormat = vfSVG;
  cExtension = '.svg';
var
  Vec: TvVectorialDocument;
begin
  Vec := TvVectorialDocument.Create;
  try
    // single_line_1    One line from (0, 20) to (30, 30)
    Vec.StartPath(0, 20);
    Vec.AddLineToPath(30, 30);
    Vec.EndPath();
    Vec.WriteToFile('single_line_1' + cExtension, cFormat);

    //    single_line_2    One line from (20, 30) to (30, 20)
    Vec.Clear;
    Vec.StartPath(20, 30);
    Vec.AddLineToPath(30, 20);
    Vec.EndPath();
    Vec.WriteToFile('single_line_2' + cExtension, cFormat);

    //    polyline_1       One line from (0, 0) to (10, 10) to (20, 30) to (30, 20)
    Vec.Clear;
    Vec.StartPath(0, 0);
    Vec.AddLineToPath(10, 10);
    Vec.AddLineToPath(20, 30);
    Vec.AddLineToPath(30, 20);
    Vec.EndPath();
    Vec.WriteToFile('polyline_1' + cExtension, cFormat);

    //    polyline_2       One line from (10, 10) to (20, 30) to (30, 20) to (40, 40)
    Vec.Clear;
    Vec.StartPath(10, 10);
    Vec.AddLineToPath(20, 30);
    Vec.AddLineToPath(30, 20);
    Vec.AddLineToPath(40, 40);
    Vec.EndPath();
    Vec.WriteToFile('polyline_2' + cExtension, cFormat);
    // bezier_1         One path starting in (0, 0) lining to (10, 10) then bezier to (20, 10) and then line to (30, 0)
    // bezier_2         One curve from (10, 10) to (20, 20)
    // text_ascii       One text written at (10, 10)
    // text_europen     One text testing european languages at (20, 20)
    // text_asian       One text testing asian languages at (30, 30)
  finally
    Vec.Free;
  end;
end.

