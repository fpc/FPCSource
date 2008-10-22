{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('imagemagick');
{$ifdef ALLPACKAGES}
    P.Directory:='imagemagick';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('buildim.pp');
      with T.Dependencies do
        begin
          AddUnit('imagemagick');
          AddUnit('magick_wand');
        end;
    T:=P.Targets.AddUnit('imagemagick.pas');
      with T.Dependencies do
        begin
          AddInclude('magick_type.inc');
          AddInclude('type.inc');
          AddInclude('cache_view.inc');
          AddInclude('compare.inc');
          AddInclude('constitute.inc');
          AddInclude('draw.inc');
          AddInclude('effect.inc');
          AddInclude('fx.inc');
          AddInclude('pixel.inc');
          AddInclude('quantize.inc');
          AddInclude('statistic.inc');
        end;
    T:=P.Targets.AddUnit('magick_wand.pas');
      with T.Dependencies do
        begin
          AddInclude('pixel_wand.inc');
          AddInclude('drawing_wand.inc');
          AddInclude('magick_attribute.inc');
          AddInclude('magick_image.inc');
          AddInclude('pixel_iterator.inc');
          AddUnit('imagemagick');
        end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('screenshot.lpr');
    P.Targets.AddExampleProgram('wanddemo.lpr');
    P.Targets.AddExampleProgram('wanddemo.dpr');
    // 'image.png

    {$ifndef ALLPACKAGES}
        Run;
        end;
    end.
    {$endif ALLPACKAGES}
