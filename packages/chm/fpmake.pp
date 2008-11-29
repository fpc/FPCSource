{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('chm');
{$ifdef ALLPACKAGES}
    P.Directory:='chm';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';

    D:=P.Dependencies.Add('fcl-xml');
      D.Version:='2.2.1';
    
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('chmbase.pas');
    T:=P.Targets.AddUnit('chmfilewriter.pas');
      with T.Dependencies do
        begin
          AddUnit('chmwriter');
        end;
    T:=P.Targets.AddUnit('chmreader.pas');
      with T.Dependencies do
        begin
          AddUnit('chmbase');
          AddUnit('paslzx');
        end;
    T:=P.Targets.AddUnit('chmsitemap.pas');
      with T.Dependencies do
        begin
          AddUnit('fasthtmlparser');
          AddUnit('htmlutil');
        end;
    T:=P.Targets.AddUnit('chmspecialfiles.pas');
      with T.Dependencies do
        begin
          AddUnit('chmtypes');
        end;
    T:=P.Targets.AddUnit('chmtypes.pas');
      with T.Dependencies do
        begin
          AddUnit('chmbase');
        end;
    T:=P.Targets.AddUnit('htmlindexer.pas');
      with T.Dependencies do
        begin
          AddUnit('fasthtmlparser');
          AddUnit('htmlutil');
        end;
    T:=P.Targets.AddUnit('chmwriter.pas');
      with T.Dependencies do
        begin
          AddUnit('chmbase');
          AddUnit('chmtypes');
          AddUnit('chmspecialfiles');
          AddUnit('paslzxcomp');
          AddUnit('chmfiftimain');
        end;
    T:=P.Targets.AddUnit('fasthtmlparser.pas');
    T:=P.Targets.AddUnit('htmlutil.pas');
    T:=P.Targets.AddUnit('paslznonslide.pas');
    T:=P.Targets.AddUnit('paslzx.pas');
    T:=P.Targets.AddUnit('paslzxcomp.pas');
      with T.Dependencies do
        begin
          AddUnit('paslznonslide');
        end;
    T:=P.Targets.AddUnit('chmfiftimain.pas');
      with T.Dependencies do
        begin
          AddUnit('htmlindexer');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
