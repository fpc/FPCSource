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
    P.Version:='2.2.2-0';

    P.Author := 'Andrew Haines';
    P.License := 'LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Standalone CHM reader and writer library';
    P.NeedLibC:= false;

    D:=P.Dependencies.Add('fcl-xml');
    D.Version:='2.2.2-0';
    
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
    T:=P.Targets.AddUnit('chmwriter.pas');
      with T.Dependencies do
        begin
          AddUnit('chmbase');
          AddUnit('chmtypes');
          AddUnit('chmspecialfiles');
          AddUnit('paslzxcomp');
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

//    P.ProgramPath.Add('src');
    T:=P.Targets.AddProgram('chmls.lpr');
    T:=P.Targets.AddProgram('chmcmd.lpr');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
