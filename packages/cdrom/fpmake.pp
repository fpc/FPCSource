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

    P:=AddPackage('cdrom');
    P.ShortName:='cdr';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.1';
    P.OSes:=[Win32,Win64,Linux];

    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Unit to read a CDROM disc TOC and get a list of CD Rom devices';
    P.NeedLibC:= False;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('cdrom.pp');
      with T.Dependencies do
        begin
          AddInclude('cdromlin.inc',[Linux]);
          AddInclude('cdromw32.inc',[Win32,Win64]);
          AddUnit('lincd',[Linux]);
          AddUnit('wincd',[Win32,Win64]);
        end;
    T:=P.Targets.AddUnit('discid.pp');
      with T.Dependencies do
        begin
          AddUnit('cdrom');
        end;
    T:=P.Targets.AddUnit('fpcddb.pp');
    T.ResourceStrings := True;

    // Linux
    T:=P.Targets.AddUnit('lincd.pp',[Linux]);
      with T.Dependencies do
        begin
          AddUnit('major');
        end;
    T:=P.Targets.AddUnit('major.pp',[Linux]);

    // Windows
    T:=P.Targets.AddUnit('cdromioctl.pp',[Win32,Win64]);
    T:=P.Targets.AddUnit('scsidefs.pp',[Win32,Win64]);
    T:=P.Targets.AddUnit('wincd.pp',[Win32,Win64]);
      with T.Dependencies do
        begin
          AddUnit('cdromioctl');
          AddUnit('wnaspi32');
          AddUnit('scsidefs');
        end;
    T:=P.Targets.AddUnit('wnaspi32.pp',[Win32,Win64]);


    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('getdiscid.pp');
    T:=P.Targets.AddExampleProgram('showcds.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

