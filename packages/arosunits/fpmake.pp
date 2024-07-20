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

    P:=AddPackage('arosunits');
    P.ShortName := 'aru';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';

    P.Author := 'Marcus Sackrow';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A set of units for AROS.';
    P.NeedLibC:= false;

    P.CPUs:=[arm,i386,x86_64];
    P.OSes:=[aros];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('agraphics.pas');
    T:=P.Targets.AddUnit('amigados.pas');
    T:=P.Targets.AddUnit('exec.pas');
    T:=P.Targets.AddUnit('hardware.pas');
    T:=P.Targets.AddUnit('inputevent.pas');
    T:=P.Targets.AddUnit('intuition.pas');
    T:=P.Targets.AddUnit('keymap.pas');
    T:=P.Targets.AddUnit('layers.pas');
    T:=P.Targets.AddUnit('longarray.pas');
    T:=P.Targets.AddUnit('tagsarray.pas');
    T:=P.Targets.AddUnit('timer.pas');
    T:=P.Targets.AddUnit('utility.pas');
    T:=P.Targets.AddUnit('diskfont.pas');
    T:=P.Targets.AddUnit('iffparse.pas');
    T:=P.Targets.AddUnit('clipboard.pas');
    T:=P.Targets.AddUnit('workbench.pas');
    T:=P.Targets.AddUnit('icon.pas');
    T:=P.Targets.AddUnit('gadtools.pas');
    T:=P.Targets.AddUnit('asl.pas');
    T:=P.Targets.AddUnit('mui.pas');
    T:=P.Targets.AddUnit('cybergraphics.pas');
    T:=P.Targets.AddUnit('locale.pas');
    T:=P.Targets.AddUnit('commodities.pas');
    T:=P.Targets.AddUnit('datatypes.pas');
    T:=P.Targets.AddUnit('serial.pas');
    T:=P.Targets.AddUnit('console.pas');
    T:=P.Targets.AddUnit('conunit.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
