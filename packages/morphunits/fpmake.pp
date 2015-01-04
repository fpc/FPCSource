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

    P:=AddPackage('morphunits');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='2.7.1';

    P.Author := 'Karoly Balogh';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A set of units for MorphOS. PowerPC only';
    P.NeedLibC:= false;

    P.CPUs:=[powerpc];
    P.OSes:=[MorphOS];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('exec.pas');
    T:=P.Targets.AddUnit('timer.pas');
    T:=P.Targets.AddUnit('utility.pas');
    T:=P.Targets.AddUnit('intuition.pas');
    T:=P.Targets.AddUnit('agraphics.pas');
    T:=P.Targets.AddUnit('amigalib.pas');
    T:=P.Targets.AddUnit('hardware.pas');
    T:=P.Targets.AddUnit('amigados.pas');
    T:=P.Targets.AddUnit('clipboard.pas');
    T:=P.Targets.AddUnit('iffparse.pas');
    T:=P.Targets.AddUnit('datatypes.pas');
    T:=P.Targets.AddUnit('keymap.pas');
    T:=P.Targets.AddUnit('asl.pas');
    T:=P.Targets.AddUnit('layers.pas');
    T:=P.Targets.AddUnit('inputevent.pas');
    T:=P.Targets.AddUnit('mui.pas');
    T:=P.Targets.AddUnit('muihelper.pas');
    T:=P.Targets.AddUnit('ahi.pas');
    T:=P.Targets.AddUnit('kvm.pas');
    T:=P.Targets.AddUnit('get9.pas');
    T:=P.Targets.AddUnit('tinygl.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
