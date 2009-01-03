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

    P:=AddPackage('x11');
{$ifdef ALLPACKAGES}
    P.Directory:='x11';
{$endif ALLPACKAGES}
    P.Version:='2.2.4-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('cursorfont.pp');
    T:=P.Targets.AddUnit('keysym.pp');
    T:=P.Targets.AddUnit('xatom.pp');
    T:=P.Targets.AddUnit('xcms.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xf86dga.pp');
      with T.Dependencies do
        begin
          AddInclude('xf86dga1.inc');
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xf86vmode.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xinerama.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xi.pp');
    T:=P.Targets.AddUnit('xkblib.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
          AddUnit('xkb');
        end;
    T:=P.Targets.AddUnit('xkb.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
          AddUnit('xi');
        end;
    T:=P.Targets.AddUnit('xlib.pp');
    T:=P.Targets.AddUnit('x.pp');
    T:=P.Targets.AddUnit('xrandr.pp');
      with T.Dependencies do
        begin
          AddInclude('randr.inc');
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xrender.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xresource.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xshm.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('xutil.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
          AddUnit('keysym');
        end;
    T:=P.Targets.AddUnit('xvlib.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
          AddUnit('xshm');
        end;
    T:=P.Targets.AddUnit('xv.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
