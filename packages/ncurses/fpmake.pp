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

    P:=AddPackage('ncurses');
{$ifdef ALLPACKAGES}
    P.Directory:='ncurses';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('menu.pp');
      with T.Dependencies do
        begin
          AddInclude('eti.inc');
          AddUnit('ncurses');
        end;
    T:=P.Targets.AddUnit('ncrt.pp');
      with T.Dependencies do
        begin
          AddInclude('ncrt.inc');
          AddUnit('ncurses');
        end;
    T:=P.Targets.AddUnit('ncurses.pp');
    T:=P.Targets.AddUnit('ocrt.pp');
      with T.Dependencies do
        begin
          AddInclude('ncrt.inc');
          AddInclude('pxpic.inc');
          AddUnit('ncurses');
          AddUnit('panel');
          AddUnit('menu');
        end;
    T:=P.Targets.AddUnit('panel.pp');
      with T.Dependencies do
        begin
          AddUnit('ncurses');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
