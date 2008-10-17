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

    P:=AddPackage('numlib');
{$ifdef ALLPACKAGES}
    P.Directory:='numlib';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('det.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('mdt');
        end;
    T:=P.Targets.AddUnit('dsl.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('eigh1.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('eigh2.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('eig.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('eigh1');
          AddUnit('eigh2');
        end;
    T:=P.Targets.AddUnit('int.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('inv.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('mdt');
          AddUnit('dsl');
        end;
    T:=P.Targets.AddUnit('iom.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('ipf.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('mdt');
          AddUnit('dsl');
          AddUnit('sle');
          AddUnit('spe');
        end;
    T:=P.Targets.AddUnit('mdt.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('dsl');
          AddUnit('omv');
        end;
    T:=P.Targets.AddUnit('numlib.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('ode.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('omv.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('roo.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('spe');
        end;
    T:=P.Targets.AddUnit('sle.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('omv');
          AddUnit('dsl');
          AddUnit('mdt');
        end;
    T:=P.Targets.AddUnit('spe.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;
    T:=P.Targets.AddUnit('spl.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
          AddUnit('sle');
        end;
    T:=P.Targets.AddUnit('typ.pas');
      with T.Dependencies do
        begin
          AddInclude('direct.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
