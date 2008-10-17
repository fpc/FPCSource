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

    P:=AddPackage('mysql');
{$ifdef ALLPACKAGES}
    P.Directory:='mysql';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('my4_sys.pp');
    T:=P.Targets.AddUnit('mysql3_comdyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql3_comtypes.inc');
        end;
    T:=P.Targets.AddUnit('mysql3_com.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql3_comtypes.inc');
          AddUnit('mysql3_version');
        end;
    T:=P.Targets.AddUnit('mysql3dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql3types.inc');
          AddInclude('mysql3impl.inc');
          AddUnit('mysql3_comdyn');
        end;
    T:=P.Targets.AddUnit('mysql3.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql3types.inc');
          AddInclude('mysql3impl.inc');
          AddUnit('mysql3_com');
          AddUnit('mysql3_version');
        end;
    T:=P.Targets.AddUnit('mysql3_version.pp');
    T:=P.Targets.AddUnit('mysql40dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;
    T:=P.Targets.AddUnit('mysql40.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;
    T:=P.Targets.AddUnit('mysql41dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;
    T:=P.Targets.AddUnit('mysql41.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;
    T:=P.Targets.AddUnit('mysql4_comdyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql4_comtypes.inc');
          AddUnit('my4_sys');
        end;
    T:=P.Targets.AddUnit('mysql4_com.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql4_comtypes.inc');
        end;
    T:=P.Targets.AddUnit('mysql4dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql4types.inc');
          AddInclude('mysql4impl.inc');
          AddUnit('my4_sys');
          AddUnit('mysql4_comdyn');
        end;
    T:=P.Targets.AddUnit('mysql4.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql4types.inc');
          AddInclude('mysql4impl.inc');
          AddUnit('my4_sys');
          AddUnit('mysql4_com');
        end;
    T:=P.Targets.AddUnit('mysql4_version.pp');
    T:=P.Targets.AddUnit('mysql50dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;
    T:=P.Targets.AddUnit('mysql50.pp');
      with T.Dependencies do
        begin
          AddInclude('mysql.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
