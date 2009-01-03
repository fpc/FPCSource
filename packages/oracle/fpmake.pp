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

    P:=AddPackage('oracle');
{$ifdef ALLPACKAGES}
    P.Directory:='oracle';
{$endif ALLPACKAGES}
    P.Version:='2.2.4-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('ocidyn.pp');
      with T.Dependencies do
        begin
          AddInclude('oci.inc');
          AddInclude('ocidfn.inc');
          AddInclude('oci1.inc');
          AddInclude('oro_interface.inc');
          AddInclude('orl.inc');
          AddInclude('ort.inc');
          AddInclude('ociap.inc');
          AddInclude('nzt.inc');
          AddInclude('nzerror.inc');
          AddInclude('oro_implementation.inc');
          AddUnit('oratypes');
        end;
    T:=P.Targets.AddUnit('oci.pp');
      with T.Dependencies do
        begin
          AddInclude('oci.inc');
          AddInclude('ocidfn.inc');
          AddInclude('oci1.inc');
          AddInclude('oro_interface.inc');
          AddInclude('orl.inc');
          AddInclude('ort.inc');
          AddInclude('ociap.inc');
          AddInclude('nzt.inc');
          AddInclude('nzerror.inc');
          AddInclude('oro_implementation.inc');
          AddUnit('oratypes');
        end;
    T:=P.Targets.AddUnit('oraoci.pp');
    T:=P.Targets.AddUnit('oratypes.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
