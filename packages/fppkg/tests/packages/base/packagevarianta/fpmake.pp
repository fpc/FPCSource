{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

var
  P : TPackage;
  T : TTarget;
  ProcVersionVariant: TPackageVariants;
  ProcVersionA,
  ProcVersionB : TPackageVariant;

begin
  with Installer do
    begin
    P:=AddPackage('packagevarianta');
    P.Version:='3.2.2';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Package which comes in two flavours: VersionA and VersionB.';

    P.SourcePath.Add('src');

    // Definition of Widgetsets
    ProcVersionVariant := AddPackageVariant('ProcVersion', true);
    p.AddPackageVariant(ProcVersionVariant);

    ProcVersionA := ProcVersionVariant.Add('versiona');
    ProcVersionB := ProcVersionVariant.Add('versionb');

    ProcVersionVariant.DefaultPackageVariantName:='versiona';

    // Not used, but for example:
    // P.IncludePath.Add('$(ProcVersion)');
    // P.SourcePath.Add('$(ProcVersion)');

    P.Options.Add('-d$(ProcVersion)');
    P.TransmitOptions.Add('-dPROC$(ProcVersion)');

    // ProcVersion-specific options
    ProcVersionA.Options.Add('-dHello');

    T:=P.Targets.AddUnit('packagevariantbaseunit.pp');

    ProcVersionB.Targets.AddUnit('packagevariantversionbonly.pp');

    Run;
    end;
end.
