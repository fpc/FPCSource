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

    P:=AddPackage('fftw');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.0.1';

    P.Version:='3.0.1';
    P.Author := 'Library: Matteo Frigo and Steven G. Johnson, header: Daniel Mantione';
    P.License := 'Library: GPL or commercial, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Library for computing the discrete Fourier transform (DFT) in one or more dimensions, of arbitrary input size, and of both real and complex data';
    P.NeedLibC:= true; // should be true for a header linking to C?
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fftw_s.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('examples/example.pas');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
