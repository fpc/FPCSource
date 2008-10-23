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

    P:=AddPackage('bzip2');
{$ifdef ALLPACKAGES}
    P.Directory:='bzip2';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Version:='2.2.2-0';
    P.Author := 'Library: Julian R. Seward, header: Daniel Mantione';
    // 3 clause becaue "prominent notice" is not required.
    P.License := 'Library: 3 clause BSD, header: 3 clause BSD ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'BZip2 decompression unit.';
    P.NeedLibC:= true;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('bzip2.pas');
      with T.Dependencies do
        begin
          AddInclude('bzip2i386.inc',[i386],AllOSes);
        end;

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('pasbzip.pas');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
