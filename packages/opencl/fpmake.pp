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

    P:=AddPackage('opencl');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.1';
    P.Author := ' Dmitry "skalogryz" Boyarintsev; Kronos group';
    P.License := 'Library: modified BSD, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A OpenCL header';
    P.NeedLibC:= true;
    P.OSes:=[linux,win64,win32,darwin];
    P.CPUs:=[i386,x86_64];

    P.Dependencies.Add('opengl');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('cl.pp');
    T:=P.Targets.AddUnit('cl_gl.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
