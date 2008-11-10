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

    P:=AddPackage('gdbint');
{$ifdef ALLPACKAGES}
    P.Directory:='gdbint';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.Author := 'Library : Cygnus, header: Peter Vreman';
    P.License := 'Library: GPL2 or later, header: LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Interface to libgdb, the GDB debugger in library format';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    //
    // NOTE: the gdbver.inc dependancies gives warnings because the makefile.fpc
    // does a "cp src/gdbver_nogdb.inc src/gdbver.inc" to create it

    T:=P.Targets.AddUnit('gdbcon.pp');
      with T.Dependencies do
        begin
          AddUnit('gdbint');
        end;
    T:=P.Targets.AddUnit('gdbint.pp');
      with T.Dependencies do
        begin
          AddInclude('gdbver.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
