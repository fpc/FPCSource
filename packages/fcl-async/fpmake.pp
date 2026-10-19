{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    fpmake.pp for fcl-async package

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}

{$mode objfpc}{$H+}

program fpmake;

uses fpmkunit;

var
  P: TPackage;
  T: TTarget;
begin
  with Installer do
  begin
    P := AddPackage('fcl-async');
    P.ShortName := 'fclasyn';
    P.Description := 'Coroutines, Fibers, and Async State Machines for Free Pascal';
    P.Version := '3.3.1';
    P.Author := 'Free Pascal development team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'http://www.freepascal.org/';

    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('vcl-compat');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-eventloop');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T := P.Targets.AddUnit('async.types.pp');
    T := P.Targets.AddUnit('async.fiber.pp');
    T := P.Targets.AddUnit('async.generator.pp');
    T := P.Targets.AddUnit('async.statemachine.pp');
    T := P.Targets.AddUnit('fclasync.pp');

    Run;
  end;
end.
