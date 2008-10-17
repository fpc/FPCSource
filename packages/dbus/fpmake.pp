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

    P:=AddPackage('dbus');
{$ifdef ALLPACKAGES}
    P.Directory:='dbus';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('dbus.pas');
      with T.Dependencies do
        begin
          AddInclude('dbus-macros.inc');
          AddInclude('dbus-arch-deps.inc');
          AddInclude('dbus-types.inc');
          AddInclude('dbus-errors.inc');
          AddInclude('dbus-address.inc');
          AddInclude('dbus-message.inc');
          AddInclude('dbus-memory.inc');
          AddInclude('dbus-shared.inc');
          AddInclude('dbus-connection.inc');
          AddInclude('dbus-bus.inc');
          AddInclude('dbus-pending-call.inc');
          AddInclude('dbus-protocol.inc');
          AddInclude('dbus-server.inc');
          AddInclude('dbus-signature.inc');
          AddInclude('dbus-threads.inc');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
