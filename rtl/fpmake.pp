{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses sysutils,fpmkunit;

{ Read RTL definitions. }
{$i fpmake.inc}

{ Unix/Posix defines }
{$i unix/fpmake.inc}

{ Load OS-specific targets and corrections }
{$i linux/fpmake.inc}
(*
  {$i amiga/fpmake.inc}
  {$i darwin/fpmake.inc}
  {$i freebsd/fpmake.inc}
  {$i palmos/fpmake.inc}
  {$i emx/fpmake.inc}
  {$i go32v2/fpmake.inc}
  {$i morphos/fpmake.inc}
  {$i atari/fpmake.inc}
  {$i macos/fpmake.inc}
  {$i netbsd/fpmake.inc}
  {$i openbsd/fpmake.inc}
  {$i win32/fpmake.inc}
  {$i beos/fpmake.inc}
  {$i netware/fpmake.inc}
  {$i os2/fpmake.inc}
  {$i solaris/fpmake.inc}
*)

Var
  T : TTarget;

begin
  InitRTL(Installer);           // Define RTL package.
  AddDefaultTargets(Installer); // Add all cross-platform units.
  // A line must be added here when adding support for a new OS.
  Case Installer.Defaults.OS of
    linux : ApplyLinuxTargets(Installer);
       
  else
    Raise EInstallerError.Create('OS not yet supported by makefile: '+OsToString(Defaults.OS));
  end;  
  Installer.EndPackage;
  Installer.Run; // Go.
end.

