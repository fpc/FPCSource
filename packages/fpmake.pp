{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit;

Var
  TBuild,T : TTarget;
  P : TPackage;
  I : Integer;
begin
  With Installer do
    begin

(*
The include lines below are generated with the following command:

/bin/ls -1 */fpmake.pp | sed 's+\(.*\)+{$include \1}+'
*)

{$include dbus/fpmake.pp}
{$include fcl-async/fpmake.pp}
{$include fcl-base/fpmake.pp}
{$include fcl-fpcunit/fpmake.pp}
{$include fcl-image/fpmake.pp}
{$include fcl-json/fpmake.pp}
{$include fcl-net/fpmake.pp}
{$include fcl-passrc/fpmake.pp}
{$include fcl-process/fpmake.pp}
{$include fcl-web/fpmake.pp}
{$include fcl-xml/fpmake.pp}
{$include fpmkunit/fpmake.pp}
{$include gdbint/fpmake.pp}
{$include hash/fpmake.pp}
{$include ibase/fpmake.pp}
{$include imagemagick/fpmake.pp}
{$include libc/fpmake.pp}
{$include libpng/fpmake.pp}
{$include mysql/fpmake.pp}
{$include ncurses/fpmake.pp}
{$include odbc/fpmake.pp}
{$include oracle/fpmake.pp}
{$include pasjpeg/fpmake.pp}
{$include paszlib/fpmake.pp}
{$include postgres/fpmake.pp}
{$include pthreads/fpmake.pp}
{$include regexpr/fpmake.pp}
{$include sqlite/fpmake.pp}
{$include unzip/fpmake.pp}
{$include winunits-base/fpmake.pp}
{$include winunits-jedi/fpmake.pp}
{$include zlib/fpmake.pp}

    Run;
    end;
end.

