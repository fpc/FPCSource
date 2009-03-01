{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit;

Var
  TBuild,T : TTarget;
  PBuild,P : TPackage;
  D : TDependency;
  I : Integer;
begin
  With Installer do
    begin

(*
The include lines below are generated with the following command:

/bin/ls -1 */fpmake.pp | sed 's+\(.*\)+{$include \1}+'
*)

{$include a52/fpmake.pp}
{$include amunits/fpmake.pp}
{$include aspell/fpmake.pp}
{$include bfd/fpmake.pp}
{$include bzip2/fpmake.pp}
{$include cdrom/fpmake.pp}
{$include dbus/fpmake.pp}
{$include dts/fpmake.pp}
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
{$include fftw/fpmake.pp}
{$include fpgtk/fpmake.pp}
{$include fpmkunit/fpmake.pp}
{$include fv/fpmake.pp}
{$include gbaunits/fpmake.pp}
{$include gdbint/fpmake.pp}
{$include gdbm/fpmake.pp}
{$include ggi/fpmake.pp}
{$include gnome1/fpmake.pp}
{$include gtk1/fpmake.pp}
{$include gtk2/fpmake.pp}
{$include hash/fpmake.pp}
{$include httpd13/fpmake.pp}
{$include httpd20/fpmake.pp}
{$include httpd22/fpmake.pp}
{$include ibase/fpmake.pp}
{$include imagemagick/fpmake.pp}
{$include imlib/fpmake.pp}
{$include ldap/fpmake.pp}
{$include libc/fpmake.pp}
{$include libcurl/fpmake.pp}
{$include libgd/fpmake.pp}
{$include libndsfpc/fpmake.pp}
{$include libpng/fpmake.pp}
{$include lua/fpmake.pp}
{$include mad/fpmake.pp}
{$include matroska/fpmake.pp}
{$include modplug/fpmake.pp}
{$include mysql/fpmake.pp}
{$include ncurses/fpmake.pp}
{$include newt/fpmake.pp}
{$include numlib/fpmake.pp}
{$include odbc/fpmake.pp}
{$include oggvorbis/fpmake.pp}
{$include opengl/fpmake.pp}
{$include openssl/fpmake.pp}
{$include oracle/fpmake.pp}
{$include os2units/fpmake.pp}
{$include palmunits/fpmake.pp}
{$include pasjpeg/fpmake.pp}
{$include paszlib/fpmake.pp}
{$include pcap/fpmake.pp}
{$include postgres/fpmake.pp}
{$include pthreads/fpmake.pp}
{$include pxlib/fpmake.pp}
{$include regexpr/fpmake.pp}
{$include rexx/fpmake.pp}
{$include sdl/fpmake.pp}
{$include sndfile/fpmake.pp}
{$include sqlite/fpmake.pp}
{$include svgalib/fpmake.pp}
{$include symbolic/fpmake.pp}
{$include syslog/fpmake.pp}
{$include tcl/fpmake.pp}
{$include unixutil/fpmake.pp}
{$include unzip/fpmake.pp}
{$include users/fpmake.pp}
{$include utmp/fpmake.pp}
{$include uuid/fpmake.pp}
{$include winunits-base/fpmake.pp}
{$include winunits-jedi/fpmake.pp}
{$include x11/fpmake.pp}
{$include xforms/fpmake.pp}
{$include zlib/fpmake.pp}

    // Create fpc-all package
    PBuild:=AddPackage('fpc-all');
    PBuild.Version:='2.2.4';
    for i:=0 to Packages.Count-1 do
      begin
        P:=Packages.PackageItems[i];
        D:=PBuild.Dependencies.Add(P.Name);
      end;

    Run;
    end;
end.

