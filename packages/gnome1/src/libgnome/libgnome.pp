unit libgnome;

{$PACKRECORDS C}
{$mode objfpc}

interface

Uses glib;

{$linklib esd}
{$linklib popt}
{$linklib db1}

const
 libgnomedll='gnome';

Type
  PPPgChar = ^PPgChar;
  PPPgfloat = ^PPgfloat;
  PPgfloat = ^Pgfloat;

  PPpid_t = ^Ppid_t;
  Ppid_t = ^pid_t;
  pid_t = longint;

  PPTime_t = ^PTime_t;
  PTime_t = ^Time_t;
  Time_t = longint;

  Ptm = ^tm;
  tm = record
    tm_sec : integer;
    tm_min : integer;
    tm_hour : integer;
    tm_mday : integer;
    tm_mon : integer;
    tm_year : integer;
    tm_wday : integer;
    tm_yday : integer;
    tm_isdst : integer;
    tm_gmtoff : PAnsiChar;
    tm_zone : PAnsiChar;
  end;

var
  gnome_user_home_dir : PAnsiChar;cvar;external;
  gnome_user_dir : PAnsiChar;cvar;external;
  gnome_user_private_dir : PAnsiChar;cvar;external;
  gnome_user_accels_dir : PAnsiChar;cvar;external;
  gnome_app_id : PAnsiChar;cvar;external;
  gnome_do_not_create_directories : AnsiChar;cvar;external;

{$define read_interface}
{$undef read_implementation}

{$include gnomeutil.inc}
{$include gnomeconfig.inc}
{$include gnomedentry.inc}
{$include gnomeexec.inc}
{$include gnomehelp.inc}
{$include gnomei18n.inc}
{$include gnomemetadata.inc}
{$include gnomemime.inc}
{$include gnomemimeinfo.inc}
{$include gnomepaper.inc}
{.$include gnomepopt.inc}
{$include gnomeremote.inc}
{$include gnomescore.inc}
{$include gnomesound.inc}
{$include gnometriggers.inc}
{$include gnomeurl.inc}

procedure gnomelib_init(app_id:PAnsiChar; app_version:PAnsiChar);cdecl;external;

implementation

{$undef read_interface}
{$define read_implementation}

{$include gnomeutil.inc}
{$include gnomeconfig.inc}
{$include gnomedentry.inc}
{$include gnomeexec.inc}
{$include gnomehelp.inc}
{$include gnomei18n.inc}
{$include gnomemetadata.inc}
{$include gnomemime.inc}
{$include gnomemimeinfo.inc}
{$include gnomepaper.inc}
{.$include gnomepopt.inc}
{$include gnomeremote.inc}
{$include gnomescore.inc}
{$include gnomesound.inc}
{$include gnometriggers.inc}
{$include gnomeurl.inc}

end.
