{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Michael Van Canneyt, Peter Vreman,
    & Daniel Mantione, members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_c21g;

interface

{$i si_intf.inc}

implementation

{$i sysnr.inc}
{$i si_impl.inc}
{$i si_c21.inc}

var
  gmon_started: boolean;
  gmon_start : record end;external name 'PASCALMAIN';
  gmon_etext : record end;external name '_etext';


procedure atexit(p: pointer); cdecl; external;
procedure monstartup (main,etext : pointer); cdecl; external;
procedure _mcleanup; cdecl; external;

procedure __gmon_start__; cdecl; public;
  begin
    if not gmon_started then
      begin
        gmon_started:=true;
        monstartup(@gmon_start,@gmon_etext);
        atexit(@_mcleanup);
      end;
  end;

end.
