{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Carl Eric Codere development team

    Base Unix unit modelled after POSIX 2001.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit BaseUnix;

Interface
{$modeswitch out}
{$inline on}
Uses UnixType;

{$i osdefs.inc}       { Compile time defines }

{$i aliasptp.inc}

{$packrecords C}

{$i errno.inc}          { Error numbers }
{$i ostypes.inc}

const
  clib = 'root';
  netlib = 'network';

{$i oscdeclh.inc}

  function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
  procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
  property errno : cint read fpgeterrno write fpseterrno;

{$i bunxovlh.inc}

function fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;

{$i genfunch.inc}

{ Fairly portable constants. I'm not going to waste time to duplicate and alias
them anywhere}

Const
  MAP_FAILED    = pointer(-1);  { mmap() has failed }
  MAP_SHARED    =  $1;          { Share changes }
  MAP_PRIVATE   =  $2;          { Changes are private }
  MAP_TYPE      =  $f;          { Mask for type of mapping }
  MAP_FIXED     = $10;          { Interpret addr exactly }

// MAP_ANON(YMOUS) is OS dependant but used in the RTL and in ostypes.inc
// Under BSD without -YMOUS, so alias it:
  MAP_ANON      = MAP_ANONYMOUS;

  PROT_READ     =  $1;          { page can be read }
  PROT_WRITE    =  $2;          { page can be written }
  PROT_EXEC     =  $4;          { page can be executed }
  PROT_NONE     =  $0;          { page can not be accessed }

implementation

{$ifdef hassysctl}
Uses Sysctl;
{$endif}

{$i genfuncs.inc}       // generic calls. (like getenv)
{$I gensigset.inc}     // general sigset funcs implementation.
{$I genfdset.inc}      // general fdset funcs.

{$i oscdecl.inc}        // implementation of wrappers in oscdeclh.inc

{$i osmacro.inc}        { macro implenenations }
{$i bunxovl.inc}        { redefs and overloads implementation }


function stime(t: ptime_t): cint; cdecl; external clib name 'stime';

function fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;
begin
  fpsettimeofday:=stime(@tp^.tv_sec);
end;


end.
