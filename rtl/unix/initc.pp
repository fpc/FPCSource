{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman,
    members of the Free Pascal development team

    This file links to libc, and handles the libc errno abstraction.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit initc;
interface
{$linklib c}

type libcint   = longint;
     plibcint = ^libcint;

function fpgetCerrno:libcint; 
procedure fpsetCerrno(err:libcint); 

{$ifdef HASGLOBALPROPERTY}
property cerrno:libcint read fpgetCerrno write fpsetcerrno;
{$endif HASGLOBALPROPERTY}

const clib = 'c'; 

implementation
// hasn't been divided up in .inc's, because I first want to see hoe
// this idea works out.

{$ifdef OpenBSD}
{$define UseOldErrnoDirectLink}
{$endif}

{$ifdef UseOldErrnoDirectLink}
Var
  interrno : libcint;external name {$ifdef OpenBSD} '_errno' {$else} 'h_errno'{$endif};

function fpgetCerrno:libcint; 

begin
  fpgetCerrno:=interrno;
end;

procedure fpsetCerrno(err:libcint); 
begin
  interrno:=err;
end;
{$else}


{$ifdef Linux}
function geterrnolocation: Plibcint; cdecl;external clib name '__errno_location';
{$else}
{$ifdef FreeBSD} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else}
{$ifdef Darwin}
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$endif}
{$endif}
{$endif}
{$endif}

function fpgetCerrno:libcint; 

begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:libcint); 
begin
  geterrnolocation^:=err;
end;

{$endif}

end.
{
  $Log$
  Revision 1.11  2004-09-08 08:32:33  marco
   * "clib" constant moved to interface, since initc is prefered way of
  initing libc/libroot/glibc

  Revision 1.10  2004/01/21 21:25:49  marco
   * openbsd fixes stage one

  Revision 1.9  2004/01/04 20:36:53  jonas
    + geterrnolocation for Darwin

  Revision 1.8  2003/12/11 18:20:50  florian
    * replaced VER1_0 by HASGLOBALPROPERTY

  Revision 1.7  2003/12/10 17:14:27  marco
   * property support under ifndef ver1_0

  Revision 1.6  2003/12/10 17:06:19  marco
   * property support used.

  Revision 1.5  2003/12/10 14:59:49  marco
   * NetBSD supported added based on Sparc and define name changed to something more sensible

  Revision 1.4  2003/12/10 11:24:25  marco
   * get/setcerrno added

  Revision 1.3  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

}
