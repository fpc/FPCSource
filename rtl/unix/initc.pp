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

{$ifndef ver1_0}
property cerrno:libcint read fpgetCerrno write fpsetcerrno;
{$endif}

implementation
// hasn't been divided up in .inc's, because I first want to see hoe
// this idea works out.

{$ifdef UseOldErrnoDirectLink}
Var
  interrno : libcint;external name 'h_errno';

function fpgetCerrno:libcint; 

begin
  fpgetCerrno:=interrno;
end;

procedure fpsetCerrno(err:libcint); 
begin
  interrno:=err;
end;
{$else}

const clib = 'c'; 

{$ifdef Linux}
function geterrnolocation: Plibcint; cdecl;external clib name '__errno_location';
{$else}
{$ifdef FreeBSD} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else}
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
  Revision 1.7  2003-12-10 17:14:27  marco
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
