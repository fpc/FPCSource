{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ifndef HAS_LIBC_PIPING}
Function PClose(Var F:file) : cint;
var
  pl : ^cint;
  res : cint;

begin
  fpclose(filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:text) :cint;
var
  pl  : ^longint;
  res : longint;

begin
  fpclose(Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;
{$ENDIF}


Function AssignPipe(var pipe_in,pipe_out:cint):cint; [public, alias : 'FPC_SYSC_ASSIGNPIPE'];
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  ret  : longint;
  errn : cint;
  {$ifdef FPC_USE_LIBC}
   fdis : array[0..1] of cint;
  {$endif}
begin
{$ifndef FPC_USE_LIBC}
 ret:=intAssignPipe(pipe_in,pipe_out,errn);
 if ret=-1 Then
  fpseterrno(errn);
{$ELSE}
 fdis[0]:=pipe_in;
 fdis[1]:=pipe_out;
 ret:=pipe(fdis);
 pipe_in:=fdis[0];
 pipe_out:=fdis[1];
{$ENDIF}
 AssignPipe:=ret;
end;

{
  $Log$
  Revision 1.1  2005-02-13 21:47:56  peter
    * include file cleanup part 2

  Revision 1.1  2005/02/13 20:01:37  peter
    * include file cleanup

  Revision 1.20  2004/04/23 19:16:24  marco
   * flock -> fpflock because of conflicting structure name

  Revision 1.19  2004/03/04 22:15:16  marco
   * UnixType changes. Please report problems to me.

  Revision 1.18  2004/01/01 17:07:21  marco
   * few small freebsd fixes backported from debugging linux

  Revision 1.17  2003/12/30 12:32:30  marco
  *** empty log message ***

  Revision 1.16  2003/11/19 17:11:40  marco
   * termio unit

  Revision 1.15  2003/11/19 10:12:02  marco
   * more cleanups

  Revision 1.14  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.13  2003/11/14 16:21:59  marco
   * linuxerror elimination

  Revision 1.12  2003/11/09 12:00:16  marco
   * pipe fix

  Revision 1.11  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.10  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.9  2003/09/15 07:09:58  marco
   * small fixes, round 1

  Revision 1.8  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.7  2003/01/05 19:02:29  marco
   * Should now work with baseunx. (gmake all works)

  Revision 1.6  2002/10/18 12:19:59  marco
   * Fixes to get the generic *BSD RTL compiling again + fixes for thread
     support. Still problems left in fexpand. (inoutres?) Therefore fixed
     sysposix not yet commited

  Revision 1.5  2002/09/07 16:01:18  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/05/06 09:35:09  marco
   * Some stuff from 1.0.x ported

}
