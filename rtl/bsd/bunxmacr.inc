{
    $Id$
    Copyright (c) 2000-2002 by Marco van de Voort

    The *BSD POSIX macro's that are used both in the Baseunix unit as the
    system unit. Not aliased via public names because I want these to be
    inlined as much as possible in the future.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}

function FPS_ISDIR(m : TMode): boolean;

begin
 FPS_ISDIR:=((m and S_IFMT) = S_IFDIR);
end;

function FPS_ISCHR(m : TMode): boolean;
begin
 FPS_ISCHR:=((m and S_IFMT) = S_IFCHR);
end;

function FPS_ISBLK(m : TMode): boolean;
begin
 FPS_ISBLK:=((m and S_IFMT) = S_IFBLK);
end;

function FPS_ISREG(m : TMode): boolean;
begin
 FPS_ISREG:=((m and S_IFMT) = S_IFREG);
end;

function FPS_ISFIFO(m : TMode): boolean;
begin
 FPS_ISFIFO:=((m and S_IFMT) = S_IFIFO);
end;

Function FPS_ISLNK(m:TMode):boolean;

begin
 FPS_ISLNK:=((m and S_IFMT) = S_IFLNK);
end;

Function FPS_ISSOCK(m:TMode):boolean;

begin
 FPS_ISSOCK:=((m and S_IFMT) = S_IFSOCK);
end;

function wifexited(status : cint): cint;
begin
 wifexited:=cint((status AND 127) =0);
end;

function wexitstatus(status : cint): cint;
begin
 wexitstatus:=(status and 127) shr 8;
end;

function wstopsig(status : cint): cint;
begin
 wstopsig:=(status and 127) shr 8;
end;

const wstopped=127;

function wifsignaled(status : cint): cint;
begin
 wifsignaled:=cint(((status and 127)<>wstopped) and ((status and 127)<>0));
end;

function wtermsig(status : cint):cint;

begin
 wtermsig:=cint(status and 127);
end;

{
  $Log$
  Revision 1.5  2003-09-27 13:04:58  peter
    * fpISxxx renamed

  Revision 1.4  2003/09/17 11:52:05  marco
   * stat macro fixes voor BSD

  Revision 1.3  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.2  2003/01/21 15:34:59  marco
   * fix octal that broke 1.0.x

  Revision 1.1  2003/01/05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.4  2002/11/12 14:19:46  marco
   * fixes to macro

  Revision 1.3  2002/10/26 18:27:51  marco
   * First series POSIX calls commits. Including getcwd.

  Revision 1.2  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/19 12:29:11  marco
   * First working POSIX *BSD system unit.


}
