{
    $Id$
    Copyright (c) 2000-2002 by Marco van de Voort

    The "linux" POSIX macro's that are used both in the POSIX unit as the
    system unit. Not aliased via public names because I want these to
    be inlined as much as possible in the future.

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

 ****************************************************************************
}

CONST 
       _S_IFDIR = $4000;
       _S_IFCHR = $2000;
       _S_IFBLK = $6000;
       _S_IFREG = $8000;
       _S_IFMT  = $f000;
       _S_IFIFO = $1000;

function S_ISDIR(m : mode_t): boolean;

begin
  S_ISDIR:=((m and _S_IFMT) = _S_IFDIR);
end;

function S_ISCHR(m : mode_t): boolean;
begin
  S_ISCHR:=((m and _S_IFMT) = _S_IFCHR);
end;

function S_ISBLK(m : mode_t): boolean;
begin
 S_ISBLK:=((m and _S_IFMT) = _S_IFBLK);
end;

function S_ISREG(m : mode_t): boolean;
begin
 S_ISREG:=((m and _S_IFMT) = _S_IFREG);
end;

function S_ISFIFO(m : mode_t): boolean;
begin
 S_ISFIFO:=((m and _S_IFMT) = _S_IFIFO);
end;

function wifexited(status : cint): cint;
begin
 wifexited:=cint((status AND $7f) =0);
end;

function wexitstatus(status : cint): cint;
begin
 wexitstatus:=(status and $FF00) shr 8;
end;

function wstopsig(status : cint): cint;
begin
 wstopsig:=(status and $FF00) shr 8;
end;

const wstopped=127;

function wifsignaled(status : cint): cint;
begin
 wifsignaled:=cint(((status and $FF)<>wstopped) and ((status and 127)<>0));
end;

function wtermsig(status : cint):cint;

begin
 wtermsig:=cint(status and 127);
end;

{
  $Log$
  Revision 1.2  2002-11-12 15:31:33  marco
   * Killed octal codes for 1.0.x compilability.

  Revision 1.1  2002/11/12 14:37:59  marco
   * Parts of new unix rtl


}
