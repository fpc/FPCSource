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

 ****************************************************************************
}

function FPISDIR(m : mode_t): boolean;

begin
 FPISDIR:=((m and %001111000000000000) = %100000000000000);
end;

function FPISCHR(m : mode_t): boolean;
begin
 FPISCHR:=((m and %001111000000000000) = %10000000000000);
end;

function FPISBLK(m : mode_t): boolean;
begin
 FPISBLK:=((m and %001111000000000000) = %110000000000000);
end;

function FPISREG(m : mode_t): boolean;
begin
 FPISREG:=((m and %001111000000000000) = %1000000000000000);
end;

function FPISFIFO(m : mode_t): boolean;
begin
 FPISFIFO:=((m and %001111000000000000) = %1000000000000);
end;

function wifexited(status : cint): cint;
begin
 wifexited:=cint((status AND &177) =0);
end;

function wexitstatus(status : cint): cint;
begin
 wexitstatus:=(status and &177) shr 8;
end;

function wstopsig(status : cint): cint;
begin
 wstopsig:=(status and &177) shr 8;
end;

const wstopped=&177;

function wifsignaled(status : cint): cint;
begin
 wifsignaled:=cint(((status and &177)<>wstopped) and ((status and &177)<>0));
end;

function wtermsig(status : cint):cint;

begin
 wtermsig:=cint(status and &177);
end;

{
  $Log$
  Revision 1.1  2003-01-05 19:01:28  marco
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
