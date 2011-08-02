{
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

function wifexited(status : cint): boolean;
begin
 wifexited:=(status AND 127) =0;
end;

function wexitstatus(status : cint): cint;
begin
 wexitstatus:=status shr 8;
end;

function wstopsig(status : cint): cint;
begin
 wstopsig:=status shr 8;
end;

const wstopped=127;

function wifsignaled(status : cint): boolean;
begin
 wifsignaled:=((status and 127)<>wstopped) and ((status and 127)<>0);
end;

function wtermsig(status : cint):cint;

begin
 wtermsig:=cint(status and 127);
end;

