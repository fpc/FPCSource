{ 
   $Id$ 
    Copyright (c) 2000-2002 by Marco van de Voort

    The "linux" posixy macro's that are used both in the Baseunx unit as the
    system unit. Not aliased via public names because I want these to be
    inlined as much as possible in the future.

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

function FpISDIR(m : TMode): boolean;

begin
  FpISDIR:=((m and S_IFMT) = S_IFDIR);
end;

function FpISCHR(m : TMode): boolean;
begin
  FpISCHR:=((m and S_IFMT) = S_IFCHR);
end;

function FpISBLK(m : TMode): boolean;
begin
 FpISBLK:=((m and S_IFMT) = S_IFBLK);
end;

function FpISREG(m : TMode): boolean;
begin
 FpISREG:=((m and S_IFMT) = S_IFREG);
end;

function FpISFIFO(m : TMode): boolean;
begin
 FpISFIFO:=((m and S_IFMT) = S_IFIFO);
end;

Function FPISLNK(m:TMode):boolean;

begin
 FPISLNK:=((m and S_IFMT) = S_IFLNK);
end;

Function FPISSOCK(m:TMode):boolean;

begin
 FPISSOCK:=((m and S_IFMT) = S_IFSOCK);
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
  Revision 1.2  2003-09-17 11:24:46  marco
   * fixes for new macro's

  Revision 1.1  2002/12/18 16:43:26  marco
   * new unix rtl, linux part.....

  Revision 1.2  2002/11/12 15:31:33  marco
   * Killed octal codes for 1.0.x compilability.

  Revision 1.1  2002/11/12 14:37:59  marco
   * Parts of new unix rtl

}
