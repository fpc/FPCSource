{
    Copyright (c) 2002 by the Free Pascal development team

    Syscall implementation for linux m68k

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02111-1301, USA.

 ****************************************************************************

}

function FpSysCall(sysnr:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL0'];
begin
  asm
    move.l sysnr, d0
    trap #0
    move.l d0, @Result
  end ['d0'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL1'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    trap #0
    move.l d0, @Result
  end ['d0', 'd1'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL2'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    move.l param2, d2
    trap #0
    move.l d0, @Result
  end ['d0', 'd1', 'd2'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL3'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    move.l param2, d2
    move.l param3, d3
    trap #0
    move.l d0, @Result
  end ['d0', 'd1', 'd2', 'd3'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL4'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    move.l param2, d2
    move.l param3, d3
    move.l param4, d4
    trap #0
    move.l d0, @Result
  end ['d0', 'd1', 'd2', 'd3', 'd4'];
    if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL5'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    move.l param2, d2
    move.l param3, d3
    move.l param4, d4
    move.l param5, d5
    trap #0
    move.l d0, @Result
  end ['d0', 'd1', 'd2', 'd3', 'd4', 'd5'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; [public,alias:'FPC_SYSCALL6'];
begin
  asm
    move.l sysnr, d0
    move.l param1, d1
    move.l param2, d2
    move.l param3, d3
    move.l param4, d4
    move.l param5, d5
    move.l param6, a0
    trap #0
    move.l d0, @Result
  end ['d0', 'd1', 'd2', 'd3', 'd4', 'd5', 'a0'];
  if (ptruint(result) > ptruint(-4096)) then
  begin
    seterrno(-result);
    result:=TSysResult(-1);
  end;
end;

