{
    Copyright (c) 2017 by Karoly Balogh

    Header for syscalls in System unit for m68k BSD

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

Type
  TSysResult64 = int64;
  TSysResult   = longint;
  TSysParam    = Longint;

function Do_SysCall(sysnr:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL0';
function Do_SysCall(sysnr,param1:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL1';
function Do_SysCall(sysnr,param1,param2:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL2';
function Do_SysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL3';
function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL4';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL5';
function Do_SysCall64(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult64; stdcall; external name 'FPC_SYSCALL5';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL6';
function Do_SysCall64(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult64; stdcall; external name 'FPC_SYSCALL6';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL7';
function Do_SysCall64(sysnr,param1,param2,param3,param4,param5,param6,param7:TSysParam):TSysResult64; stdcall; external name 'FPC_SYSCALL7';
function Do_SysCall(sysnr,param1,param2,param3,param4,param5,param6,param7,param8:TSysParam):TSysResult; stdcall; external name 'FPC_SYSCALL8';
function Do_SysCall64(sysnr,param1,param2,param3,param4,param5,param6,param7,param8:TSysParam):TSysResult64; stdcall; external name 'FPC_SYSCALL8';
