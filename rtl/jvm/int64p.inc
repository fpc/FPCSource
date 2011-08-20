{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Q- no overflow checking }
{$R- no range checking }

{$ifndef FPC_SYSTEM_HAS_DIV_QWORD}
{$define FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword; compilerproc;
      var
        signmask, tmpz: qword;
        tmpq, rest: int64;
      begin
        { emulate unsigned division using signed division, see
          http://hackers-delight.org.ua/060.htm }
         signmask:=qword(not(SarInt64(int64(n),63)));
         tmpz:=z and signmask;
         tmpq:=(int64(tmpz shr 1) div int64(n)) * 2;
         rest:=z-tmpq*n;
         fpc_div_qword:=tmpq+ord(rest>=n);
      end;
{$endif FPC_SYSTEM_HAS_DIV_QWORD}


{$ifndef FPC_SYSTEM_HAS_MOD_QWORD}
{$define FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword; compilerproc;
      var
        signmask, tmpz: qword;
        tmpq: int64;
      begin
        { emulate unsigned division using signed division, see
          http://hackers-delight.org.ua/060.htm }
         signmask:=qword(not(SarInt64(int64(n),63)));
         tmpz:=z and signmask;
         tmpq:=(int64(tmpz shr 1) div int64(n)) * 2;
         fpc_mod_qword:=z-tmpq*n;
         if fpc_mod_qword>=n then
           dec(fpc_mod_qword,n);
      end;
{$endif FPC_SYSTEM_HAS_MOD_QWORD}


{ lie to prevent two overloads for sqr(jlong) }
{$define FPC_SYSTEM_HAS_SQR_QWORD}

