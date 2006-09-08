Unit sysctl;

{
   This file is part of the Free Pascal run time library.
   (c) 2002 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   sysctl.h header conversion, taken from FreeBSD 4.6, mainly as base
   to implement UNAME on.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

Interface

uses
  unixtype;

{$ifndef FPC_USE_LIBC}
{$define FPC_USE_SYSCALL}
{$endif}


{$Packrecords C}

{
 * Copyright (c) 1989, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Karels at Berkeley Software Design, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)sysctl.h    8.1 (Berkeley) 6/2/93
 * $FreeBSD: src/sys/sys/sysctl.h,v 1.81.2.8 2002/03/17 11:08:38 alfred Exp $
 }


TYPE    CtlNameRec = Record
                      Name: ansistring; {String[LongestStringInCtlNames]}
                      CtlType:cint;
                      end;


{$I sysctlh.inc}

// sysctl only one that is tested. user_definable part of the sysctl
// function is not implemented
//

{$ifdef FPC_USE_LIBC}
function FPsysctl (Name: pchar; namelen:cuint; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint; cdecl; external name 'sysctl';
function FPsysctlbyname (Name: pchar; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint; cdecl; external name 'sysctlbyname';
function FPsysctlnametomib (Name: pchar;mibp:pcint;sizep:psize_t):cint; cdecl; external name 'sysctltomib';
{$else}
function FPsysctl (Name: pchar; namelen:cuint; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint;
function FPsysctlbyname (Name: pchar; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint;
function FPsysctlnametomib (Name: pchar; mibp:pcint;sizep:psize_t):cint;
{$endif}

Implementation

{$ifndef FPC_USE_LIBC}
Uses Syscall;
{$ENDIF}

{$ifndef FPC_USE_LIBC}
{$ifdef FreeBSD}
CONST  syscall_nr___sysctl                    = 202;
{$endif}

function FPsysctl (Name: pchar; namelen:cuint; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint;

Begin
        if (name[0] <> chr(CTL_USER)) Then
           exit(do_syscall(syscall_nr___sysctl,longint(name), namelen, longint(oldp), longint(oldlenp), longint(newp), longint(newlen)))
        else
         Exit(0);
End;

function FPsysctlbyname (Name: pchar; oldp:pointer;oldlenp:psize_t; newp:pointer;newlen:size_t):cint;
Var
        name2oid_oid    : array[0..1] of cint;
        real_oid        : array[0..CTL_MAXNAME+1] of cint;
        error           : cint;
        oidlen          : size_t;
Begin
        name2oid_oid[0] := 0;   {This is magic & undocumented! }
        name2oid_oid[1] := 3;

        oidlen := sizeof(real_oid);
        error := FPsysctl(@name2oid_oid, 2, @real_oid, @oidlen, name,
                       strlen(name));
        if (error < 0)  Then
                Exit(error);
        oidlen := Oidlen DIV sizeof (cint);
        error := FPsysctl(@real_oid, oidlen, oldp, oldlenp, newp, newlen);
        exit(error);
End;

function FPsysctlnametomib (Name: pchar; mibp:pcint;sizep:psize_t):cint;
Var     oid   : array[0..1] OF cint;
        error : cint;

Begin
        oid[0] := 0;
        oid[1] := 3;
        sizep^:=sizep^*sizeof(cint);
        error := FPsysctl(@oid, 2, mibp, sizep, name, strlen(name));
        sizep^ := sizep^ div sizeof (cint);

        if (error < 0)  Then
                Exit (error);
        FPsysctlnametomib:=0;
End;
{$endif}

end.
