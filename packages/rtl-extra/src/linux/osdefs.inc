{
    Copyright (c) 2000-2002 by Marco van de Voort

    Target dependent defines used when compileing the baseunix unit

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
    MA 02110-1301, USA.

 ****************************************************************************}

{$ifndef cpux86_64}
  {$define usestime}              // Use stime() syscall instead of settimeofday
{$endif}
{$define oldreaddir}            // Keep using readdir system call instead
                                // of userland getdents stuff.
{$define usedomain}             // Allow uname with "domain" entry.
                                // (which is a GNU extension)
{$ifdef FPC_USE_LIBC}
  {$define usegetcwd}
  {$if not defined(cpux86) and not defined(cpux86_64)}
  {$linklib m}
  {$endif}
{$endif}

{$if defined(cpupowerpc) or defined(cpupowerpc64) or defined(cpui386) or
  (defined(cpuarm) and defined(FPC_ABI_EABI))}
  {$DEFINE has_ugetrlimit}
{$endif}

{$if (defined(cpuarm) and defined(FPC_ABI_EABI))}
  {$define FPC_USEGETTIMEOFDAY}
  {$undef usestime}

  {$define NO_SYSCALL_TIME}
  {$define NO_SYSCALL_UMOUNT}
  {$define NO_SYSCALL_STIME}
  {$define NO_SYSCALL_ALARM}
  {$define NO_SYSCALL_UTIME}
  {$define NO_SYSCALL_GETRLIMIT}
  {$define NO_SYSCALL_SELECT}
  {$define NO_SYSCALL_READDIR}
  {$define NO_SYSCALL_MMAP}
  {$define NO_SYSCALL_SOCKETCALL}
  {$define NO_SYSCALL_SYSCALL}
  {$define NO_SYSCALL_IPC}
{$endif}

 {$ifdef CPUARM}
{$define WAIT4}
{$endif CPUARM}

{$ifdef CPUx86_64}
{$define WAIT4}
{$endif CPUx86_64}

{$ifdef CPUSPARC}
{$define WAIT4}
{$endif CPUSPARC}

{$ifdef cpui386}
{$define OLDMMAP}
{$endif cpui386}

{$ifdef cpum68k}
{$define OLDMMAP}
{$endif cpum68k}

{$ifdef cpuarm}
  {$ifdef FPC_ABI_EABI}
    {$define MMAP2}
  {$else FPC_ABI_EABI}
    {$define OLDMMAP}
  {$endif FPC_ABI_EABI}
{$endif cpuarm}

