{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ definition of pthread_mutex_t, because needed in both ptypes.inc and }
{ in sysosh.inc                                                        }

{ use a macro rather than a constant, so this name doesn't get exported
  from the system unit interface; macro's have to be on at this point
  because they're use to propagate the MUTEXTYPENAME here }

{$if defined(CPUMIPS) or defined(cpuaarch64)}
{$define USE_PTHREAD_SIZEOF}

{$if defined(cpuaarch64)}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 48}
{$elseif defined(CPU64)}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 40}
{$else CPU64}
  {$define __SIZEOF_PTHREAD_MUTEX_T := 24}
{$endif CPU64}

{$endif MIPS}

  MUTEXTYPENAME = record
    case byte of
{$ifdef USE_PTHREAD_SIZEOF}
      0 : (
        __size : array[0..__SIZEOF_PTHREAD_MUTEX_T-1] of char;
        __align : sizeint;
      );
{$endif}
      1 : (
        __m_reserved: longint;
        __m_count: longint;
        __m_owner: pointer;
        __m_kind:  longint;
        __m_lock:  record
           __status: sizeint;
          __spinlock: longint;
        end;
       );
  end;

{$ifdef __SIZEOF_PTHREAD_MUTEX_T}
{$undef __SIZEOF_PTHREAD_MUTEX_T}
{$endif __SIZEOF_PTHREAD_MUTEX_T}
{$macro off}