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
(* from /usr/include/pthread_types.h 
struct	__pthread_mutex_st {
	unsigned int	ptm_magic;
	pthread_spin_t	ptm_errorcheck;
#ifdef __CPU_SIMPLE_LOCK_PAD
	uint8_t		ptm_pad1[3];
#endif
	pthread_spin_t	ptm_interlock;	/* unused - backwards compat */
#ifdef __CPU_SIMPLE_LOCK_PAD
	uint8_t		ptm_pad2[3];
#endif
	volatile pthread_t ptm_owner;
	pthread_t * volatile ptm_waiters;
	unsigned int	ptm_recursed;
	void		*ptm_spare2;	/* unused - backwards compat */
};
*)

{$ifdef I386}
  {$define __CPU_SIMPLE_LOCK_PAD}
{$endif}
{$ifdef X86_64}
  {$define __CPU_SIMPLE_LOCK_PAD}
{$endif}
  record
    ptm_magic : dword { should be cuint};
    ptm_errorcheck : char { should be pthread_spin_t};
{$ifdef __CPU_SIMPLE_LOCK_PAD}
    ptm_pad : array[0..2] of char;
{$endif _CPU_SIMPLE_LOCK_PAD}
    ptm_interlock : char { should be pthread_spinn_t};
{$ifdef __CPU_SIMPLE_LOCK_PAD}
    ptm_pad2 : array[0..2] of char;
{$endif _CPU_SIMPLE_LOCK_PAD}
    ptm_owner : pointer { should be pthread_t};
    ptm_waiter :  pointer { should be ppthread_t};
    ptm_recursed : dword { should be cuint};
    ptm_spare2 : pointer;
  end;

