{
  This file is part of the Free Pascal run time library.
  Copyright (c) 2011 by Free Pascal development team

  This file implements all the types/constants related
  to signals for FreeBSD/powerpc64 (ELFv2).

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}
	SigContextRec = ^TSigContext;
    TSigContext = record
       mc_mask      : sigset_t;          { signal mask to restore }
    { General-purpose registers r0..r31 (64-bit). }
	   mc_gpr   : array[0..31] of cULong;

    { Link register, condition register, fixed-point exception, count register }
       mc_lr    : cULong;
       mc_cr    : cULong;
       mc_xer   : cULong;
       mc_ctr   : cULong;

    { Saved instruction address / machine state.
      On FreeBSD/powerpc64 the PC is in mc_srr0. }
       mc_srr0  : cULong;  { program counter }
       mc_srr1  : cULong;  { machine state register }
       mc_fpreg  : array[0..31] of Double;   { F0–F31 }
       mc_fpscr   : cULong;
    { FreeBSD/powerpc64 adds vector + extra FP state }
       mc_vscr    : QWord;
       mc_vrsave  : QWord;
       mc_gqr     : array[0..7] of QWord;
       mc_vreg    : array[0..63] of QWord;

    { Reserve space for FP/vector/etc. state so this record is not
      smaller than the real kernel mcontext_t. Adjust size if needed. }
       mc_reserved : array[0..255] of cUChar;
    end;
    

	pstack_t = ^stack_t;
	stack_t = record
	  ss_sp: PAnsiChar;                       {* signal stack base *}
      ss_size: size_t;                    {* signal stack length *}
      ss_flags: cInt;                     {* SS_DISABLE and/or SS_ONSTACK *}
	end;
	TStack = stack_t;
	PStack = pstack_t;

	PUContext = ^pucontext_t;
	pucontext_t = ^tucontext_t;
    tucontext_t = record
       uc_flags    : cULong;
       uc_link     : pucontext_t;
       uc_stack    : stack_t;
       uc_mcontext : TSigContext;
       uc_sigmask  : sigset_t;
    end;
