{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    The base *BSD syscalls required to implement the system unit. These
    are aliased for use in other units (to avoid poluting the system units
    interface)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

{$i ostypes.inc}

{$ifdef FPC_USE_LIBC}
  {$Linklib c}
  // Out of date atm.

{$ifdef FPC_IS_SYSTEM}
{$i oscdeclh.inc}
{$endif}
{$I bunxmacr.inc}

{
  $Log$
  Revision 1.1  2005-02-10 17:30:54  peter
    * renamed to solaris

  Revision 1.1  2005/02/07 22:17:26  peter
    * updated for 1.9.x unix rtl

}


