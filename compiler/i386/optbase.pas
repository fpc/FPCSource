{
    $Id$
    Copyright (c) 1998-2002 by the Free Pascal development team

    This routine contains the basic tables and information
    for the generic optimizers and cpu specific optimizations.

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
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{# This unit should define cpu specific information which is required
   for the optimizers.
}
unit optbase;

interface


implementation


end.

{
  $Log$
  Revision 1.4  2004-10-04 20:46:22  peter
    * spilling code rewritten for x86. It now used the generic
      spilling routines. Special x86 optimization still needs
      to be added.
    * Spilling fixed when both operands needed to be spilled
    * Cleanup of spilling routine, do_spill_readwritten removed

  Revision 1.3  2004/06/20 08:55:31  florian
    * logs truncated

}
