{
    $Id$
    Copyright (c) 2004 by Free Pascal Development Team

    This unit implements support import,export,link routines
    for the MorphOS (PowerPC) target

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
unit t_morph;

{$i fpcdefs.inc}

interface


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_morph;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterTarget(system_powerpc_morphos_info);
end.
{
  $Log$
  Revision 1.1  2004-02-13 05:46:58  karoly
   * added powerpc-morphos target


}
