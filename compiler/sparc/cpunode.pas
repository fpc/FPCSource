{******************************************************************************
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the iSPARC code generator

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

 *****************************************************************************}
unit CpuNode;

{$I fpcdefs.inc}

interface
{ This unit is used to define the specific CPU implementations. All needed
actions are included in the INITALIZATION part of these units. This explains
the behaviour of such a unit having just a USES clause! }

implementation

  uses
    ncgbas,ncgflw,ncgcnv,ncgld,ncgmem,ncgcon,ncgset,
    ncpuadd,ncpucall,ncpumat,ncpuinln,ncpucnv,ncpuobj,ncpuset,
    { this not really a node }
    rgcpu;

end.
{
    $Log$
    Revision 1.11  2004-10-30 22:01:11  florian
      * jmp table code generation for case statement on sparc

    Revision 1.10  2004/06/20 08:55:32  florian
      * logs truncated

    Revision 1.9  2004/06/16 20:07:10  florian
      * dwarf branch merged

    Revision 1.8.2.1  2004/05/13 20:10:38  florian
      * released variant and interface support

}
