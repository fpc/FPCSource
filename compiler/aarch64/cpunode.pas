{******************************************************************************
    Copyright (c) 2014 by Florian Klaempfl

    Includes the aarch64 code generator

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
unit cpunode;

{$I fpcdefs.inc}

interface
{ This unit is used to define the specific CPU implementations. All needed
actions are included in the INITALIZATION part of these units. This explains
the behaviour of such a unit having just a USES clause! }

implementation

  uses
    ncgbas,ncgflw,ncgcal,ncgcnv,ncgld,ncgmem,ncgcon,ncgset,ncgobjc,
    { symtable }
    symcpu,
    aasmdef,
{$ifndef llvm}
    narmbas,
    ncpuadd,ncpumat,ncpumem,ncpuinl,ncpucnv,ncpuset,ncpucon,ncpuflw,naarch64util
{$else llvm}
    llvmnode
{$endif llvm}
    ;

end.
