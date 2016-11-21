{******************************************************************************
    Copyright (c) 2000-2010 by Florian Klaempfl and Jonas Maebe

    Includes the LLVM code generator

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
unit llvmnode;

{$I fpcdefs.inc}

interface
{ This unit is used to define the specific CPU implementations. All needed
actions are included in the INITALIZATION part of these units. This explains
the behaviour of such a unit having just a USES clause! }

implementation

    { always first include the target-specific unit, then the llvm one
      to ensure that the llvm one is initialized later (-> overrides
      settings)
    }
  uses
    ncgbas,ncgflw,ncgcnv,ncgld,ncgmem,ncgcon,ncgset,
    ncgadd,ncgcal,ncgmat,ncginl,
    tgllvm,hlcgllvm,
    nllvmadd,nllvmbas,nllvmcal,nllvmcnv,nllvmcon,nllvmflw,nllvminl,nllvmld,
    nllvmmat,nllvmmem,nllvmtcon,nllvmutil,
    llvmpara,
    symllvm;

end.
