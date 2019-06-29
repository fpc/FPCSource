{
    Copyright (c) 2019 by Jonas Maebe

    Generate LLVM bytecode for set/case nodes

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
unit nllvmset;

{$i fpcdefs.inc}

interface

  uses
    nset, ncgset;

  type
    tllvmcasenode = class(tcgcasenode)
     protected
      procedure genlinearlist(hp: pcaselabel); override;
    end;


implementation

  procedure tllvmcasenode.genlinearlist(hp: pcaselabel);
    begin
      { genlinearlist constantly updates the case value in the register,
        which causes tons of spilling with LLVM due to the need to bring
        it back into SSA form. LLVM will recognise and optimise the linear
        cmp list just as well (or even better), while the code that FPC
        has to generate is much smaller (no spilling) }
      genlinearcmplist(hp);
    end;

begin
  ccasenode:=tllvmcasenode;
end.

