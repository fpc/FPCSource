{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the PowerPC code generator

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
unit nppcutil;

{$i defines.inc}

interface

    uses
      symtype,node,cpubase;

    procedure increfofs(var ref: treference; amount: longint);

implementation

    uses
       globtype,globals,systems,verbose,
       cutils,
       aasm,cpuasm,
       symconst,symbase,symdef,symsym,symtable,
{$ifdef GDB}
       gdb,
{$endif GDB}
       types,
       ncon,nld,
       pass_1,pass_2,
       cgbase,temp_gen,
       cga,regvars,cgobj,cgcpu;


    procedure increfofs(var ref: treference; amount: longint);

      begin
        if (ref.index = R_NO) and
           ((ref.offset + amount) >= low(smallint)) and
           ((ref.offset + amount) <= high(smallint)) then
          inc(offset,amount)
        else
          begin
            cg.load_address_ref_reg(exprasmlist,ref,ref.base);
            ref.index := R_NO;
            ref.symbol := nil;
            ref.offset := amount;
      end;

end.
{
  $Log$
  Revision 1.2  2002-04-06 18:13:02  jonas
    * several powerpc-related additions and fixes

  Revision 1.1  2001/09/29 21:33:12  jonas
    + implemented bool_to_int and int_to_int (+ helper in nppcutil)


}
