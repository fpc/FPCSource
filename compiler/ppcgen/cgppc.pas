{
    Copyright (c) 2006 by Florian Klaempfl

    This unit implements the common part of the code generator for the PowerPC

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
unit cgppc;

{$i fpcdefs.inc}

  interface

    uses
       globtype,symtype,symdef,
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,aasmdata,
       cpubase,cpuinfo,cgutils,rgcpu;

    type
      tcgppcgen = class(tcg)
        procedure a_call_ref(list : TAsmList;ref: treference); override;
     end;

  implementation

    uses
       globals,verbose,systems,cutils,
       symconst,symsym,fmodule,
       rgobj,tgobj,cpupi,procinfo,paramgr;

  procedure tcgppcgen.a_call_ref(list : TAsmList;ref: treference);
    var
      tempreg : TRegister;
    begin
      tempreg := getintregister(list, OS_ADDR);
      a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,tempreg);
      a_call_reg(list,tempreg);
    end;

end.
