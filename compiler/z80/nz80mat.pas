{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Z80 assembler for math nodes

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
unit nz80mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type

      { tz80notnode }

      tz80notnode = class(tcgnotnode)
      protected
        procedure second_boolean;override;
      end;


implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,
      ncgutil,cgobj,cgutils,
      hlcgobj;

{*****************************************************************************
                                tz80notnode
*****************************************************************************}


    procedure tz80notnode.second_boolean;
      begin
        if not handle_locjump then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            secondpass(left);

            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=left.location.resflags;
                  inverse_flags(location.resflags);
                end;
              else
                internalerror(2020042208);
            end;
          end;
      end;


begin
   cnotnode:=tz80notnode;
end.
