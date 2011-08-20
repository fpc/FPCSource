{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM byetcode for in memory related nodes

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
unit njvmmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cpubase,
      node,nmem,ncgmem;

    type
       tjvmvecnode = class(tcgvecnode)
         procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,
      cutils,verbose,
      symdef,defutil,
      aasmdata,pass_2,
      cgutils,hlcgobj;

{*****************************************************************************
                             TJVMVECNODE
*****************************************************************************}

    procedure tjvmvecnode.pass_generate_code;
      var
        newsize: tcgsize;
      begin
        { This routine is not used for Strings, as they are a class type and
          you have to use charAt() there to load a character (and you cannot
          change characters; you have to create a new string in that case)

          As far as arrays are concerned: we have to create a trefererence
          with arrayreftype in [art_indexreg,art_indexref], and ref.base =
          pointer to the array (i.e., left.location.register) }
        secondpass(left);
        newsize:=def_cgsize(resultdef);
        if left.location.loc=LOC_CREFERENCE then
          location_reset_ref(location,LOC_CREFERENCE,newsize,left.location.reference.alignment)
        else
          location_reset_ref(location,LOC_REFERENCE,newsize,left.location.reference.alignment);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
        location.reference.base:=left.location.register;
        secondpass(right);
        { simplify index location if necessary, since array references support
          an index in memory, but not an another array index }
        if (right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
           (right.location.reference.arrayreftype<>art_none) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

        case right.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            begin
              location.reference.arrayreftype:=art_indexreg;
              location.reference.index:=right.location.register;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              location.reference.arrayreftype:=art_indexref;
              location.reference.indexbase:=right.location.reference.base;
              location.reference.indexsymbol:=right.location.reference.symbol;
              location.reference.indexoffset:=right.location.reference.offset;
            end;
          LOC_CONSTANT:
            begin
              location.reference.arrayreftype:=art_indexconst;
              location.reference.indexoffset:=right.location.value;
            end
          else
            internalerror(2011012002);
        end;
      end;


begin
   cvecnode:=tjvmvecnode;
end.
