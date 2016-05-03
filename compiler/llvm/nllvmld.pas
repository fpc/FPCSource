{
    Copyright (c) 2012 by Jonas Maebe

    Generate LLVM bytecode for nodes that handle loads and assignments

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
unit nllvmld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgutils,
      symtype,
      node,ncgld,ncgnstld;

    type
      tllvmloadnode = class(tcgnestloadnode)
        function pass_1: tnode; override;
        procedure pass_generate_code; override;
      end;

      { tllvmarrayconstructornode }

      tllvmarrayconstructornode = class(tcgarrayconstructornode)
       protected
        procedure makearrayref(var ref: treference; eledef: tdef); override;
      end;


implementation

     uses
       systems,
       aasmdata,
       nld,
       symtable,symconst,symdef,symsym,
       tgobj,cgbase,hlcgobj;

function tllvmloadnode.pass_1: tnode;
  begin
    result:=inherited;
    if assigned(result) then
      exit;
    case symtableentry.typ of
      procsym:
        begin
          if assigned(left) then
            expectloc:=LOC_REFERENCE;
        end;
    end;
  end;

procedure tllvmloadnode.pass_generate_code;
  var
    pvdef: tprocvardef;
    href, mpref: treference;
    field: tfieldvarsym;
    procreg, selfreg: tregister;
  begin
    inherited;
    case symtableentry.typ of
      procsym:
        begin
          { if the result is returned in two registers, force it to memory into
            a single memory location, as we don't use the registerhi/register
            location hack for llvm (llvm will put it back into registers itself)
          }
          if assigned(left) then
            begin
              pvdef:=tprocvardef(procdef.getcopyas(procvardef,pc_normal));
              { on little endian, location.register contains proc and
                location.registerhi contains self; on big endian, it's the
                other way around }
              tg.gethltemp(current_asmdata.CurrAsmList,resultdef,resultdef.size,tt_normal,href);
              if target_info.endian=endian_little then
                begin
                  procreg:=location.register;
                  selfreg:=location.registerhi
                end
              else
                begin
                  procreg:=location.registerhi;
                  selfreg:=location.register
                end;
              mpref:=href;
              hlcg.g_ptrtypecast_ref(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef),cpointerdef.getreusable(methodpointertype),mpref);
              hlcg.g_load_reg_field_by_name(current_asmdata.CurrAsmList,cprocvardef.getreusableprocaddr(procdef),trecorddef(methodpointertype),procreg,'proc',mpref);
              hlcg.g_load_reg_field_by_name(current_asmdata.CurrAsmList,left.resultdef,trecorddef(methodpointertype),selfreg,'self',mpref);
              location_reset_ref(location,LOC_REFERENCE,location.size,href.alignment);
              location.reference:=href;
            end;
        end;
    end;
  end;

{ tllvmarrayconstructornode }

procedure tllvmarrayconstructornode.makearrayref(var ref: treference; eledef: tdef);
  begin
    { the array elements are addressed as pointer to the individual elements ->
      convert }
    hlcg.g_ptrtypecast_ref(current_asmdata.CurrAsmList,cpointerdef.getreusable(resultdef),cpointerdef.getreusable(eledef),ref);
  end;


begin
  cloadnode:=tllvmloadnode;
  carrayconstructornode:=tllvmarrayconstructornode;
end.

