{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Generate assembler for nodes that handle type conversions which are
    the same for all (most) processors

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
unit ncgcnv;

{$i defines.inc}

interface

    uses
       node,ncnv;

    type
       tcgtypeconvnode = class(ttypeconvnode)
         procedure second_cstring_to_pchar;override;
         procedure second_string_to_chararray;override;
         procedure second_array_to_pointer;override;
         procedure second_pointer_to_array;override;
         procedure second_char_to_string;override;
         procedure second_real_to_real;override;
         procedure second_cord_to_pointer;override;
         procedure second_proc_to_procvar;override;
         procedure second_bool_to_int;override;
         procedure second_ansistring_to_pchar;override;
         procedure second_class_to_intf;override;
         procedure second_char_to_char;override;
       end;

  implementation

    uses
      cutils,verbose,
      aasm,symconst,symdef,
      ncon,ncal,
      cpubase,cpuinfo,
      pass_2,
      cgbase,
      cga,cgobj,cgcpu,
{$ifdef i386}
      n386util,
{$endif i386}
      tgcpu,temp_gen
      ;


    procedure tcgtypeconvnode.second_cstring_to_pchar;

      var
        hr : treference;

      begin
         clear_location(location);
         location.loc:=LOC_REGISTER;
         case tstringdef(left.resulttype.def).string_typ of
           st_shortstring :
             begin
               inc(left.location.reference.offset);
               del_reference(left.location.reference);
               location.register:=getregisterint;
               cg.a_loadaddress_ref_reg(exprasmlist,left.location.reference,
                 location.register);
             end;
           st_ansistring :
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  reset_reference(hr);
                  hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  location.register:=getregisterint;
                  cg.a_loadaddress_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  del_reference(left.location.reference);
                  location.register:=getregisterint;
                  cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                    location.register);
                end;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
           st_widestring:
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  reset_reference(hr);
                  hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  location.register:=getregisterint;
                  cg.a_loadaddress_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  del_reference(left.location.reference);
                  location.register:=getregisterint;
{$warning Todo: convert widestrings to ascii when typecasting them to pchars}
                  cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                    location.register);
                end;
             end;
         end;
      end;


    procedure tcgtypeconvnode.second_string_to_chararray;

      var
        arrsize: longint;

      begin
         with tarraydef(resulttype.def) do
           arrsize := highrange-lowrange+1;
         if (left.nodetype = stringconstn) and
            { left.length+1 since there's always a terminating #0 character (JM) }
            (tstringconstnode(left).len+1 >= arrsize) and
            (tstringdef(left.resulttype.def).string_typ=st_shortstring) then
           begin
             inc(location.reference.offset);
             exit;
           end
         else
           { should be handled already in resulttype pass (JM) }
           internalerror(200108292);
      end;


    procedure tcgtypeconvnode.second_array_to_pointer;

      begin
         del_reference(left.location.reference);
         clear_location(location);
         location.loc:=LOC_REGISTER;
         location.register:=getregisterint;
         cg.a_loadaddress_ref_reg(exprasmlist,left.location.reference,
           location.register);
      end;


    procedure tcgtypeconvnode.second_pointer_to_array;

      begin
        clear_location(location);
        location.loc:=LOC_REFERENCE;
        reset_reference(location.reference);
        case left.location.loc of
          LOC_REGISTER :
            location.reference.base:=left.location.register;
          LOC_CREGISTER :
            begin
              location.reference.base:=getregisterint;
              cg.a_load_reg_reg(exprasmlist,OS_32,left.location.register,
                location.reference.base);
            end
         else
            begin
              del_reference(left.location.reference);
              location.reference.base:=getregisterint;
              cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                location.reference.base);
            end;
        end;
      end;


    procedure tcgtypeconvnode.second_char_to_string;

      begin
         clear_location(location);
         location.loc:=LOC_MEM;
         case tstringdef(resulttype.def).string_typ of
           st_shortstring :
             begin
               gettempofsizereference(256,location.reference);
               loadshortstring(left,self);
             end;
           { the rest is removed in the resulttype pass and converted to compilerprocs }
           else
            internalerror(4179);
        end;
      end;


    procedure tcgtypeconvnode.second_real_to_real;
      begin
         case left.location.loc of
            LOC_FPU : ;
            LOC_CFPUREGISTER:
              begin
                 location:=left.location;
                 exit;
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 floatload(tfloatdef(left.resulttype.def).typ,
                   left.location.reference);
                 { we have to free the reference }
                 del_reference(left.location.reference);
              end;
         end;
         clear_location(location);
         location.loc:=LOC_FPU;
      end;


    procedure tcgtypeconvnode.second_cord_to_pointer;
      begin
        { this can't happen because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;


    procedure tcgtypeconvnode.second_proc_to_procvar;

      begin
        { method pointer ? }
        if assigned(tcallnode(left).left) then
          begin
             set_location(location,left.location);
          end
        else
          begin
             clear_location(location);
             location.loc:=LOC_REGISTER;
             del_reference(left.location.reference);
             location.register:=getregisterint;
             cg.a_loadaddress_ref_reg(exprasmlist,left.location.reference,
               location.register);
          end;
      end;


    procedure tcgtypeconvnode.second_bool_to_int;

      var
         oldtruelabel,oldfalselabel,hlabel : tasmlabel;
         newsize,
         opsize : tcgsize;

      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(left);
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(location,left.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;
         clear_location(location);
         location.loc:=LOC_REGISTER;
         del_location(left.location);
         location.register:=getregisterint;
         { size of the boolean we're converting }
         opsize := def_cgsize(left.resulttype.def);
         { size of the destination }
         newsize := def_cgsize(resulttype.def);
         { the the source size is bigger than the destination, we can }
         { simply decrease the sources size (since wordbool(true) =   }
         { boolean(true) etc... (JM)                                  }
         case newsize of
           OS_8,OS_S8:
             begin
               opsize := OS_8;
{$ifdef i386}
               location.register := makereg8(location.register);
               if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                 makereg8(left.location.register);
{$endif i386}
             end;
           OS_16,OS_S16:
             begin
{$ifdef i386}
               location.register := makereg16(location.register);
{$endif i386}
               if opsize in [OS_32,OS_S32] then
                 begin
                   opsize := OS_16;
{$ifdef i386}
                   if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                     makereg16(left.location.register);
{$endif i386}
                 end
             end;
         end;
         case left.location.loc of
            LOC_MEM,
      LOC_REFERENCE :
           cg.a_load_ref_reg(exprasmlist,opsize,left.location.reference,
             location.register);
       LOC_REGISTER,
      LOC_CREGISTER :
           if left.location.register<>location.register then
             cg.a_load_reg_reg(exprasmlist,opsize,left.location.register,
               location.register);
          LOC_FLAGS :
            cg.g_flags2reg(exprasmlist,left.location.resflags,location.register);
           LOC_JUMP :
             begin
               getlabel(hlabel);
               cg.a_label(exprasmlist,truelabel);
               cg.a_load_const_reg(exprasmlist,newsize,1,location.register);
               cg.a_jmp_cond(exprasmlist,OC_NONE,hlabel);
               cg.a_label(exprasmlist,falselabel);
               cg.a_load_const_reg(exprasmlist,newsize,0,location.register);
               cg.a_label(exprasmlist,hlabel);
             end;
         else
           internalerror(10061);
         end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure tcgtypeconvnode.second_ansistring_to_pchar;
      var
         l1 : tasmlabel;
         hr : treference;
      begin
         clear_location(location);
         location.loc:=LOC_REGISTER;
         getlabel(l1);
         case left.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              location.register:=left.location.register;
            LOC_MEM,LOC_REFERENCE:
              begin
                del_reference(left.location.reference);
                location.register:=getregisterint;
                cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                  location.register);
              end;
         end;
         cg.a_cmp_const_reg_label(exprasmlist,OS_32,OC_NE,0,location.register,
           l1);
         reset_reference(hr);
         hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
         cg.a_loadaddress_ref_reg(exprasmlist,hr,location.register);
         cg.a_label(exprasmlist,l1);
      end;


    procedure tcgtypeconvnode.second_class_to_intf;
      var
         hreg : tregister;
         l1 : tasmlabel;
      begin
         case left.location.loc of
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 del_reference(left.location.reference);
                 hreg:=getregisterint;
                 cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                   hreg);
              end;
            LOC_CREGISTER:
              begin
                 hreg:=getregisterint;
                 cg.a_load_reg_reg(exprasmlist,OS_32,left.location.register,
                   hreg);
              end;
            LOC_REGISTER:
              hreg:=left.location.register;
            else internalerror(121120001);
         end;
         getlabel(l1);
         cg.a_cmp_const_reg_label(exprasmlist,OS_32,OC_EQ,0,hreg,l1);
         cg.a_op_const_reg(exprasmlist,OP_ADD,aword(
           tobjectdef(left.resulttype.def).implementedinterfaces.ioffsets(
           tobjectdef(left.resulttype.def).implementedinterfaces.searchintf(
           resulttype.def))^),hreg);
         cg.a_label(exprasmlist,l1);
         location.loc:=LOC_REGISTER;
         location.register:=hreg;
      end;


    procedure tcgtypeconvnode.second_char_to_char;
      begin
        {$warning todo: add RTL routine for widechar-char conversion }
        { Quick hack to atleast generate 'working' code (PFV) }
        second_int_to_int;
      end;

begin
  ctypeconvnode := tcgtypeconvnode;
end.

{
  $Log$
  Revision 1.4  2001-12-31 09:53:15  jonas
    * changed remaining "getregister32" calls to "getregisterint"

  Revision 1.3  2001/10/04 14:33:28  jonas
    * fixed range check errors

  Revision 1.2  2001/09/30 16:16:28  jonas
    - removed unused units form uses-clause and unused local vars

  Revision 1.1  2001/09/29 21:32:47  jonas
    * almost all second pass typeconvnode helpers are now processor independent
    * fixed converting boolean to int64/qword
    * fixed register allocation bugs which could cause internalerror 10
    * isnode and asnode are completely processor indepent now as well
    * fpc_do_as now returns its class argument (necessary to be able to use it
      properly with compilerproc)


}
