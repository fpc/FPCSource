{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate generic inline nodes

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
unit ncginl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl;

    type
       tcginlinenode = class(tinlinenode)
          procedure pass_2;override;
          procedure second_assigned;virtual; abstract;
          procedure second_assert;virtual;
          procedure second_sizeoftypeof;virtual;
          procedure second_length;virtual;
          procedure second_predsucc;virtual;
          procedure second_incdec;virtual;
          procedure second_typeinfo;virtual;
          procedure second_includeexclude;virtual; abstract;
          procedure second_pi; virtual;
          procedure second_arctan_real; virtual;
          procedure second_abs_real; virtual;
          procedure second_sqr_real; virtual;
          procedure second_sqrt_real; virtual;
          procedure second_ln_real; virtual;
          procedure second_cos_real; virtual;
          procedure second_sin_real; virtual;
       end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,fmodule,
      symconst,symdef,defbase,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_1,pass_2,
      cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;


{*****************************************************************************
                              TCGINLINENODE
*****************************************************************************}


    procedure tcginlinenode.pass_2;
       var
         asmop : tasmop;
         l : longint;
         oldpushedparasize : longint;
      begin
         { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case inlinenumber of
            in_assert_x_y:
              begin
                 second_Assert;
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 second_SizeofTypeOf;
              end;
            in_length_x :
              begin
                 second_Length;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 second_PredSucc;
              end;
            in_dec_x,
            in_inc_x :
              begin
                second_IncDec;
              end;
            in_typeinfo_x:
               begin
                  second_TypeInfo;
               end;
            in_assigned_x :
              begin
                 second_Assigned;
              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 second_IncludeExclude;
              end;
            in_pi:
              begin
                second_pi;
              end;
            in_sin_extended:
              begin
                second_sin_real;
              end;
            in_arctan_extended:
              begin
                second_arctan_real;
              end;
            in_abs_extended:
              begin
                second_abs_real;
              end;
            in_sqr_extended:
              begin
                second_sqr_real;
              end;
            in_sqrt_extended:
              begin
                second_sqrt_real;
              end;
            in_ln_extended:
              begin
                second_ln_real;
              end;
            in_cos_extended:
              begin
                 second_cos_real;
              end;
{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
                 location_reset(location,LOC_MMXREGISTER,OS_NO);
                 if left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else if tcallparanode(left).left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else
                   begin
                      {!!!!!!!}
                   end;
              end;
{$endif SUPPORT_MMX}
            else internalerror(9);
         end;
         { reset pushedparasize }
         pushedparasize:=oldpushedparasize;
      end;


{*****************************************************************************
                          ASSERT GENERIC HANDLING
*****************************************************************************}
    procedure tcginlinenode.second_Assert;
     var
         hp2 : tstringconstnode;
         otlabel,oflabel{,l1}   : tasmlabel;
     begin
       { the node should be removed in the firstpass }
       if not (cs_do_assertion in aktlocalswitches) then
          internalerror(7123458);
       otlabel:=truelabel;
       oflabel:=falselabel;
       getlabel(truelabel);
       getlabel(falselabel);
       secondpass(tcallparanode(left).left);
       maketojumpbool(exprasmlist,tcallparanode(left).left,lr_load_regvars);
       cg.a_label(exprasmlist,falselabel);
       { erroraddr }
       cg.a_param_reg(exprasmlist,OS_ADDR,FRAME_POINTER_REG,paramanager.getintparaloc(4));
       { lineno }
       cg.a_param_const(exprasmlist,OS_INT,aktfilepos.line,paramanager.getintparaloc(3));
       { filename string }
       hp2:=cstringconstnode.createstr(current_module.sourcefiles.get_file_name(aktfilepos.fileindex),st_shortstring);
       firstpass(hp2);
       secondpass(hp2);
       if codegenerror then
          exit;
       cg.a_paramaddr_ref(exprasmlist,hp2.location.reference,paramanager.getintparaloc(2));
       hp2.free;
       { push msg }
       secondpass(tcallparanode(tcallparanode(left).right).left);
       cg.a_paramaddr_ref(exprasmlist,tcallparanode(tcallparanode(left).right).left.location.reference,paramanager.getintparaloc(1));
       { call }
       cg.a_call_name(exprasmlist,'FPC_ASSERT');
       cg.a_label(exprasmlist,truelabel);
       truelabel:=otlabel;
       falselabel:=oflabel;
     end;


{*****************************************************************************
                          SIZEOF / TYPEOF GENERIC HANDLING
*****************************************************************************}

    { second_handle_ the sizeof and typeof routines }
    procedure tcginlinenode.second_SizeOfTypeOf;
      var
         href : treference;
         hregister : tregister;
      begin
        location_reset(location,LOC_REGISTER,OS_ADDR);
        { for both cases load vmt }
        if left.nodetype=typen then
          begin
            hregister:=rg.getaddressregister(exprasmlist);
            reference_reset_symbol(href,newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),0);
            cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
          end
        else
          begin
            secondpass(left);
            location_release(exprasmlist,left.location);
            hregister:=rg.getaddressregister(exprasmlist);
            { load VMT pointer }
            inc(left.location.reference.offset,tobjectdef(left.resulttype.def).vmt_offset);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,hregister);
          end;
        { in sizeof load size }
        if inlinenumber=in_sizeof_x then
           begin
             reference_reset_base(href,hregister,0);
             rg.ungetaddressregister(exprasmlist,hregister);
             hregister:=rg.getregisterint(exprasmlist);
             cg.a_load_ref_reg(exprasmlist,OS_INT,href,hregister);
           end;
        location.register:=hregister;
     end;



{*****************************************************************************
                          LENGTH GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_Length;
      var
        lengthlab : tasmlabel;
        hregister : tregister;
        href : treference;
      begin
        secondpass(left);
        { length in ansi strings is at offset -8 }
        if is_ansistring(left.resulttype.def) or
           is_widestring(left.resulttype.def) then
            begin
              location_force_reg(exprasmlist,left.location,OS_ADDR,false);
              hregister:=left.location.register;
              getlabel(lengthlab);
              cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,hregister,lengthlab);
              reference_reset_base(href,hregister,-8);
              cg.a_load_ref_reg(exprasmlist,OS_32,href,hregister);
              cg.a_label(exprasmlist,lengthlab);
              location_reset(location,LOC_REGISTER,OS_32);
              location.register:=hregister;
            end
         else
            begin
              location_copy(location,left.location);
              location.size:=OS_8;
            end;
      end;


{*****************************************************************************
                         PRED/SUCC GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_PredSucc;
      var
         cgsize : TCGSize;
         cgop : topcg;
      begin
        secondpass(left);
        if inlinenumber=in_pred_x then
           cgop:=OP_SUB
        else
           cgop:=OP_ADD;
        cgsize:=def_cgsize(resulttype.def);

        { we need a value in a register }
        location_copy(location,left.location);
        location_force_reg(exprasmlist,location,cgsize,false);

        if cgsize in [OS_64,OS_S64] then
          cg64.a_op64_const_reg(exprasmlist,cgop,1,
                      location.register64)
        else
          cg.a_op_const_reg(exprasmlist,cgop,1,location.register);

        cg.g_overflowcheck(exprasmlist,self);
        cg.g_rangecheck(exprasmlist,self,resulttype.def);
      end;


{*****************************************************************************
                         INC/DEC GENERIC HANDLING
*****************************************************************************}
      procedure tcginlinenode.second_IncDec;
       const
         addsubop:array[in_inc_x..in_dec_x] of TOpCG=(OP_ADD,OP_SUB);
        var
         addvalue : longint;
         addconstant : boolean;
         hregisterhi,
         hregister : tregister;
         cgsize : tcgsize;
         pushedregs : tmaybesave;
        begin
          { set defaults }
          addconstant:=true;
          { load first parameter, must be a reference }
          secondpass(tcallparanode(left).left);
          cgsize:=def_cgsize(tcallparanode(left).left.resulttype.def);
          { get addvalue }
          case tcallparanode(left).left.resulttype.def.deftype of
            orddef,
            enumdef :
                addvalue:=1;
            pointerdef :
               begin
                 if is_void(tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def) then
                   addvalue:=1
                 else
                   addvalue:=tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def.size;
               end;
           else
               internalerror(10081);
          end;
          { second_ argument specified?, must be a s32bit in register }
          if assigned(tcallparanode(left).right) then
            begin
              maybe_save(exprasmlist,tcallparanode(tcallparanode(left).right).left.registers32,
                 tcallparanode(left).left.location,pushedregs);
              secondpass(tcallparanode(tcallparanode(left).right).left);
              maybe_restore(exprasmlist,tcallparanode(left).left.location,pushedregs);
              { when constant, just multiply the addvalue }
              if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                 addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
              else
                begin
                  location_force_reg(exprasmlist,tcallparanode(tcallparanode(left).right).left.location,cgsize,false);
                  hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
                  hregisterhi:=tcallparanode(tcallparanode(left).right).left.location.registerhigh;
                  { insert multiply with addvalue if its >1 }
                  if addvalue>1 then
                    cg.a_op_const_reg(exprasmlist,OP_IMUL,addvalue,hregister);
                  addconstant:=false;
                end;
            end;
          { write the add instruction }
          if addconstant then
            begin
              if cgsize in [OS_64,OS_S64] then
               cg64.a_op64_const_loc(exprasmlist,addsubop[inlinenumber],
                  addvalue,tcallparanode(left).left.location)
              else
               cg.a_op_const_loc(exprasmlist,addsubop[inlinenumber],
                  addvalue,tcallparanode(left).left.location);
            end
           else
             begin
               if cgsize in [OS_64,OS_S64] then
                 cg64.a_op64_reg_loc(exprasmlist,addsubop[inlinenumber],
                   joinreg64(hregister,hregisterhi),tcallparanode(left).left.location)
               else
                 cg.a_op_reg_loc(exprasmlist,addsubop[inlinenumber],
                   hregister,tcallparanode(left).left.location);
                 location_release(exprasmlist,tcallparanode(tcallparanode(left).right).left.location);
             end;
          cg.g_overflowcheck(exprasmlist,tcallparanode(left).left);
          cg.g_rangecheck(exprasmlist,tcallparanode(left).left,tcallparanode(left).left.resulttype.def);
        end;


{*****************************************************************************
                         TYPEINFO GENERIC HANDLING
*****************************************************************************}
      procedure tcginlinenode.second_typeinfo;
        var
         href : treference;
        begin
          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=rg.getaddressregister(exprasmlist);
          reference_reset_symbol(href,tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(fullrtti),0);
          cg.a_loadaddr_ref_reg(exprasmlist,href,location.register);
        end;


{*****************************************************************************
                         ASSIGNED GENERIC HANDLING
*****************************************************************************}
(*
      procedure tcginlinenode.second_Assigned;
        var
         hreg : tregister;
         ptrvalidlabel : tasmlabel;
        begin
          secondpass(tcallparanode(left).left);
          location_release(exprasmlist,tcallparanode(left).left.location);
          hreg := rg.getregisterint(exprasmlist);
          if (tcallparanode(left).left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
            begin
              { if pointer is non-nil, and is in register, this directly the value we can use }
              cg.a_load_reg_reg(exprasmlist, OS_ADDR, tcallparanode(left).left.location.register, hreg);
            end
          else
            begin
              getlabel(ptrvalidlabel);
              cg.a_load_const_reg(exprasmlist, OS_INT, 1, hreg);
              cg.a_cmp_const_ref_label(exprasmlist, OS_ADDR, OC_NE, 0,
                  tcallparanode(left).left.location.reference, ptrvalidlabel);
              cg.a_load_const_reg(exprasmlist, OS_INT, 0, hreg);
              cg.a_label(exprasmlist,ptrvalidlabel);
            end;
          location.register := hreg;
          location_reset(location,LOC_REGISTER,OS_INT);
          WriteLn('Exiting assigned node!');
        end;

*)
{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}
(*
      procedure tcginlinenode.second_IncludeExclude;
        var
         scratch_reg : boolean;
         hregister : tregister;
         asmop : tasmop;
         L : longint;
         pushedregs : TMaybesave;
         cgop : topcg;
         addrreg, hregister2: tregister;
         {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
        begin
          location_copy(location,left.location);
          secondpass(tcallparanode(left).left);
          if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
            begin
              { calculate bit position }
              l:=1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod 32);

              { determine operator }
              if inlinenumber=in_include_x_y then
                cgop:=OP_OR
              else
                begin
                  cgop:=OP_AND;
                  l:=not(l);
                end;
              if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                begin
                  inc(tcallparanode(left).left.location.reference.offset,
                    (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div 32)*4);
                  cg.a_op_const_ref(exprasmlist,cgop,OS_INT,l,tcallparanode(left).left.location.reference);
                  location_release(exprasmlist,tcallparanode(left).left.location);
                end
              else
                { LOC_CREGISTER }
                begin
                  cg.a_op_const_reg(exprasmlist,cgop,l,tcallparanode(left).left.location.register);
                end;
            end
          else
            begin
             use_small:=
                 { set type }
                 (tsetdef(tcallparanode(left).left.resulttype.def).settype=smallset) 
                  and
                   { elemenut number between 1 and 32 } 
                  ((tcallparanode(tcallparanode(left).right).left.resulttype.def.deftype=orddef) and 
                   (torddef(tcallparanode(tcallparanode(left).right).left.resulttype.def).high<=32) or
                   (tcallparanode(tcallparanode(left).right).left.resulttype.def.deftype=enumdef) and 
                   (tenumdef(tcallparanode(tcallparanode(left).right).left.resulttype.def).max<=32));
            
              { generate code for the element to set }
              maybe_save(exprasmlist,tcallparanode(tcallparanode(left).right).left.registers32,
                        tcallparanode(left).left.location,pushedregs);
              secondpass(tcallparanode(tcallparanode(left).right).left);
              maybe_restore(exprasmlist,tcallparanode(left).left.location,pushedregs);
              
              { bitnumber - which must be loaded into register }
              hregister := cg.get_scratch_reg_int(exprasmlist);
              hregister2 := rg.getregisterint(exprasmlist);
              
              case tcallparanode(tcallparanode(left).right).left.location.loc of
                 LOC_CREGISTER,
                 LOC_REGISTER
                   begin
                     cg.a_load_reg_reg(exprasmlist,OS_INT,
                       tcallparanode(tcallparanode(left).right).left.location.loc.register),hregister);
                   end;
                 LOC_REFERENCE:
                   begin
                     cgsize := def_cgsize(tcallparanode(tcallparanode(left).right).left.resulttype.def);
                     cg.a_load_ref_reg(exprasmlist,cgsize,
                       tcallparanode(tcallparanode(left).right).left.location.loc.reference),hregister);
                   end;
               else
                 internalerror(20020727);
               end;
               { hregister contains the bitnumber to add }
               cg.a_load_const_reg(exprasmlist, OS_INT, 1, hregister2);
               cg.a_op_reg_reg(exprasmlist, OP_SHL, OS_INT, hregister, hregister2);
              
              
              if use_small then
                begin
                  { possiblities : 
                       bitnumber : LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER
                       set value : LOC_REFERENCE, LOC_REGISTER
                  }
                  { location of set }
                  if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                    begin
                     if inlinenumber=in_include_x_y then
                       begin
                         cg.a_op_reg_ref(exprasmlist, OP_OR, OS_32, hregister2, 
                         tcallparanode(left).left.location.loc.reference);
                       end
                     else
                       begin
                         cg.a_op_reg_reg(exprasmlist, OP_NOT, OS_32, hregister2, 
                         hregister2);
                         cg.a_op_reg_ref(exprasmlist, OP_AND, OS_32, hregister2, 
                         tcallparanode(left).left.location.loc.reference);
                       end;
                      
                    end
                  else
                    internalerror(20020728);
                end
              else
                begin
                  { possiblities : 
                       bitnumber : LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER
                       set value : LOC_REFERENCE
                  }
                  { hregister contains the bitnumber (div 32 to get the correct offset) }
                  cg.a_op_const_reg(exprasmlist, OP_SHR, OS_INT, 5, hregister);
                  { calculate the correct address of the operand }
                  cg.a_loadaddr_ref_reg(exprasmlist, tcallparanode(left).left.location.loc.reference,addrreg);
                  cg.a_op_reg_reg(exprasmlist, OP_ADD, OS_INT, hregister, addrreg);
                  reference_reset_base(href,addrreg,0);
                  
                  if inlinenumber=in_include_x_y then
                       begin
                         cg.a_op_reg_ref(exprasmlist, OP_OR, OS_32, hregister2, href);
                       end
                     else
                       begin
                         cg.a_op_reg_reg(exprasmlist, OP_NOT, OS_32, hregister2, 
                         hregister2);
                         cg.a_op_reg_ref(exprasmlist, OP_AND, OS_32, hregister2, href);
                       end;
                       
                  end;
                  
        end;
*)
{*****************************************************************************
                            FLOAT GENERIC HANDLING
*****************************************************************************}

{
  These routines all call internal RTL routines, so if they are
  called here, they give an internal error
}
    procedure tcginlinenode.second_pi;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_arctan_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_abs_real;
      begin
        internalerror(20020718);
      end;


    procedure tcginlinenode.second_sqr_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_sqrt_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_ln_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_cos_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_sin_real;
      begin
        internalerror(20020718);
      end;

begin
   cinlinenode:=tcginlinenode;
end.

{
  $Log$
  Revision 1.9  2002-08-04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.8  2002/07/31 07:54:59  jonas
    * re-enabled second_assigned()

  Revision 1.7  2002/07/30 20:50:43  florian
    * the code generator knows now if parameters are in registers

  Revision 1.6  2002/07/29 21:23:42  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.5  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.4  2002/07/26 09:45:20  florian
    * fixed a mistake in yesterday's commit, forgot to commit it

  Revision 1.3  2002/07/25 22:58:30  florian
  no message

  Revision 1.2  2002/07/25 17:55:41  carl
    + First working revision

  Revision 1.1  2002/07/24 04:07:49  carl
   + first revision (incomplete)
}
