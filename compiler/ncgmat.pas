{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate generic mathematical nodes

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
unit ncgmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,cpubase,cgbase,cginfo;

type
      tcgunaryminusnode = class(tunaryminusnode)
      protected
         { This routine is called to change the sign of the
           floating point value in the floating point
           register r.

           This routine should be overriden, since
           the generic version is not optimal at all. The
           generic version assumes that floating
           point values are stored in the register
           in IEEE-754 format.
         }
         procedure emit_float_sign_change(r: tregister; _size : tcgsize);virtual;
{$ifdef SUPPORT_MMX}
         procedure second_mmx;virtual;abstract;
{$endif SUPPORT_MMX}
         procedure second_64bit;virtual;
         procedure second_integer;virtual;
         procedure second_float;virtual;
      public
         procedure pass_2;override;
      end;

      tcgmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      protected
         { This routine must do an actual 32-bit division, be it
           signed or unsigned. The result must set into the the
           @var(num) register.

           @param(signed Indicates if the division must be signed)
           @param(denum  Register containing the denominator
           @param(num    Register containing the numerator, will also receive result)

           The actual optimizations regarding shifts have already
           been done and emitted, so this should really a do a divide.
         }
         procedure emit_div_reg_reg(signed: boolean;denum,num : tregister);virtual;abstract;
         { This routine must do an actual 32-bit modulo, be it
           signed or unsigned. The result must set into the the
           @var(num) register.

           @param(signed Indicates if the modulo must be signed)
           @param(denum  Register containing the denominator
           @param(num    Register containing the numerator, will also receive result)

           The actual optimizations regarding shifts have already
           been done and emitted, so this should really a do a modulo.
         }
         procedure emit_mod_reg_reg(signed: boolean;denum,num : tregister);virtual;abstract;
         { This routine must do an actual 64-bit division, be it
           signed or unsigned. The result must set into the the
           @var(num) register.

           @param(signed Indicates if the division must be signed)
           @param(denum  Register containing the denominator
           @param(num    Register containing the numerator, will also receive result)

           The actual optimizations regarding shifts have already
           been done and emitted, so this should really a do a divide.
           Currently, this routine should only be implemented on
           64-bit systems, otherwise a helper is called in 1st pass.
         }
         procedure emit64_div_reg_reg(signed: boolean;denum,num : tregister64);virtual;
      end;

      tcgshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      tcgnotnode = class(tnotnode)
      protected
         procedure second_boolean;virtual;abstract;
{$ifdef SUPPORT_MMX}
         procedure second_mmx;virtual;abstract;
{$endif SUPPORT_MMX}
         procedure second_64bit;virtual;
         procedure second_integer;virtual;
      public
         procedure pass_2;override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defutil,
      pass_1,pass_2,
      ncon,
      cpuinfo,
      tgobj,ncgutil,cgobj,rgobj,paramgr,cg64f32;

{*****************************************************************************
                          TCGUNARYMINUSNODE
*****************************************************************************}

    procedure tcgunaryminusnode.emit_float_sign_change(r: tregister; _size : tcgsize);
      var
        href : treference;
        hreg : tregister;
      begin
        { get a temporary memory reference to store the floating
          point value
        }
        tg.gettemp(exprasmlist,tcgsize2size[_size],tt_normal,href);
        { store the floating point value in the temporary memory area }
        cg.a_loadfpu_reg_ref(exprasmlist,_size,r,href);
        { only single and double ieee are supported }
        if _size = OS_F64 then
          begin
            { on little-endian machine the most significant
              32-bit value is stored at the highest address
            }
            if target_info.endian = endian_little then
              inc(href.offset,4);
          end
        else
        if _size <> OS_F32 then
           internalerror(20020814);
        hreg := rg.getregisterint(exprasmlist,OS_32);
        { load value }
        cg.a_load_ref_reg(exprasmlist,OS_32,OS_32,href,hreg);
        { bitwise complement copied value }
        cg.a_op_reg_reg(exprasmlist,OP_NOT,OS_32,hreg,hreg);
        { sign-bit is bit 31/63 of single/double }
        cg.a_op_const_reg(exprasmlist,OP_AND,OS_32,aword($80000000),hreg);
        { or with value in reference memory }
        cg.a_op_reg_ref(exprasmlist,OP_OR,OS_32,hreg,href);
        rg.ungetregisterint(exprasmlist,hreg);
        { store the floating point value in the temporary memory area }
        if _size = OS_F64 then
          begin
            { on little-endian machine the most significant
              32-bit value is stored at the highest address
            }
            if target_info.endian = endian_little then
              dec(href.offset,4);
          end;
        cg.a_loadfpu_ref_reg(exprasmlist,_size,href,r);
      end;


    procedure tcgunaryminusnode.second_64bit;
      begin
        secondpass(left);
        { load left operator in a register }
        location_copy(location,left.location);
        location_force_reg(exprasmlist,location,OS_64,false);
        cg64.a_op64_loc_reg(exprasmlist,OP_NEG,
           location,joinreg64(location.registerlow,location.registerhigh));
      end;


    procedure tcgunaryminusnode.second_float;
      begin
        secondpass(left);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
        case left.location.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              reference_release(exprasmlist,left.location.reference);
              location.register:=rg.getregisterfpu(exprasmlist,location.size);
              cg.a_loadfpu_ref_reg(exprasmlist,
                 def_cgsize(left.resulttype.def),
                 left.location.reference,location.register);
              emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));
            end;
          LOC_FPUREGISTER:
            begin
               location.register:=left.location.register;
               emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));
            end;
          LOC_CFPUREGISTER:
            begin
               location.register:=rg.getregisterfpu(exprasmlist,location.size);
               cg.a_loadfpu_reg_reg(exprasmlist,left.location.register,location.register);
               emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));
            end;
          else
            internalerror(200306021);
        end;
      end;


    procedure tcgunaryminusnode.second_integer;
      begin
        secondpass(left);
        { load left operator in a register }
        location_copy(location,left.location);
        location_force_reg(exprasmlist,location,OS_INT,false);
        cg.a_op_reg_reg(exprasmlist,OP_NEG,OS_INT,location.register,location.register);
      end;


    procedure tcgunaryminusnode.pass_2;
      begin
         if is_64bit(left.resulttype.def) then
           second_64bit
{$ifdef SUPPORT_MMX}
         else
           if (cs_mmx in aktlocalswitches) and is_mmx_able_array(left.resulttype.def) then
             second_mmx
{$endif SUPPORT_MMX}
         else
           if (left.resulttype.def.deftype=floatdef) then
             second_float
         else
           second_integer;
      end;


{*****************************************************************************
                             TCGMODDIVNODE
*****************************************************************************}

    procedure tcgmoddivnode.emit64_div_reg_reg(signed: boolean; denum,num:tregister64);
      begin
        { handled in pass_1 already, unless pass_1 is
          overriden
        }
        { should be handled in pass_1 (JM) }
        internalerror(200109052);
      end;


    procedure tcgmoddivnode.pass_2;
      var
         hreg1 : tregister;
         hdenom : tregister;
         power : longint;
         hl : tasmlabel;
         pushedregs : tmaybesave;
      begin
         secondpass(left);
         if codegenerror then
          exit;
        {$ifndef newra}
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
        {$endif}
         secondpass(right);
        {$ifndef newra}
         maybe_restore(exprasmlist,left.location,pushedregs);
        {$endif newra}
         if codegenerror then
          exit;
         location_copy(location,left.location);

         if is_64bit(resulttype.def) then
           begin
             { this code valid for 64-bit cpu's only ,
               otherwise helpers are called in pass_1
             }
             location_force_reg(exprasmlist,location,OS_64,false);
             location_copy(location,left.location);
             location_force_reg(exprasmlist,right.location,OS_64,false);
             emit64_div_reg_reg(is_signed(left.resulttype.def),
               joinreg64(right.location.registerlow,right.location.registerhigh),
               joinreg64(location.registerlow,location.registerhigh));
           end
         else
           begin
              { put numerator in register }
              location_force_reg(exprasmlist,left.location,OS_INT,false);
              hreg1:=left.location.register;

              if (nodetype=divn) and
                 (right.nodetype=ordconstn) and
                 ispowerof2(tordconstnode(right).value,power) then
                Begin
                  { for signed numbers, the numerator must be adjusted before the
                    shift instruction, but not wih unsigned numbers! Otherwise,
                    "Cardinal($ffffffff) div 16" overflows! (JM) }
                  If is_signed(left.resulttype.def) Then
                    Begin
                      objectlibrary.getlabel(hl);
                      cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_GT,0,hreg1,hl);
                      if power=1 then
                        cg.a_op_const_reg(exprasmlist,OP_ADD,OS_INT,1,hreg1)
                      else
                        cg.a_op_const_reg(exprasmlist,OP_ADD,OS_INT,tordconstnode(right).value-1,hreg1);
                      cg.a_label(exprasmlist,hl);
                      cg.a_op_const_reg(exprasmlist,OP_SAR,OS_INT,power,hreg1);
                    End
                  Else { not signed }
                    cg.a_op_const_reg(exprasmlist,OP_SHR,OS_INT,power,hreg1);
                End
              else
                begin
                  { bring denominator to hdenom }
                  { hdenom is always free, it's }
                  { only used for temporary }
                  { purposes                }
                  hdenom := rg.getregisterint(exprasmlist,OS_INT);
                  if right.location.loc<>LOC_CREGISTER then
                   location_release(exprasmlist,right.location);
                  cg.a_load_loc_reg(exprasmlist,right.location.size,right.location,hdenom);
                  { verify if the divisor is zero, if so return an error
                    immediately
                  }
                  objectlibrary.getlabel(hl);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_NE,0,hdenom,hl);
                  cg.a_param_const(exprasmlist,OS_S32,200,paramanager.getintparaloc(1));
                  cg.a_call_name(exprasmlist,'FPC_HANDLERROR');
                  cg.a_label(exprasmlist,hl);
                  if nodetype = modn then
                    emit_mod_reg_reg(is_signed(left.resulttype.def),hdenom,hreg1)
                  else
                    emit_div_reg_reg(is_signed(left.resulttype.def),hdenom,hreg1);
                end;
              location_reset(location,LOC_REGISTER,OS_INT);
              location.register:=hreg1;
           end;
        cg.g_overflowcheck(exprasmlist,location,resulttype.def);
      end;


{*****************************************************************************
                             TCGSHLRSHRNODE
*****************************************************************************}


    procedure tcgshlshrnode.pass_2;
      var
         hcountreg : tregister;
         op : topcg;
         pushedregs : tmaybesave;
         freescratch : boolean;
      begin
         freescratch:=false;
         secondpass(left);
      {$ifndef newra}
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
      {$endif newra}
         secondpass(right);
      {$ifndef newra}
         maybe_restore(exprasmlist,left.location,pushedregs);
      {$endif}
         { determine operator }
         case nodetype of
           shln: op:=OP_SHL;
           shrn: op:=OP_SHR;
         end;

         if is_64bit(left.resulttype.def) then
           begin
              { already hanled in 1st pass }
              internalerror(2002081501);
(*  Normally for 64-bit cpu's this here should be here,
    and only pass_1 need to be overriden, but dunno how to
    do that!
              location_reset(location,LOC_REGISTER,OS_64);

              { load left operator in a register }
              location_force_reg(exprasmlist,left.location,OS_64,false);
              location_copy(location,left.location);

              if (right.nodetype=ordconstn) then
                begin
                   cg64.a_op64_const_reg(exprasmlist,op,tordconstnode(right).value,
                     joinreg64(location.registerlow,location.registerhigh));
                end
              else
                begin
                  { this should be handled in pass_1 }
                  internalerror(2002081501);

                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                       hcountreg:=cg.get_scratch_reg_int(exprasmlist);
                       cg.a_load_loc_reg(exprasmlist,right.location.size,right.location,hcountreg);
                       freescratch := true;
                     end
                   else
                      hcountreg:=right.location.register;
                   cg64.a_op64_reg_reg(exprasmlist,op,hcountreg,
                     joinreg64(location.registerlow,location.registerhigh));
                   if freescratch then
                      cg.free_scratch_reg(exprasmlist,hcountreg);
                end;*)
           end
         else
           begin
              { load left operators in a register }
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_INT,false);

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                begin
                   { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)
                   if right.value<=31 then
                   }
                   cg.a_op_const_reg(exprasmlist,op,location.size,
                     tordconstnode(right).value and 31,location.register);
                   {
                   else
                     emit_reg_reg(A_XOR,S_L,hregister1,
                       hregister1);
                   }
                end
              else
                begin
                   { load right operators in a register - this
                     is done since most target cpu which will use this
                     node do not support a shift count in a mem. location (cec)
                   }
                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                     {$ifdef newra}
                       hcountreg:=rg.getregisterint(exprasmlist,OS_INT);
                     {$else}
                       hcountreg:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
                     {$endif}
                       freescratch := true;
                       cg.a_load_loc_reg(exprasmlist,right.location.size,right.location,hcountreg);
                     end
                   else
                     hcountreg:=right.location.register;
                   cg.a_op_reg_reg(exprasmlist,op,OS_INT,hcountreg,location.register);
                 {$ifdef newra}
                   if freescratch then
                      rg.ungetregisterint(exprasmlist,hcountreg);
                 {$else}
                   if freescratch then
                      cg.free_scratch_reg(exprasmlist,hcountreg);
                 {$endif}
                end;
           end;
      end;


{*****************************************************************************
                               TCGNOTNODE
*****************************************************************************}

    procedure tcgnotnode.second_64bit;
      begin
        secondpass(left);
        location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
        location_copy(location,left.location);
        { perform the NOT operation }
        cg64.a_op64_reg_reg(exprasmlist,OP_NOT,left.location.register64,location.register64);
      end;


    procedure tcgnotnode.second_integer;
      begin
        secondpass(left);
        location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
        location_copy(location,left.location);
        { perform the NOT operation }
        cg.a_op_reg_reg(exprasmlist,OP_NOT,location.size,location.register,location.register);
      end;


    procedure tcgnotnode.pass_2;
      begin
        if is_boolean(resulttype.def) then
          second_boolean
{$ifdef SUPPORT_MMX}
        else if (cs_mmx in aktlocalswitches) and is_mmx_able_array(left.resulttype.def) then
          second_mmx
{$endif SUPPORT_MMX}
        else if is_64bit(left.resulttype.def) then
          second_64bit
        else
          second_integer;
      end;

begin
   cmoddivnode:=tcgmoddivnode;
   cunaryminusnode:=tcgunaryminusnode;
   cshlshrnode:=tcgshlshrnode;
   cnotnode:=tcgnotnode;
end.
{
  $Log$
  Revision 1.13  2003-06-03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.12  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.11  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.10  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.9  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.8  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.7  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.6  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.5  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.4  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.3  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.2  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.1  2002/08/14 19:26:55  carl
    + generic int_to_real type conversion
    + generic unaryminus node

}
