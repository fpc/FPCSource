{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 64 bit int
    arithmethics on 32 bit processors

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
{# This unit implements the code generation for 64 bit int arithmethics on
   32 bit processors.
}
unit cg64f32;

  {$i fpcdefs.inc}

  interface

    uses
       aasmbase,aasmtai,aasmcpu,
       cpuinfo, cpubase,
       cginfo, cgobj,
       node,symtype
{$ifdef delphi}
       ,dmisc
{$endif}
       ;

    type
      {# Defines all the methods required on 32-bit processors
         to handle 64-bit integers.
      }
      tcg64f32 = class(tcg64)
        procedure a_reg_alloc(list : taasmoutput;r : tregister64);override;
        procedure a_reg_dealloc(list : taasmoutput;r : tregister64);override;
        procedure a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);override;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);override;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64{$ifdef newra};delete:boolean{$endif});override;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64{$ifdef newra};delete:boolean{$endif});override;
        procedure a_load64_const_reg(list : taasmoutput;value: qword;reg : tregister64);override;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64{$ifdef newra};delete: boolean{$endif});override;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);override;
        procedure a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);override;
        procedure a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);override;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;

        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);override;
        procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;reg : tregister64; const ref: treference);override;
        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;value : qword;const l: tlocation);override;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);override;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);override;

        procedure a_param64_reg(list : taasmoutput;reg : tregister64;const locpara : tparalocation);override;
        procedure a_param64_const(list : taasmoutput;value : qword;const locpara : tparalocation);override;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);override;

        {# This routine tries to optimize the a_op64_const_reg operation, by
           removing superfluous opcodes. Returns TRUE if normal processing
           must continue in op64_const_reg, otherwise, everything is processed
           entirely in this routine, by emitting the appropriate 32-bit opcodes.
        }
        function optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : qword; var reg: tregister64): boolean;override;

        procedure g_rangecheck64(list: taasmoutput; const p: tnode;
          const todef: tdef); override;
      end;

    {# Creates a tregister64 record from 2 32 Bit registers. }
    function joinreg64(reglo,reghi : tregister) : tregister64;

  implementation

    uses
       globtype,globals,systems,
       cgbase,
       verbose,
       symbase,symconst,symdef,defutil,rgobj,tgobj;


    function joinreg64(reglo,reghi : tregister) : tregister64;
      begin
         result.reglo:=reglo;
         result.reghi:=reghi;
      end;

    procedure tcg64f32.a_reg_alloc(list : taasmoutput;r : tregister64);

      begin
         list.concat(tai_regalloc.alloc(r.reglo));
         list.concat(tai_regalloc.alloc(r.reghi));
      end;

    procedure tcg64f32.a_reg_dealloc(list : taasmoutput;r : tregister64);

      begin
         list.concat(tai_regalloc.dealloc(r.reglo));
         list.concat(tai_regalloc.dealloc(r.reghi));
      end;

    procedure tcg64f32.a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);
      var
        tmpreg: tregister;
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg:=reg.reglo;
            reg.reglo:=reg.reghi;
            reg.reghi:=tmpreg;
          end;
        cg.a_load_reg_ref(list,OS_32,reg.reglo,ref);
        tmpref := ref;
        inc(tmpref.offset,4);
        cg.a_load_reg_ref(list,OS_32,reg.reghi,tmpref);
      end;

    procedure tcg64f32.a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          swap_qword(value);
        cg.a_load_const_ref(list,OS_32,lo(value),ref);
        tmpref := ref;
        inc(tmpref.offset,4);
        cg.a_load_const_ref(list,OS_32,hi(value),tmpref);
      end;


    procedure tcg64f32.a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64{$ifdef newra};delete:boolean{$endif});
      var
        tmpreg: tregister;
        tmpref: treference;
        got_scratch: boolean;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg := reg.reglo;
            reg.reglo := reg.reghi;
            reg.reghi := tmpreg;
          end;
        got_scratch:=false;
        tmpref := ref;
        if (tmpref.base.number=reg.reglo.number) then
         begin
         {$ifdef newra}
           tmpreg:=rg.getaddressregister(list);
         {$else}
           tmpreg := cg.get_scratch_reg_address(list);
         {$endif}
           got_scratch:=true;
           cg.a_load_reg_reg(list,OS_ADDR,OS_ADDR,tmpref.base,tmpreg);
           tmpref.base:=tmpreg;
         end
        else
         { this works only for the i386, thus the i386 needs to override  }
         { this method and this method must be replaced by a more generic }
         { implementation FK                                              }
         if (tmpref.index.number=reg.reglo.number) then
          begin
          {$ifdef newra}
            tmpreg:=rg.getaddressregister(list);
          {$else}
            tmpreg:=cg.get_scratch_reg_address(list);
          {$endif}
            got_scratch:=true;
            cg.a_load_reg_reg(list,OS_ADDR,OS_ADDR,tmpref.index,tmpreg);
            tmpref.index:=tmpreg;
          end;
        cg.a_load_ref_reg(list,OS_32,tmpref,reg.reglo);
        inc(tmpref.offset,4);
{$ifdef newra}
        if delete then
          begin
            tg.ungetiftemp(list,tmpref);
            reference_release(list,tmpref);
          end;
{$endif}
        cg.a_load_ref_reg(list,OS_32,tmpref,reg.reghi);
{$ifdef newra}
        if got_scratch then
          rg.ungetregisterint(list,tmpreg);
{$else}
        if got_scratch then
          cg.free_scratch_reg(list,tmpreg);
{$endif}
      end;


    procedure tcg64f32.a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64{$ifdef newra};delete:boolean{$endif});

      begin
      {$ifdef newra}
        if delete then
          rg.ungetregisterint(list,regsrc.reglo);
      {$endif}
        cg.a_load_reg_reg(list,OS_32,OS_32,regsrc.reglo,regdst.reglo);
      {$ifdef newra}
        if delete then
          rg.ungetregisterint(list,regsrc.reghi);
      {$endif}
        cg.a_load_reg_reg(list,OS_32,OS_32,regsrc.reghi,regdst.reghi);
      end;

    procedure tcg64f32.a_load64_const_reg(list : taasmoutput;value : qword;reg : tregister64);

      begin
        cg.a_load_const_reg(list,OS_32,lo(value),reg.reglo);
        cg.a_load_const_reg(list,OS_32,hi(value),reg.reghi);
      end;

    procedure tcg64f32.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64{$ifdef newra};delete :boolean{$endif});

      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_load64_ref_reg(list,l.reference,reg{$ifdef newra},delete{$endif});
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_reg_reg(list,l.register64,reg{$ifdef newra},delete{$endif});
          LOC_CONSTANT :
            a_load64_const_reg(list,l.valueqword,reg);
          else
            internalerror(200112292);
        end;
      end;


    procedure tcg64f32.a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);
      begin
        case l.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_reg_ref(list,l.reg64,ref);
          LOC_CONSTANT :
            a_load64_const_ref(list,l.valueqword,ref);
          else
            internalerror(200203288);
        end;
      end;


    procedure tcg64f32.a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);

      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_load64_const_ref(list,value,l.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_const_reg(list,value,l.reg64);
          else
            internalerror(200112293);
        end;
      end;


    procedure tcg64f32.a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);

      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_load64_reg_ref(list,reg,l.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_reg_reg(list,reg,l.register64{$ifdef newra},false{$endif});
          else
            internalerror(200112293);
        end;
      end;



    procedure tcg64f32.a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          cg.a_load_reg_ref(list,OS_32,reg,ref)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            cg.a_load_reg_ref(list,OS_32,reg,tmpref)
          end;
      end;

    procedure tcg64f32.a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_little then
          cg.a_load_reg_ref(list,OS_32,reg,ref)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            cg.a_load_reg_ref(list,OS_32,reg,tmpref)
          end;
      end;

    procedure tcg64f32.a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          cg.a_load_ref_reg(list,OS_32,ref,reg)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            cg.a_load_ref_reg(list,OS_32,tmpref,reg)
          end;
      end;

    procedure tcg64f32.a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_little then
          cg.a_load_ref_reg(list,OS_32,ref,reg)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            cg.a_load_ref_reg(list,OS_32,tmpref,reg)
          end;
      end;

    procedure tcg64f32.a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
        case l.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE :
            a_load64low_ref_reg(list,l.reference,reg);
          LOC_REGISTER :
            cg.a_load_reg_reg(list,OS_32,OS_32,l.registerlow,reg);
          LOC_CONSTANT :
            cg.a_load_const_reg(list,OS_32,lo(l.valueqword),reg);
          else
            internalerror(200203244);
        end;
      end;

    procedure tcg64f32.a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
        case l.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE :
            a_load64high_ref_reg(list,l.reference,reg);
          LOC_REGISTER :
            cg.a_load_reg_reg(list,OS_32,OS_32,l.registerhigh,reg);
          LOC_CONSTANT :
            cg.a_load_const_reg(list,OS_32,hi(l.valueqword),reg);
          else
            internalerror(200203244);
        end;
      end;


    procedure tcg64f32.a_op64_const_loc(list : taasmoutput;op:TOpCG;value : qword;const l: tlocation);
      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op64_const_ref(list,op,value,l.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_op64_const_reg(list,op,value,l.register64);
          else
            internalerror(200203292);
        end;
      end;


    procedure tcg64f32.a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);
      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op64_reg_ref(list,op,reg,l.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_op64_reg_reg(list,op,reg,l.register64);
          else
            internalerror(2002032422);
        end;
      end;



    procedure tcg64f32.a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);
      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op64_ref_reg(list,op,l.reference,reg);
          LOC_REGISTER,LOC_CREGISTER:
            a_op64_reg_reg(list,op,l.register64,reg);
          LOC_CONSTANT :
            a_op64_const_reg(list,op,l.valueqword,reg);
          else
            internalerror(200203242);
        end;
      end;


    procedure tcg64f32.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);
      var
        tempreg: tregister64;
      begin
      {$ifdef newra}
        tempreg.reghi:=rg.getregisterint(list,OS_INT);
        tempreg.reglo:=rg.getregisterint(list,OS_INT);
      {$else}
        tempreg.reghi := cg.get_scratch_reg_int(list,OS_INT);
        tempreg.reglo := cg.get_scratch_reg_int(list,OS_INT);
      {$endif}
        a_load64_ref_reg(list,ref,tempreg{$ifdef newra},false{$endif});
        a_op64_reg_reg(list,op,tempreg,reg);
      {$ifdef newra}
        rg.ungetregisterint(list,tempreg.reglo);
        rg.ungetregisterint(list,tempreg.reghi);
      {$else}
        cg.free_scratch_reg(list,tempreg.reglo);
        cg.free_scratch_reg(list,tempreg.reghi);
      {$endif}
      end;


    procedure tcg64f32.a_op64_reg_ref(list : taasmoutput;op:TOpCG;reg : tregister64; const ref: treference);
      var
        tempreg: tregister64;
      begin
      {$ifdef newra}
        tempreg.reghi:=rg.getregisterint(list,OS_INT);
        tempreg.reglo:=rg.getregisterint(list,OS_INT);
      {$else}
        tempreg.reghi := cg.get_scratch_reg_int(list,OS_INT);
        tempreg.reglo := cg.get_scratch_reg_int(list,OS_INT);
      {$endif}
        a_load64_ref_reg(list,ref,tempreg{$ifdef newra},false{$endif});
        a_op64_reg_reg(list,op,reg,tempreg);
        a_load64_reg_ref(list,tempreg,ref);
      {$ifdef newra}
        rg.ungetregisterint(list,tempreg.reglo);
        rg.ungetregisterint(list,tempreg.reghi);
      {$else}
        cg.free_scratch_reg(list,tempreg.reglo);
        cg.free_scratch_reg(list,tempreg.reghi);
      {$endif}
      end;


    procedure tcg64f32.a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);
      var
        tempreg: tregister64;
      begin
      {$ifdef newra}
        tempreg.reghi:=rg.getregisterint(list,OS_INT);
        tempreg.reglo:=rg.getregisterint(list,OS_INT);
      {$else}
        tempreg.reghi := cg.get_scratch_reg_int(list,OS_INT);
        tempreg.reglo := cg.get_scratch_reg_int(list,OS_INT);
      {$endif}
        a_load64_ref_reg(list,ref,tempreg{$ifdef newra},false{$endif});
        a_op64_const_reg(list,op,value,tempreg);
        a_load64_reg_ref(list,tempreg,ref);
      {$ifdef newra}
        rg.ungetregisterint(list,tempreg.reglo);
        rg.ungetregisterint(list,tempreg.reghi);
      {$else}
        cg.free_scratch_reg(list,tempreg.reglo);
        cg.free_scratch_reg(list,tempreg.reghi);
      {$endif}
      end;

    procedure tcg64f32.a_param64_reg(list : taasmoutput;reg : tregister64;const locpara : tparalocation);
      var
        tmplochi,tmploclo: tparalocation;
      begin
        tmplochi:=locpara;
        tmploclo:=locpara;
        if locpara.size=OS_S64 then
          tmplochi.size:=OS_S32
        else
          tmplochi.size:=OS_32;
        tmploclo.size:=OS_32;
        case locpara.loc of
           LOC_REGISTER:
             tmplochi.register:=tmplochi.registerhigh;
           { !!! i386 doesn't pass proper locations here
            so always take a loc_reference, since that's what it uses (JM)
           LOC_REFERENCE:
           }
           else
             if target_info.endian=endian_big then
               inc(tmploclo.reference.offset,4)
             else
               inc(tmplochi.reference.offset,4);
           {
           else
             internalerror(2003042702);
           }
        end;

         cg.a_param_reg(list,OS_32,reg.reghi,tmplochi);
         cg.a_param_reg(list,OS_32,reg.reglo,tmploclo);
      end;


    procedure tcg64f32.a_param64_const(list : taasmoutput;value : qword;const locpara : tparalocation);
      var
        tmplochi,tmploclo: tparalocation;
      begin
        tmplochi:=locpara;
        tmploclo:=locpara;
        if locpara.size=OS_S64 then
          tmplochi.size:=OS_S32
        else
          tmplochi.size:=OS_32;
        tmploclo.size:=OS_32;
        case locpara.loc of
           LOC_REGISTER:
             tmplochi.register:=tmplochi.registerhigh;
           { !!! i386 doesn't pass proper locations here
            so always take a loc_reference, since that's what it uses (JM)
           LOC_REFERENCE:
           }
           else
             if target_info.endian=endian_big then
               inc(tmploclo.reference.offset,4)
             else
               inc(tmplochi.reference.offset,4);
           {
           else
             internalerror(2003042702);
           }
        end;
        cg.a_param_const(list,OS_32,hi(value),tmplochi);
        cg.a_param_const(list,OS_32,lo(value),tmploclo);
      end;


    procedure tcg64f32.a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
        tmprefhi,tmpreflo : treference;
        tmploclo,tmplochi : tparalocation;
      begin
        tmprefhi:=r;
        tmpreflo:=r;
        tmplochi:=locpara;
        tmploclo:=locpara;
        if locpara.size=OS_S64 then
          tmplochi.size:=OS_S32
        else
          tmplochi.size:=OS_32;
        tmploclo.size:=OS_32;
        case locpara.loc of
           LOC_REGISTER:
             begin
                if target_info.endian=endian_big then
                  inc(tmpreflo.offset,4)
                else
                  inc(tmprefhi.offset,4);
                tmplochi.register:=tmplochi.registerhigh;
             end;
           { !!! i386 doesn't pass proper locations here
            so always take a loc_reference, since that's what it uses (JM)
           LOC_REFERENCE:
           }
           else
             begin
                if target_info.endian=endian_big then
                  begin
                    inc(tmpreflo.offset,4);
                    inc(tmploclo.reference.offset,4);
                  end
                else
                  begin
                    inc(tmprefhi.offset,4);
                    inc(tmplochi.reference.offset,4);
                  end;
             end
           {
           else
             internalerror(2003042701);
           }
        end;
        cg.a_param_ref(list,OS_32,tmprefhi,tmplochi);
        cg.a_param_ref(list,OS_32,tmpreflo,tmploclo);
      end;


    procedure tcg64f32.a_param64_loc(list : taasmoutput;const l:tlocation;const locpara : tparalocation);
      begin
        case l.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            a_param64_reg(list,l.register64,locpara);
          LOC_CONSTANT :
            a_param64_const(list,l.valueqword,locpara);
          LOC_CREFERENCE,
          LOC_REFERENCE :
            a_param64_ref(list,l.reference,locpara);
        else
          internalerror(200203287);
        end;
      end;


    procedure tcg64f32.g_rangecheck64(list : taasmoutput;const p : tnode;const todef : tdef);

      var
        neglabel,
        poslabel,
        endlabel: tasmlabel;
        hreg   : tregister;
        hdef   :  torddef;
        fromdef : tdef;
        opsize   : tcgsize;
        oldregisterdef: boolean;
        from_signed,to_signed: boolean;
        got_scratch: boolean;

      begin
         fromdef:=p.resulttype.def;
         from_signed := is_signed(fromdef);
         to_signed := is_signed(todef);

         if not is_64bit(todef) then
           begin
             oldregisterdef := registerdef;
             registerdef := false;

             { get the high dword in a register }
             if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
               begin
                 hreg := p.location.registerhigh;
                 got_scratch := false
               end
             else
               begin
               {$ifdef newra}
                 hreg:=rg.getregisterint(list,OS_INT);
               {$else}
                 hreg := cg.get_scratch_reg_int(list,OS_INT);
               {$endif}
                 got_scratch := true;
                 a_load64high_ref_reg(list,p.location.reference,hreg);
               end;
             objectlibrary.getlabel(poslabel);

             { check high dword, must be 0 (for positive numbers) }
             cg.a_cmp_const_reg_label(list,OS_32,OC_EQ,0,hreg,poslabel);

             { It can also be $ffffffff, but only for negative numbers }
             if from_signed and to_signed then
               begin
                 objectlibrary.getlabel(neglabel);
                 cg.a_cmp_const_reg_label(list,OS_32,OC_EQ,aword(-1),hreg,neglabel);
               end;
             { !!! freeing of register should happen directly after compare! (JM) }
           {$ifdef newra}
             if got_scratch then
               rg.ungetregisterint(list,hreg);
           {$else}
             if got_scratch then
               cg.free_scratch_reg(list,hreg);
           {$endif}
             { For all other values we have a range check error }
             cg.a_call_name(list,'FPC_RANGEERROR');

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             cg.a_label(list,poslabel);
             hdef:=torddef.create(u32bit,0,cardinal($ffffffff));
             { the real p.resulttype.def is already saved in fromdef }
             p.resulttype.def := hdef;
             { no use in calling just "g_rangecheck" since that one will }
             { simply call the inherited method too (JM)                 }
             cg.g_rangecheck(list,p,todef);
             hdef.free;
             { restore original resulttype.def }
             p.resulttype.def := fromdef;

             if from_signed and to_signed then
               begin
                 objectlibrary.getlabel(endlabel);
                 cg.a_jmp_always(list,endlabel);
                 { if the high dword = $ffffffff, then the low dword (when }
                 { considered as a longint) must be < 0                    }
                 cg.a_label(list,neglabel);
                 if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                   begin
                     hreg := p.location.registerlow;
                     got_scratch := false
                   end
                 else
                   begin
                   {$ifdef newra}
                     hreg:=rg.getregisterint(list,OS_INT);
                   {$else}
                     hreg := cg.get_scratch_reg_int(list,OS_INT);
                   {$endif}
                     got_scratch := true;
                     a_load64low_ref_reg(list,p.location.reference,hreg);
                   end;
                 { get a new neglabel (JM) }
                 objectlibrary.getlabel(neglabel);
                 cg.a_cmp_const_reg_label(list,OS_32,OC_LT,0,hreg,neglabel);
                 { !!! freeing of register should happen directly after compare! (JM) }
               {$ifdef newra}
                 if got_scratch then
                   rg.ungetregisterint(list,hreg);
               {$else}
                 if got_scratch then
                   cg.free_scratch_reg(list,hreg);
               {$endif}

                 cg.a_call_name(list,'FPC_RANGEERROR');

                 { if we get here, the 64bit value lies between }
                 { longint($80000000) and -1 (JM)               }
                 cg.a_label(list,neglabel);
                 hdef:=torddef.create(s32bit,longint($80000000),-1);
                 p.resulttype.def := hdef;
                 cg.g_rangecheck(list,p,todef);
                 hdef.free;
                 cg.a_label(list,endlabel);
               end;
             registerdef := oldregisterdef;
             p.resulttype.def := fromdef;
             { restore p's resulttype.def }
           end
         else
           { todef = 64bit int }
           { no 64bit subranges supported, so only a small check is necessary }

           { if both are signed or both are unsigned, no problem! }
           if (from_signed xor to_signed) and
              { also not if the fromdef is unsigned and < 64bit, since that will }
              { always fit in a 64bit int (todef is 64bit)                       }
              (from_signed or
               (torddef(fromdef).typ = u64bit)) then
             begin
               { in all cases, there is only a problem if the higest bit is set }
               if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                 begin
                   if is_64bit(fromdef) then
                     begin
                       hreg := p.location.registerhigh;
                       opsize := OS_32;
                     end
                   else
                     begin
                       hreg := p.location.register;
                       opsize := def_cgsize(p.resulttype.def);
                     end;
                   got_scratch := false;
                 end
               else
                 begin
                 {$ifdef newra}
                   hreg:=rg.getregisterint(list,OS_INT);
                 {$else}
                   hreg := cg.get_scratch_reg_int(list,OS_INT);
                 {$endif}
                   got_scratch := true;

                   opsize := def_cgsize(p.resulttype.def);
                   if opsize in [OS_64,OS_S64] then
                     a_load64high_ref_reg(list,p.location.reference,hreg)
                   else
                     cg.a_load_ref_reg(list,opsize,p.location.reference,hreg);
                 end;
               objectlibrary.getlabel(poslabel);
               cg.a_cmp_const_reg_label(list,opsize,OC_GTE,0,hreg,poslabel);

               { !!! freeing of register should happen directly after compare! (JM) }
             {$ifdef newra}
               if got_scratch then
                 rg.ungetregisterint(list,hreg);
             {$else}
               if got_scratch then
                 cg.free_scratch_reg(list,hreg);
             {$endif}
               cg.a_call_name(list,'FPC_RANGEERROR');
               cg.a_label(list,poslabel);
             end;
      end;

    function tcg64f32.optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : qword; var reg: tregister64): boolean;
      var
        lowvalue, highvalue : cardinal;
        hreg: tregister;
      begin
        lowvalue := cardinal(a);
        highvalue:= a shr 32;
        { assume it will be optimized out }
        optimize64_op_const_reg := true;
        case op of
        OP_ADD:
           begin
             if a = 0 then
                exit;
           end;
        OP_AND:
           begin
              if lowvalue <> high(cardinal) then
                cg.a_op_const_reg(list,op,OS_32,lowvalue,reg.reglo);
              if highvalue <> high(cardinal) then
                cg.a_op_const_reg(list,op,OS_32,highvalue,reg.reghi);
              { already emitted correctly }
              exit;
           end;
        OP_OR:
           begin
              if lowvalue <> 0 then
                cg.a_op_const_reg(list,op,OS_32,lowvalue,reg.reglo);
              if highvalue <> 0 then
                cg.a_op_const_reg(list,op,OS_32,highvalue,reg.reghi);
              { already emitted correctly }
              exit;
           end;
        OP_SUB:
           begin
             if a = 0 then
                exit;
           end;
        OP_XOR:
           begin
           end;
        OP_SHL:
           begin
             if a = 0 then
                 exit;
             { simply clear low-register
               and shift the rest and swap
               registers.
             }
             if (a > 31) then
               begin
                 cg.a_load_const_reg(list,OS_32,0,reg.reglo);
                 cg.a_op_const_reg(list,OP_SHL,OS_32,a mod 32,reg.reghi);
                 { swap the registers }
                 hreg := reg.reghi;
                 reg.reghi := reg.reglo;
                 reg.reglo := hreg;
                 exit;
               end;
           end;
        OP_SHR:
           begin
             if a = 0 then exit;
             { simply clear high-register
               and shift the rest and swap
               registers.
             }
             if (a > 31) then
               begin
                 cg.a_load_const_reg(list,OS_32,0,reg.reghi);
                 cg.a_op_const_reg(list,OP_SHL,OS_32,a mod 32,reg.reglo);
                 { swap the registers }
                 hreg := reg.reghi;
                 reg.reghi := reg.reglo;
                 reg.reglo := hreg;
                 exit;
               end;
           end;
        OP_IMUL,OP_MUL:
           begin
             if a = 1 then exit;
           end;
        OP_IDIV,OP_DIV:
            begin
             if a = 1 then exit;
            end;
        else
           internalerror(20020817);
        end;
        optimize64_op_const_reg := false;
      end;

(*
    procedure int64f32_assignment_int64_reg(p : passignmentnode);

      begin
      end;


begin
   p2_assignment:=@int64f32_assignement_int64;
*)
end.
{
  $Log$
  Revision 1.46  2003-06-03 13:01:59  daniel
    * Register allocator finished

  Revision 1.45  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.44  2003/05/14 19:31:37  jonas
    * fixed a_param64_reg

  Revision 1.43  2003/04/27 14:48:09  jonas
    * fixed Florian's quick hack :)
    * fixed small bug 64bit range checking code

  Revision 1.42  2003/04/27 09:10:49  florian
    * quick fix for param64 for intel

  Revision 1.41  2003/04/27 08:23:51  florian
    * fixed parameter passing for 64 bit ints

  Revision 1.40  2003/04/23 20:16:03  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.39  2003/04/22 10:09:34  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.38  2003/04/07 08:52:58  jonas
    * fixed compiling error

  Revision 1.37  2003/04/07 08:45:09  jonas
    + generic a_op64_reg_ref implementation

  Revision 1.36  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.35  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.34  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.33  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.32  2002/11/25 17:43:16  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.31  2002/10/05 12:43:23  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.30  2002/09/17 18:54:01  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.29  2002/09/10 21:24:38  jonas
    * fixed a_param64_ref

  Revision 1.28  2002/09/07 15:25:00  peter
    * old logs removed and tabs fixed

  Revision 1.27  2002/08/19 18:17:47  carl
    + optimize64_op_const_reg implemented (optimizes 64-bit constant opcodes)
    * more fixes to m68k for 64-bit operations

  Revision 1.26  2002/08/17 22:09:43  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.25  2002/08/14 18:41:47  jonas
    - remove valuelow/valuehigh fields from tlocation, because they depend
      on the endianess of the host operating system -> difficult to get
      right. Use lo/hi(location.valueqword) instead (remember to use
      valueqword and not value!!)

  Revision 1.24  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.23  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.22  2002/07/28 15:57:15  jonas
    * fixed a_load64_const_reg() for big endian systems

  Revision 1.21  2002/07/20 11:57:52  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.20  2002/07/12 10:14:26  jonas
    * some big-endian fixes

  Revision 1.19  2002/07/11 07:23:17  jonas
    + generic implementations of a_op64_ref_reg() and a_op64_const_ref()
      (only works for processors with >2 scratch registers)

  Revision 1.18  2002/07/10 11:12:44  jonas
    * fixed a_op64_const_loc()

  Revision 1.17  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.16  2002/07/01 18:46:21  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.15  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.14  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.13  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.12  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.10  2002/05/12 16:53:04  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.9  2002/04/25 20:16:38  peter
    * moved more routines from cga/n386util

  Revision 1.8  2002/04/21 15:28:51  carl
  * a_jmp_cond -> a_jmp_always

  Revision 1.7  2002/04/07 13:21:18  carl
  + more documentation

}
