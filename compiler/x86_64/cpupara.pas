{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for x86-64 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
{ Generates the argument location information for x86-64 target.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,cgbase,
      symconst,symbase,symtype,symdef,
      paramgr;

    type
       tx86_64paramanager = class(tparamanager)
       private
          procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
       public
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
       end;

  implementation

    uses
       cutils,verbose,
       cpuinfo,systems,
       defutil;

    const
      paraintsupregs : array[0..5] of tsuperregister = (RS_RDI,RS_RSI,RS_RDX,RS_RCX,RS_R8,RS_R9);
      parammsupregs : array[0..7] of tsuperregister = (RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7);

    procedure getvalueparaloc(p : tdef;var loc_lo,loc_hi:tcgloc);
      begin
        loc_lo:=LOC_INVALID;
        loc_hi:=LOC_INVALID;
        case p.deftype of
           orddef:
             begin
               loc_lo:=LOC_REGISTER;
               {$warning TODO 128bit also needs lochigh}
             end;
           floatdef:
             begin
               case tfloatdef(p).typ of
                  s80real:
                    loc_lo:=LOC_REFERENCE;
                  s32real,
                  s64real,
                  s64comp,
                  s64currency :
                    loc_lo:=LOC_MMREGISTER;
                  s128real:
                    begin
                      loc_lo:=LOC_MMREGISTER;
                      loc_hi:=LOC_MMREGISTER;
                      {$warning TODO float 128bit needs SSEUP lochigh}
                    end;
               end;
             end;
           recorddef:
             begin
               if p.size<=16 then
                 begin
                   {$warning TODO location depends on the fields}
                   loc_lo:=LOC_REFERENCE;
                 end
               else
                 loc_lo:=LOC_REFERENCE;
             end;
           objectdef:
             begin
               if is_object(p) then
                 loc_lo:=LOC_REFERENCE
               else
                 loc_lo:=LOC_REGISTER;
             end;
           arraydef:
             loc_lo:=LOC_REFERENCE;
           variantdef:
             loc_lo:=LOC_REFERENCE;
           stringdef:
             if is_shortstring(p) or is_longstring(p) then
               loc_lo:=LOC_REFERENCE
             else
               loc_lo:=LOC_REGISTER;
           setdef:
             if is_smallset(p) then
               loc_lo:=LOC_REGISTER
             else
               loc_lo:=LOC_REFERENCE;
           procvardef:
             begin
               { This is a record < 16 bytes }
               if (po_methodpointer in tprocvardef(p).procoptions) then
                 begin
                   loc_lo:=LOC_REGISTER;
                   loc_hi:=LOC_REGISTER;
                 end
               else
                 loc_lo:=LOC_REGISTER;
             end;
           else
             begin
               { default for pointers,enums,etc }
               loc_lo:=LOC_REGISTER;
             end;
        end;
      end;


    function tx86_64paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[RS_RAX,RS_RCX,RS_RDX,RS_RSI,RS_RDI,RS_R8,RS_R9,RS_R10,RS_R11];
      end;


    function tx86_64paramanager.get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[RS_XMM0..RS_XMM15];
      end;


    function tx86_64paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[RS_ST0..RS_ST7];
      end;


    function tx86_64paramanager.getintparaloc(calloption : tproccalloption; nr : longint): tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         result.size:=OS_INT;
         if nr<1 then
           internalerror(200304303)
         else if nr<=high(paraintsupregs)+1 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=newreg(R_INTREGISTER,paraintsupregs[nr-1],R_SUBWHOLE);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=(nr-6)*8;
           end;
      end;


    procedure tx86_64paramanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : tparalocation;
      begin
        { Function return }
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
        { Return in FPU register? }
        if is_extended(p.rettype.def) then
          begin
            paraloc.loc:=LOC_FPUREGISTER;
            paraloc.register:=NR_FPU_RESULT_REG;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc.loc:=LOC_REGISTER;
            paraloc.register:=NR_FUNCTION_RETURN_REG;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    function tx86_64paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        hp : tparaitem;
        paraloc : tparalocation;
        subreg : tsubregister;
        pushaddr : boolean;
        l,intparareg,mmparareg,
        varalign,
        paraalign,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        parasize:=0;
        paraalign:=get_para_align(p.proccalloption);
        { Register parameters are assigned from left to right }
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            pushaddr:=push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption);
            if pushaddr then
              begin
                paraloc.size:=OS_ADDR;
                paraloc.loc:=LOC_REGISTER;
                paraloc.lochigh:=LOC_INVALID;
              end
            else
              begin
                paraloc.size:=def_cgsize(hp.paratype.def);
                getvalueparaloc(hp.paratype.def,paraloc.loc,paraloc.lochigh);
              end;
            paraloc.alignment:=paraalign;
            { Location low }
            if (paraloc.loc=LOC_REGISTER) and
               (intparareg<=high(paraintsupregs)) then
              begin
                if (paraloc.size=OS_NO) or (paraloc.lochigh<>LOC_INVALID) then
                  subreg:=R_SUBWHOLE
                else
                  subreg:=cgsize2subreg(paraloc.size);
                paraloc.register:=newreg(R_INTREGISTER,paraintsupregs[intparareg],subreg);
                inc(intparareg);
              end
            else
             if (paraloc.loc=LOC_MMREGISTER) and
                (mmparareg<=high(parammsupregs)) then
              begin
                paraloc.register:=newreg(R_MMREGISTER,parammsupregs[mmparareg],R_SUBNONE);
                inc(mmparareg);
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                paraloc.lochigh:=LOC_INVALID;
                if side=callerside then
                  paraloc.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=size_2_align(l);
                paraloc.reference.offset:=parasize;
                varalign:=used_align(varalign,paraalign,paraalign);
                parasize:=align(parasize+l,varalign);
              end;
            { Location High if required }
            if (paraloc.lochigh<>LOC_INVALID) then
              begin
                if (paraloc.lochigh=LOC_REGISTER) and
                   (intparareg<=high(paraintsupregs)) then
                  begin
                    paraloc.registerhigh:=newreg(R_INTREGISTER,paraintsupregs[intparareg],R_SUBWHOLE);
                    inc(intparareg);
                  end
                else
                 if (paraloc.lochigh=LOC_MMREGISTER) and
                    (mmparareg<=high(parammsupregs)) then
                  begin
                    paraloc.registerhigh:=newreg(R_MMREGISTER,parammsupregs[mmparareg],R_SUBNONE);
                    inc(mmparareg);
                  end
                else
                  begin
                    { Release when location low has already registers
                      assigned }
                    if paraloc.loc=LOC_REGISTER then
                      dec(intparareg);
                    if paraloc.loc=LOC_MMREGISTER then
                      dec(mmparareg);
                    { Overwrite with LOC_REFERENCE }
                    paraloc.loc:=LOC_REFERENCE;
                    paraloc.lochigh:=LOC_INVALID;
                    fillchar(paraloc.reference,sizeof(paraloc.reference),0);
                    if side=callerside then
                      paraloc.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc.reference.index:=NR_FRAME_POINTER_REG;
                    l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                    varalign:=size_2_align(l);
                    paraloc.reference.offset:=parasize;
                    varalign:=used_align(varalign,paraalign,paraalign);
                    parasize:=align(parasize+l,varalign);
                  end;
              end;
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { Register parameters are assigned from left-to-right, but the
          offsets on the stack are right-to-left. There is no need
          to reverse the offset, only adapt the calleeside with the
          start offset of the first param on the stack }
        if side=calleeside then
          begin
            hp:=tparaitem(p.para.first);
            while assigned(hp) do
              begin
                if (hp.paraloc[side].loc=LOC_REFERENCE) then
                  inc(hp.paraloc[side].reference.offset,target_info.first_parm_offset);
                hp:=tparaitem(hp.next);
              end;
          end;
        { Create Function result paraloc }
        create_funcret_paraloc_info(p,side);
        { We need to return the size allocated on the stack }
        result:=parasize;
      end;


begin
   paramanager:=tx86_64paramanager.create;
end.
{
  $Log$
  Revision 1.7  2004-02-04 22:01:13  peter
    * first try to get cpupara working for x86_64

  Revision 1.6  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.5  2003/12/24 00:10:03  florian
    - delete parameter in cg64 methods removed

  Revision 1.4  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.3  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.2  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code
}
