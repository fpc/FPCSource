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
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,cgbase,
      symconst,symbase,symtype,symdef,
      aasmtai,
      parabase,paramgr;

    type
       tx86_64paramanager = class(tparamanager)
       private
          procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
          procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                               var intparareg,mmparareg,parasize:longint);
       public
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;override;
       end;

  implementation

    uses
       cutils,verbose,
       systems,
       defutil;

    const
      paraintsupregs : array[0..5] of tsuperregister = (RS_RDI,RS_RSI,RS_RDX,RS_RCX,RS_R8,RS_R9);
      parammsupregs : array[0..7] of tsuperregister = (RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7);

    procedure getvalueparaloc(p : tdef;var loc1,loc2:tcgloc);
      begin
        loc1:=LOC_INVALID;
        loc2:=LOC_INVALID;
        case p.deftype of
           orddef:
             begin
               loc1:=LOC_REGISTER;
               {$warning TODO 128bit also needs lochigh}
             end;
           floatdef:
             begin
               case tfloatdef(p).typ of
                  s80real:
                    loc1:=LOC_REFERENCE;
                  s32real,
                  s64real :
                    loc1:=LOC_MMREGISTER;
                  s64currency,
                  s64comp :
                    loc1:=LOC_REGISTER;
                  s128real:
                    begin
                      loc1:=LOC_MMREGISTER;
                      loc2:=LOC_MMREGISTER;
                      {$warning TODO float 128bit needs SSEUP lochigh}
                    end;
               end;
             end;
           recorddef:
             begin
               if p.size<=16 then
                 begin
                   {$warning TODO location depends on the fields}
                   loc1:=LOC_REFERENCE;
                 end
               else
                 loc1:=LOC_REFERENCE;
             end;
           objectdef:
             begin
               if is_object(p) then
                 loc1:=LOC_REFERENCE
               else
                 loc1:=LOC_REGISTER;
             end;
           arraydef:
             loc1:=LOC_REFERENCE;
           variantdef:
             loc1:=LOC_REFERENCE;
           stringdef:
             if is_shortstring(p) or is_longstring(p) then
               loc1:=LOC_REFERENCE
             else
               loc1:=LOC_REGISTER;
           setdef:
             if is_smallset(p) then
               loc1:=LOC_REGISTER
             else
               loc1:=LOC_REFERENCE;
           procvardef:
             begin
               { This is a record < 16 bytes }
               if (po_methodpointer in tprocvardef(p).procoptions) then
                 begin
                   loc1:=LOC_REGISTER;
                   loc2:=LOC_REGISTER;
                 end
               else
                 loc1:=LOC_REGISTER;
             end;
           else
             begin
               { default for pointers,enums,etc }
               loc1:=LOC_REGISTER;
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


    procedure tx86_64paramanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=OS_INT;
        cgpara.alignment:=get_para_align(calloption);
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=OS_INT;
           if nr<1 then
             internalerror(200304303)
           else if nr<=high(paraintsupregs)+1 then
             begin
                loc:=LOC_REGISTER;
                register:=newreg(R_INTREGISTER,paraintsupregs[nr-1],R_SUBWHOLE);
             end
           else
             begin
                loc:=LOC_REFERENCE;
                reference.index:=NR_STACK_POINTER_REG;
                reference.offset:=(nr-6)*sizeof(aint);
             end;
          end;
      end;


    procedure tx86_64paramanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : pcgparalocation;
        retcgsize : tcgsize;
      begin
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          retcgsize:=OS_ADDR
        else
          retcgsize:=def_cgsize(p.rettype.def);
        p.funcret_paraloc[side].reset;
        p.funcret_paraloc[side].Alignment:=std_param_align;
        p.funcret_paraloc[side].size:=retcgsize;
        { void has no location }
        if is_void(p.rettype.def) then
          exit;
        paraloc:=p.funcret_paraloc[side].add_location;
        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            case tfloatdef(p.rettype.def).typ of
              s32real,s64real:
                begin
                  paraloc^.loc:=LOC_MMREGISTER;
                  paraloc^.register:=NR_MM_RESULT_REG;
                end;
              s64currency,
              s64comp,
              s80real:
                begin
                  paraloc^.loc:=LOC_FPUREGISTER;
                  paraloc^.register:=NR_FPU_RESULT_REG;
                end;
              else
                internalerror(200405034);
            end;
            paraloc^.size:=retcgsize;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc^.loc:=LOC_REGISTER;
            paraloc^.size:=retcgsize;
            if side=callerside then
              paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(retcgsize))
            else
              paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(retcgsize));
          end
        else
          begin
            paraloc^.loc:=LOC_REFERENCE;
            paraloc^.size:=retcgsize;
          end;
      end;


    procedure tx86_64paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                            var intparareg,mmparareg,parasize:longint);
      var
        hp : tparaitem;
        paraloc,
        paraloc2 : pcgparalocation;
        subreg : tsubregister;
        pushaddr : boolean;
        paracgsize : tcgsize;
        loc1,loc2 : tcgloc;
        l,
        varalign,
        paraalign : longint;
      begin
        paraalign:=get_para_align(p.proccalloption);
        { Register parameters are assigned from left to right }
        hp:=firstpara;
        while assigned(hp) do
          begin
            pushaddr:=push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption);
            if pushaddr then
              begin
                loc1:=LOC_REGISTER;
                loc2:=LOC_INVALID;
                paracgsize:=OS_ADDR;
              end
            else
              begin
                getvalueparaloc(hp.paratype.def,loc1,loc2);
                paracgsize:=def_cgsize(hp.paratype.def);
                if paracgsize=OS_C64 then
                  paracgsize:=OS_64;
              end;
            hp.paraloc[side].reset;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].Alignment:=paraalign;
            { First location }
            paraloc:=hp.paraloc[side].add_location;
            paraloc^.size:=paracgsize;
            if (loc1=LOC_REGISTER) and
               (intparareg<=high(paraintsupregs)) then
              begin
                if (paracgsize=OS_NO) or (loc2<>LOC_INVALID) then
                  begin
                    paraloc^.size:=OS_INT;
                    subreg:=R_SUBWHOLE;
                  end
                else
                  begin
                    paraloc^.size:=paracgsize;
                    subreg:=cgsize2subreg(paracgsize);
                  end;
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=newreg(R_INTREGISTER,paraintsupregs[intparareg],subreg);
                inc(intparareg);
              end
            else if (loc1=LOC_MMREGISTER) and
                    (mmparareg<=high(parammsupregs)) then
              begin
                paraloc^.loc:=LOC_MMREGISTER;
                paraloc^.register:=newreg(R_MMREGISTER,parammsupregs[mmparareg],R_SUBNONE);
                if paracgsize=OS_F128 then
                  paraloc^.size:=OS_F64
                else
                  paraloc^.size:=paracgsize;
                inc(mmparareg);
              end
            else
              begin
                paraloc^.loc:=LOC_REFERENCE;
                if side=callerside then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=size_2_align(l);
                paraloc^.reference.offset:=parasize;
                varalign:=used_align(varalign,paraalign,paraalign);
                parasize:=align(parasize+l,varalign);
              end;
            { Second location }
            if (loc2<>LOC_INVALID) then
              begin
                if (loc2=LOC_REGISTER) and
                   (intparareg<=high(paraintsupregs)) then
                  begin
                    paraloc2:=hp.paraloc[side].add_location;
                    paraloc2^.loc:=LOC_REGISTER;
                    paraloc2^.register:=newreg(R_INTREGISTER,paraintsupregs[intparareg],R_SUBWHOLE);
                    paraloc2^.size:=OS_INT;
                    inc(intparareg);
                  end
                else
                 if (loc2=LOC_MMREGISTER) and
                    (mmparareg<=high(parammsupregs)) then
                  begin
                    paraloc2:=hp.paraloc[side].add_location;
                    paraloc2^.loc:=LOC_REGISTER;
                    paraloc2^.register:=newreg(R_MMREGISTER,parammsupregs[mmparareg],R_SUBNONE);
                    if paracgsize=OS_F128 then
                      paraloc2^.size:=OS_F64
                    else
                      paraloc2^.size:=paracgsize;
                    inc(mmparareg);
                  end
                else
                  begin
                    { Release when location low has already registers
                      assigned }
                    if paraloc^.loc=LOC_REGISTER then
                      dec(intparareg);
                    if paraloc^.loc=LOC_MMREGISTER then
                      dec(mmparareg);
                    { Overwrite with LOC_REFERENCE }
                    paraloc^.loc:=LOC_REFERENCE;
                    if side=callerside then
                      paraloc^.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                    l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                    varalign:=size_2_align(l);
                    paraloc^.reference.offset:=parasize;
                    varalign:=used_align(varalign,paraalign,paraalign);
                    parasize:=align(parasize+l,varalign);
                  end;
              end;
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
                with hp.paraloc[side].location^ do
                  if (loc=LOC_REFERENCE) then
                    inc(reference.offset,target_info.first_parm_offset);
                hp:=tparaitem(hp.next);
              end;
          end;
      end;


    function tx86_64paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;
      var
        intparareg,mmparareg,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        parasize:=0;
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,tparaitem(p.para.first),intparareg,mmparareg,parasize);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,tparaitem(varargspara.first),intparareg,mmparareg,parasize);
        { store used no. of SSE registers, that needs to be passed in %AL }
        varargspara.mmregsused:=mmparareg;
        result:=parasize;
      end;


    function tx86_64paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        intparareg,mmparareg,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        parasize:=0;
        create_paraloc_info_intern(p,side,tparaitem(p.para.first),intparareg,mmparareg,parasize);
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
  Revision 1.10  2004-09-21 17:25:13  peter
    * paraloc branch merged

  Revision 1.9.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.9  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.8  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.7.2.7  2004/05/03 20:18:52  peter
    * fixes for tprintf

  Revision 1.7.2.6  2004/05/02 21:37:35  florian
    * setting of func. ret. for i386 fixed

  Revision 1.7.2.5  2004/05/02 20:56:55  florian
    * more fixes to handle_return_value update

  Revision 1.7.2.4  2004/05/02 19:08:01  florian
    * rewrote tcgcallnode.handle_return_value

}
