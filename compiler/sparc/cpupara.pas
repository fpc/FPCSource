{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Calling conventions for the SPARC

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
 *****************************************************************************}
unit cpupara;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      aasmtai,globtype,
      symconst,symbase,symtype,symdef,paramgr;

    type
      TSparcParaManager=class(TParaManager)
        function copy_value_on_stack(def : tdef;calloption : tproccalloption) : boolean;override;
        function push_addr_param(def : tdef;calloption : tproccalloption) : boolean;override;
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        function getintparaloc(list: taasmoutput; nr : longint) : tparalocation;override;
        procedure freeintparaloc(list: taasmoutput; nr : longint); override;
        {Creates location information related to the parameter of the function}
        procedure allocparaloc(list: taasmoutput; const loc: tparalocation);override;
        procedure freeparaloc(list: taasmoutput; const loc: tparalocation);override;
        procedure create_paraloc_info(p:TAbstractProcDef; side: tcallercallee);override;
        procedure splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);override;
      end;


implementation

    uses
      verbose,
      cpuinfo,cginfo,cgbase,
      defutil,rgobj;

    function tsparcparamanager.copy_value_on_stack(def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tsparcparamanager.push_addr_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        push_addr_param:=false;
        case def.deftype of
          recorddef,
          arraydef,
          variantdef,
          formaldef :
            push_addr_param:=true;
          objectdef :
            push_addr_param:=is_object(def);
          stringdef :
            push_addr_param:=(tstringdef(def).string_typ in [st_shortstring,st_longstring]);
          procvardef :
            push_addr_param:=(po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            push_addr_param:=(tsetdef(def).settype<>smallset);
        end;
      end;


    function TSparcParaManager.GetIntParaLoc(List:TAasmOutput;nr:longint):TParaLocation;
      begin
        if nr<1 then
          InternalError(2002100806);
        FillChar(GetIntParaLoc,SizeOf(TParaLocation),0);
        Dec(nr);
        with GetIntParaLoc do
         begin
           { The six first parameters are passed into registers }
           if nr<6 then
            begin
              loc:=LOC_REGISTER;
              register.enum:=R_INTREGISTER;
              register.number:=(RS_O0+nr) shl 8;
              rg.getexplicitregisterint(list,register.number);
            end
           else
           { The other parameters are passed on the stack }
            begin
              loc:=LOC_REFERENCE;
              reference.index.enum:=R_INTREGISTER;
              reference.index.number:=NR_STACK_POINTER_REG;
              reference.offset:=92+(nr-6)*4;
            end;
           size:=OS_INT;
         end;
      end;


    procedure tsparcparamanager.freeintparaloc(list: taasmoutput; nr : longint);

      var
        r: tregister;

      begin
        if nr<1 then
          internalerror(2003060401);
        Dec(nr);
        if nr<6 then
          begin
            r.enum:=R_INTREGISTER;
            r.number:=(RS_O0+nr) shl 8;
            rg.ungetregisterint(list,r);
          end;
      end;


    procedure tsparcparamanager.allocparaloc(list: taasmoutput; const loc: tparalocation);
      begin
        if (loc.loc=LOC_REFERENCE) and
           (loc.low_in_reg) then
          rg.getexplicitregisterint(list,loc.lowreg.number);
        inherited allocparaloc(list,loc);
      end;


    procedure tsparcparamanager.freeparaloc(list: taasmoutput; const loc: tparalocation);
      begin
        if (loc.loc=LOC_REFERENCE) and
           (loc.low_in_reg) then
          rg.ungetregisterint(list,loc.lowreg);
        inherited freeparaloc(list,loc);
      end;


    procedure TSparcParaManager.create_paraloc_info(p:TAbstractProcDef; side: tcallercallee);
      var
        nextintreg : tsuperregister;
        stack_offset : longint;
        hp : tparaitem;
        is_64bit : boolean;
        paraloc : tparalocation;
      begin
        nextintreg:=RS_O0;
        { Nested procedures have the parent framepoint in o0 }
        if p.parast.symtablelevel>normal_function_level then
          inc(NextIntReg);
        stack_offset:=92;
        hp:=TParaItem(p.para.First);
        while assigned(hp) do
          begin
            fillchar(paraloc,sizeof(tparalocation),0);
            if push_addr_param(hp.paratype.def,p.proccalloption) or (hp.paratyp in [vs_var,vs_out]) then
              paraloc.size:=OS_ADDR
            else
              begin
                paraloc.size:=def_cgSize(hp.paratype.def);
                if paraloc.size=OS_NO then
                  paraloc.size:=OS_ADDR;
              end;
            is_64bit:=(paraloc.size in [OS_64,OS_S64,OS_F64]);
            if NextIntReg<=RS_O5-ord(is_64bit) then
              begin
                paraloc.loc:=LOC_REGISTER;
                { big endian }
                if is_64bit then
                  begin
                    paraloc.registerhigh.enum:=R_INTREGISTER;
                    paraloc.registerhigh.number:=nextintreg shl 8;
                    inc(nextintreg);
                  end;
                paraloc.registerlow.enum:=R_INTREGISTER;
                paraloc.registerlow.number:=NextIntReg shl 8;
                inc(NextIntReg);
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                { Low part need to be in O5 if still available }
                if NextIntReg<=RS_O5 then
                  begin
                    paraloc.low_in_reg:=true;
                    paraloc.lowreg.enum:=R_INTREGISTER;
                    paraloc.lowreg.number:=nextintreg shl 8;
                  end;
                nextintreg:=RS_O6;
                paraloc.reference.index.enum:=R_INTREGISTER;
                paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                paraloc.reference.offset:=stack_offset;
                if is_64bit and
                   (not paraloc.low_in_reg) then
                  inc(stack_offset,8)
                else
                  inc(stack_offset,4);
              end;
            hp.paraloc[side]:=paraloc;
            if side = calleeside then
              begin
                { update callee paraloc and use Ix registers instead
                  of Ox registers }
                if hp.paraloc[calleeside].loc=LOC_REGISTER then
                  begin
                    { big endian }
                    if is_64bit then
                      inc(hp.paraloc[calleeside].registerhigh.number,(RS_I0-RS_O0) shl 8);
                    inc(hp.paraloc[calleeside].registerlow.number,(RS_I0-RS_O0) shl 8);
                  end
                else
                  begin
                    if hp.paraloc[calleeside].low_in_reg then
                      inc(hp.paraloc[calleeside].lowreg.number,(RS_I0-RS_O0) shl 8);
                    inc(hp.paraloc[calleeside].reference.index.number,(RS_I0-RS_O0) shl 8);
                  end;
              end;
            hp:=TParaItem(hp.Next);
          end;

        { Function return }
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            paraloc.loc:=LOC_FPUREGISTER;
            paraloc.register.enum:=FPU_RESULT_REG;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc.loc:=LOC_REGISTER;
{$ifndef cpu64bit}
            if paraloc.size in [OS_64,OS_S64] then
             begin
               paraloc.register64.reglo.enum:=R_INTREGISTER;
               if side=callerside then
                 paraloc.register64.reglo.number:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 paraloc.register64.reglo.number:=NR_FUNCTION_RETURN64_LOW_REG;
               paraloc.register64.reghi.enum:=R_INTREGISTER;
               if side=callerside then
                 paraloc.register64.reghi.number:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc.register64.reghi.number:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
{$endif cpu64bit}
             begin
               paraloc.register.enum:=R_INTREGISTER;
               if side=callerside then
                 paraloc.register.number:=NR_FUNCTION_RESULT_REG
               else
                 paraloc.register.number:=NR_FUNCTION_RETURN_REG;
             end;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    procedure tsparcparamanager.splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);
      begin
        { Word 0 is in register, word 1 is in reference }
        if (locpara.loc=LOC_REFERENCE) and locpara.low_in_reg then
          begin
            { high }
            lochipara:=locpara;
            if locpara.size=OS_S64 then
              lochipara.size:=OS_S32
            else
              lochipara.size:=OS_32;
            lochipara.low_in_reg:=false;
            { low }
            loclopara:=locpara;
            loclopara.size:=OS_32;
            loclopara.loc:=LOC_REGISTER;
            loclopara.register:=locpara.lowreg;
          end
        else
          inherited splitparaloc64(locpara,loclopara,lochipara);
      end;

begin
   ParaManager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.27  2003-08-11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.26  2003/07/08 21:25:00  peter
    * sparc fixes

  Revision 1.25  2003/07/06 22:10:56  peter
    * big endian first allocates high

  Revision 1.24  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.23  2003/07/05 20:11:41  jonas
    * create_paraloc_info() is now called separately for the caller and
      callee info
    * fixed ppc cycle

  Revision 1.22  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.21  2003/06/17 16:36:59  peter
    * freeintparaloc

  Revision 1.20  2003/06/09 21:44:14  mazen
  * fix compile problem related to modification
    of the declareation of GetIntParaLoc in the
    ancestor's declaration

  Revision 1.19  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.18  2003/05/31 01:00:51  peter
    * register fixes

  Revision 1.17  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.16  2003/04/23 13:35:39  peter
    * fix sparc compile

  Revision 1.15  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.14  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.13  2003/01/05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.12  2002/11/25 19:21:49  mazen
  * fixed support of nSparcInline

  Revision 1.11  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.10  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.9  2002/11/03 20:22:40  mazen
  * parameter handling updated

  Revision 1.8  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.7  2002/10/10 19:57:51  mazen
  * Just to update repsitory

  Revision 1.6  2002/10/10 15:10:39  mazen
  * Internal error fixed, but usually i386 parameter model used

  Revision 1.5  2002/10/09 13:52:19  mazen
  just incase some one wolud help me debugging that\!

  Revision 1.4  2002/10/08 21:02:22  mazen
  * debugging register allocation

  Revision 1.3  2002/10/07 20:33:05  mazen
  word alignement modified in g_stack_frame

  Revision 1.2  2002/10/04 21:57:42  mazen
  * register allocation for parameters now done in cpupara, but InternalError(200109223) in cgcpu.pas:1053 is still not fixed du to location_force problem in ncgutils.pas:419

  Revision 1.1  2002/08/21 13:30:07  mazen
  *** empty log message ***

  Revision 1.2  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled
}
