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
      aasmtai,
      symconst,symbase,symtype,symdef,paramgr;

    type
      TSparcParaManager=class(TParaManager)
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        function GetIntParaLoc(List:TAasmOutput;nr:longint):TParaLocation;override;
        {Creates location information related to the parameter of the function}
        procedure create_param_loc_info(p:TAbstractProcDef);override;
        {Returns the location where the invisible parameter for structured function
        results will be passed.}
        function GetFuncRetParaLoc(p:TAbstractProcDef):TParaLocation;override;
      end;


implementation

    uses
      verbose,
      cpuinfo,cginfo,cgbase,
      defutil;

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
              register.number:=(RS_I0+nr) shl 8;
            end
           else
           { The other parameters are passed into the frame }
            begin
              loc:=LOC_REFERENCE;
              reference.index.enum:=R_INTREGISTER;
              reference.index.number:=NR_FRAME_POINTER_REG;
              reference.offset:=-68-nr*4;
            end;
           size:=OS_INT;
         end;
      end;


    function GetParaLoc(p:TDef):TCGLoc;
      begin
        { Later, the LOC_REFERENCE is in most cases changed into
          LOC_REGISTER if push_addr_param for the def is true}
        case p.DefType of
          OrdDef:
            GetParaLoc:=LOC_REGISTER;
          FloatDef:
            GetParaLoc:=LOC_FPUREGISTER;
          enumdef:
            getparaloc:=LOC_REGISTER;
          pointerdef:
            getparaloc:=LOC_REGISTER;
          formaldef:
            getparaloc:=LOC_REGISTER;
          classrefdef:
            getparaloc:=LOC_REGISTER;
          recorddef:
            getparaloc:=LOC_REFERENCE;
          objectdef:
            if is_object(p) then
              getparaloc:=LOC_REFERENCE
            else
              getparaloc:=LOC_REGISTER;
          stringdef:
            if is_shortstring(p) or is_longstring(p) then
              getparaloc:=LOC_REFERENCE
            else
              getparaloc:=LOC_REGISTER;
          procvardef:
            if (po_methodpointer in tprocvardef(p).procoptions) then
              getparaloc:=LOC_REFERENCE
            else
              getparaloc:=LOC_REGISTER;
          filedef:
            getparaloc:=LOC_REGISTER;
          arraydef:
            getparaloc:=LOC_REFERENCE;
          setdef:
            if is_smallset(p) then
              getparaloc:=LOC_REGISTER
            else
              getparaloc:=LOC_REFERENCE;
          variantdef:
            getparaloc:=LOC_REFERENCE;
          { avoid problems with errornous definitions }
          errordef:
            getparaloc:=LOC_REGISTER;
          else
            internalerror(2002071001);
        end;
      end;


    procedure TSparcParaManager.create_param_loc_info(p:TAbstractProcDef);
      var
        nextintreg : tsuperregister;
        nextfloatreg : toldregister;
        stack_offset : longint;
        hp : tparaitem;
        loc : tcgloc;
        is_64bit : boolean;
      begin
        nextintreg:=RS_O0;
        nextfloatreg:=R_F0;
        stack_offset:=92;
        hp:=TParaItem(p.para.First);
        while assigned(hp) do
          begin
            loc:=GetParaLoc(hp.paratype.def);
            case loc of
              LOC_REGISTER:
                begin
                  hp.paraloc.size:=def_cgSize(hp.paratype.def);
                  if hp.paraloc.size=OS_NO then
                    hp.paraloc.size:=OS_ADDR;
                  is_64bit:=(hp.paraloc.size in [OS_64,OS_S64]);
                  if NextIntReg<=RS_I5-ord(is_64bit) then
                    begin
                      hp.paraloc.loc:=LOC_REGISTER;
                      hp.paraloc.registerlow.enum:=R_INTREGISTER;
                      hp.paraloc.registerlow.number:=NextIntReg shl 8;
                      inc(NextIntReg);
                      if is_64bit then
                        begin
                          hp.paraloc.registerhigh.enum:=R_INTREGISTER;
                          hp.paraloc.registerhigh.number:=nextintreg shl 8;
                          inc(nextintreg);
                        end;
                    end
                  else
                    begin
                      nextintreg:=RS_I6;
                      hp.paraloc.loc:=LOC_REFERENCE;
                      hp.paraloc.reference.index.enum:=R_INTREGISTER;
                      hp.paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                      hp.paraloc.reference.offset:=stack_offset;
                      if not is_64bit then
                        inc(stack_offset,4)
                      else
                        inc(stack_offset,8);
                    end;
                end;

              LOC_FPUREGISTER:
                begin
                  if hp.paratyp in [vs_var,vs_out] then
                    begin
                      if NextIntReg<=RS_O5 then
                        begin
                          hp.paraloc.size:=OS_ADDR;
                          hp.paraloc.loc:=LOC_REGISTER;
                          hp.paraloc.register.enum:=R_INTREGISTER;
                          hp.paraloc.register.number:=nextintreg shl 8;
                          inc(nextintreg);
                        end
                      else
                        begin
                          {!!!!!!!}
                          hp.paraloc.size:=def_cgsize(hp.paratype.def);
                          internalerror(2002071006);
                        end;
                    end
                  else if nextfloatreg<=R_F10 then
                    begin
                      hp.paraloc.size:=def_cgsize(hp.paratype.def);
                      hp.paraloc.loc:=LOC_FPUREGISTER;
                      { Doubles use 2 FPU regs, align on even register }
                      if (hp.paraloc.size<>OS_F32) and
                         odd(ord(nextfloatreg)-ord(R_F0)) then
                        inc(nextfloatreg);
                      hp.paraloc.register.enum:=nextfloatreg;
                      inc(nextfloatreg);
                      { Doubles use 2 FPU regs }
                      if hp.paraloc.size<>OS_F32 then
                        inc(nextfloatreg);
                    end
                  else
                    begin
                      {!!!!!!!}
                      hp.paraloc.size:=def_cgsize(hp.paratype.def);
                      internalerror(2002071004);
                    end;
                end;

              LOC_REFERENCE:
                begin
                   hp.paraloc.size:=OS_ADDR;
                   if push_addr_param(hp.paratype.def,p.proccalloption) or (hp.paratyp in [vs_var,vs_out]) then
                     begin
                        if nextintreg<=RS_O5 then
                          begin
                             hp.paraloc.loc:=LOC_REGISTER;
                             hp.paraloc.register.enum:=R_INTREGISTER;
                             hp.paraloc.register.number:=nextintreg shl 8;
                             inc(nextintreg);
                          end
                        else
                           begin
                              hp.paraloc.loc:=LOC_REFERENCE;
                              hp.paraloc.reference.index.enum:=R_INTREGISTER;
                              hp.paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                              hp.paraloc.reference.offset:=stack_offset;
                              inc(stack_offset,4);
                          end;
                     end
                   else
                     begin
                        hp.paraloc.loc:=LOC_REFERENCE;
                        hp.paraloc.reference.index.enum:=R_INTREGISTER;
                        hp.paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                        hp.paraloc.reference.offset:=stack_offset;
                        inc(stack_offset,hp.paratype.def.size);
                     end;
                end;

              else
                internalerror(2002071002);
            end;
            hp:=TParaItem(hp.Next);
          end;
      end;


    function tSparcParaManager.GetFuncRetParaLoc(p:TAbstractProcDef):TParaLocation;
      begin
        with GetFuncRetParaLoc do
         begin
           case p.rettype.def.deftype of
             orddef,enumdef:
               begin
                 loc:=LOC_REGISTER;
                 register.enum:=R_INTREGISTER;
                 register.number:=NR_FUNCTION_RETURN_REG;
                 size:=def_cgsize(p.rettype.def);
                 if size in [OS_S64,OS_64] then
                   internalerror(200305309);
               end;
             floatdef:
               begin
                 loc:=LOC_FPUREGISTER;
                 register.enum:=R_F1;
                 size:=def_cgsize(p.rettype.def);
               end;
             setdef,
             variantdef,
             pointerdef,
             formaldef,
             classrefdef,
             recorddef,
             objectdef,
             stringdef,
             procvardef,
             filedef,
             arraydef,
             errordef:
               begin
                 loc:=LOC_REFERENCE;
                 reference.index.enum:=R_INTREGISTER;
                 reference.index.number:=NR_FRAME_POINTER_REG;
                 reference.offset:=64;
                 size:=OS_ADDR;
               end;
             else
               internalerror(2002090903);
           end;
         end;
      end;

begin
   ParaManager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.20  2003-06-09 21:44:14  mazen
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
