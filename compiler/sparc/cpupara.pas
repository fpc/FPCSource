{******************************************************************************
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC specific calling conventions

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
{SPARC specific calling conventions are handled by this unit}
{$INCLUDE fpcdefs.inc}
interface
uses
  cpubase,
  symconst,symbase,symtype,symdef,paramgr;
type
  TSparcParaManager=class(TParaManager)
    {Returns a structure giving the information on the storage of the parameter
    (which must be an integer parameter)
    @param(nr Parameter number of routine, starting from 1)}
    function GetIntParaLoc(nr:longint):TParaLocation;override;
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
function TSparcParaManager.GetIntParaLoc(nr:longint):TParaLocation;
  begin
    if nr<1
    then
      InternalError(2002100806);
    FillChar(GetIntParaLoc,SizeOf(TParaLocation),0);
    Dec(nr);
    with GetIntParaLoc do
      if nr<6
      then{The six first parameters are passed into registers}
        begin
          loc:=LOC_REGISTER;
          register:=TRegister(LongInt(R_i0)+nr);
        end
      else{The other parameters are passed into the frame}
        begin
          loc:=LOC_REFERENCE;
          reference.index.enum:=frame_pointer_reg;
          reference.offset:=-68-nr*4;
        end;
  end;
function GetParaLoc(p:TDef):TLoc;
  begin
{Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER if
push_addr_param for the def is true}
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
        if is_object(p)
        then
          getparaloc:=LOC_REFERENCE
        else
          getparaloc:=LOC_REGISTER;
      stringdef:
        if is_shortstring(p) or is_longstring(p)
        then
          getparaloc:=LOC_REFERENCE
        else
          getparaloc:=LOC_REGISTER;
      procvardef:
        if (po_methodpointer in tprocvardef(p).procoptions)
        then
          getparaloc:=LOC_REFERENCE
        else
          getparaloc:=LOC_REGISTER;
      filedef:
        getparaloc:=LOC_REGISTER;
      arraydef:
        getparaloc:=LOC_REFERENCE;
      setdef:
        if is_smallset(p)
        then
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
    nextintreg,nextfloatreg:tregister;
    stack_offset:aword;
    hp:tparaitem;
    loc:tloc;
    is_64bit:boolean;
  begin
    nextintreg.enum:=R_O0;
    nextfloatreg.enum:=R_F0;
    stack_offset:=92;
    hp:=TParaItem(p.para.First);
    while assigned(hp) do
      begin
        loc:=GetParaLoc(hp.paratype.def);
        case loc of
          LOC_REGISTER:
            begin
              hp.paraloc.size:=def_cgSize(hp.paratype.def);
              if hp.paraloc.size=OS_NO
              then
                hp.paraloc.size:=OS_ADDR;
              is_64bit:=hp.paraloc.size in [OS_64,OS_S64];
              if NextIntReg.enum<=ToldRegister(ord(R_i5)-ord(is_64bit))
              then
                begin
                  hp.paraloc.loc:=LOC_REGISTER;
                  hp.paraloc.registerlow:=NextIntReg;
                  inc(NextIntReg.enum);
                  if is_64bit
                  then
                    begin
                      hp.paraloc.registerhigh:=nextintreg;
                      inc(nextintreg.enum);
                    end;
                end
              else
                begin
                  nextintreg.enum:=R_i6;
                  hp.paraloc.loc:=LOC_REFERENCE;
                  hp.paraloc.reference.index.enum:=stack_pointer_reg;
                  hp.paraloc.reference.offset:=stack_offset;
                  if not is_64bit
                  then
                    inc(stack_offset,4)
                  else
                    inc(stack_offset,8);
                end;
            end;
          LOC_FPUREGISTER:
            begin
              if hp.paratyp in [vs_var,vs_out]
              then
                begin
                  if NextIntReg.enum<=R_O5
                  then
                    begin
                      hp.paraloc.size:=OS_ADDR;
                      hp.paraloc.loc:=LOC_REGISTER;
                      hp.paraloc.register:=nextintreg;
                      inc(nextintreg.enum);
                    end
                  else
                    begin
                      {!!!!!!!}
                      hp.paraloc.size:=def_cgsize(hp.paratype.def);
                      internalerror(2002071006);
                    end;
                end
              else if nextfloatreg.enum<=R_F10 then
                begin
                  hp.paraloc.size:=def_cgsize(hp.paratype.def);
                  hp.paraloc.loc:=LOC_FPUREGISTER;
                  hp.paraloc.register:=nextfloatreg;
                  inc(nextfloatreg.enum);
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
                           if nextintreg.enum<=R_O5 then
                             begin
                                hp.paraloc.loc:=LOC_REGISTER;
                                hp.paraloc.register:=nextintreg;
                                inc(nextintreg.enum);
                             end
                           else
                              begin
                                 hp.paraloc.loc:=LOC_REFERENCE;
                                 hp.paraloc.reference.index.enum:=stack_pointer_reg;
                                 hp.paraloc.reference.offset:=stack_offset;
                                 inc(stack_offset,4);
                             end;
                        end
                      else
                        begin
                           hp.paraloc.loc:=LOC_REFERENCE;
                           hp.paraloc.reference.index.enum:=stack_pointer_reg;
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
      case p.rettype.def.deftype of
        orddef,enumdef:
          begin
            loc:=LOC_REGISTER;
            register.enum:=return_result_reg;
            size:=def_cgsize(p.rettype.def);
            if size in [OS_S64,OS_64]
            then
              RegisterHigh.enum:=R_I1;
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
            reference.index.enum:=frame_pointer_reg;
            reference.offset:=64;
            size:=OS_ADDR;
          end;
        else
          internalerror(2002090903);
      end;
  end;
begin
   ParaManager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.14  2003-01-08 18:43:58  daniel
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
