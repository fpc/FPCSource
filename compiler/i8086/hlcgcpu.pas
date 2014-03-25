{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgcpu;

{$i fpcdefs.inc}

interface

  uses
    globals,
    aasmdata,
    symtype,symdef,parabase,
    cgbase,cgutils,
    hlcgobj, hlcgx86;


  type
    thlcgcpu = class(thlcgx86)
     protected
      procedure gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint); override;
     public
      function getaddressregister(list:TAsmList;size:tdef):Tregister;override;

      procedure g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister); override;
      procedure g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation); override;

      procedure location_force_mem(list:TAsmList;var l:tlocation;size:tdef);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globtype,verbose,
    paramgr,
    cpubase,cpuinfo,tgobj,cgobj,cgcpu,
    defutil,
    symconst;

  { thlcgcpu }

  procedure thlcgcpu.gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint);
    var
      locsize : tcgsize;
      tmploc : tlocation;
      href   : treference;
      stacksize   : longint;
    begin
      if not(l.size in [OS_32,OS_S32,OS_64,OS_S64,OS_128,OS_S128]) then
        locsize:=l.size
      else
        locsize:=int_float_cgsize(tcgsize2size[l.size]);
      case l.loc of
        LOC_FPUREGISTER,
        LOC_CFPUREGISTER:
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    begin
                      cg.g_stackpointer_alloc(list,stacksize);
                      reference_reset_base(href,NR_STACK_POINTER_REG,0,sizeof(pint));
                    end
                  else
                    reference_reset_base(href,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
                  cg.a_loadfpu_reg_ref(list,locsize,locsize,l.register,href);
                end;
              LOC_FPUREGISTER:
                begin
                  cg.a_loadfpu_reg_reg(list,locsize,cgpara.location^.size,l.register,cgpara.location^.register);
                end;
              { can happen if a record with only 1 "single field" is
                returned in a floating point register and then is directly
                passed to a regcall parameter }
              LOC_REGISTER:
                begin
                  tmploc:=l;
                  location_force_mem(list,tmploc,size);
                  case locsize of
                    OS_F32:
                      tmploc.size:=OS_32;
                    OS_F64:
                      tmploc.size:=OS_64;
                    else
                      internalerror(2010053116);
                  end;
                  cg.a_load_loc_cgpara(list,tmploc,cgpara);
                  location_freetemp(list,tmploc);
                end
              else
                internalerror(2010053003);
            end;
          end;
        LOC_MMREGISTER,
        LOC_CMMREGISTER:
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  { can't use TCGSize2Size[l.size], because the size of an
                    80 bit extended parameter can be either 10 or 12 bytes }
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    begin
                      cg.g_stackpointer_alloc(list,stacksize);
                      reference_reset_base(href,NR_STACK_POINTER_REG,0,sizeof(pint));
                    end
                  else
                    reference_reset_base(href,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
                  cg.a_loadmm_reg_ref(list,locsize,locsize,l.register,href,mms_movescalar);
                end;
              LOC_FPUREGISTER:
                begin
                  tmploc:=l;
                  location_force_mem(list,tmploc,size);
                  cg.a_loadfpu_ref_cgpara(list,tmploc.size,tmploc.reference,cgpara);
                  location_freetemp(list,tmploc);
                end;
              else
                internalerror(2010053004);
            end;
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE :
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    cg.a_load_ref_cgpara(list,locsize,l.reference,cgpara)
                  else
                    begin
                      reference_reset_base(href,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
                      cg.g_concatcopy(list,l.reference,href,stacksize);
                    end;
                end;
              LOC_FPUREGISTER:
                begin
                  cg.a_loadfpu_ref_cgpara(list,locsize,l.reference,cgpara);
                end;
              else
                internalerror(2010053005);
            end;
          end;
        else
          internalerror(2002042430);
      end;
    end;


  function thlcgcpu.getaddressregister(list: TAsmList; size: tdef): Tregister;
    begin
      if is_farpointer(size) or is_hugepointer(size) then
        Result:=cg.getintregister(list,OS_32)
      else
        Result:=cg.getintregister(list,OS_16);
    end;


  procedure thlcgcpu.g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg8086(cg).g_copyvaluepara_openarray(list,ref,lenloc,arrdef.elesize,destreg);
    end;


  procedure thlcgcpu.g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg8086(cg).g_releasevaluepara_openarray(list,l);
    end;


  procedure thlcgcpu.location_force_mem(list: TAsmList; var l: tlocation; size: tdef);
    var
      r,tmpref: treference;
      is_sixbyterecord: Boolean;
      is_fourbyterecord: Boolean;
      is_methodptr: Boolean;
      is_nestedprocptr: Boolean;
    begin
      is_sixbyterecord:=(size.typ=recorddef) and (size.size=6);
      is_fourbyterecord:=(size.typ=recorddef) and (size.size=4);
      is_methodptr:=(size.typ=procvardef)
        and (po_methodpointer in tprocvardef(size).procoptions)
        and not(po_addressonly in tprocvardef(size).procoptions);
      is_nestedprocptr:=(size.typ=procvardef)
        and is_nested_pd(tprocvardef(size))
        and not(po_addressonly in tprocvardef(size).procoptions);

      { handle i8086 method pointers (incl. 6-byte mixed near + far),
        6-byte records and nested proc ptrs }
      if (is_sixbyterecord or is_methodptr or is_nestedprocptr) and (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tg.gethltemp(list,size,size.size,tt_normal,r);
          tmpref:=r;

          if current_settings.x86memorymodel in x86_far_code_models then
            begin
              cg.a_load_reg_ref(list,OS_32,OS_32,l.register,tmpref);
              inc(tmpref.offset,4);
            end
          else
            begin
              cg.a_load_reg_ref(list,OS_16,OS_16,l.register,tmpref);
              inc(tmpref.offset,2);
            end;
          if current_settings.x86memorymodel in x86_far_data_models then
            cg.a_load_reg_ref(list,OS_32,OS_32,l.registerhi,tmpref)
          else
            cg.a_load_reg_ref(list,OS_16,OS_16,l.registerhi,tmpref);

          location_reset_ref(l,LOC_REFERENCE,l.size,0);
          l.reference:=r;
        end
      { 4-byte records in registers need special handling as well. A record may
        be located in registerhi:register if it was converted from a procvar or
        in GetNextReg(register):register if it was converted from a longint.
        We can tell between the two by checking whether registerhi has been set. }
      else if is_fourbyterecord and (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tg.gethltemp(list,size,size.size,tt_normal,r);
          tmpref:=r;

          cg.a_load_reg_ref(list,OS_16,OS_16,l.register,tmpref);
          inc(tmpref.offset,2);
          if l.registerhi<>tregister(0) then
            cg.a_load_reg_ref(list,OS_16,OS_16,l.registerhi,tmpref)
          else
            cg.a_load_reg_ref(list,OS_16,OS_16,GetNextReg(l.register),tmpref);

          location_reset_ref(l,LOC_REFERENCE,l.size,0);
          l.reference:=r;
        end
      else
        inherited;
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;



end.
