{
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC64 specific calling conventions

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
unit cpupara;

{$I fpcdefs.inc}

interface

uses
  globtype,
  aasmtai,aasmdata,
  cpubase,
  symconst, symtype, symdef, symsym,
  paramgr, parabase, cgbase, cgutils;

type
  tppcparamanager = class(tparamanager)
    function get_volatile_registers_int(calloption: tproccalloption):
      tcpuregisterset; override;
    function get_volatile_registers_fpu(calloption: tproccalloption):
      tcpuregisterset; override;
    function push_addr_param(varspez: tvarspez; def: tdef; calloption:
      tproccalloption): boolean; override;

    procedure getintparaloc(calloption: tproccalloption; nr: longint; var
      cgpara: TCGPara); override;
    function create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint; override;
    function create_varargs_paraloc_info(p: tabstractprocdef; varargspara:
      tvarargsparalist): longint; override;
    function get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tlocation;override;
    procedure create_funcretloc_info(p: tabstractprocdef; side: tcallercallee);

  private
    procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister;
      var cur_stack_offset: aword);
    function create_paraloc_info_intern(p: tabstractprocdef; side:
      tcallercallee; paras: tparalist;
      var curintreg, curfloatreg, curmmreg: tsuperregister; var
        cur_stack_offset: aword; isVararg : boolean): longint;
    function parseparaloc(p: tparavarsym; const s: string): boolean; override;
  end;

implementation

uses
  verbose, systems,
  defutil,
  procinfo, cpupi;

function tppcparamanager.get_volatile_registers_int(calloption:
  tproccalloption): tcpuregisterset;
begin
  result := [RS_R0,RS_R3..RS_R12];
  if (target_info.system = system_powerpc64_darwin) then
    include(result,RS_R2);
end;

function tppcparamanager.get_volatile_registers_fpu(calloption:
  tproccalloption): tcpuregisterset;
begin
  result := [RS_F0..RS_F13];
end;

procedure tppcparamanager.getintparaloc(calloption: tproccalloption; nr:
  longint; var cgpara: TCGPara);
var
  paraloc: pcgparalocation;
begin
  cgpara.reset;
  cgpara.size := OS_ADDR;
  cgpara.intsize := sizeof(pint);
  cgpara.alignment := get_para_align(calloption);
  paraloc := cgpara.add_location;
  with paraloc^ do begin
    size := OS_INT;
    if (nr <= 8) then begin
      if (nr = 0) then
        internalerror(200309271);
      loc := LOC_REGISTER;
      register := newreg(R_INTREGISTER, RS_R2 + nr, R_SUBWHOLE);
    end else begin
      loc := LOC_REFERENCE;
      paraloc^.reference.index := NR_STACK_POINTER_REG;
      reference.offset := sizeof(aint) * (nr - 8);
    end;
  end;
end;

function getparaloc(p: tdef): tcgloc;

begin
  { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
    if push_addr_param for the def is true
  }
  case p.typ of
    orddef:
      result := LOC_REGISTER;
    floatdef:
      result := LOC_FPUREGISTER;
    enumdef:
      result := LOC_REGISTER;
    pointerdef:
      result := LOC_REGISTER;
    formaldef:
      result := LOC_REGISTER;
    classrefdef:
      result := LOC_REGISTER;
    procvardef,
    recorddef:
      result := LOC_REGISTER;
    objectdef:
      if is_object(p) then
        result := LOC_REFERENCE
      else
        result := LOC_REGISTER;
    stringdef:
      if is_shortstring(p) or is_longstring(p) then
        result := LOC_REFERENCE
      else
        result := LOC_REGISTER;
    filedef:
      result := LOC_REGISTER;
    arraydef:
      result := LOC_REFERENCE;
    setdef:
      if is_smallset(p) then
        result := LOC_REGISTER
      else
        result := LOC_REFERENCE;
    variantdef:
      result := LOC_REFERENCE;
    { avoid problems with errornous definitions }
    errordef:
      result := LOC_REGISTER;
  else
    internalerror(2002071001);
  end;
end;

function tppcparamanager.push_addr_param(varspez: tvarspez; def: tdef;
  calloption: tproccalloption): boolean;
begin
  result := false;
  { var,out always require address }
  if varspez in [vs_var, vs_out] then
  begin
    result := true;
    exit;
  end;
  case def.typ of
    variantdef,
    formaldef:
      result := true;
    procvardef,
    recorddef:
      result :=
        ((varspez = vs_const) and
         (
          (not (calloption in [pocall_cdecl, pocall_cppdecl]) and
          (def.size > 8))
         ) or
         (calloption = pocall_mwpascal)
        );
    arraydef:
      result := (tarraydef(def).highrange >= tarraydef(def).lowrange) or
        is_open_array(def) or
        is_array_of_const(def) or
        is_array_constructor(def);
    objectdef:
      result := is_object(def);
    setdef:
      result := not is_smallset(def);
    stringdef:
      result := tstringdef(def).stringtype in [st_shortstring, st_longstring];
  end;
end;

procedure tppcparamanager.init_values(var curintreg, curfloatreg, curmmreg:
  tsuperregister; var cur_stack_offset: aword);
begin
  { register parameter save area begins at 48(r2) }
  cur_stack_offset := 48;
  curintreg := RS_R3;
  curfloatreg := RS_F1;
  curmmreg := RS_M2;
end;

procedure tppcparamanager.create_funcretloc_info(p: tabstractprocdef; side:
  tcallercallee);
begin
  p.funcretloc[side]:=get_funcretloc(p,side,p.returndef);
end;

function tppcparamanager.get_funcretloc(p : tabstractprocdef; side:
  tcallercallee; def: tdef): tlocation;
var
  retcgsize: tcgsize;
begin
  { Constructors return self instead of a boolean }
  if (p.proctypeoption = potype_constructor) then
    retcgsize := OS_ADDR
  else
    retcgsize := def_cgsize(def);

  location_reset(result, LOC_INVALID, OS_NO);
  result.size := retcgsize;
  { void has no location }
  if is_void(def) then begin
    result.loc := LOC_VOID;
    exit;
  end;
  { Return is passed as var parameter }
  if ret_in_param(def, p.proccalloption) then
    begin
      result.loc := LOC_REFERENCE;
      result.size := retcgsize;
      exit;
    end;
  { Return in FPU register? }
  if def.typ = floatdef then begin
    result.loc := LOC_FPUREGISTER;
    result.register := NR_FPU_RESULT_REG;
    result.size := retcgsize;
  end else
    { Return in register }
    begin
      result.loc := LOC_REGISTER;
      result.size := retcgsize;
      if side = callerside then
        result.register := newreg(R_INTREGISTER,
          RS_FUNCTION_RESULT_REG, cgsize2subreg(R_INTREGISTER, retcgsize))
      else
        result.register := newreg(R_INTREGISTER,
          RS_FUNCTION_RETURN_REG, cgsize2subreg(R_INTREGISTER, retcgsize));
    end;
end;

function tppcparamanager.create_paraloc_info(p: tabstractprocdef; side:
  tcallercallee): longint;
var
  cur_stack_offset: aword;
  curintreg, curfloatreg, curmmreg : tsuperregister;
begin
  init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);

  result := create_paraloc_info_intern(p, side, p.paras, curintreg, curfloatreg,
    curmmreg, cur_stack_offset, false);

  create_funcretloc_info(p, side);
end;

function tppcparamanager.create_paraloc_info_intern(p: tabstractprocdef; side:
  tcallercallee; paras: tparalist;
  var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset:
  aword; isVararg : boolean): longint;
var
  stack_offset: longint;
  paralen: aint;
  nextintreg, nextfloatreg, nextmmreg : tsuperregister;
  paradef: tdef;
  paraloc: pcgparalocation;
  i: integer;
  hp: tparavarsym;
  loc: tcgloc;
  paracgsize: tcgsize;

  parashift : byte;

begin
{$IFDEF extdebug}
  if po_explicitparaloc in p.procoptions then
    internalerror(200411141);
{$ENDIF extdebug}

  result := 0;
  nextintreg := curintreg;
  nextfloatreg := curfloatreg;
  nextmmreg := curmmreg;
  stack_offset := cur_stack_offset;

  for i := 0 to paras.count - 1 do begin
    parashift := 0;
    hp := tparavarsym(paras[i]);

    paradef := hp.vardef;
    { Syscall for Morphos can have already a paraloc set; not supported on ppc64 }
    if (vo_has_explicit_paraloc in hp.varoptions) then begin
      internalerror(200412153);
    end;
    hp.paraloc[side].reset;
    { currently only support C-style array of const }
    if (p.proccalloption in [pocall_cdecl, pocall_cppdecl]) and
      is_array_of_const(paradef) then begin
      paraloc := hp.paraloc[side].add_location;
      { hack: the paraloc must be valid, but is not actually used }
      paraloc^.loc := LOC_REGISTER;
      paraloc^.register := NR_R0;
      paraloc^.size := OS_ADDR;
      break;
    end;

    if (hp.varspez in [vs_var, vs_out]) or
      push_addr_param(hp.varspez, paradef, p.proccalloption) or
      is_open_array(paradef) or
      is_array_of_const(paradef) then begin
      paradef := voidpointertype;
      loc := LOC_REGISTER;
      paracgsize := OS_ADDR;
      paralen := tcgsize2size[OS_ADDR];
    end else begin
      if not is_special_array(paradef) then
        paralen := paradef.size
      else
        paralen := tcgsize2size[def_cgsize(paradef)];
      if (paradef.typ = recorddef) and
        (hp.varspez in [vs_value, vs_const]) then begin
        { if a record has only one field and that field is }
        { non-composite (not array or record), it must be  }
        { passed according to the rules of that type.       }
        if (trecorddef(hp.vardef).symtable.SymList.count = 1) and
          (not trecorddef(hp.vardef).isunion)  and
          (tabstractvarsym(trecorddef(hp.vardef).symtable.SymList[0]).vardef.typ in [orddef, enumdef, floatdef])  then begin
          paradef :=
            tabstractvarsym(trecorddef(hp.vardef).symtable.SymList[0]).vardef;
          loc := getparaloc(paradef);
          paracgsize := def_cgsize(paradef);
        end else begin
          loc := LOC_REGISTER;
          paracgsize := int_cgsize(paralen);
          if (paralen in [3,5,6,7]) then
            parashift := (8-paralen) * 8;
        end;
      end else begin
        loc := getparaloc(paradef);
        paracgsize := def_cgsize(paradef);
        { for things like formaldef }
        if (paracgsize = OS_NO) then begin
          paracgsize := OS_ADDR;
          paralen := tcgsize2size[OS_ADDR];
        end;
      end
    end;

    { patch FPU values into integer registers if we currently have
     to pass them as vararg parameters
    }
    if (isVararg) and (paradef.typ = floatdef) then begin
      loc := LOC_REGISTER;
      if paracgsize = OS_F64 then
        paracgsize := OS_64
      else
        paracgsize := OS_32;
    end;

    hp.paraloc[side].alignment := std_param_align;
    hp.paraloc[side].size := paracgsize;
    hp.paraloc[side].intsize := paralen;
    if (paralen = 0) then
      if (paradef.typ = recorddef) then begin
        paraloc := hp.paraloc[side].add_location;
        paraloc^.loc := LOC_VOID;
      end else
        internalerror(2005011310);
    { can become < 0 for e.g. 3-byte records }

    while (paralen > 0) do begin
      paraloc := hp.paraloc[side].add_location;
      if (loc = LOC_REGISTER) and (nextintreg <= RS_R10) then begin
        paraloc^.loc := loc;
        paraloc^.shiftval := parashift;

        { make sure we don't lose whether or not the type is signed }
        if (paracgsize <> OS_NO) and (paradef.typ <> orddef) then
          paracgsize := int_cgsize(paralen);
        if (paracgsize in [OS_NO,OS_128,OS_S128]) then
          paraloc^.size := OS_INT
        else
          paraloc^.size := paracgsize;

        paraloc^.register := newreg(R_INTREGISTER, nextintreg, R_SUBNONE);
        inc(nextintreg);
        dec(paralen, tcgsize2size[paraloc^.size]);

        inc(stack_offset, sizeof(pint));
      end else if (loc = LOC_FPUREGISTER) and
        (nextfloatreg <= RS_F13) then begin
        paraloc^.loc := loc;
        paraloc^.size := paracgsize;
        paraloc^.register := newreg(R_FPUREGISTER, nextfloatreg, R_SUBWHOLE);
        { the PPC64 ABI says that the GPR index is increased for every parameter, no matter
        which type it is stored in }
        inc(nextintreg);
        inc(nextfloatreg);
        dec(paralen, tcgsize2size[paraloc^.size]);

        inc(stack_offset, tcgsize2size[OS_FLOAT]);
      end else if (loc = LOC_MMREGISTER) then begin
        { Altivec not supported }
        internalerror(200510192);
      end else begin
        { either LOC_REFERENCE, or one of the above which must be passed on the
        stack because of insufficient registers }
        paraloc^.loc := LOC_REFERENCE;
        case loc of
          LOC_FPUREGISTER:
            paraloc^.size:=int_float_cgsize(paralen);
          LOC_REGISTER,
          LOC_REFERENCE:
            paraloc^.size:=int_cgsize(paralen);
          else
            internalerror(2006011101);
        end;
        if (side = callerside) then
          paraloc^.reference.index := NR_STACK_POINTER_REG
        else begin
          { during procedure entry, NR_OLD_STACK_POINTER_REG contains the old stack pointer }
          paraloc^.reference.index := NR_OLD_STACK_POINTER_REG;
          tppcprocinfo(current_procinfo).needs_frame_pointer := true;
        end;
        paraloc^.reference.offset := stack_offset;

        { align temp contents to next register size }
        inc(stack_offset, align(paralen, 8));
        paralen := 0;
      end;
    end;
  end;

  curintreg := nextintreg;
  curfloatreg := nextfloatreg;
  curmmreg := nextmmreg;
  cur_stack_offset := stack_offset;
  result := stack_offset;
end;

function tppcparamanager.create_varargs_paraloc_info(p: tabstractprocdef;
  varargspara: tvarargsparalist): longint;
var
  cur_stack_offset: aword;
  parasize, l: longint;
  curintreg, firstfloatreg, curfloatreg, curmmreg: tsuperregister;
  i: integer;
  hp: tparavarsym;
  paraloc: pcgparalocation;
begin
  init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);
  firstfloatreg := curfloatreg;

  result := create_paraloc_info_intern(p, callerside, p.paras, curintreg,
    curfloatreg, curmmreg, cur_stack_offset, false);
  if (p.proccalloption in [pocall_cdecl, pocall_cppdecl]) then begin
    { just continue loading the parameters in the registers }
    result := create_paraloc_info_intern(p, callerside, varargspara, curintreg,
      curfloatreg, curmmreg, cur_stack_offset, true);
    { varargs routines have to reserve at least 64 bytes for the PPC64 ABI }
    if (result < 64) then
      result := 64;
  end else begin
    parasize := cur_stack_offset;
    for i := 0 to varargspara.count - 1 do begin
      hp := tparavarsym(varargspara[i]);
      hp.paraloc[callerside].alignment := 8;
      paraloc := hp.paraloc[callerside].add_location;
      paraloc^.loc := LOC_REFERENCE;
      paraloc^.size := def_cgsize(hp.vardef);
      paraloc^.reference.index := NR_STACK_POINTER_REG;
      l := push_size(hp.varspez, hp.vardef, p.proccalloption);
      paraloc^.reference.offset := parasize;
      parasize := parasize + l;
    end;
    result := parasize;
  end;
  if curfloatreg <> firstfloatreg then
    include(varargspara.varargsinfo, va_uses_float_reg);
end;

function tppcparamanager.parseparaloc(p: tparavarsym; const s: string): boolean;
begin
  { not supported/required for PowerPC64-linux target }
  internalerror(200404182);
  result := true;
end;


{

    breaks e.g. tests/test/cg/tpara1

procedure tppcparamanager.createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);
var
  paraloc : pcgparalocation;
begin
  paraloc:=parasym.paraloc[callerside].location;
  { Do not create a temporary if the value is pushed }
  if assigned(paraloc) and
    (paraloc^.loc=LOC_REFERENCE) and
    (paraloc^.reference.index=NR_STACK_POINTER_REG) then
    duplicateparaloc(list,calloption,parasym,cgpara)
  else
    inherited createtempparaloc(list,calloption,parasym,cgpara);
end;
}

begin
  paramanager := tppcparamanager.create;
end.

