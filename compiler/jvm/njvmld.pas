{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM assembler for nodes that handle loads and assignments

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
unit njvmld;

{$I fpcdefs.inc}

interface

uses
  globtype,
  symtype,
  cgutils,
  node, ncgld, ncgnstld;

type
  tjvmloadnode = class(tcgnestloadnode)
   protected
    function is_copyout_addr_param_load: boolean;
   public
    function is_addr_param_load: boolean; override;
    procedure pass_generate_code; override;
  end;

  tjvmassignmentnode  = class(tcgassignmentnode)
    function pass_1: tnode; override;
  end;

  tjvmarrayconstructornode = class(tcgarrayconstructornode)
   protected
    procedure makearrayref(var ref: treference; eledef: tdef); override;
    procedure advancearrayoffset(var ref: treference; elesize: asizeint); override;
    procedure wrapmanagedvarrec(var n: tnode);override;
  end;

implementation

uses
  verbose,
  aasmdata,
  nbas,nld,ncal,ncon,ninl,nmem,ncnv,
  symconst,symsym,symdef,symtable,defutil,jvmdef,
  paramgr,
  pass_1,
  cgbase,hlcgobj;

{ tjvmassignmentnode }

function tjvmassignmentnode.pass_1: tnode;
  var
    block: tblocknode;
    tempn: ttempcreatenode;
    stat: tstatementnode;
    target: tnode;
    psym: tsym;
  begin
    { intercept writes to string elements, because Java strings are immutable
      -> detour via StringBuilder
    }
    target:=left.actualtargetnode;
    if (target.nodetype=vecn) and
       (is_wide_or_unicode_string(tvecnode(target).left.resultdef) or
        is_ansistring(tvecnode(target).left.resultdef)) then
      begin
        { prevent errors in case of an expression such as
            word(str[x]):=1234;
        }
        inserttypeconv_explicit(right,cwidechartype);
        result:=ccallnode.createintern('fpc_'+tstringdef(tvecnode(target).left.resultdef).stringtypname+'_setchar',
          ccallparanode.create(right,
            ccallparanode.create(tvecnode(target).right,
              ccallparanode.create(tvecnode(target).left.getcopy,nil))));
        result:=cassignmentnode.create(tvecnode(target).left,result);
        right:=nil;
        tvecnode(target).left:=nil;
        tvecnode(target).right:=nil;
        exit;
      end
    else if (target.nodetype=vecn) and
       is_shortstring(tvecnode(target).left.resultdef) then
      begin
        { prevent errors in case of an expression such as
            byte(str[x]):=12;
        }
        inserttypeconv_explicit(right,cchartype);
        { call ShortstringClass(@shortstring).setChar(index,char) }
        tvecnode(target).left:=caddrnode.create_internal(tvecnode(target).left);
        { avoid useless typecheck when casting to shortstringclass }
        include(tvecnode(target).left.flags,nf_typedaddr);
        inserttypeconv_explicit(tvecnode(target).left,java_shortstring);
        psym:=search_struct_member(tabstractrecorddef(java_shortstring),'SETCHAR');
        if not assigned(psym) or
           (psym.typ<>procsym) then
          internalerror(2011052408);
        result:=
          ccallnode.create(
            ccallparanode.create(right,
              ccallparanode.create(tvecnode(target).right,nil)),
            tprocsym(psym),psym.owner,tvecnode(target).left,[]);
        right:=nil;
        tvecnode(target).left:=nil;
        tvecnode(target).right:=nil;
        exit;
      end
    else if target.resultdef.typ=formaldef then
      begin
        if right.resultdef.typ in [orddef,floatdef] then
          right:=cinlinenode.create(in_box_x,false,right)
        else if jvmimplicitpointertype(right.resultdef) then
          begin
            { we have to assign the address of a deep copy of the type to the
              object in the formalpara -> create a temp, assign the value to
              the temp, then assign the address in the temp to the para }
            block:=internalstatements(stat);
            tempn:=ctempcreatenode.create_value(right.resultdef,right.resultdef.size,
              tt_persistent,false,right);
            addstatement(stat,tempn);
            right:=caddrnode.create(ctemprefnode.create(tempn));
            inserttypeconv_explicit(right,java_jlobject);
            addstatement(stat,ctempdeletenode.create_normal_temp(tempn));
            addstatement(stat,ctypeconvnode.create_explicit(
              caddrnode.create(ctemprefnode.create(tempn)),java_jlobject));
            right:=block;
          end;
        typecheckpass(right);
        result:=inherited;
        exit;
      end
    else
      result:=inherited;
  end;


function tjvmloadnode.is_copyout_addr_param_load: boolean;
  begin
    result:=
      { passed via array of one element }
      ((symtable.symtabletype=parasymtable) and
       (symtableentry.typ=paravarsym) and
       paramanager.push_copyout_param(tparavarsym(symtableentry).varspez,resultdef,tprocdef(symtable.defowner).proccalloption));
  end;


function tjvmloadnode.is_addr_param_load: boolean;
  begin
    result:=
      (inherited and
       not jvmimplicitpointertype(tparavarsym(symtableentry).vardef) and
       (tparavarsym(symtableentry).vardef.typ<>formaldef)) or
      is_copyout_addr_param_load;
  end;


procedure tjvmloadnode.pass_generate_code;
  begin
    if is_copyout_addr_param_load then
      begin
        { in case of nested access, load address of field in nestedfpstruct }
        if assigned(left) then
          generate_nested_access(tabstractnormalvarsym(symtableentry));
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),4);
        location.reference.arrayreftype:=art_indexconst;
        location.reference.base:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        location.reference.indexoffset:=0;
        { load the field from the nestedfpstruct, or the parameter location.
          In both cases, the result is an array of one element containing the
          parameter value }
        if assigned(left) then
          hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,left.location,location.reference.base)
        else
          hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,tparavarsym(symtableentry).localloc,location.reference.base);
      end
    else if symtableentry.typ=procsym then
      { handled in tjvmcnvnode.first_proc_to_procvar }
      internalerror(2011072408)
    else
      inherited pass_generate_code;
  end;


{ tjvmarrayconstructornode }

procedure tjvmarrayconstructornode.makearrayref(var ref: treference; eledef: tdef);
  var
    basereg: tregister;
  begin
    { arrays are implicitly dereferenced }
    basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
    hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,ref,basereg);
    reference_reset_base(ref,basereg,0,1);
    ref.arrayreftype:=art_indexconst;
    ref.indexoffset:=0;
  end;


procedure tjvmarrayconstructornode.advancearrayoffset(var ref: treference; elesize: asizeint);
  begin
    inc(ref.indexoffset);
  end;


procedure tjvmarrayconstructornode.wrapmanagedvarrec(var n: tnode);
  var
    varrecdef: trecorddef;
    block: tblocknode;
    stat: tstatementnode;
    temp: ttempcreatenode;
  begin
    varrecdef:=trecorddef(search_system_type('TVARREC').typedef);
    block:=internalstatements(stat);
    temp:=ctempcreatenode.create(varrecdef,varrecdef.size,tt_persistent,false);
    addstatement(stat,temp);
    addstatement(stat,
      ccallnode.createinternmethod(
        ctemprefnode.create(temp),'INIT',ccallparanode.create(n,nil)));
    { note: this will not free the record contents, but just let its reference
      on the stack be reused -- which is ok, because the reference will be
      stored into the open array parameter }
    addstatement(stat,ctempdeletenode.create_normal_temp(temp));
    addstatement(stat,ctemprefnode.create(temp));
    n:=block;
    firstpass(n);
  end;


begin
  cloadnode:=tjvmloadnode;
  cassignmentnode:=tjvmassignmentnode;
  carrayconstructornode:=tjvmarrayconstructornode;
end.

