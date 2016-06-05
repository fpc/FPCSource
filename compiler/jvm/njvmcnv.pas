{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate JVM code for type converting nodes

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

 ****************************************************************************}
unit njvmcnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,
      symtype;

    type
       tjvmtypeconvnode = class(tcgtypeconvnode)
          class function target_specific_need_equal_typeconv(fromdef, todef: tdef): boolean; override;

          function typecheck_dynarray_to_openarray: tnode; override;
          function typecheck_string_to_chararray: tnode; override;
          function typecheck_string_to_string: tnode;override;
          function typecheck_char_to_string: tnode; override;
          function typecheck_proc_to_procvar: tnode; override;
          function pass_1: tnode; override;
          function simplify(forinline: boolean): tnode; override;
          function first_cstring_to_pchar: tnode;override;
          function first_set_to_set : tnode;override;
          function first_nil_to_methodprocvar: tnode; override;
          function first_proc_to_procvar: tnode; override;
          function first_ansistring_to_pchar: tnode; override;

          procedure second_int_to_int;override;
          procedure second_cstring_to_pchar;override;
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
          function first_int_to_real: tnode; override;
          procedure second_pointer_to_array;override;
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
          procedure second_proc_to_procvar;override;
          procedure second_nil_to_methodprocvar;override;
          procedure second_bool_to_int;override;
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
          procedure second_elem_to_openarray; override;
          function target_specific_explicit_typeconv: boolean; override;
          function target_specific_general_typeconv: boolean; override;
         protected
          function do_target_specific_explicit_typeconv(check_only: boolean; out resnode: tnode): boolean;
       end;

       tjvmasnode = class(tcgasnode)
        protected
         { to discern beween "obj as tclassref" and "tclassref(obj)" }
         classreftypecast: boolean;
         function target_specific_typecheck: boolean;override;
        public
         function pass_1 : tnode;override;
         procedure pass_generate_code; override;
         function dogetcopy: tnode; override;
         function docompare(p: tnode): boolean; override;
         constructor ppuload(t: tnodetype; ppufile: tcompilerppufile); override;
         procedure ppuwrite(ppufile: tcompilerppufile); override;
       end;

       tjvmisnode = class(tisnode)
        protected
         function target_specific_typecheck: boolean;override;
        public
         function pass_1 : tnode;override;
         procedure pass_generate_code; override;
       end;

implementation

   uses
      verbose,globals,globtype,constexp,cutils,
      symbase,symconst,symdef,symsym,symcpu,symtable,aasmbase,aasmdata,
      defutil,defcmp,jvmdef,
      cgbase,cgutils,pass_1,pass_2,
      nbas,ncon,ncal,ninl,nld,nmem,procinfo,
      nutils,paramgr,
      cpubase,cpuinfo,aasmcpu,
      tgobj,hlcgobj,hlcgcpu;


{*****************************************************************************
                            TypeCheckTypeConv
*****************************************************************************}

    function isvalidprocvartypeconv(fromdef, todef: tdef): boolean;

      var
        tmethoddef: tdef;

      function docheck(def1,def2: tdef): boolean;
        begin
          result:=false;
          if def1.typ<>procvardef then
            exit;
          { is_addressonly procvars are treated like regular pointer-sized data,
            po_methodpointer procvars like implicit pointers to a struct }
          if tprocvardef(def1).is_addressonly then
            result:=
              ((def2.typ=procvardef) and
               tprocvardef(def2).is_addressonly) or
              (def2=java_jlobject) or
              (def2=voidpointertype)
          else if po_methodpointer in tprocvardef(def1).procoptions then
            begin
              if not assigned(tmethoddef) then
                tmethoddef:=search_system_type('TMETHOD').typedef;
              result:=
                (def2=methodpointertype) or
                (def2=tmethoddef) or
                ((def2.typ=procvardef) and
                 (po_methodpointer in tprocvardef(def2).procoptions));
            end;
          { can't typecast nested procvars, they need 3 data pointers }
        end;

      begin
        tmethoddef:=nil;
        result:=
          docheck(fromdef,todef) or
          docheck(todef,fromdef);
      end;


   class function tjvmtypeconvnode.target_specific_need_equal_typeconv(fromdef, todef: tdef): boolean;
     begin
       result:=
         (fromdef<>todef) and
         { two procdefs that are structurally the same but semantically different
           still need a convertion }
         (
          ((fromdef.typ=procvardef) and
           (todef.typ=procvardef))
         );
     end;


   function tjvmtypeconvnode.typecheck_dynarray_to_openarray: tnode;
     begin
       { all arrays are equal in Java }
       result:=nil;
       convtype:=tc_equal;
     end;


   function tjvmtypeconvnode.typecheck_string_to_chararray: tnode;
     var
       newblock: tblocknode;
       newstat: tstatementnode;
       restemp: ttempcreatenode;
       chartype: string;
     begin
       if (left.nodetype = stringconstn) and
          (tstringconstnode(left).cst_type=cst_conststring) then
         inserttypeconv(left,cunicodestringtype);
       { even constant strings have to be handled via a helper }
       if is_widechar(tarraydef(resultdef).elementdef) then
         chartype:='widechar'
       else
         chartype:='char';
       newblock:=internalstatements(newstat);
       restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
       addstatement(newstat,restemp);
       addstatement(newstat,ccallnode.createintern('fpc_'+tstringdef(left.resultdef).stringtypname+
         '_to_'+chartype+'array',ccallparanode.create(left,ccallparanode.create(
         ctemprefnode.create(restemp),nil))));
       addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
       addstatement(newstat,ctemprefnode.create(restemp));
       result:=newblock;
       left:=nil;
     end;


   function tjvmtypeconvnode.typecheck_string_to_string: tnode;
     begin
       { make sure the generic code gets a stringdef }
       if (maybe_find_real_class_definition(resultdef,false)=java_jlstring) or
          (maybe_find_real_class_definition(left.resultdef,false)=java_jlstring) then
         begin
           left:=ctypeconvnode.create(left,cunicodestringtype);
           left.flags:=flags;
           result:=ctypeconvnode.create(left,resultdef);
           result.flags:=flags;
           left:=nil;
         end
       else
         result:=inherited;
     end;


   function tjvmtypeconvnode.typecheck_char_to_string: tnode;
    begin
      { make sure the generic code gets a stringdef }
      if self.totypedef=java_jlstring then
        begin
          inserttypeconv(left,cunicodestringtype);
          inserttypeconv(left,totypedef);
          result:=left;
          left:=nil;
          exit;
        end;
      result:=inherited;
    end;


   function tjvmtypeconvnode.typecheck_proc_to_procvar: tnode;
    begin
      result:=inherited typecheck_proc_to_procvar;
      if not assigned(totypedef) or
         (totypedef.typ<>procvardef) then
        begin
          if assigned(tcpuprocvardef(resultdef).classdef) then
            internalerror(2011072405);
          { associate generic classdef; this is the result of an @proc
            expression, and such expressions can never result in a direct call
            -> no invoke() method required (which only exists in custom
            constructed descendents created for defined procvar types) }
          if is_nested_pd(tabstractprocdef(resultdef)) then
            { todo }
            internalerror(2011072406)
          else
            tcpuprocvardef(resultdef).classdef:=java_procvarbase;
        end;
    end;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tjvmtypeconvnode.first_int_to_real: tnode;
      begin
        if not is_64bitint(left.resultdef) and
           not is_currency(left.resultdef) then
          if is_signed(left.resultdef) or
             (left.resultdef.size<4) then
            inserttypeconv(left,s32inttype)
          else
            inserttypeconv(left,u32inttype);
        firstpass(left);
        result := nil;
        expectloc:=LOC_FPUREGISTER;
      end;


    function tjvmtypeconvnode.pass_1: tnode;
      begin
        if (nf_explicit in flags) or
           { some implicit type conversions from voidpointer to other types
             (such as dynamic array) are allowed too, even though the types are
             incompatible -> make sure we check those too and insert checkcast
             instructions as necessary }
           (is_voidpointer(left.resultdef) and
            not is_voidpointer(resultdef)) then
          begin
            do_target_specific_explicit_typeconv(false,result);
            if assigned(result) then
              exit;
          end;
        result:=inherited pass_1;
      end;

    function tjvmtypeconvnode.simplify(forinline: boolean): tnode;
      begin
        result:=inherited simplify(forinline);
        if assigned(result) then
          exit;
        { string constants passed to java.lang.String must be converted to
          widestring }
        if ((is_conststringnode(left) and
             not(tstringconstnode(left).cst_type in [cst_unicodestring,cst_widestring])) or
            is_constcharnode(left)) and
           (maybe_find_real_class_definition(resultdef,false)=java_jlstring) then
          inserttypeconv(left,cunicodestringtype);
      end;


    function tjvmtypeconvnode.first_cstring_to_pchar: tnode;
      var
        vs: tstaticvarsym;
      begin
        result:=inherited;
        if assigned(result) then
          exit;
        { nil pointer -> valid address }
        if (left.nodetype=stringconstn) and
           (tstringconstnode(left).cst_type in [cst_widestring,cst_unicodestring,cst_ansistring]) and
           (tstringconstnode(left).len=0) then
          begin
            if tstringconstnode(left).cst_type=cst_ansistring then
              vs:=tstaticvarsym(systemunit.Find('FPC_EMPTYANSICHAR'))
            else
              vs:=tstaticvarsym(systemunit.Find('FPC_EMPTYWIDECHAR'));
            if not assigned(vs) then
              internalerror(2012052605);
            result:=caddrnode.create(cloadnode.create(vs,vs.owner));
            result:=ctypeconvnode.create_explicit(result,resultdef);
          end;
      end;


    function tjvmtypeconvnode.first_set_to_set: tnode;
      var
        setclassdef: tdef;
        helpername: string;
      begin
        result:=nil;
        if (left.nodetype=setconstn) then
          result:=inherited
        { on native targets, only the binary layout has to match. Here, both
          sets also have to be either of enums or ordinals, and in case of
          enums they have to be of the same base type }
        else if (tsetdef(left.resultdef).elementdef.typ=enumdef)=(tsetdef(resultdef).elementdef.typ=enumdef) and
            ((tsetdef(left.resultdef).elementdef.typ<>enumdef) or
             (tenumdef(tsetdef(left.resultdef).elementdef).getbasedef=tenumdef(tsetdef(resultdef).elementdef).getbasedef)) and
            (tsetdef(left.resultdef).setbase=tsetdef(resultdef).setbase) and
            (left.resultdef.size=resultdef.size) then
          begin
            result:=left;
            left:=nil;
          end
        else
          begin
            { 'deep' conversion }
            if tsetdef(resultdef).elementdef.typ<>enumdef then
              begin
                if tsetdef(left.resultdef).elementdef.typ<>enumdef then
                  helpername:='fpc_bitset_to_bitset'
                else
                  helpername:='fpc_enumset_to_bitset';
                result:=ccallnode.createintern(helpername,ccallparanode.create(
                  genintconstnode(tsetdef(resultdef).setbase), ccallparanode.create(
                    genintconstnode(tsetdef(left.resultdef).setbase),
                      ccallparanode.create(left,nil))));
              end
            else
              begin
                if tsetdef(left.resultdef).elementdef.typ<>enumdef then
                  begin
                    helpername:='fpcBitSetToEnumSet';
                    setclassdef:=java_jubitset;
                  end
                else
                  begin
                    helpername:='fpcEnumSetToEnumSet';
                    setclassdef:=java_juenumset;
                  end;
                left:=caddrnode.create_internal(left);
                include(left.flags,nf_typedaddr);
                inserttypeconv_explicit(left,setclassdef);
                result:=ccallnode.createinternmethod(
                  cloadvmtaddrnode.create(ctypenode.create(setclassdef)),
                  helpername,ccallparanode.create(
                    genintconstnode(tsetdef(resultdef).setbase), ccallparanode.create(
                      genintconstnode(tsetdef(left.resultdef).setbase),
                        ccallparanode.create(left,nil))));
              end;
            inserttypeconv_explicit(result,getpointerdef(resultdef));
            result:=cderefnode.create(result);
            { reused }
            left:=nil;
          end;

      end;


    function tjvmtypeconvnode.first_nil_to_methodprocvar: tnode;
      begin
        result:=inherited first_nil_to_methodprocvar;
        if assigned(result) then
          exit;
        if not assigned(tcpuprocvardef(resultdef).classdef) then
          tcpuprocvardef(resultdef).classdef:=java_procvarbase;
        result:=ccallnode.createinternmethod(
          cloadvmtaddrnode.create(ctypenode.create(tcpuprocvardef(resultdef).classdef)),'CREATE',nil);
        { method pointer is an implicit pointer type }
        result:=ctypeconvnode.create_explicit(result,getpointerdef(resultdef));
        result:=cderefnode.create(result);
      end;


    function tjvmtypeconvnode.first_proc_to_procvar: tnode;
      var
        constrparas: tcallparanode;
        newpara: tnode;
        procdefparas: tarrayconstructornode;
        pvs: tparavarsym;
        fvs: tsym;
        i: longint;
        corrclass: tdef;
        jlclass: tobjectdef;
        encodedtype: tsymstr;
        procload: tnode;
        procdef: tprocdef;
        st: tsymtable;
        pushaddr: boolean;
      begin
        result:=inherited first_proc_to_procvar;
        if assigned(result) then
          exit;
        procdef:=tloadnode(left).procdef;
        procload:=tloadnode(left).left;
        if not assigned(procload) then
          begin
            { nested or regular routine -> figure out whether unit-level or
              nested, and if nested whether it's nested in a method or in a
              regular routine }
            st:=procdef.owner;
            while st.symtabletype=localsymtable do
              st:=st.defowner.owner;
            if st.symtabletype in [objectsymtable,recordsymtable] then
              { nested routine in method -> part of encloding class }
              procload:=cloadvmtaddrnode.create(ctypenode.create(tdef(st.defowner)))
            else
              begin
                { regular procedure/function -> get type representing unit
                  class }
                while not(st.symtabletype in [staticsymtable,globalsymtable]) do
                  st:=st.defowner.owner;
                corrclass:=search_named_unit_globaltype(st.realname^,'__FPC_JVM_MODULE_CLASS_ALIAS$',true).typedef;
                procload:=cloadvmtaddrnode.create(ctypenode.create(tdef(corrclass)));
              end;
          end;
        { todo: support nested procvars }
        if is_nested_pd(procdef) then
          internalerror(2011072607);
        { constructor FpcBaseProcVarType.create(inst: jlobject; const method: unicodestring; const argTypes: array of JLClass); }
        constrparas:=ccallparanode.create(ctypeconvnode.create_explicit(procload,java_jlobject),nil);
        if not assigned(procdef.import_name) then
          constrparas:=ccallparanode.create(cstringconstnode.createstr(procdef.procsym.realname),constrparas)
        else
          constrparas:=ccallparanode.create(cstringconstnode.createstr(procdef.import_name^),constrparas);
        procdefparas:=nil;
        jlclass:=tobjectdef(search_system_type('JLCLASS').typedef);
        { in reverse to make it easier to build the arrayconstructorn }
        for i:=procdef.paras.count-1 downto 0 do
          begin
            pvs:=tparavarsym(procdef.paras[i]);
            { self is is an implicit parameter for normal methods }
            if (vo_is_self in pvs.varoptions) and
               not(po_classmethod in procdef.procoptions) then
              continue;
            { in case of an arraydef, pass by jlclass.forName() to get the classdef
              (could be optimized by adding support to loadvmtaddrnode to also deal
               with arrays, although we'd have to create specific arraydefs for var/
               out/constref parameters }
             pushaddr:=paramanager.push_copyout_param(pvs.varspez,pvs.vardef,procdef.proccalloption);
             if pushaddr or
                (pvs.vardef.typ=arraydef) then
               begin
                 encodedtype:=jvmencodetype(pvs.vardef,false);
                 if pushaddr then
                   encodedtype:='['+encodedtype;
                 replace(encodedtype,'/','.');
                 newpara:=ccallnode.createinternmethod(cloadvmtaddrnode.create(ctypenode.create(jlclass)),'FORNAME',
                   ccallparanode.create(cstringconstnode.createstr(encodedtype),nil));
               end
             else
               begin
                 corrclass:=jvmgetcorrespondingclassdef(pvs.vardef);
                 if pvs.vardef.typ in [orddef,floatdef] then
                   begin
                     { get the class representing the primitive type }
                     fvs:=search_struct_member(tobjectdef(corrclass),'FTYPE');
                     newpara:=nil;
                     if not handle_staticfield_access(fvs,newpara) then
                       internalerror(2011072417);
                   end
                 else
                   newpara:=cloadvmtaddrnode.create(ctypenode.create(corrclass));
                 newpara:=ctypeconvnode.create_explicit(newpara,jlclass);
               end;
            procdefparas:=carrayconstructornode.create(newpara,procdefparas);
          end;
        if not assigned(procdefparas) then
          procdefparas:=carrayconstructornode.create(nil,nil);
        constrparas:=ccallparanode.create(procdefparas,constrparas);
        result:=ccallnode.createinternmethod(cloadvmtaddrnode.create(ctypenode.create(tcpuprocvardef(resultdef).classdef)),'CREATE',constrparas);
        { typecast to the procvar type }
        if tprocvardef(resultdef).is_addressonly then
          result:=ctypeconvnode.create_explicit(result,resultdef)
        else
          begin
            result:=ctypeconvnode.create_explicit(result,getpointerdef(resultdef));
            result:=cderefnode.create(result)
          end;
        { reused }
        tloadnode(left).left:=nil;
      end;


    function tjvmtypeconvnode.first_ansistring_to_pchar: tnode;
      var
        ps: tsym;
      begin
        { also called for unicodestring->pwidechar, not supported since we can't
          directly access the characters in java.lang.String }
        if not is_ansistring(left.resultdef) or
           not is_pchar(resultdef) then
          begin
            CGMessage2(type_e_illegal_type_conversion,left.resultdef.typename,resultdef.typename);
            result:=nil;
            exit;
          end;
        ps:=search_struct_member(java_ansistring,'INTERNCHARS');
        if not assigned(ps) or
           (ps.typ<>procsym) then
          internalerror(2011081401);
        { AnsistringClass.internChars is a static class method that will either
          return the internal fdata ansichar array of the string, or an array
          with a single #0 }
        result:=ccallnode.create(ccallparanode.create(left,nil),tprocsym(ps),
          ps.owner,
          cloadvmtaddrnode.create(ctypenode.create(java_ansistring)),[]);
        include(result.flags,nf_isproperty);
        result:=ctypeconvnode.create_explicit(result,resultdef);
        { reused }
        left:=nil;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure tjvmtypeconvnode.second_int_to_int;
      var
        ressize,
        leftsize : longint;
      begin
        { insert range check if not explicit conversion }
        if not(nf_explicit in flags) then
          hlcg.g_rangecheck(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef);

        { is the result size smaller? when typecasting from void
          we always reuse the current location, because there is
          nothing that we can load in a register }
        ressize:=resultdef.size;
        leftsize :=left.resultdef.size;
        if ((ressize<>leftsize) or
            ((left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
             (left.location.reference.arrayreftype<>art_none) and
             (is_widechar(left.resultdef)<>is_widechar(resultdef))) or
            is_bitpacked_access(left)) and
           not is_void(left.resultdef) then
          begin
            location_copy(location,left.location);
            { reuse a loc_reference when the newsize is larger than
              than the original and 4 bytes, because all <= 4 byte loads will
              result in a stack slot that occupies 4 bytes.

              Except
                a) for arrays (they use different load instructions for
                   differently sized data types) or symbols (idem)
                b) when going from 4 to 8 bytes, because these are different
                   data types
            }
            if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
               not assigned(location.reference.symbol) and
               (location.reference.arrayreftype=art_none) and
               (ressize>leftsize) and
               (ressize=4) then
              begin
                location.size:=def_cgsize(resultdef);
                { no adjustment of the offset even though Java is big endian,
                  because the load instruction will remain the same }
              end
            else
              hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,false);
          end
        else
          begin
            if ((ressize < sizeof(aint)) and
                (def_cgsize(left.resultdef)<>def_cgsize(resultdef))) or
               (is_widechar(left.resultdef)<>is_widechar(resultdef)) then
              begin
                location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
                location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
                hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,left.resultdef,resultdef,left.location,location.register);
              end
            else
              location_copy(location,left.location);
          end;
      end;


    procedure tjvmtypeconvnode.second_cstring_to_pchar;
      begin
        location_copy(location,left.location);
      end;


    procedure tjvmtypeconvnode.second_pointer_to_array;
      begin
        { arrays are implicit pointers in Java -> same location }
        location_copy(location,left.location);
      end;


    procedure tjvmtypeconvnode.second_int_to_real;
      var
        srcsize, ressize: longint;

      procedure convertsignedstackloc;
        begin
          case srcsize of
            4:
              case ressize of
                4:
                  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i2f));
                8:
                  begin
                    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i2d));
                    thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
                  end;
                else
                  internalerror(2011010601);
              end;
            8:
              case ressize of
                4:
                  begin
                    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_l2f));
                    thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
                  end;
                8:
                  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_l2d));
                else
                  internalerror(2011010602);
              end;
            else
              internalerror(2011010603);
          end;
        end;

      var
        signeddef : tdef;
        l1 : tasmlabel;

      begin
        srcsize:=left.resultdef.size;
        ressize:=resultdef.size;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);

        { first always convert as if it's a signed number }
        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        convertsignedstackloc;
        if not is_signed(left.resultdef) then
          begin
            { if it was unsigned, add high(cardinal)+1/high(qword)+1 in case
              the signed interpretation is < 0 }
            current_asmdata.getjumplabel(l1);
            if srcsize=4 then
              signeddef:=s32inttype
            else
              signeddef:=s64inttype;
            hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,signeddef,OC_GTE,0,left.location,l1);
            if srcsize=4 then
              thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,4294967296.0)
            else
              thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,18446744073709551616.0);
            if ressize=4 then
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_fadd))
            else
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dadd));
            hlcg.a_label(current_asmdata.CurrAsmList,l1);
          end;
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


    procedure tjvmtypeconvnode.second_proc_to_procvar;
      begin
        internalerror(2011072506);
      end;


    procedure tjvmtypeconvnode.second_nil_to_methodprocvar;
      var
        r: Treference;
      begin
        tg.gethltemp(current_asmdata.currasmlist,java_jlobject,java_jlobject.size,tt_normal,r);
        hlcg.a_load_const_ref(current_asmdata.CurrAsmList,java_jlobject,0,r);
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),1);
        location.reference:=r;
      end;


    procedure tjvmtypeconvnode.second_bool_to_int;
      var
         newsize: tcgsize;
         oldTrueLabel,oldFalseLabel : tasmlabel;
      begin
         oldTrueLabel:=current_procinfo.CurrTrueLabel;
         oldFalseLabel:=current_procinfo.CurrFalseLabel;
         current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
         current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
         secondpass(left);
         location_copy(location,left.location);
         newsize:=def_cgsize(resultdef);
         { byte(bytebool) or word(wordbool) or longint(longbool) must be }
         { accepted for var parameters and assignments, and must not     }
         { change the ordinal value or value location.                   }
         { htypechk.valid_for_assign ensures that such locations with a  }
         { size<sizeof(register) cannot be LOC_CREGISTER (they otherwise }
         { could be in case of a plain assignment), and LOC_REGISTER can }
         { never be an assignment target. The remaining LOC_REGISTER/    }
         { LOC_CREGISTER locations do have to be sign/zero-extended.     }

         {   -- Note: this does not work for Java and 2/4 byte sized
                      values, because bytebool/wordbool are signed and
                      are stored in 4 byte locations -> will result in
                      "byte" with the value high(cardinal); see remark
                      in second_int_to_int above regarding consequences }
         if not(nf_explicit in flags) or
            (location.loc in [LOC_FLAGS,LOC_JUMP]) or
            ((newsize<>left.location.size) and
             ((left.resultdef.size<>resultdef.size) or
              not(left.resultdef.size in [4,8]))
            ) then
           hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
         else
           { may differ in sign, e.g. bytebool -> byte   }
           location.size:=newsize;
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
      end;


    procedure tjvmtypeconvnode.second_int_to_bool;
      var
        hlabel1,hlabel2,oldTrueLabel,oldFalseLabel : tasmlabel;
        newsize  : tcgsize;
      begin
        oldTrueLabel:=current_procinfo.CurrTrueLabel;
        oldFalseLabel:=current_procinfo.CurrFalseLabel;
        current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
        current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
        secondpass(left);
        if codegenerror then
          exit;

        { Explicit typecasts from any ordinal type to a boolean type }
        { must not change the ordinal value                          }
        { Exception: Android verifier... }
        if (nf_explicit in flags) and
           not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) and
           not(current_settings.cputype=cpu_dalvik) then
          begin
             location_copy(location,left.location);
             newsize:=def_cgsize(resultdef);
             { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
             if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
               hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
             else
               location.size:=newsize;
             current_procinfo.CurrTrueLabel:=oldTrueLabel;
             current_procinfo.CurrFalseLabel:=oldFalseLabel;
             exit;
          end;

       location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
       location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
       current_asmdata.getjumplabel(hlabel2);
       case left.location.loc of
         LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER:
           begin
             current_asmdata.getjumplabel(hlabel1);
             hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,left.resultdef,OC_EQ,0,left.location,hlabel1);
           end;
         LOC_JUMP :
           begin
             hlabel1:=current_procinfo.CurrFalseLabel;
             hlcg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
           end;
         else
           internalerror(10062);
       end;

       if not(is_cbool(resultdef)) then
         thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,1,R_INTREGISTER)
       else
         thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,-1,R_INTREGISTER);
       { we jump over the next constant load -> they don't appear on the
         stack simulataneously }
       thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
       hlcg.a_jmp_always(current_asmdata.CurrAsmList,hlabel2);
       hlcg.a_label(current_asmdata.CurrAsmList,hlabel1);
       thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,0,R_INTREGISTER);
       hlcg.a_label(current_asmdata.CurrAsmList,hlabel2);
       thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);

       current_procinfo.CurrTrueLabel:=oldTrueLabel;
       current_procinfo.CurrFalseLabel:=oldFalseLabel;
     end;


    procedure tjvmtypeconvnode.second_elem_to_openarray;
      var
        primitivetype: boolean;
        opc: tasmop;
        mangledname: string;
        basereg: tregister;
        arrayref: treference;
      begin
        { create an array with one element of the required type }
        thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s32inttype,1,R_INTREGISTER);
        mangledname:=jvmarrtype(left.resultdef,primitivetype);
        if primitivetype then
          opc:=a_newarray
        else
          opc:=a_anewarray;
        { doesn't change stack height: one int replaced by one reference }
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname)));
        { store the data in the newly created array }
        basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,java_jlobject,basereg);
        reference_reset_base(arrayref,basereg,0,4);
        arrayref.arrayreftype:=art_indexconst;
        arrayref.indexoffset:=0;
        hlcg.a_load_loc_ref(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location,arrayref);
        location_reset_ref(location,LOC_REFERENCE,OS_ADDR,4);
        tg.gethltemp(current_asmdata.CurrAsmList,java_jlobject,4,tt_normal,location.reference);
        hlcg.a_load_reg_ref(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,basereg,location.reference);
      end;


    procedure get_most_nested_types(var fromdef, todef: tdef);
      begin
       while is_dynamic_array(fromdef) and
             is_dynamic_array(todef) do
         begin
           fromdef:=tarraydef(fromdef).elementdef;
           todef:=tarraydef(todef).elementdef;
         end;
       fromdef:=maybe_find_real_class_definition(fromdef,false);
       todef:=maybe_find_real_class_definition(todef,false);
      end;


    function tjvmtypeconvnode.do_target_specific_explicit_typeconv(check_only: boolean; out resnode: tnode): boolean;

      { handle explicit typecast from int to to real or vice versa }
      function int_real_explicit_typecast(fdef: tfloatdef; const singlemethod, doublemethod: string): tnode;
        var
          csym: ttypesym;
          psym: tsym;
        begin
         { use the float/double to raw bits methods to get the bit pattern }
          if fdef.floattype=s32real then
            begin
              csym:=search_system_type('JLFLOAT');
              psym:=search_struct_member(tobjectdef(csym.typedef),singlemethod);
            end
          else
            begin
              csym:=search_system_type('JLDOUBLE');
              psym:=search_struct_member(tobjectdef(csym.typedef),doublemethod);
            end;
          if not assigned(psym) or
             (psym.typ<>procsym) then
            internalerror(2011012901);
          { call the (static class) method to get the raw bits }
          result:=ccallnode.create(ccallparanode.create(left,nil),
            tprocsym(psym),psym.owner,
            cloadvmtaddrnode.create(ctypenode.create(csym.typedef)),[]);
          { convert the result to the result type of this type conversion node }
          inserttypeconv_explicit(result,resultdef);
          { left is reused }
          left:=nil;
        end;

      function ord_enum_explicit_typecast(fdef: torddef; todef: tcpuenumdef): tnode;
        var
          psym: tsym;
        begin
          { we only create a class for the basedefs }
          todef:=tcpuenumdef(todef.getbasedef);
          psym:=search_struct_member(todef.classdef,'FPCVALUEOF');
          if not assigned(psym) or
             (psym.typ<>procsym) then
            internalerror(2011062601);
          result:=ccallnode.create(ccallparanode.create(left,nil),
            tprocsym(psym),psym.owner,
            cloadvmtaddrnode.create(ctypenode.create(todef.classdef)),[]);
          { convert the result to the result type of this type conversion node }
          inserttypeconv_explicit(result,resultdef);
          { left is reused }
          left:=nil;
        end;

      function enum_ord_explicit_typecast(fdef: tcpuenumdef; todef: torddef): tnode;
        var
          psym: tsym;
        begin
          { we only create a class for the basedef }
          fdef:=tcpuenumdef(fdef.getbasedef);
          psym:=search_struct_member(fdef.classdef,'FPCORDINAL');
          if not assigned(psym) or
             (psym.typ<>procsym) then
            internalerror(2011062602);
          result:=ccallnode.create(nil,tprocsym(psym),psym.owner,left,[]);
          { convert the result to the result type of this type conversion node }
          inserttypeconv_explicit(result,resultdef);
          { left is reused }
          left:=nil;
        end;

      function from_set_explicit_typecast: tnode;
        var
          helpername: string;
          setconvdef: tdef;
        begin
         if tsetdef(left.resultdef).elementdef.typ=enumdef then
           begin
             setconvdef:=java_juenumset;
             helpername:='fpc_enumset_to_'
           end
         else
           begin
             setconvdef:=java_jubitset;
             helpername:='fpc_bitset_to_'
           end;
         if left.resultdef.size<=4 then
           helpername:=helpername+'int'
         else
           helpername:=helpername+'long';
          result:=ccallnode.createintern(helpername,ccallparanode.create(
            genintconstnode(left.resultdef.size),ccallparanode.create(genintconstnode(tsetdef(left.resultdef).setbase),
            ccallparanode.create(ctypeconvnode.create_explicit(left,setconvdef),nil))));
          left:=nil;
        end;

      function to_set_explicit_typecast: tnode;
        var
          enumclassdef: tobjectdef;
          mp: tnode;
          helpername: string;
        begin
          if tsetdef(resultdef).elementdef.typ=enumdef then
            begin
              inserttypeconv_explicit(left,s64inttype);
              enumclassdef:=tcpuenumdef(tenumdef(tsetdef(resultdef).elementdef).getbasedef).classdef;
              mp:=cloadvmtaddrnode.create(ctypenode.create(enumclassdef));
              helpername:='fpcLongToEnumSet';
              { enumclass.fpcLongToEnumSet(left,setbase,setsize) }
              result:=ccallnode.createinternmethod(mp,helpername,
                ccallparanode.create(genintconstnode(resultdef.size),
                  ccallparanode.create(genintconstnode(tsetdef(resultdef).setbase),
                    ccallparanode.create(left,nil))));
            end
          else
            begin
              if left.resultdef.size<=4 then
                begin
                  helpername:='fpc_int_to_bitset';
                  inserttypeconv_explicit(left,s32inttype);
                end
              else
                begin
                  helpername:='fpc_long_to_bitset';
                  inserttypeconv_explicit(left,s64inttype);
                end;
              result:=ccallnode.createintern(helpername,
                ccallparanode.create(genintconstnode(resultdef.size),
                  ccallparanode.create(genintconstnode(tsetdef(resultdef).setbase),
                    ccallparanode.create(left,nil))));
            end;
        end;

      function procvar_to_procvar(fromdef, todef: tdef): tnode;
        var
          fsym: tsym;
        begin
          result:=nil;
          if fromdef=todef then
            exit;
          fsym:=tfieldvarsym(search_struct_member(tcpuprocvardef(fromdef).classdef,'METHOD'));
          if not assigned(fsym) or
             (fsym.typ<>fieldvarsym) then
            internalerror(2011072414);
          { can either be a procvar or a procvarclass }
          if fromdef.typ=procvardef then
            begin
              left:=ctypeconvnode.create_explicit(left,tcpuprocvardef(fromdef).classdef);
              include(left.flags,nf_load_procvar);
              typecheckpass(left);
            end;
          result:=csubscriptnode.create(fsym,left);
          { create destination procvartype with info from source }
          result:=ccallnode.createinternmethod(
            cloadvmtaddrnode.create(ctypenode.create(tcpuprocvardef(todef).classdef)),
            'CREATE',ccallparanode.create(result,nil));
          left:=nil;
        end;

      function procvar_to_tmethod(fromdef, todef: tdef): tnode;
        var
          fsym: tsym;
        begin
          { must be procedure-of-object -> implicit pointer type -> get address
            before typecasting to corresponding classdef }
          left:=caddrnode.create_internal(left);
          inserttypeconv_explicit(left,tcpuprocvardef(fromdef).classdef);
          fsym:=tfieldvarsym(search_struct_member(tcpuprocvardef(fromdef).classdef,'METHOD'));
          if not assigned(fsym) or
             (fsym.typ<>fieldvarsym) then
            internalerror(2011072414);
          result:=csubscriptnode.create(fsym,left);
          left:=nil;
        end;

      function tmethod_to_procvar(fromdef, todef: tdef): tnode;
        var
          fsym: tsym;
        begin
          fsym:=tfieldvarsym(search_struct_member(tcpuprocvardef(todef).classdef,'METHOD'));
          if not assigned(fsym) or
             (fsym.typ<>fieldvarsym) then
            internalerror(2011072415);
          result:=ccallnode.createinternmethod(cloadvmtaddrnode.create(ctypenode.create(tcpuprocvardef(todef).classdef)),
            'CREATE',ccallparanode.create(left,nil));
          left:=nil;
        end;

      function ptr_no_typecheck_required(fromdef, todef: tdef): boolean;

        function check_type_equality(def1,def2: tdef): boolean;
          begin
            result:=true;
            if is_ansistring(def1) and
               (def2=java_ansistring) then
              exit;
            if is_wide_or_unicode_string(def1) and
               (def2=java_jlstring) then
              exit;
            if def1.typ=pointerdef then
              begin
                if is_shortstring(tpointerdef(def1).pointeddef) and
                   (def2=java_shortstring) then
                  exit;
                { pointer-to-set to JUEnumSet/JUBitSet }
                if (tpointerdef(def1).pointeddef.typ=setdef) then
                  begin
                    if not assigned(tsetdef(tpointerdef(def1).pointeddef).elementdef) then
                      begin
                        if (def2=java_jubitset) or
                           (def2=java_juenumset) then
                          exit;
                      end
                    else if tsetdef(tpointerdef(def1).pointeddef).elementdef.typ=enumdef then
                      begin
                        if def2=java_juenumset then
                          exit;
                      end
                    else if def2=java_jubitset then
                      exit;
                  end;
              end;
            result:=false;
          end;

        function check_array_type_equality(def1,def2: tdef): boolean;
          begin
            result:=true;
            if is_shortstring(def1) and
               (def2=java_shortstring) then
              exit;
            result:=false;
          end;

        begin
          result:=true;
          { check procvar conversion compatibility via their classes }
          if fromdef.typ=procvardef then
            fromdef:=tcpuprocvardef(fromdef).classdef;
          if todef.typ=procvardef then
            todef:=tcpuprocvardef(todef).classdef;
          if (todef=java_jlobject) or
             (todef=voidpointertype) then
            exit;
          if compare_defs(fromdef,todef,nothingn)>=te_equal then
            exit;
          { trecorddef.is_related() must work for inheritance/method checking,
            but do not allow records to be directly typecasted into class/
            pointer types (you have to use FpcBaseRecordType(@rec) instead) }
          if not is_record(fromdef) and
             def_is_related(fromdef,todef) then
            exit;
          if check_type_equality(fromdef,todef) then
            exit;
          if check_type_equality(todef,fromdef) then
            exit;
          if (fromdef.typ=pointerdef) and
             (tpointerdef(fromdef).pointeddef.typ=recorddef) and
             (todef=java_fpcbaserecordtype) then
            exit;
          { all classrefs are currently java.lang.Class at the bytecode level }
          if (fromdef.typ=classrefdef) and
             (todef.typ=objectdef) and
             (todef=search_system_type('JLCLASS').typedef) then
            exit;
          if (fromdef.typ=classrefdef) and
             (todef.typ=classrefdef) and
             def_is_related(tclassrefdef(fromdef).pointeddef,tclassrefdef(todef).pointeddef) then
            exit;
          { special case: "array of shortstring" to "array of ShortstringClass"
            and "array of <record>" to "array of FpcRecordBaseType" (normally
            you have to use ShortstringClass(@shortstrvar) etc, but that's not
            possible in case of passing arrays to e.g. setlength) }
          if is_dynamic_array(left.resultdef) and
             is_dynamic_array(resultdef) then
            begin
             if check_array_type_equality(fromdef,todef) or
                check_array_type_equality(todef,fromdef) then
               exit;
             if is_record(fromdef) and
                (todef=java_fpcbaserecordtype) then
               exit;
            end;
          result:=false;
        end;

      var
        fromclasscompatible,
        toclasscompatible: boolean;
        fromdef,
        todef: tdef;
        fromarrtype,
        toarrtype: char;
      begin
        resnode:=nil;
        if not(convtype in [tc_equal,tc_int_2_int,tc_int_2_bool,tc_bool_2_int,tc_class_2_intf]) or
           ((convtype in [tc_equal,tc_int_2_int,tc_bool_2_int,tc_int_2_bool]) and
            ((left.resultdef.typ=orddef) and
             (resultdef.typ=orddef))) then
          begin
            result:=false;
            exit
          end;
        { This routine is only called for explicit typeconversions of same-sized
          entities that aren't handled by normal type conversions -> bit pattern
          reinterpretations. In the JVM, many of these also need special
          handling because of the type safety. }

        { don't allow conversions between object-based and non-object-based
          types }
        fromclasscompatible:=
          (left.resultdef.typ=formaldef) or
          (left.resultdef.typ=pointerdef) or
          is_java_class_or_interface(left.resultdef) or
          is_dynamic_array(left.resultdef) or
          ((left.resultdef.typ in [stringdef,classrefdef]) and
           not is_shortstring(left.resultdef)) or
          (left.resultdef.typ=enumdef) or
          { procvar2procvar needs special handling }
          ((left.resultdef.typ=procvardef) and
           tprocvardef(left.resultdef).is_addressonly and
           (resultdef.typ<>procvardef));
        toclasscompatible:=
          (resultdef.typ=pointerdef) or
          is_java_class_or_interface(resultdef) or
          is_dynamic_array(resultdef) or
          ((resultdef.typ in [stringdef,classrefdef]) and
           not is_shortstring(resultdef)) or
          (resultdef.typ=enumdef) or
          ((resultdef.typ=procvardef) and
           tprocvardef(resultdef).is_addressonly);
        { typescasts from void (the result of untyped_ptr^) to an implicit
          pointertype (record, array, ...) also needs a typecheck }
        if is_void(left.resultdef) and
           jvmimplicitpointertype(resultdef) then
          begin
            fromclasscompatible:=true;
            toclasscompatible:=true;
          end;

        if fromclasscompatible and toclasscompatible then
          begin
            { we need an as-node to check the validity of the conversion (since
              it wasn't handled by another type conversion, we know it can't
              have been valid normally)

              Exceptions: (most nested) destination is
                * java.lang.Object, since everything is compatible with that type
                * related to source
                * a primitive that are represented by the same type in Java
                  (e.g., byte and shortint) }

            { in case of arrays, check the compatibility of the innermost types }
            fromdef:=left.resultdef;
            todef:=resultdef;
            get_most_nested_types(fromdef,todef);
            { in case of enums, get the equivalent class definitions }
            if (fromdef.typ=enumdef) then
              fromdef:=tenumdef(fromdef).getbasedef;
            if (todef.typ=enumdef) then
              todef:=tenumdef(todef).getbasedef;
            fromarrtype:=jvmarrtype_setlength(fromdef);
            toarrtype:=jvmarrtype_setlength(todef);
            if not ptr_no_typecheck_required(fromdef,todef) then
              begin
                if (fromarrtype in ['A','R','T','E','L','P']) or
                   (fromarrtype<>toarrtype) then
                  begin
                    if not check_only and
                       not assignment_side then
                      begin
                        resnode:=ctypenode.create(resultdef);
                        if resultdef.typ=objectdef then
                          resnode:=cloadvmtaddrnode.create(resnode);
                        resnode:=casnode.create_internal(left,resnode);
                        if resultdef.typ=classrefdef then
                          tjvmasnode(resnode).classreftypecast:=true;
                        left:=nil;
                      end
                  end
                { typecasting from a child to a parent type on the assignment side
                  will (rightly) mess up the type safety verification of the JVM }
                else if assignment_side then
                  CGMessage(type_e_no_managed_assign_generic_typecast);
              end;
            result:=true;
            exit;
          end;

        { a formaldef can be converted to anything, but not on the assignment
          side }
        if (left.resultdef.typ=formaldef) and
           not assignment_side then
          begin
            if resultdef.typ in [orddef,floatdef] then
              begin
                if not check_only then
                  begin
                    resnode:=cinlinenode.create(in_unbox_x_y,false,
                      ccallparanode.create(ctypenode.create(resultdef),
                        ccallparanode.create(left,nil)));
                    left:=nil;
                  end;
                result:=true;
                exit;
              end
            else if jvmimplicitpointertype(resultdef) then
              begin
                { typecast formaldef to pointer to the type, then deref, so that
                  a proper checkcast is inserted }
                if not check_only then
                  begin
                    resnode:=ctypeconvnode.create_explicit(left,getpointerdef(resultdef));
                    resnode:=cderefnode.create(resnode);
                    left:=nil;
                  end;
                result:=true;
                exit;
              end;
            result:=false;
            exit;
          end;

        { procvar to tmethod and vice versa, and procvar to procvar }
        if isvalidprocvartypeconv(left.resultdef,resultdef) then
          begin
            if not check_only then
              begin
                if (left.resultdef.typ=procvardef) and
                   (resultdef.typ=procvardef) then
                  resnode:=procvar_to_procvar(left.resultdef,resultdef)
                else if left.resultdef.typ=procvardef then
                  resnode:=procvar_to_tmethod(left.resultdef,resultdef)
                else
                  resnode:=tmethod_to_procvar(left.resultdef,resultdef);
              end;
            result:=true;
            exit;
          end;

        { don't allow conversions between different classes of primitive types,
          except for a few special cases }

        { float to int/enum explicit type conversion: get the bits }
        if (left.resultdef.typ=floatdef) and
           (is_integer(resultdef) or
            (resultdef.typ=enumdef)) then
          begin
            if not check_only then
              resnode:=int_real_explicit_typecast(tfloatdef(left.resultdef),'FLOATTORAWINTBITS','DOUBLETORAWLONGBITS');
            result:=true;
            exit;
          end;
        { int to float explicit type conversion: also use the bits }
        if (is_integer(left.resultdef) or
            (left.resultdef.typ=enumdef)) and
           (resultdef.typ=floatdef) then
          begin
            if not check_only then
              begin
                if (left.resultdef.typ=enumdef) then
                  inserttypeconv_explicit(left,s32inttype);
                resnode:=int_real_explicit_typecast(tfloatdef(resultdef),'INTBITSTOFLOAT','LONGBITSTODOUBLE');
              end;
            result:=true;
            exit;
          end;

        { enums }
        if (left.resultdef.typ=enumdef) or
           (resultdef.typ=enumdef) then
          begin
            { both enum? }
           if (resultdef.typ=left.resultdef.typ) then
             begin
               { same base type -> nothing special }
               fromdef:=tenumdef(left.resultdef).getbasedef;
               todef:=tenumdef(resultdef).getbasedef;
               if fromdef=todef then
                 begin
                   result:=false;
                   exit;
                 end;
               { convert via ordinal intermediate }
               if not check_only then
                 begin;
                   inserttypeconv_explicit(left,s32inttype);
                   inserttypeconv_explicit(left,resultdef);
                   resnode:=left;
                   left:=nil
                 end;
               result:=true;
               exit;
             end;
           { enum to orddef & vice versa }
           if left.resultdef.typ=orddef then
             begin
               if not check_only then
                 resnode:=ord_enum_explicit_typecast(torddef(left.resultdef),tcpuenumdef(resultdef));
               result:=true;
               exit;
             end
           else if resultdef.typ=orddef then
             begin
               if not check_only then
                 resnode:=enum_ord_explicit_typecast(tcpuenumdef(left.resultdef),torddef(resultdef));
               result:=true;
               exit;
             end
          end;

        { sets }
        if (left.resultdef.typ=setdef) or
           (resultdef.typ=setdef) then
          begin
            { set -> ord/enum/other-set-type }
            if (resultdef.typ in [orddef,enumdef]) then
              begin
                if not check_only then
                  begin
                    resnode:=from_set_explicit_typecast;
                    { convert to desired result }
                    inserttypeconv_explicit(resnode,resultdef);
                  end;
                result:=true;
                exit;
              end
            { ord/enum -> set }
            else if (left.resultdef.typ in [orddef,enumdef]) then
              begin
                if not check_only then
                  begin
                    resnode:=to_set_explicit_typecast;
                    { convert to desired result }
                    inserttypeconv_explicit(resnode,getpointerdef(resultdef));
                    resnode:=cderefnode.create(resnode);
                  end;
                result:=true;
                exit;
              end;
            { if someone needs it, float->set and set->float explicit typecasts
              could also be added (cannot be handled by the above, because
              float(intvalue) will convert rather than re-interpret the value) }
          end;

        { anything not explicitly handled is a problem }
        result:=true;
        CGMessage2(type_e_illegal_type_conversion,left.resultdef.typename,resultdef.typename);
      end;


    function tjvmtypeconvnode.target_specific_explicit_typeconv: boolean;
      var
        dummyres: tnode;
      begin
        result:=do_target_specific_explicit_typeconv(true,dummyres);
      end;



    function tjvmtypeconvnode.target_specific_general_typeconv: boolean;
      begin
        result:=false;
        { on the JVM platform, enums can always be converted to class instances,
          because enums /are/ class instances there. To prevent the
          typechecking/conversion code from assuming it can treat it like any
          ordinal constant, firstpass() it so that the ordinal constant gets
          replaced with a load of a staticvarsym. This is not done in
          pass_typecheck, because that would prevent many optimizations }
        if (left.nodetype=ordconstn) and
           (left.resultdef.typ=enumdef) and
           (resultdef.typ=objectdef) then
          firstpass(left);
      end;


    {*****************************************************************************
                         AsNode and IsNode common helpers
    *****************************************************************************}

  function asis_target_specific_typecheck(node: tasisnode): boolean;
    var
      realtodef: tdef;
      temp: tnode;
    begin
      { the JVM supports loadvmtaddrnodes for interface types, but the generic
        as/is code doesn't -> convert such loadvmtaddrnodes back to plain
        type nodes here (they only make sense in the context of treating them
        as entities loaded to store into e.g. a JLClass) }
      if (node.right.resultdef.typ=classrefdef) and
         is_javainterface(tclassrefdef(node.right.resultdef).pointeddef) and
         (node.right.nodetype=loadvmtaddrn) and
         (tloadvmtaddrnode(node.right).left.nodetype=typen) then
        begin
          temp:=tloadvmtaddrnode(node.right).left;
          tloadvmtaddrnode(node.right).left:=nil;
          node.right.free;
          node.right:=temp;
        end;

      if not(nf_internal in node.flags) then
        begin
          { handle using normal code }
          result:=false;
          exit;
        end;
      result:=true;
      { these are converted type conversion nodes, to insert the checkcast
        operations }
      realtodef:=node.right.resultdef;
      if (realtodef.typ=classrefdef) and
         ((node.nodetype<>asn) or
          not tjvmasnode(node).classreftypecast) then
        realtodef:=tclassrefdef(realtodef).pointeddef;
      realtodef:=maybe_find_real_class_definition(realtodef,false);
      if result then
        if node.nodetype=asn then
          node.resultdef:=realtodef
        else
          node.resultdef:=pasbool8type;
    end;


  function asis_pass_1(node: tasisnode; const methodname: string): tnode;
    var
      ps: tsym;
      call: tnode;
      jlclass: tobjectdef;
    begin
      result:=nil;
      firstpass(node.left);
      if not(node.right.nodetype in [typen,loadvmtaddrn]) then
        begin
          if (node.nodetype=isn) or
             not assigned(tasnode(node).call) then
            begin
              if not is_javaclassref(node.right.resultdef) then
                internalerror(2011041920);
              firstpass(node.right);
              jlclass:=tobjectdef(search_system_type('JLCLASS').typedef);
              ps:=search_struct_member(jlclass,methodname);
              if not assigned(ps) or
                 (ps.typ<>procsym) then
                internalerror(2011041910);
              call:=ccallnode.create(ccallparanode.create(node.left,nil),tprocsym(ps),ps.owner,ctypeconvnode.create_explicit(node.right,jlclass),[]);
              node.left:=nil;
              node.right:=nil;
              firstpass(call);
              if codegenerror then
                exit;
              if node.nodetype=isn then
                result:=call
              else
                begin
                  tasnode(node).call:=call;
                  node.expectloc:=call.expectloc;
                end;
            end;
        end
      else
        begin
          node.expectloc:=LOC_REGISTER;
          result:=nil;
        end;
    end;


  function asis_generate_code(node: tasisnode; opcode: tasmop): boolean;
    var
      checkdef: tdef;
    begin
      if (node.nodetype=asn) and
         assigned(tasnode(node).call) then
        begin
          result:=false;
          exit;
        end;
      result:=true;
      secondpass(node.left);
      thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,node.left.resultdef,node.left.location);
      location_freetemp(current_asmdata.CurrAsmList,node.left.location);
      { Perform a checkcast instruction, which will raise an exception in case
        the actual type does not match/inherit from the expected type.

        Object types need the full type name (package+class name), arrays only
        the array definition }
      if node.nodetype=asn then
        checkdef:=node.resultdef
      else if node.right.resultdef.typ=classrefdef then
        checkdef:=tclassrefdef(node.right.resultdef).pointeddef
      else
        checkdef:=node.right.resultdef;
      thlcgjvm(hlcg).gen_typecheck(current_asmdata.CurrAsmList,opcode,checkdef);
      location_reset(node.location,LOC_REGISTER,OS_ADDR);
      node.location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,node.resultdef);
      thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,node.resultdef,node.location.register);
    end;

    {*****************************************************************************
                                 TJVMAsNode
    *****************************************************************************}

  function tjvmasnode.target_specific_typecheck: boolean;
    begin
      result:=asis_target_specific_typecheck(self);
    end;


  function tjvmasnode.pass_1: tnode;
    begin
      result:=asis_pass_1(self,'CAST');
    end;


  procedure tjvmasnode.pass_generate_code;
    begin
      if not asis_generate_code(self,a_checkcast) then
        inherited;
    end;


  function tjvmasnode.dogetcopy: tnode;
    begin
      result:=inherited dogetcopy;
      tjvmasnode(result).classreftypecast:=classreftypecast;
    end;


  function tjvmasnode.docompare(p: tnode): boolean;
    begin
      result:=
        inherited docompare(p) and
        (tjvmasnode(p).classreftypecast=classreftypecast);
    end;


  constructor tjvmasnode.ppuload(t: tnodetype; ppufile: tcompilerppufile);
    begin
      inherited;
      classreftypecast:=boolean(ppufile.getbyte);
    end;


  procedure tjvmasnode.ppuwrite(ppufile: tcompilerppufile);
    begin
      inherited ppuwrite(ppufile);
      ppufile.putbyte(byte(classreftypecast));
    end;


  {*****************************************************************************
                               TJVMIsNode
  *****************************************************************************}


  function tjvmisnode.target_specific_typecheck: boolean;
    begin
      result:=asis_target_specific_typecheck(self);
    end;


  function tjvmisnode.pass_1: tnode;
    begin
      result:=asis_pass_1(self,'ISINSTANCE');
    end;


  procedure tjvmisnode.pass_generate_code;
    begin
      if not asis_generate_code(self,a_instanceof) then
        inherited;
    end;




begin
  ctypeconvnode:=tjvmtypeconvnode;
  casnode:=tjvmasnode;
  cisnode:=tjvmisnode;
end.
