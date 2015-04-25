{
    Copyright (c) 2013 by Jonas Maebe

    This unit implements some LLVM type helper routines.

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

{$i fpcdefs.inc}

unit llvmdef;

interface

    uses
      cclasses,globtype,
      aasmbase,
      parabase,
      symbase,symtype,symdef,
      llvmbase;

   type
     { there are three different circumstances in which procdefs are used:
        a) definition of a procdef that's implemented in the current module or
           declaration of an external routine that's called in the current one
        b) alias declaration of a procdef implemented in the current module
        c) defining a procvar type
       The main differences between the contexts are:
        a) information about sign extension of result type, proc name, parameter names & sign-extension info & types
        b) no information about sign extension of result type, proc name, no parameter names, no information about sign extension of parameters, parameter types
        c) no information about sign extension of result type, no proc name, no parameter names, no information about sign extension of parameters, parameter types
      }
     tllvmprocdefdecltype = (lpd_decl,lpd_alias,lpd_procvar);

    { returns the identifier to use as typename for a def in llvm (llvm only
      allows naming struct types) -- only supported for defs with a typesym, and
      only for tabstractrecorddef descendantds and complex procvars }
    function llvmtypeidentifier(def: tdef): TSymStr;

    { encode a type into the internal format used by LLVM (for a type
      declaration) }
    function llvmencodetypedecl(def: tdef): TSymStr;

    { same as above, but use a type name if possible (for any use) }
    function llvmencodetypename(def: tdef): TSymStr;

    { encode a procdef/procvardef into the internal format used by LLVM }
    function llvmencodeproctype(def: tabstractprocdef; const customname: TSymStr; pddecltype: tllvmprocdefdecltype): TSymStr;
    { incremental version of the above }
    procedure llvmaddencodedproctype(def: tabstractprocdef; const customname: TSymStr; pddecltype: tllvmprocdefdecltype; var encodedstr: TSymStr);

    { function result types may have to be represented differently, e.g. a
      record consisting of 4 longints must be returned as a record consisting of
      two int64's on x86-64. This function is used to create (and reuse)
      temporary recorddefs for such purposes.}
    function llvmgettemprecorddef(fieldtypes: tfplist; packrecords, recordalignmin, maxcrecordalign: shortint): trecorddef;

    { get the llvm type corresponding to a parameter, e.g. a record containing
      two integer int64 for an arbitrary record split over two individual int64
      parameters, or an int32 for an int16 parameter on a platform that requires
      such parameters to be zero/sign extended. The second parameter can be used
      to get the type before zero/sign extension, as e.g. required to generate
      function declarations. }
    function llvmgetcgparadef(const cgpara: tcgpara; beforevalueext: boolean): tdef;

    { can be used to extract the value extension info from acgpara. Pass in
      the def of the cgpara as first parameter and a local variable holding
      a copy of the def of the location (value extension only makes sense for
      ordinal parameters that are smaller than a single location). The routine
      will return the def of the location without sign extension (if applicable)
      and the kind of sign extension that was originally performed in the
      signext parameter }
    procedure llvmextractvalueextinfo(paradef: tdef; var paralocdef: tdef; out signext: tllvmvalueextension);

    { returns whether a paraloc should be translated into an llvm "byval"
      parameter. These are declared as pointers to a particular type, but
      usually turned into copies onto the stack. The exact behaviour for
      parameters that should be passed in registers is undefined and depends on
      the platform, and furthermore this modifier sometimes inhibits
      optimizations.  As a result,we only use it for aggregate parameters of
      which we know that they should be passed on the stack }
    function llvmbyvalparaloc(paraloc: pcgparalocation): boolean;

    { returns whether a def is representated by an aggregate type in llvm
      (struct, array) }
    function llvmaggregatetype(def: tdef): boolean;

    function llvmconvop(fromsize, tosize: tdef): tllvmop;

    { mangle a global identifier so that it's recognised by LLVM as a global
      (in the sense of module-global) label and so that it won't mangle the
      name further according to platform conventions (we already did that) }
    function llvmmangledname(const s: TSymStr): TSymStr;

    function llvmasmsymname(const sym: TAsmSymbol): TSymStr;


implementation

  uses
    cutils,constexp,
    verbose,systems,
    fmodule,
    symtable,symconst,symsym,
    llvmsym,hlcgobj,
    defutil,cgbase,paramgr;


{******************************************************************
                          Type encoding
*******************************************************************}

  function llvmtypeidentifier(def: tdef): TSymStr;
    begin
      if not assigned(def.typesym) then
        internalerror(2015041901);
      result:='%"typ.'+def.fullownerhierarchyname+'.'+def.typesym.realname+'"'
    end;


  function llvmaggregatetype(def: tdef): boolean;
    begin
      result:=
        (def.typ in [recorddef,filedef,variantdef]) or
        ((def.typ=arraydef) and
         not is_dynamic_array(def)) or
        ((def.typ=setdef) and
         not is_smallset(def)) or
        is_shortstring(def) or
        is_object(def) or
        ((def.typ=procvardef) and
         not tprocvardef(def).is_addressonly)
    end;


  function llvmconvop(fromsize, tosize: tdef): tllvmop;
    var
      fromregtyp,
      toregtyp: tregistertype;
      frombytesize,
      tobytesize: asizeint;
    begin
      fromregtyp:=chlcgobj.def2regtyp(fromsize);
      toregtyp:=chlcgobj.def2regtyp(tosize);
      { int to pointer or vice versa }
      if fromregtyp=R_ADDRESSREGISTER then
        begin
          case toregtyp of
            R_INTREGISTER:
              result:=la_ptrtoint;
            R_ADDRESSREGISTER:
              result:=la_bitcast;
            else
              result:=la_ptrtoint_to_x;
            end;
        end
      else if toregtyp=R_ADDRESSREGISTER then
        begin
          case fromregtyp of
            R_INTREGISTER:
              result:=la_inttoptr;
            R_ADDRESSREGISTER:
              result:=la_bitcast;
            else
              result:=la_x_to_inttoptr;
            end;
        end
      else
        begin
          frombytesize:=fromsize.size;
          tobytesize:=tosize.size;
          { need zero/sign extension, float truncation or plain bitcast? }
          if tobytesize<>frombytesize then
            begin
              case fromregtyp of
                R_FPUREGISTER,
                R_MMREGISTER:
                  begin
                    { todo: update once we support vectors }
                    if not(toregtyp in [R_FPUREGISTER,R_MMREGISTER]) then
                      internalerror(2014062203);
                    if tobytesize<frombytesize then
                      result:=la_fptrunc
                    else
                      result:=la_fpext
                  end;
                else
                  begin
                    if tobytesize<frombytesize then
                      result:=la_trunc
                    else if is_signed(fromsize) then
                      { fromsize is signed -> sign extension }
                      result:=la_sext
                    else
                      result:=la_zext;
                  end;
              end;
            end
          else
            result:=la_bitcast;
        end;
    end;


  function llvmmangledname(const s: TSymStr): TSymStr;
    begin
      result:='@"\01'+s+'"';
    end;

  function llvmasmsymname(const sym: TAsmSymbol): TSymStr;
    begin
      { AT_ADDR and AT_LABEL represent labels in the code, which have
        a different type in llvm compared to (global) data labels }
      if sym.bind=AB_TEMP then
        result:='%'+sym.name
      else if not(sym.typ in [AT_LABEL,AT_ADDR]) then
        result:=llvmmangledname(sym.name)
      else
        result:='label %'+sym.name;
    end;


  function llvmbyvalparaloc(paraloc: pcgparalocation): boolean;
    begin
      { "byval" is broken for register paras on several platforms in llvm
        (search for "byval" in llvm's bug tracker). Additionally, it should only
        be used to pass aggregate parameters on the stack, because it reportedly
        inhibits llvm's midlevel optimizers.

        Exception (for now?): parameters that have special shifting
          requirements, because modelling those in llvm is not easy (and clang
          nor llvm-gcc seem to do so either) }
      result:=
        ((paraloc^.loc=LOC_REFERENCE) and
         llvmaggregatetype(paraloc^.def)) or
        (paraloc^.shiftval<>0)
    end;


  procedure llvmaddencodedabstractrecordtype(def: tabstractrecorddef; var encodedstr: TSymStr); forward;

  type
    tllvmencodeflag = (lef_inaggregate, lef_noimplicitderef, lef_typedecl);
    tllvmencodeflags = set of tllvmencodeflag;

    procedure llvmaddencodedtype_intern(def: tdef; const flags: tllvmencodeflags; var encodedstr: TSymStr);
      begin
        case def.typ of
          stringdef :
            begin
              case tstringdef(def).stringtype of
                st_widestring,
                st_unicodestring:
                  { the variable does not point to the header, but to a
                    null-terminated string/array with undefined bounds }
                  encodedstr:=encodedstr+'i16*';
                st_ansistring:
                  encodedstr:=encodedstr+'i8*';
                st_shortstring:
                  { length byte followed by string bytes }
                  if tstringdef(def).len>0 then
                    encodedstr:=encodedstr+'['+tostr(tstringdef(def).len+1)+' x i8]'
                  else
                    encodedstr:=encodedstr+'[0 x i8]';
                else
                  internalerror(2013100201);
              end;
            end;
          enumdef:
            begin
              encodedstr:=encodedstr+'i'+tostr(def.size*8);
            end;
          orddef :
            begin
              if is_void(def) then
                encodedstr:=encodedstr+'void'
              { mainly required because comparison operations return i1, and
                otherwise we always have to immediatel extend them to i8 for
                no good reason; besides, Pascal booleans can only contain 0
                or 1 in valid code anyway (famous last words...) }
              else if torddef(def).ordtype=pasbool8 then
                encodedstr:=encodedstr+'i1'
              else
                encodedstr:=encodedstr+'i'+tostr(def.size*8);
            end;
          pointerdef :
            begin
              if is_voidpointer(def) then
                encodedstr:=encodedstr+'i8*'
              else
                begin
                  llvmaddencodedtype_intern(tpointerdef(def).pointeddef,[],encodedstr);
                  encodedstr:=encodedstr+'*';
                end;
            end;
          floatdef :
            begin
              case tfloatdef(def).floattype of
                s32real:
                  encodedstr:=encodedstr+'float';
                s64real:
                  encodedstr:=encodedstr+'double';
                { necessary to be able to force our own size/alignment }
                s80real:
                  { prevent llvm from allocating the standard ABI size for
                    extended }
                  if lef_inaggregate in flags then
                    encodedstr:=encodedstr+'[10 x i8]'
                  else
                    encodedstr:=encodedstr+'x86_fp80';
                sc80real:
                  encodedstr:=encodedstr+'x86_fp80';
                s64comp,
                s64currency:
                  encodedstr:=encodedstr+'i64';
                s128real:
{$if defined(powerpc) or defined(powerpc128)}
                  encodedstr:=encodedstr+'ppc_fp128';
{$else}
                  encodedstr:=encodedstr+'fp128';
{$endif}
                else
                  internalerror(2013100202);
              end;
            end;
          filedef :
            begin
              case tfiledef(def).filetyp of
                ft_text    :
                  llvmaddencodedtype_intern(search_system_type('TEXTREC').typedef,[lef_inaggregate]+[lef_typedecl]*flags,encodedstr);
                ft_typed,
                ft_untyped :
                  llvmaddencodedtype_intern(search_system_type('FILEREC').typedef,[lef_inaggregate]+[lef_typedecl]*flags,encodedstr);
                else
                  internalerror(2013100203);
              end;
            end;
          recorddef :
            begin
              { avoid endlessly recursive definitions }
              if assigned(def.typesym) and
                 ((lef_inaggregate in flags) or
                  not(lef_typedecl in flags)) then
                encodedstr:=encodedstr+llvmtypeidentifier(def)
              else
                llvmaddencodedabstractrecordtype(trecorddef(def),encodedstr);
            end;
          variantdef :
            begin
              llvmaddencodedtype_intern(search_system_type('TVARDATA').typedef,[lef_inaggregate]+[lef_typedecl]*flags,encodedstr);
            end;
          classrefdef :
            begin
              { todo: define proper type for VMT and use that  }
              encodedstr:=encodedstr+'i8*';
            end;
          setdef :
            begin
              { just an array as far as llvm is concerned; don't use a "packed
                array of i1" or so, this requires special support in backends
                and guarantees nothing about the internal format }
              if is_smallset(def) then
                llvmaddencodedtype_intern(cgsize_orddef(def_cgsize(def)),[lef_inaggregate],encodedstr)
              else
                encodedstr:=encodedstr+'['+tostr(tsetdef(def).size)+' x i8]';
            end;
          formaldef :
            begin
              { var/const/out x (always treated as "pass by reference" -> don't
                add extra "*" here) }
              encodedstr:=encodedstr+'i8';
            end;
          arraydef :
            begin
              if is_array_of_const(def) then
                begin
                  encodedstr:=encodedstr+'[0 x ';
                  llvmaddencodedtype_intern(search_system_type('TVARREC').typedef,[lef_inaggregate],encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else if is_open_array(def) then
                begin
                  encodedstr:=encodedstr+'[0 x ';
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,[lef_inaggregate],encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else if is_dynamic_array(def) then
                begin
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,[],encodedstr);
                  encodedstr:=encodedstr+'*';
                end
              else if is_packed_array(def) then
                begin
                  encodedstr:=encodedstr+'['+tostr(tarraydef(def).size div tarraydef(def).elementdef.packedbitsize)+' x ';
                  { encode as an array of integers with the size on which we
                    perform the packedbits operations }
                  llvmaddencodedtype_intern(cgsize_orddef(int_cgsize(packedbitsloadsize(tarraydef(def).elementdef.packedbitsize))),[lef_inaggregate],encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else
                begin
                  encodedstr:=encodedstr+'['+tostr(tarraydef(def).elecount)+' x ';
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,[lef_inaggregate],encodedstr);
                  encodedstr:=encodedstr+']';
                end;
            end;
          procdef,
          procvardef :
            begin
              if (def.typ=procdef) or
                 tprocvardef(def).is_addressonly then
                begin
                  llvmaddencodedproctype(tabstractprocdef(def),'',lpd_procvar,encodedstr);
                  if def.typ=procvardef then
                    encodedstr:=encodedstr+'*';
                end
              else if ((lef_inaggregate in flags) or
                  not(lef_typedecl in flags)) and
                 assigned(tprocvardef(def).typesym) then
                begin
                  { in case the procvardef recursively references itself, e.g.
                    via a pointer }
                  encodedstr:=encodedstr+llvmtypeidentifier(def)
                end
              else
                begin
                  encodedstr:=encodedstr+'{';
                  { code pointer }
                  llvmaddencodedproctype(tabstractprocdef(def),'',lpd_procvar,encodedstr);
                  { data pointer (maybe todo: generate actual layout if
                    available) }
                  encodedstr:=encodedstr+'*, i8*}';
                end;
            end;
          objectdef :
            case tobjectdef(def).objecttype of
              odt_class,
              odt_objcclass,
              odt_object,
              odt_cppclass:
                begin
                  if not(lef_typedecl in flags) and
                     assigned(def.typesym) then
                    encodedstr:=encodedstr+llvmtypeidentifier(def)
                  else
                    llvmaddencodedabstractrecordtype(tabstractrecorddef(def),encodedstr);
                  if ([lef_typedecl,lef_noimplicitderef]*flags=[]) and
                     is_implicit_pointer_object_type(def) then
                    encodedstr:=encodedstr+'*'
                end;
              odt_interfacecom,
              odt_interfacecom_function,
              odt_interfacecom_property,
              odt_interfacecorba,
              odt_dispinterface,
              odt_objcprotocol:
                begin
                  { opaque for now }
                  encodedstr:=encodedstr+'i8*'
                end;
              else
                internalerror(2013100601);
            end;
          undefineddef,
          errordef :
            internalerror(2013100604);
        else
          internalerror(2013100603);
        end;
      end;


    function llvmencodetypename(def: tdef): TSymStr;
      begin
        result:='';
        llvmaddencodedtype_intern(def,[],result);
      end;


    procedure llvmaddencodedtype(def: tdef; inaggregate: boolean; var encodedstr: TSymStr);
      var
        flags: tllvmencodeflags;
      begin
        if inaggregate then
          flags:=[lef_inaggregate]
        else
          flags:=[];
        llvmaddencodedtype_intern(def,flags,encodedstr);
      end;


    procedure llvmaddencodedabstractrecordtype(def: tabstractrecorddef; var encodedstr: TSymStr);
      var
        st: tllvmshadowsymtable;
        symdeflist: tfpobjectlist;
        i: longint;
      begin
        st:=tabstractrecordsymtable(def.symtable).llvmst;
        symdeflist:=st.symdeflist;

        if tabstractrecordsymtable(def.symtable).usefieldalignment<>C_alignment then
          encodedstr:=encodedstr+'<';
        encodedstr:=encodedstr+'{ ';
        if symdeflist.count>0 then
          begin
            i:=0;
            if (def.typ=objectdef) and
               assigned(tobjectdef(def).childof) and
               is_class_or_interface_or_dispinterface(tllvmshadowsymtableentry(symdeflist[0]).def) then
              begin
                { insert the struct for the class rather than a pointer to the struct }
                if (tllvmshadowsymtableentry(symdeflist[0]).def.typ<>objectdef) then
                  internalerror(2008070601);
                llvmaddencodedtype_intern(tllvmshadowsymtableentry(symdeflist[0]).def,[lef_inaggregate,lef_noimplicitderef],encodedstr);
                inc(i);
              end;
            while i<symdeflist.count do
              begin
                if i<>0 then
                  encodedstr:=encodedstr+', ';
                llvmaddencodedtype_intern(tllvmshadowsymtableentry(symdeflist[i]).def,[lef_inaggregate],encodedstr);
                inc(i);
              end;
          end;
        encodedstr:=encodedstr+' }';
        if tabstractrecordsymtable(def.symtable).usefieldalignment<>C_alignment then
          encodedstr:=encodedstr+'>';
      end;


    procedure llvmextractvalueextinfo(paradef: tdef; var paralocdef: tdef; out signext: tllvmvalueextension);
      begin
        { implicit zero/sign extension for ABI compliance? (yes, if the size
          of a paraloc is larger than the size of the entire parameter) }
        if is_ordinal(paradef) and
           is_ordinal(paralocdef) and
           (paradef.size<paralocdef.size) then
          begin
            paralocdef:=paradef;
            if is_signed(paradef) then
              signext:=lve_signext
            else
              signext:=lve_zeroext
          end
        else
          signext:=lve_none;
      end;


    procedure llvmaddencodedparaloctype(hp: tparavarsym; proccalloption: tproccalloption; withparaname, withattributes: boolean; var first: boolean; var encodedstr: TSymStr);
      var
        paraloc: PCGParaLocation;
        signext: tllvmvalueextension;
        usedef: tdef;
      begin
        if (proccalloption in cdecl_pocalls) and
           is_array_of_const(hp.vardef) then
          begin
            if not first then
               encodedstr:=encodedstr+', '
            else
              first:=false;
            encodedstr:=encodedstr+'...';
            exit
          end;
        paraloc:=hp.paraloc[calleeside].location;
        repeat
          usedef:=paraloc^.def;
          llvmextractvalueextinfo(hp.vardef,usedef,signext);
          { implicit zero/sign extension for ABI compliance? }
          if not first then
             encodedstr:=encodedstr+', '
          else
            first:=false;
          llvmaddencodedtype_intern(usedef,[lef_inaggregate],encodedstr);
          { in case signextstr<>'', there should be only one paraloc -> no need
            to clear (reason: it means that the paraloc is larger than the
            original parameter) }
          if withattributes then
            encodedstr:=encodedstr+llvmvalueextension2str[signext];
          { sret: hidden pointer for structured function result }
          if vo_is_funcret in hp.varoptions then
            begin
              if withattributes then
                encodedstr:=encodedstr+' sret'
            end
          else if not paramanager.push_addr_param(hp.varspez,hp.vardef,proccalloption) and
             llvmbyvalparaloc(paraloc) then
            begin
              if withattributes then
                encodedstr:=encodedstr+'* byval'
              else
                encodedstr:=encodedstr+'*';
            end;
          if withparaname then
            begin
              if paraloc^.llvmloc.loc<>LOC_REFERENCE then
                internalerror(2014010803);
              encodedstr:=encodedstr+' '+llvmasmsymname(paraloc^.llvmloc.sym);
            end;
          paraloc:=paraloc^.next;
        until not assigned(paraloc);
      end;


    function llvmencodeproctype(def: tabstractprocdef; const customname: TSymStr; pddecltype: tllvmprocdefdecltype): TSymStr;
      begin
        result:='';
        llvmaddencodedproctype(def,customname,pddecltype,result);
      end;


    procedure llvmaddencodedproctype(def: tabstractprocdef; const customname: TSymStr; pddecltype: tllvmprocdefdecltype; var encodedstr: TSymStr);
      var
        usedef: tdef;
        paranr: longint;
        hp: tparavarsym;
        signext: tllvmvalueextension;
        first: boolean;
      begin
        def.init_paraloc_info(calleeside);
        first:=true;
        { function result (return-by-ref is handled explicitly) }
        if not paramanager.ret_in_param(def.returndef,def) then
          begin
            usedef:=llvmgetcgparadef(def.funcretloc[calleeside],false);
            llvmextractvalueextinfo(def.returndef,usedef,signext);
            { specifying result sign extention information for an alias causes
              an error for some reason }
            if pddecltype in [lpd_decl] then
              encodedstr:=encodedstr+llvmvalueextension2str[signext];
            encodedstr:=encodedstr+' ';
            llvmaddencodedtype_intern(usedef,[lef_inaggregate],encodedstr);
          end
        else
          begin
            encodedstr:=encodedstr+' ';
            llvmaddencodedtype(voidtype,false,encodedstr);
          end;
        encodedstr:=encodedstr+' ';
        { add procname? }
        if (pddecltype in [lpd_decl]) and
           (def.typ=procdef) then
          if customname='' then
            encodedstr:=encodedstr+llvmmangledname(tprocdef(def).mangledname)
          else
            encodedstr:=encodedstr+llvmmangledname(customname);
        encodedstr:=encodedstr+'(';
        { parameters }
        first:=true;
        for paranr:=0 to def.paras.count-1 do
          begin
            hp:=tparavarsym(def.paras[paranr]);
            llvmaddencodedparaloctype(hp,def.proccalloption,pddecltype in [lpd_decl],not(pddecltype in [lpd_procvar,lpd_alias]),first,encodedstr);
          end;
        if po_varargs in def.procoptions then
          begin
            if not first then
              encodedstr:=encodedstr+', ';
            encodedstr:=encodedstr+'...';
          end;
        encodedstr:=encodedstr+')'
      end;


    function llvmgettemprecorddef(fieldtypes: tfplist; packrecords, recordalignmin, maxcrecordalign: shortint): trecorddef;
      var
        i: longint;
        res: PHashSetItem;
        oldsymtablestack: tsymtablestack;
        hrecst: trecordsymtable;
        hdef: tdef;
        hrecdef: trecorddef;
        sym: tfieldvarsym;
        typename: string;
      begin
        typename:='$llvmstruct_';
        for i:=0 to fieldtypes.count-1 do
          begin
            hdef:=tdef(fieldtypes[i]);
            case hdef.typ of
              orddef:
                case torddef(hdef).ordtype of
                  s8bit,
                  u8bit:
                    typename:=typename+'i8';
                  s16bit,
                  u16bit:
                    typename:=typename+'i16';
                  s32bit,
                  u32bit:
                    typename:=typename+'i32';
                  s64bit,
                  u64bit:
                    typename:=typename+'i64';
                  else
                    { other types should not appear currently, add as needed }
                    internalerror(2014012001);
                end;
              floatdef:
                case tfloatdef(hdef).floattype of
                  s32real:
                    typename:=typename+'f32';
                  s64real:
                    typename:=typename+'f64';
                  else
                    { other types should not appear currently, add as needed }
                    internalerror(2014012008);
                  end;
              else
                { other types should not appear currently, add as needed }
                internalerror(2014012009);
            end;
          end;
        if not assigned(current_module) then
          internalerror(2014012002);
        res:=current_module.llvmdefs.FindOrAdd(@typename[1],length(typename));
        if not assigned(res^.Data) then
          begin
            res^.Data:=crecorddef.create_global_internal(typename,packrecords,
              recordalignmin,maxcrecordalign);
            trecorddef(res^.Data).add_fields_from_deflist(fieldtypes);
          end;
        result:=trecorddef(res^.Data);
      end;


    function llvmgetcgparadef(const cgpara: tcgpara; beforevalueext: boolean): tdef;
      var
        retdeflist: tfplist;
        retloc: pcgparalocation;
        usedef: tdef;
        valueext: tllvmvalueextension;
      begin
        { single location }
        if not assigned(cgpara.location^.next) then
          begin
            { def of the location, except in case of zero/sign-extension }
            usedef:=cgpara.location^.def;
            if beforevalueext then
              llvmextractvalueextinfo(cgpara.def,usedef,valueext);
            result:=usedef;
            exit
          end;
        { multiple locations -> create temp record }
        retdeflist:=tfplist.create;
        retloc:=cgpara.location;
        repeat
          retdeflist.add(retloc^.def);
          retloc:=retloc^.next;
        until not assigned(retloc);
        result:=llvmgettemprecorddef(retdeflist,C_alignment,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
      end;


    function llvmencodetypedecl(def: tdef): TSymStr;
      begin
        result:='';
        llvmaddencodedtype_intern(def,[lef_typedecl],result);
      end;


end.
