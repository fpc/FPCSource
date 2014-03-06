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
      globtype,
      symbase,symtype,symdef;

    { Encode a type into the internal format used by LLVM. }
    function llvmencodetype(def: tdef): TSymStr;

    { incremental version of llvmencodetype(). "inaggregate" indicates whether
      this was a recursive call to get the type of an entity part of an
      aggregate type (array, record, ...) }
    procedure llvmaddencodedtype(def: tdef; inaggregate: boolean; var encodedstr: TSymStr);

    function llvmencodeproctype(def: tabstractprocdef; withprocname, withparanames: boolean): TSymStr;
    procedure llvmaddencodedproctype(def: tabstractprocdef; withprocname, withparanames: boolean; var encodedstr: TSymStr);

    { returns whether a def is representated by an aggregate type in llvm
      (struct, array) }
    function llvmaggregatetype(def: tdef): boolean;


implementation

  uses
    cutils,cclasses,constexp,
    verbose,systems,
    fmodule,
    symtable,symconst,symsym,
    llvmsym,
    defutil,cgbase,parabase,paramgr;

{******************************************************************
                          Type encoding
*******************************************************************}

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


    procedure llvmaddencodedabstractrecordtype(def: tabstractrecorddef; var encodedstr: TSymStr); forward;

    procedure llvmaddencodedtype_intern(def: tdef; inaggregate, noimplicitderef: boolean; var encodedstr: TSymStr);
      begin
        case def.typ of
          stringdef :
            begin
              case tstringdef(def).stringtype of
                st_widestring,
                st_unicodestring:
                  { the variable does not point to the header, but to a
                    null-terminated string/array with undefined bounds }
                  encodedstr:=encodedstr+'[0 x i16]';
                st_ansistring:
                  encodedstr:=encodedstr+'[0 x i8]';
                st_shortstring:
                  { length byte followed by string bytes }
                  if tstringdef(def).len>0 then
                    encodedstr:=encodedstr+'{i8, ['+tostr(tstringdef(def).len)+' x i8]}'
                  else
                    encodedstr:=encodedstr+'{i8, [0 x i8]}';
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
                  llvmaddencodedtype_intern(tpointerdef(def).pointeddef,inaggregate,false,encodedstr);
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
                  if inaggregate then
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
                  llvmaddencodedtype_intern(search_system_type('TEXTREC').typedef,inaggregate,false,encodedstr);
                ft_typed,
                ft_untyped :
                  llvmaddencodedtype_intern(search_system_type('FILEREC').typedef,inaggregate,false,encodedstr);
                else
                  internalerror(2013100203);
              end;
            end;
          recorddef :
            begin
              llvmaddencodedabstractrecordtype(trecorddef(def),encodedstr);
            end;
          variantdef :
            begin
              llvmaddencodedtype_intern(search_system_type('TVARDATA').typedef,inaggregate,false,encodedstr);
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
              encodedstr:=encodedstr+'['+tostr(tsetdef(def).size)+' x i8]';
            end;
          formaldef :
            begin
              { var/const/out x }
              encodedstr:=encodedstr+'i8*';
            end;
          arraydef :
            begin
              if is_array_of_const(def) then
                begin
                  encodedstr:=encodedstr+'[0 x ';
                  llvmaddencodedtype_intern(search_system_type('TVARREC').typedef,true,false,encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else if is_open_array(def) then
                begin
                  encodedstr:=encodedstr+'[0 x ';
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,true,false,encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else if is_dynamic_array(def) then
                begin
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,false,false,encodedstr);
                  encodedstr:=encodedstr+'*';
                end
              else if is_packed_array(def) then
                begin
                  encodedstr:=encodedstr+'['+tostr(tarraydef(def).size div tarraydef(def).elementdef.packedbitsize)+' x ';
                  { encode as an array of integers with the size on which we
                    perform the packedbits operations }
                  llvmaddencodedtype_intern(cgsize_orddef(int_cgsize(packedbitsloadsize(tarraydef(def).elementdef.packedbitsize))),true,false,encodedstr);
                  encodedstr:=encodedstr+']';
                end
              else
                begin
                  encodedstr:=encodedstr+'['+tostr(tarraydef(def).elecount)+' x ';
                  llvmaddencodedtype_intern(tarraydef(def).elementdef,true,false,encodedstr);
                  encodedstr:=encodedstr+']';
                end;
            end;
          procvardef :
            begin
              if tprocvardef(def).is_addressonly then
                begin
                  llvmaddencodedproctype(tprocdef(def),false,false,encodedstr);
                  encodedstr:=encodedstr+'*';
                end
              else
                begin
                  encodedstr:=encodedstr+'{';
                  { code pointer }
                  llvmaddencodedproctype(tprocvardef(def),false,false,encodedstr);
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
                  { for now don't handle fields yet }
                  encodedstr:=encodedstr+'{[i8 x '+tostr(def.size)+']}';
                  if not noimplicitderef and
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
          procdef :
            begin
              llvmaddencodedproctype(tprocdef(def),true,false,encodedstr);
            end;
        else
          internalerror(2013100603);
        end;
      end;


    procedure llvmaddencodedtype(def: tdef; inaggregate: boolean; var encodedstr: TSymStr);
      begin
        llvmaddencodedtype_intern(def,inaggregate,false,encodedstr);
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
                llvmaddencodedtype_intern(tllvmshadowsymtableentry(symdeflist[0]).def,true,true,encodedstr);
                inc(i);
              end;
            while i<symdeflist.count do
              begin
                if i<>0 then
                  encodedstr:=encodedstr+', ';
                llvmaddencodedtype_intern(tllvmshadowsymtableentry(symdeflist[i]).def,true,false,encodedstr);
                inc(i);
              end;
          end;
        encodedstr:=encodedstr+' }';
        if tabstractrecordsymtable(def.symtable).usefieldalignment<>C_alignment then
          encodedstr:=encodedstr+'>';
      end;


    procedure llvmrefineordinaldef(paradef, paralocdef: tdef; out usedef: tdef; out signextstr: TSymStr);
      begin
        { implicit zero/sign extension for ABI compliance? (yes, if the size
          of a paraloc is larger than the size of the entire parameter) }
        if is_ordinal(paradef) and
           is_ordinal(paralocdef) and
           (paradef.size<paralocdef.size) then
          begin
            usedef:=paradef;
            if is_signed(paradef) then
              signextstr:='signext '
            else
              signextstr:='zeroext '
          end
        else
          begin
            usedef:=paralocdef;
            signextstr:='';
          end;
      end;


    procedure llvmaddencodedparaloctype(hp: tparavarsym; const para: tcgpara; proccalloption: tproccalloption; withparaname: boolean; var first: boolean; var encodedstr: TSymStr);

      { the default for llvm is to pass aggregates in integer registers or
        on the stack (as the ABI prescribes). Records that require special
        handling, e.g. (partly) passing in fpu registers, have to be handled
        explicitly. This function returns whether an aggregate is handled
        specially }
      function hasnondefaultparaloc: boolean;
        var
          loc: PCGParaLocation;
        begin
          loc:=para.Location;
          result:=true;
          while assigned(loc) do
            begin
              if not(loc^.loc in [LOC_REGISTER,LOC_REFERENCE]) then
                exit;
            end;
          result:=false;
        end;

      var
        paraloc: PCGParaLocation;
        signextstr: TSymStr;
        usedef: tdef;
        closestruct: boolean;
      begin
        { byval: a pointer to a type that should actually be passed by
            value (e.g. a record that should be passed on the stack) }
         if assigned(hp) and
            (hp.vardef.typ in [arraydef,recorddef,objectdef]) and
            not paramanager.push_addr_param(hp.varspez,hp.vardef,proccalloption) and
            not hasnondefaultparaloc then
          begin
            llvmaddencodedtype(hp.vardef,false,encodedstr);
            encodedstr:=encodedstr+'* byval';
            if withparaname then
              encodedstr:=encodedstr+' '+para.location^.llvmloc.name;
            exit;
          end;

        closestruct:=false;
        paraloc:=para.location;
        if not assigned(hp) then
          begin
            { if a function returns a composite value (e.g. 2 sse register),
              those are represented as a struct }
            if assigned(paraloc^.next) then
              begin
                encodedstr:=encodedstr+'{';
                closestruct:=true;
              end;
          end;
        repeat
          usedef:=paraloc^.def;
          llvmrefineordinaldef(para.def,paraloc^.def,usedef,signextstr);
          { implicit zero/sign extension for ABI compliance? }
          if not assigned(hp) then
            encodedstr:=encodedstr+signextstr;
          if not first then
             encodedstr:=encodedstr+', '
          else
            first:=false;
          llvmaddencodedtype(usedef,false,encodedstr);
          { in case signextstr<>'', there should be only one paraloc -> no need
            to clear (reason: it means that the paraloc is larger than the
            original parameter) }
          if assigned(hp) then
            encodedstr:=encodedstr+signextstr;
          if assigned(hp) then
            begin
              { sret: hidden pointer for structured function result }
              if vo_is_funcret in hp.varoptions then
                encodedstr:=encodedstr+' sret'
            end;
          if withparaname then
            encodedstr:=encodedstr+' '+paraloc^.llvmloc.name;
          paraloc:=paraloc^.next;
        until not assigned(paraloc);
        if closestruct then
          encodedstr:=encodedstr+'}'
      end;


    function llvmencodeproctype(def: tabstractprocdef; withprocname, withparanames: boolean): TSymStr;
      begin
        result:='';
        llvmaddencodedproctype(def,withprocname,withparanames,result);
      end;


    procedure llvmaddencodedproctype(def: tabstractprocdef; withprocname, withparanames: boolean; var encodedstr: TSymStr);
      var
        paranr: longint;
        para: tcgpara;
        hp: tparavarsym;
        first: boolean;
      begin
        def.init_paraloc_info(calleeside);
        first:=true;
        { function result (return-by-ref is handled explicitly) }
        if not paramanager.ret_in_param(def.returndef,def) then
          llvmaddencodedparaloctype(nil,def.funcretloc[calleeside],def.proccalloption,false,first,encodedstr)
        else
          llvmaddencodedtype(voidtype,false,encodedstr);
        encodedstr:=encodedstr+' ';
        if withprocname and
           (def.typ=procdef) then
          encodedstr:=encodedstr+tprocdef(def).mangledname;
        encodedstr:=encodedstr+'(';
        { parameters }
        first:=true;
        for paranr:=0 to def.paras.count-1 do
          begin
            hp:=tparavarsym(def.paras[paranr]);
            llvmaddencodedparaloctype(hp,hp.paraloc[calleeside],def.proccalloption,withparanames,first,encodedstr);
          end;
        encodedstr:=encodedstr+')'
      end;


    function llvmencodetype(def: tdef): TSymStr;
      begin
        result:='';
        llvmaddencodedtype(def,false,result);
      end;


end.
