{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This file implements the node for sub procedure calling


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
unit ncal;

{$i fpcdefs.inc}
{$define nice_ncal}

interface

    uses
       node,
       {$ifdef state_tracking}
       nstate,
       {$endif state_tracking}
       symbase,symtype,symppu,symsym,symdef,symtable;

    type
       tcallnode = class(tbinarynode)
          { the symbol containing the definition of the procedure }
          { to call                                               }
          symtableprocentry : tprocsym;
          { the symtable containing symtableprocentry }
          symtableproc   : tsymtable;
          { the definition of the procedure to call }
          procdefinition : tabstractprocdef;
          methodpointer  : tnode;

          { separately specified resulttype for some compilerprocs (e.g. }
          { you can't have a function with an "array of char" resulttype }
          { the RTL) (JM)                                                }
          restype: ttype;
          restypeset: boolean;

          { function return reference node, this is used to pass an already
            allocated reference for a ret_in_param return value }
          funcretrefnode : tnode;

          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(l:tnode; v : tprocsym;st : tsymtable; mp : tnode);virtual;
          constructor createintern(const name: string; params: tnode);
          constructor createinternres(const name: string; params: tnode; const res: ttype);
          constructor createinternreturn(const name: string; params: tnode; returnnode : tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function  getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function  pass_1 : tnode;override;
       {$ifdef nice_ncal}
          function  choose_definition_to_call(paralength:byte;var errorexit:boolean):Tnode;
       {$endif}
          function  det_resulttype:tnode;override;
       {$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
       {$endif state_tracking}
          function  docompare(p: tnode): boolean; override;
          procedure set_procvar(procvar:tnode);
       end;
       tcallnodeclass = class of tcallnode;

       tcallparaflags = (
          { flags used by tcallparanode }
          cpf_exact_match_found,
          cpf_convlevel1found,
          cpf_convlevel2found,
          cpf_is_colon_para
{$ifdef nice_ncal}
          ,cpf_nomatchfound
{$endif}
       );

       tcallparanode = class(tbinarynode)
          callparaflags : set of tcallparaflags;
          hightree : tnode;
          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(expr,next : tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure gen_high_tree(openstring:boolean);
          procedure get_paratype;
          procedure insert_typeconv(defcoll : tparaitem;do_count : boolean);
          procedure det_registers;
          procedure firstcallparan(defcoll : tparaitem;do_count : boolean);
          procedure secondcallparan(defcoll : tparaitem;
                   push_from_left_to_right,inlined,is_cdecl : boolean;
                   para_alignment,para_offset : longint);virtual;abstract;
          function docompare(p: tnode): boolean; override;
       end;
       tcallparanodeclass = class of tcallparanode;

       tprocinlinenode = class(tnode)
          inlinetree : tnode;
          inlineprocdef : tprocdef;
          retoffset,para_offset,para_size : longint;
          constructor create(p:tprocdef);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function det_resulttype : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tprocinlinenodeclass = class of tprocinlinenode;

    function reverseparameters(p: tcallparanode): tcallparanode;


    var
       ccallnode : tcallnodeclass;
       ccallparanode : tcallparanodeclass;
       cprocinlinenode : tprocinlinenodeclass;

implementation

    uses
      cutils,globtype,systems,
      verbose,globals,
      symconst,paramgr,defbase,
      htypechk,pass_1,cpuinfo,cpubase,
      ncnv,nld,ninl,nadd,ncon,
      rgobj,cgbase
      ;


{****************************************************************************
                             HELPERS
 ****************************************************************************}

    function reverseparameters(p: tcallparanode): tcallparanode;
      var
        hp1, hp2: tcallparanode;
      begin
        hp1:=nil;
        while assigned(p) do
          begin
             { pull out }
             hp2:=p;
             p:=tcallparanode(p.right);
             { pull in }
             hp2.right:=hp1;
             hp1:=hp2;
          end;
        reverseparameters:=hp1;
      end;


    procedure search_class_overloads(aprocsym : tprocsym);
    { searches n in symtable of pd and all anchestors }
      var
        speedvalue : cardinal;
        srsym      : tprocsym;
        s          : string;
        srpdl      : pprocdeflist;
        objdef     : tobjectdef;
      begin
        if aprocsym.overloadchecked then
         exit;
        aprocsym.overloadchecked:=true;
        if (aprocsym.owner.symtabletype<>objectsymtable) then
         internalerror(200111021);
        objdef:=tobjectdef(aprocsym.owner.defowner);
        { we start in the parent }
        if not assigned(objdef.childof) then
         exit;
        objdef:=objdef.childof;
        s:=aprocsym.name;
        speedvalue:=getspeedvalue(s);
        while assigned(objdef) do
         begin
           srsym:=tprocsym(objdef.symtable.speedsearch(s,speedvalue));
           if assigned(srsym) then
            begin
              if (srsym.typ<>procsym) then
               internalerror(200111022);
              if srsym.is_visible_for_proc(aktprocdef) then
               begin
                 srsym.add_para_match_to(Aprocsym);
                 { we can stop if the overloads were already added
                  for the found symbol }
                 if srsym.overloadchecked then
                  break;
               end;
            end;
           { next parent }
           objdef:=objdef.childof;
         end;
      end;


{****************************************************************************
                             TCALLPARANODE
 ****************************************************************************}

    constructor tcallparanode.create(expr,next : tnode);

      begin
         inherited create(callparan,expr,next);
         hightree:=nil;
         if assigned(expr) then
          expr.set_file_line(self);
         callparaflags:=[];
      end;

    destructor tcallparanode.destroy;

      begin
         hightree.free;
         inherited destroy;
      end;


    constructor tcallparanode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getsmallset(callparaflags);
        hightree:=ppuloadnode(ppufile);
      end;


    procedure tcallparanode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putsmallset(callparaflags);
        ppuwritenode(ppufile,hightree);
      end;


    procedure tcallparanode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(hightree) then
          hightree.derefimpl;
      end;


    function tcallparanode.getcopy : tnode;

      var
         n : tcallparanode;

      begin
         n:=tcallparanode(inherited getcopy);
         n.callparaflags:=callparaflags;
         if assigned(hightree) then
           n.hightree:=hightree.getcopy
         else
           n.hightree:=nil;
         result:=n;
      end;

    procedure tcallparanode.insertintolist(l : tnodelist);

      begin
      end;


    procedure tcallparanode.get_paratype;
      var
        old_get_para_resulttype : boolean;
        old_array_constructor : boolean;
      begin
         inc(parsing_para_level);
         if assigned(right) then
          tcallparanode(right).get_paratype;
         old_array_constructor:=allow_array_constructor;
         old_get_para_resulttype:=get_para_resulttype;
         get_para_resulttype:=true;
         allow_array_constructor:=true;
         resulttypepass(left);
         get_para_resulttype:=old_get_para_resulttype;
         allow_array_constructor:=old_array_constructor;
         if codegenerror then
          resulttype:=generrortype
         else
          resulttype:=left.resulttype;
         dec(parsing_para_level);
      end;


    function is_var_para_incompatible(from_def,to_def:Tdef):boolean;

    {Might be an idea to move this to defbase...}

    begin
        is_var_para_incompatible:=
               { allows conversion from word to integer and
                byte to shortint, but only for TP7 compatibility }
                (not(
                   (m_tp7 in aktmodeswitches) and
                   (from_def.deftype=orddef) and
                   (to_def.deftype=orddef) and
                   (from_def.size=to_def.size)
                    ) and
              { an implicit pointer conversion is allowed }
                not(
                   (from_def.deftype=pointerdef) and
                   (to_def.deftype=pointerdef)
                    ) and
              { child classes can be also passed }
                not(
                   (from_def.deftype=objectdef) and
                   (to_def.deftype=objectdef) and
                   tobjectdef(from_def).is_related(tobjectdef(to_def))
                   ) and
              { passing a single element to a openarray of the same type }
                not(
                   (is_open_array(to_def) and
                   is_equal(tarraydef(to_def).elementtype.def,from_def))
                   ) and
              { an implicit file conversion is also allowed }
              { from a typed file to an untyped one           }
                not(
                   (from_def.deftype=filedef) and
                   (to_def.deftype=filedef) and
                   (tfiledef(to_def).filetyp = ft_untyped) and
                   (tfiledef(from_def).filetyp = ft_typed)
                    ) and
                not(is_equal(from_def,to_def)));

    end;

    procedure tcallparanode.insert_typeconv(defcoll : tparaitem;do_count : boolean);
      var
        oldtype     : ttype;
{$ifdef extdebug}
        store_count_ref : boolean;
{$endif def extdebug}
        p1 : tnode;
      begin
         inc(parsing_para_level);

         if not assigned(defcoll) then
           internalerror(200104261);

{$ifdef extdebug}
         if do_count then
           begin
             store_count_ref:=count_ref;
             count_ref:=true;
           end;
{$endif def extdebug}
         if assigned(right) then
           begin
             { if we are a para that belongs to varargs then keep
               the current defcoll }
             if (nf_varargs_para in flags) then
              tcallparanode(right).insert_typeconv(defcoll,do_count)
             else
              tcallparanode(right).insert_typeconv(tparaitem(defcoll.next),do_count);
           end;

         { Be sure to have the resulttype }
         if not assigned(left.resulttype.def) then
           resulttypepass(left);

         { Handle varargs directly, no typeconvs or typechecking needed }
         if (nf_varargs_para in flags) then
          begin
            { convert pascal to C types }
            case left.resulttype.def.deftype of
              stringdef :
                inserttypeconv(left,charpointertype);
              floatdef :
                inserttypeconv(left,s64floattype);
            end;
            set_varstate(left,true);
            resulttype:=left.resulttype;
            dec(parsing_para_level);
            exit;
          end;

         { Do we need arrayconstructor -> set conversion, then insert
           it here before the arrayconstructor node breaks the tree
           with its conversions of enum->ord }
         if (left.nodetype=arrayconstructorn) and
            (defcoll.paratype.def.deftype=setdef) then
           inserttypeconv(left,defcoll.paratype);

         { set some settings needed for arrayconstructor }
         if is_array_constructor(left.resulttype.def) then
          begin
            if is_array_of_const(defcoll.paratype.def) then
             begin
               if assigned(aktcallprocdef) and
                  (aktcallprocdef.proccalloption in [pocall_cppdecl,pocall_cdecl]) and
                  (po_external in aktcallprocdef.procoptions) then
                 include(left.flags,nf_cargs);
               { force variant array }
               include(left.flags,nf_forcevaria);
             end
            else
             begin
               include(left.flags,nf_novariaallowed);
               { now that the resultting type is know we can insert the required
                 typeconvs for the array constructor }
               tarrayconstructornode(left).force_type(tarraydef(defcoll.paratype.def).elementtype);
             end;
          end;

         { check if local proc/func is assigned to procvar }
         if left.resulttype.def.deftype=procvardef then
           test_local_to_procvar(tprocvardef(left.resulttype.def),defcoll.paratype.def);

         { generate the high() value tree }
         if not(assigned(aktcallprocdef) and
                (aktcallprocdef.proccalloption in [pocall_cppdecl,pocall_cdecl]) and
                (po_external in aktcallprocdef.procoptions)) and
            paramanager.push_high_param(defcoll.paratype.def) then
           gen_high_tree(is_open_string(defcoll.paratype.def));

         { test conversions }
         if not(is_shortstring(left.resulttype.def) and
                is_shortstring(defcoll.paratype.def)) and
                (defcoll.paratype.def.deftype<>formaldef) then
           begin
              if (defcoll.paratyp in [vs_var,vs_out]) and
               is_var_para_incompatible(left.resulttype.def,defcoll.paratype.def) then
                  begin
                     CGMessagePos2(left.fileinfo,parser_e_call_by_ref_without_typeconv,
                       left.resulttype.def.typename,defcoll.paratype.def.typename);
                  end;
              { Process open parameters }
              if paramanager.push_high_param(defcoll.paratype.def) then
               begin
                 { insert type conv but hold the ranges of the array }
                 oldtype:=left.resulttype;
                 inserttypeconv(left,defcoll.paratype);
                 left.resulttype:=oldtype;
               end
              else
               begin
                 inserttypeconv(left,defcoll.paratype);
               end;
              if codegenerror then
                begin
                   dec(parsing_para_level);
                   exit;
                end;
           end;

         { check var strings }
         if (cs_strict_var_strings in aktlocalswitches) and
            is_shortstring(left.resulttype.def) and
            is_shortstring(defcoll.paratype.def) and
            (defcoll.paratyp in [vs_out,vs_var]) and
            not(is_open_string(defcoll.paratype.def)) and
            not(is_equal(left.resulttype.def,defcoll.paratype.def)) then
            begin
               aktfilepos:=left.fileinfo;
               CGMessage(type_e_strict_var_string_violation);
            end;

         { Handle formal parameters separate }
         if (defcoll.paratype.def.deftype=formaldef) then
           begin
             { load procvar if a procedure is passed }
             if (m_tp_procvar in aktmodeswitches) and
                (left.nodetype=calln) and
                (is_void(left.resulttype.def)) then
              begin
                p1:=cloadnode.create_procvar(tcallnode(left).symtableprocentry,
                   tprocdef(tcallnode(left).procdefinition),tcallnode(left).symtableproc);
                if assigned(tcallnode(left).right) then
                 tloadnode(p1).set_mp(tcallnode(left).right);
                left.free;
                left:=p1;
                resulttypepass(left);
              end;

             case defcoll.paratyp of
               vs_var,
               vs_out :
                 begin
                   if not valid_for_formal_var(left) then
                    CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                 end;
               vs_const :
                 begin
                   if not valid_for_formal_const(left) then
                    CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                 end;
             end;
           end
         else
           begin
             { check if the argument is allowed }
             if (defcoll.paratyp in [vs_out,vs_var]) then
               valid_for_var(left);
           end;

         if defcoll.paratyp in [vs_var,vs_const] then
           begin
              { Causes problems with const ansistrings if also }
              { done for vs_const (JM)                         }
              if defcoll.paratyp = vs_var then
                set_unique(left);
              make_not_regable(left);
           end;

         { ansistrings out paramaters doesn't need to be  }
         { unique, they are finalized                     }
         if defcoll.paratyp=vs_out then
           make_not_regable(left);

         if do_count then
          begin
            { not completly proper, but avoids some warnings }
            if (defcoll.paratyp in [vs_var,vs_out]) then
             set_funcret_is_valid(left);
            set_varstate(left,not(defcoll.paratyp in [vs_var,vs_out]));
          end;
         { must only be done after typeconv PM }
         resulttype:=defcoll.paratype;
         dec(parsing_para_level);
{$ifdef extdebug}
         if do_count then
           count_ref:=store_count_ref;
{$endif def extdebug}
      end;


    procedure tcallparanode.det_registers;
      var
        old_get_para_resulttype : boolean;
        old_array_constructor : boolean;
      begin
         if assigned(right) then
           begin
              tcallparanode(right).det_registers;

              registers32:=right.registers32;
              registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=right.registersmmx;
{$endif}
           end;

         old_array_constructor:=allow_array_constructor;
         old_get_para_resulttype:=get_para_resulttype;
         get_para_resulttype:=true;
         allow_array_constructor:=true;
         firstpass(left);
         get_para_resulttype:=old_get_para_resulttype;
         allow_array_constructor:=old_array_constructor;

         if left.registers32>registers32 then
           registers32:=left.registers32;
         if left.registersfpu>registersfpu then
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         if left.registersmmx>registersmmx then
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
      end;


    procedure tcallparanode.firstcallparan(defcoll : tparaitem;do_count : boolean);
      begin
        if not assigned(left.resulttype.def) then
         begin
           get_paratype;
           if assigned(defcoll) then
            insert_typeconv(defcoll,do_count);
         end;
        det_registers;
      end;


    procedure tcallparanode.gen_high_tree(openstring:boolean);
      var
        temp: tnode;
        len : integer;
        loadconst : boolean;
      begin
        if assigned(hightree) then
          exit;
        len:=-1;
        loadconst:=true;
        case left.resulttype.def.deftype of
          arraydef :
            begin
              { handle via a normal inline in_high_x node }
              loadconst := false;
              hightree := geninlinenode(in_high_x,false,left.getcopy);
              { only substract low(array) if it's <> 0 }
              temp := geninlinenode(in_low_x,false,left.getcopy);
              firstpass(temp);
              if (temp.nodetype <> ordconstn) or
                 (tordconstnode(temp).value <> 0) then
                hightree := caddnode.create(subn,hightree,temp)
              else
                temp.free;
            end;
          stringdef :
            begin
              if openstring then
               begin
                 { handle via a normal inline in_high_x node }
                 loadconst := false;
                 hightree := geninlinenode(in_high_x,false,left.getcopy);
               end
              else
             { passing a string to an array of char }
               begin
                 if (left.nodetype=stringconstn) then
                   begin
                     len:=str_length(left);
                     if len>0 then
                      dec(len);
                   end
                 else
                   begin
                     hightree:=caddnode.create(subn,geninlinenode(in_length_x,false,left.getcopy),
                                               cordconstnode.create(1,s32bittype));
                     loadconst:=false;
                   end;
               end;
           end;
        else
          len:=0;
        end;
        if loadconst then
          hightree:=cordconstnode.create(len,s32bittype)
        else
          hightree:=ctypeconvnode.create(hightree,s32bittype);
        firstpass(hightree);
      end;


    function tcallparanode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (callparaflags = tcallparanode(p).callparaflags) and
          hightree.isequal(tcallparanode(p).hightree);
      end;

{****************************************************************************
                                 TCALLNODE
 ****************************************************************************}

    constructor tcallnode.create(l:tnode;v : tprocsym;st : tsymtable; mp : tnode);

      begin
         inherited create(calln,l,nil);
         symtableprocentry:=v;
         symtableproc:=st;
         include(flags,nf_return_value_used);
         methodpointer:=mp;
         procdefinition:=nil;
         restypeset := false;
         funcretrefnode:=nil;
      end;


     constructor tcallnode.createintern(const name: string; params: tnode);
       var
         srsym: tsym;
         symowner: tsymtable;
       begin
         if not (cs_compilesystem in aktmoduleswitches) then
           begin
             srsym := searchsymonlyin(systemunit,name);
             symowner := systemunit;
           end
         else
           begin
             searchsym(name,srsym,symowner);
             if not assigned(srsym) then
               searchsym(upper(name),srsym,symowner);
           end;
         if not assigned(srsym) or
            (srsym.typ <> procsym) then
           begin
             writeln('unknown compilerproc ',name);
             internalerror(200107271);
           end;
         self.create(params,tprocsym(srsym),symowner,nil);
       end;

    constructor tcallnode.createinternres(const name: string; params: tnode; const res: ttype);
      begin
        self.createintern(name,params);
        restype := res;
        restypeset := true;
        { both the normal and specified resulttype either have to be returned via a }
        { parameter or not, but no mixing (JM)                                      }
        if paramanager.ret_in_param(restype.def) xor paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def) then
          internalerror(200108291);
      end;


    constructor tcallnode.createinternreturn(const name: string; params: tnode; returnnode : tnode);
      begin
        self.createintern(name,params);
        funcretrefnode:=returnnode;
        if not paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def) then
          internalerror(200204247);
      end;


    destructor tcallnode.destroy;
      begin
         methodpointer.free;
         funcretrefnode.free;
         inherited destroy;
      end;


    constructor tcallnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        symtableprocentry:=tprocsym(ppufile.getderef);
{$warning FIXME: No withsymtable support}
        symtableproc:=nil;
        procdefinition:=tprocdef(ppufile.getderef);
        restypeset:=boolean(ppufile.getbyte);
        methodpointer:=ppuloadnode(ppufile);
        funcretrefnode:=ppuloadnode(ppufile);
      end;


    procedure tcallnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(symtableprocentry);
        ppufile.putderef(procdefinition);
        ppufile.putbyte(byte(restypeset));
        ppuwritenode(ppufile,methodpointer);
        ppuwritenode(ppufile,funcretrefnode);
      end;


    procedure tcallnode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(symtableprocentry));
        symtableproc:=symtableprocentry.owner;
        resolvedef(pointer(procdefinition));
        if assigned(methodpointer) then
          methodpointer.derefimpl;
        if assigned(funcretrefnode) then
          funcretrefnode.derefimpl;
      end;


    procedure tcallnode.set_procvar(procvar:tnode);
      begin
        right:=procvar;
      end;

    function tcallnode.getcopy : tnode;
      var
        n : tcallnode;
      begin
        n:=tcallnode(inherited getcopy);
        n.symtableprocentry:=symtableprocentry;
        n.symtableproc:=symtableproc;
        n.procdefinition:=procdefinition;
        n.restype := restype;
        n.restypeset := restypeset;
        if assigned(methodpointer) then
         n.methodpointer:=methodpointer.getcopy
        else
         n.methodpointer:=nil;
        if assigned(funcretrefnode) then
         n.funcretrefnode:=funcretrefnode.getcopy
        else
         n.funcretrefnode:=nil;
        result:=n;
      end;

    procedure tcallnode.insertintolist(l : tnodelist);

      begin
      end;

{$ifdef nice_ncal}

    function Tcallnode.choose_definition_to_call(paralength:byte;var errorexit:boolean):Tnode;

      { check if the resulttype.def from tree p is equal with def, needed
        for stringconstn and formaldef }
      function is_equal(p:tcallparanode;def:tdef) : boolean;

        begin
           { safety check }
           if not (assigned(def) or assigned(p.resulttype.def)) then
            begin
              is_equal:=false;
              exit;
            end;
           { all types can be passed to a formaldef }
           is_equal:=(def.deftype=formaldef) or
             (defbase.is_equal(p.resulttype.def,def))
           { integer constants are compatible with all integer parameters if
             the specified value matches the range }
             or
             (
              (tbinarynode(p).left.nodetype=ordconstn) and
              is_integer(p.resulttype.def) and
              is_integer(def) and
              (tordconstnode(p.left).value>=torddef(def).low) and
              (tordconstnode(p.left).value<=torddef(def).high)
             )
           { to support ansi/long/wide strings in a proper way }
           { string and string[10] are assumed as equal }
           { when searching the correct overloaded procedure   }
             or
             (
              (def.deftype=stringdef) and (p.resulttype.def.deftype=stringdef) and
              (tstringdef(def).string_typ=tstringdef(p.resulttype.def).string_typ)
             )
             or
             (
              (p.left.nodetype=stringconstn) and
              (is_ansistring(p.resulttype.def) and is_pchar(def))
             )
             or
             (
              (p.left.nodetype=ordconstn) and
              (is_char(p.resulttype.def) and (is_shortstring(def) or is_ansistring(def)))
             )
           { set can also be a not yet converted array constructor }
             or
             (
              (def.deftype=setdef) and (p.resulttype.def.deftype=arraydef) and
              (tarraydef(p.resulttype.def).IsConstructor) and not(tarraydef(p.resulttype.def).IsVariant)
             )
           { in tp7 mode proc -> procvar is allowed }
             or
             (
              (m_tp_procvar in aktmodeswitches) and
              (def.deftype=procvardef) and (p.left.nodetype=calln) and
              (proc_to_procvar_equal(tprocdef(tcallnode(p.left).procdefinition),tprocvardef(def),false))
             )
             ;
        end;

        procedure get_candidate_information(var cl2_count,cl1_count,equal_count,exact_count:byte;
                                            var ordspace:double;
                                            treeparas:Tcallparanode;candparas:Tparaitem);

        {Gets information how the parameters would be converted to the candidate.}

        var hcvt:Tconverttype;
            from_def,to_def:Tdef;

        begin
            cl2_count:=0;
            cl1_count:=0;
            equal_count:=0;
            exact_count:=0;
            ordspace:=0;
            while candparas<>nil do
                begin
                    from_def:=treeparas.resulttype.def;
                    to_def:=candparas.paratype.def;
                    if to_def=from_def then
                        inc(exact_count)
                    { if a type is totally included in the other        }
                    { we don't fear an overflow ,                       }
                    { so we can do as if it is an equal match           }
                    else if (treeparas.left.nodetype=ordconstn) and is_integer(to_def) then
                        begin
                            inc(equal_count);
                            ordspace:=ordspace+(double(Torddef(from_def).low)-Torddef(to_def).low)+
                                         (double(Torddef(to_def).high)-Torddef(from_def).high);
                        end
                    else if ((from_def.deftype=orddef) and (to_def.deftype=orddef)) and
                     (is_in_limit(from_def,to_def) or
                      ((candparas.paratyp in [vs_var,vs_out]) and (from_def.size=to_def.size))
                     ) then
                        begin
                            ordspace:=ordspace+Torddef(to_def).high;
                            ordspace:=ordspace-Torddef(to_def).low;
                            inc(equal_count);
                        end
                    else if is_equal(treeparas,to_def) then
                        inc(equal_count)
                    else
                        case isconvertable(from_def,to_def,
                         hcvt,treeparas.left.nodetype,false) of
                            0:
                                internalerror(200208021);
                            1:
                                inc(cl1_count);
                            2:
                                inc(cl2_count);
                        end;
                    treeparas:=Tcallparanode(treeparas.right);
                    candparas:=Tparaitem(candparas.next);
                end;
        end;

    type  Tcandidate_array=array[1..$ffff] of Tprocdef;
          Pcandidate_array=^Tcandidate_array;

    var candidate_alloc,candidates_left,candidate_count:cardinal;
        c1,c2,delete_start:cardinal;
        cl2_count1,cl1_count1,equal_count1,exact_count1:byte;
        ordspace1:double;
        cl2_count2,cl1_count2,equal_count2,exact_count2:byte;
        ordspace2:double;
        i,n:cardinal;
        pt:Tcallparanode;
        def:Tprocdef;
        hcvt:Tconverttype;
        pdc:Tparaitem;
        hpt:Tnode;
        srprocsym:Tprocsym;
        srsymtable:Tsymtable;
        candidate_defs:Pcandidate_array;

    begin
        if fileinfo.line=398 then
          i:=0;
        choose_definition_to_call:=nil;
        errorexit:=true;

        { when the definition has overload directive set, we search for
          overloaded definitions in the class, this only needs to be done once
          for class entries as the tree keeps always the same }
        if (not symtableprocentry.overloadchecked) and
         (po_overload in symtableprocentry.first_procdef.procoptions) and
         (symtableprocentry.owner.symtabletype=objectsymtable) then
            search_class_overloads(symtableprocentry);

        {Collect all procedures which have the same # of parameters }
        candidates_left:=0;
        candidate_count:=0;
        candidate_alloc:=32;
        getmem(candidate_defs,candidate_alloc*sizeof(Tprocdef));
        srprocsym:=symtableprocentry;
        srsymtable:=symtableprocentry.owner;
        repeat
            for i:=1 to srprocsym.procdef_count do
                begin
                    def:=srprocsym.procdef[i];
                    { only when the # of parameters are supported by the procedure }
                    if (paralength>=def.minparacount) and
                       ((po_varargs in def.procoptions) or (paralength<=def.maxparacount)) then
                      begin
                        candidate_defs^[i]:=def;
                        inc(candidates_left);
                      end
                    else
                      candidate_defs^[i]:=nil;
                    inc(candidate_count);
                    if candidate_alloc=candidate_count then
                      begin
                        candidate_alloc:=candidate_alloc*2;
                        reallocmem(candidate_defs,candidate_alloc*sizeof(Tprocdef));
                      end;
                end;
            if po_overload in srprocsym.first_procdef.procoptions then
                begin
                    repeat
                        srprocsym:=nil;
                        repeat
                            srsymtable:=srsymtable.next;
                        until (srsymtable=nil) or (srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable]);
                        if assigned(srsymtable) then
                            srprocsym:=Tprocsym(srsymtable.speedsearch(symtableprocentry.name,symtableprocentry.speedvalue));
                    until (srsymtable=nil) or (srprocsym<>nil);
                    if not assigned(srprocsym) then
                      break;
                end
            else
                break;
        until false;

        { no procedures found? then there is something wrong
          with the parameter size }
        if candidates_left=0 then
            begin
                { in tp mode we can try to convert to procvar if
                  there are no parameters specified }
                if not(assigned(left)) and
                 (m_tp_procvar in aktmodeswitches) then
                    begin
                        hpt:=cloadnode.create(tprocsym(symtableprocentry),symtableproc);
                        if (symtableprocentry.owner.symtabletype=objectsymtable) and
                         assigned(methodpointer) then
                            tloadnode(hpt).set_mp(methodpointer.getcopy);
                        resulttypepass(hpt);
                        choose_definition_to_call:=hpt;
                    end
                else
                    begin
                        if assigned(left) then
                            aktfilepos:=left.fileinfo;
                        cgmessage(parser_e_wrong_parameter_size);
                        symtableprocentry.write_parameter_lists(nil);
                    end;
                exit;
            end;
        {Walk through all candidates and remove the ones
         that have incompatible parameters.}
        for i:=1 to candidate_count do
            if assigned(candidate_defs^[i]) then
                begin
                    def:=candidate_defs^[i];
                    {Walk through all parameters.}
                    pdc:=Tparaitem(def.para.first);
                    pt:=Tcallparanode(left);
                    while assigned(pdc) do
                        begin
                            if pdc.paratyp in [vs_var,vs_out] then
                                if is_var_para_incompatible(pt.resulttype.def,pdc.paratype.def) and
                                 not(is_shortstring(pt.resulttype.def) and is_shortstring(pdc.paratype.def)) and
                                 (pdc.paratype.def.deftype<>formaldef) then
                                    begin
                                      {Not convertable, def is no longer a candidate.}
                                      candidate_defs^[i]:=nil;
                                      dec(candidates_left);
                                      break;
                                    end
                                else
                                    exclude(pt.callparaflags,cpf_nomatchfound)
                            else
                                if (pt.resulttype.def<>pdc.paratype.def) and
                                 ((isconvertable(pt.resulttype.def,pdc.paratype.def,
                                                 hcvt,pt.left.nodetype,false)=0) and
                                  not is_equal(pt,pdc.paratype.def)) then
                                    begin
                                      {Not convertable, def is no longer a candidate.}
                                      candidate_defs^[i]:=nil;
                                      dec(candidates_left);
                                      break;
                                    end
                                else
                                    exclude(pt.callparaflags,cpf_nomatchfound);
                            pdc:=Tparaitem(pdc.next);
                            pt:=Tcallparanode(pt.right);
                        end;
                end;
        {Are there any candidates left?}
        if candidates_left=0 then
            begin
                {There is an error, must be wrong type, because
                 wrong size is already checked (PFV) }
                pt:=Tcallparanode(left);
                n:=0;
                while assigned(pt) do
                    if cpf_nomatchfound in pt.callparaflags then
                        break
                    else
                        begin
                            pt:=tcallparanode(pt.right);
                            inc(n);
                        end;
                if not(assigned(pt) and assigned(pt.resulttype.def)) then
                    internalerror(39393);
                {Def contains the last candidate tested.}
                pdc:=Tparaitem(def.para.first);
                for i:=1 to n do
                    pdc:=Tparaitem(pdc.next);
                aktfilepos:=pt.fileinfo;
                cgmessage3(type_e_wrong_parameter_type,tostr(n+1),
                           pt.resulttype.def.typename,pdc.paratype.def.typename);
                symtableprocentry.write_parameter_lists(nil);
                exit;
            end;
       {If there is more candidate that can be called, we have to
        find the most suitable one. We collect the following
        information:
        - Amount of convertlevel 2 parameters.
        - Amount of convertlevel 1 parameters.
        - Amount of equal parameters.
        - Amount of exact parameters.
        - Amount of ordinal space the destination parameters
          provide. For exampe, a word provides 65535-255=65280
          of ordinal space above a byte.

        The first criterium is the candidate that has the least
        convertlevel 2 parameters. The next criterium is
        the candidate that has the most exact parameters, next
        criterium is the least ordinal space and
        the last criterium is the most equal parameters. (DM)}
        if candidates_left>1 then
            begin
                {Find the first candidate.}
                c1:=1;
                while c1<=candidate_count do
                    if assigned(candidate_defs^[c1]) then
                        break
                    else
                        inc(c1);
                delete_start:=c1;
                {Get information about candidate c1.}
                get_candidate_information(cl2_count1,cl1_count1,equal_count1,
                                          exact_count1,ordspace1,Tcallparanode(left),
                                          Tparaitem(candidate_defs^[c1].para.first));
                {Find the other candidates and eliminate the lesser ones.}
                c2:=c1+1;
                while c2<=candidate_count do
                    if assigned(candidate_defs^[c2]) then
                        begin
                            {Candidate found, get information on it.}
                            get_candidate_information(cl2_count2,cl1_count2,equal_count2,
                                                      exact_count2,ordspace2,Tcallparanode(left),
                                                      Tparaitem(candidate_defs^[c2].para.first));
                            {Is c1 the better candidate?}
                            if (cl2_count1<cl2_count2) or
                             ((cl2_count1=cl2_count2) and (exact_count1>exact_count2)) or
                             ((cl2_count1=cl2_count2) and (exact_count1=exact_count2) and (equal_count1>equal_count2)) or
                             ((cl2_count1=cl2_count2) and (exact_count1=exact_count2) and (equal_count1=equal_count2) and (ordspace1<ordspace2)) then
                                {C1 is better, drop c2.}
                                candidate_defs^[c2]:=nil
                            {Is c2 the better candidate?}
                            else if (cl2_count2<cl2_count1) or
                             ((cl2_count2=cl2_count1) and (exact_count2>exact_count1)) or
                             ((cl2_count2=cl2_count1) and (exact_count2=exact_count1) and (equal_count2>equal_count1)) or
                             ((cl2_count2=cl2_count1) and (exact_count2=exact_count1) and (equal_count2=equal_count1) and (ordspace2<ordspace1)) then
                                begin
                                    {C2 is better, drop all previous
                                     candidates.}
                                    for i:=delete_start to c2-1 do
                                      candidate_defs^[i]:=nil;
                                    delete_start:=c2;
                                    c1:=c2;
                                    cl2_count1:=cl2_count2;
                                    cl1_count1:=cl1_count2;
                                    equal_count1:=equal_count2;
                                    exact_count1:=exact_count2;
                                    ordspace1:=ordspace2;
                                end;
                            {else the candidates have no advantage over each other,
                             do nothing}
                            inc(c2);
                        end
                    else
                        inc(c2);
            end;
        {Count the candidates that are left.}
        candidates_left:=0;
        for i:=1 to candidate_count do
            if assigned(candidate_defs^[i]) then
              begin
                inc(candidates_left);
                procdefinition:=candidate_defs^[i];
              end;
        if candidates_left>1 then
            begin
                cgmessage(cg_e_cant_choose_overload_function);
                symtableprocentry.write_parameter_lists(nil);
                exit;
            end;
        freemem(candidate_defs,candidate_alloc*sizeof(Tprocdef));
        if make_ref then
            begin
                Tprocdef(procdefinition).lastref:=Tref.create(Tprocdef(procdefinition).lastref,@fileinfo);
                inc(Tprocdef(procdefinition).refcount);
                if Tprocdef(procdefinition).defref=nil then
                    Tprocdef(procdefinition).defref:=Tprocdef(procdefinition).lastref;
            end;
        { big error for with statements
          symtableproc:=procdefinition.owner;
          but neede for overloaded operators !! }
        if symtableproc=nil then
            symtableproc:=procdefinition.owner;
        errorexit:=false;
    end;

    function tcallnode.det_resulttype:tnode;


    var lastpara,paralength:byte;
        oldcallprocdef:Tabstractprocdef;
        pt:Tcallparanode;
        i,n:byte;
        e,is_const:boolean;
        pdc:Tparaitem;
        hpt:Tnode;

    label errorexit;

    begin
        result:=nil;

        oldcallprocdef:=aktcallprocdef;
        aktcallprocdef:=nil;

        { determine length of parameter list }
        pt:=tcallparanode(left);
        paralength:=0;
        while assigned(pt) do
          begin
            include(pt.callparaflags,cpf_nomatchfound);
            inc(paralength);
            pt:=tcallparanode(pt.right);
          end;

        { determine the type of the parameters }
        if assigned(left) then
          begin
            tcallparanode(left).get_paratype;
            if codegenerror then
             goto errorexit;
          end;

        { procedure variable ? }
        if assigned(right) then
           begin
              set_varstate(right,true);
              resulttypepass(right);
              if codegenerror then
               exit;

              procdefinition:=tabstractprocdef(right.resulttype.def);

              { check the amount of parameters }
              pdc:=tparaitem(procdefinition.Para.first);
              pt:=tcallparanode(left);
              lastpara:=paralength;
              while assigned(pdc) and assigned(pt) do
                begin
                  { only goto next para if we're out of the varargs }
                  if not(po_varargs in procdefinition.procoptions) or
                     (lastpara<=procdefinition.maxparacount) then
                    pdc:=tparaitem(pdc.next);
                  pt:=tcallparanode(pt.right);
                  dec(lastpara);
                end;
              if assigned(pt) or assigned(pdc) then
                begin
                   if assigned(pt) then
                     aktfilepos:=pt.fileinfo;
                   CGMessage(parser_e_wrong_parameter_size);
                end;
           end
        else
        { not a procedure variable }
            begin
                { do we know the procedure to call ? }
                if not(assigned(procdefinition)) then
                    begin
                        result:=choose_definition_to_call(paralength,e);
                        if e then
                            goto errorexit;
                    end;
(*              To do!!!
                { add needed default parameters }
                if assigned(procdefinition) and
                 (paralength<procdefinition.maxparacount) then
                    begin
                        { add default parameters, just read back the skipped
                          paras starting from firstPara.previous, when not available
                          (all parameters are default) then start with the last
                          parameter and read backward (PFV) }
                        if not assigned(procs^.firstpara) then
                            pdc:=tparaitem(procs^.data.Para.last)
                        else
                            pdc:=tparaitem(procs^.firstPara.previous);
                        while assigned(pdc) do
                            begin
                                if not assigned(pdc.defaultvalue) then
                                    internalerror(751349858);
                                left:=ccallparanode.create(genconstsymtree(tconstsym(pdc.defaultvalue)),left);
                                pdc:=tparaitem(pdc.previous);
                            end;
                    end;
*)
            end;
        { handle predefined procedures }
        is_const:=(po_internconst in procdefinition.procoptions) and
                  ((block_type in [bt_const,bt_type]) or
                   (assigned(left) and (tcallparanode(left).left.nodetype in [realconstn,ordconstn])));
        if (procdefinition.proccalloption=pocall_internproc) or is_const then
            begin
                if assigned(left) then
                    begin
                        { ptr and settextbuf needs two args }
                        if assigned(tcallparanode(left).right) then
                            begin
                                hpt:=geninlinenode(Tprocdef(procdefinition).extnumber,is_const,left);
                                left:=nil;
                            end
                        else
                            begin
                                hpt:=geninlinenode(Tprocdef(procdefinition).extnumber,is_const,Tcallparanode(left).left);
                                Tcallparanode(left).left:=nil;
                            end;
                    end
                else
                    hpt:=geninlinenode(Tprocdef(procdefinition).extnumber,is_const,nil);
                result:=hpt;
                goto errorexit;
            end;
{$ifdef dummy}
        { Calling a message method directly ? }
        if assigned(procdefinition) and
         (po_containsself in procdefinition.procoptions) then
            message(cg_e_cannot_call_message_direct);
{$endif}

        { ensure that the result type is set }
        if not restypeset then
            resulttype:=procdefinition.rettype
        else
            resulttype:=restype;

        { modify the exit code, in case of special cases }
        if (not is_void(resulttype.def)) then
            begin
                if paramanager.ret_in_acc(resulttype.def) then
                    begin
                        { wide- and ansistrings are returned in EAX    }
                        { but they are imm. moved to a memory location }
                        if is_widestring(resulttype.def) or
                         is_ansistring(resulttype.def) then
                            begin
                                { we use ansistrings so no fast exit here }
                                if assigned(procinfo) then
                                    procinfo.no_fast_exit:=true;
                            end;
                    end;
            end;
        { constructors return their current class type, not the type where the
          constructor is declared, this can be different because of inheritance }
        if (procdefinition.proctypeoption=potype_constructor) then
            begin
                if assigned(methodpointer) and
                 assigned(methodpointer.resulttype.def) and
                 (methodpointer.resulttype.def.deftype=classrefdef) then
                    resulttype:=tclassrefdef(methodpointer.resulttype.def).pointertype;
            end;

        { flag all callparanodes that belong to the varargs }
        if (po_varargs in procdefinition.procoptions) then
            begin
                pt:=tcallparanode(left);
                i:=paralength;
                while (i>procdefinition.maxparacount) do
                    begin
                        include(tcallparanode(pt).flags,nf_varargs_para);
                        pt:=tcallparanode(pt.right);
                        dec(i);
                    end;
            end;

        { insert type conversions }
        if assigned(left) then
            begin
                aktcallprocdef:=procdefinition;
                tcallparanode(left).insert_typeconv(tparaitem(procdefinition.Para.first),true);
            end;
    errorexit:
        { Reset some settings back }
        aktcallprocdef:=oldcallprocdef;
    end;

{$else}
    function tcallnode.det_resulttype:tnode;
      type
         pprocdefcoll = ^tprocdefcoll;
         tprocdefcoll = record
            data      : tprocdef;
            nextpara  : tparaitem;
            firstpara : tparaitem;
            next      : pprocdefcoll;
         end;
      var
         hp,procs,hp2 : pprocdefcoll;
         pd : pprocdeflist;
         oldcallprocdef : tabstractprocdef;
         def_from,def_to,conv_to : tdef;
         hpt : tnode;
         pt : tcallparanode;
         exactmatch : boolean;
         paralength,lastpara : longint;
         lastparatype : tdef;
         pdc : tparaitem;
         { only Dummy }
         hcvt : tconverttype;
      label
        errorexit;

      { check if the resulttype.def from tree p is equal with def, needed
        for stringconstn and formaldef }
      function is_equal(p:tcallparanode;def:tdef) : boolean;

        begin
           { safety check }
           if not (assigned(def) or assigned(p.resulttype.def)) then
            begin
              is_equal:=false;
              exit;
            end;
           { all types can be passed to a formaldef }
           is_equal:=(def.deftype=formaldef) or
             (defbase.is_equal(p.resulttype.def,def))
           { integer constants are compatible with all integer parameters if
             the specified value matches the range }
             or
             (
              (tbinarynode(p).left.nodetype=ordconstn) and
              is_integer(p.resulttype.def) and
              is_integer(def) and
              (tordconstnode(p.left).value>=torddef(def).low) and
              (tordconstnode(p.left).value<=torddef(def).high)
             )
           { to support ansi/long/wide strings in a proper way }
           { string and string[10] are assumed as equal }
           { when searching the correct overloaded procedure   }
             or
             (
              (def.deftype=stringdef) and (p.resulttype.def.deftype=stringdef) and
              (tstringdef(def).string_typ=tstringdef(p.resulttype.def).string_typ)
             )
             or
             (
              (p.left.nodetype=stringconstn) and
              (is_ansistring(p.resulttype.def) and is_pchar(def))
             )
             or
             (
              (p.left.nodetype=ordconstn) and
              (is_char(p.resulttype.def) and (is_shortstring(def) or is_ansistring(def)))
             )
           { set can also be a not yet converted array constructor }
             or
             (
              (def.deftype=setdef) and (p.resulttype.def.deftype=arraydef) and
              (tarraydef(p.resulttype.def).IsConstructor) and not(tarraydef(p.resulttype.def).IsVariant)
             )
           { in tp7 mode proc -> procvar is allowed }
             or
             (
              (m_tp_procvar in aktmodeswitches) and
              (def.deftype=procvardef) and (p.left.nodetype=calln) and
              (proc_to_procvar_equal(tprocdef(tcallnode(p.left).procdefinition),tprocvardef(def),false))
             )
             ;
        end;

      var
        i : longint;
        found,
        is_const : boolean;
        bestord  : torddef;
        srprocsym  : tprocsym;
        srsymtable : tsymtable;
      begin
         result:=nil;
         procs:=nil;

         oldcallprocdef:=aktcallprocdef;
         aktcallprocdef:=nil;

         { determine length of parameter list }
         pt:=tcallparanode(left);
         paralength:=0;
         while assigned(pt) do
          begin
            inc(paralength);
            pt:=tcallparanode(pt.right);
          end;

         { determine the type of the parameters }
         if assigned(left) then
          begin
            tcallparanode(left).get_paratype;
            if codegenerror then
             goto errorexit;
          end;

         { procedure variable ? }
         if assigned(right) then
           begin
              set_varstate(right,true);
              resulttypepass(right);
              if codegenerror then
               exit;

              procdefinition:=tabstractprocdef(right.resulttype.def);

              { check the amount of parameters }
              pdc:=tparaitem(procdefinition.Para.first);
              pt:=tcallparanode(left);
              lastpara:=paralength;
              while assigned(pdc) and assigned(pt) do
                begin
                  { only goto next para if we're out of the varargs }
                  if not(po_varargs in procdefinition.procoptions) or
                     (lastpara<=procdefinition.maxparacount) then
                    pdc:=tparaitem(pdc.next);
                  pt:=tcallparanode(pt.right);
                  dec(lastpara);
                end;
              if assigned(pt) or assigned(pdc) then
                begin
                   if assigned(pt) then
                     aktfilepos:=pt.fileinfo;
                   CGMessage(parser_e_wrong_parameter_size);
                end;
           end
         else
         { not a procedure variable }
           begin
              { do we know the procedure to call ? }
              if not(assigned(procdefinition)) then
                begin
                   { when the definition has overload directive set, we search for
                     overloaded definitions in the class, this only needs to be done once
                     for class entries as the tree keeps always the same }
                   if (not symtableprocentry.overloadchecked) and
                      (po_overload in symtableprocentry.first_procdef.procoptions) and
                      (symtableprocentry.owner.symtabletype=objectsymtable) then
                    search_class_overloads(symtableprocentry);

                   { link all procedures which have the same # of parameters }
                   pd:=symtableprocentry.defs;
                   while assigned(pd) do
                     begin
                        { only when the # of parameter are supported by the
                          procedure }
                        if (paralength>=pd^.def.minparacount) and
                           ((po_varargs in pd^.def.procoptions) or { varargs }
                            (paralength<=pd^.def.maxparacount)) then
                          begin
                             new(hp);
                             hp^.data:=pd^.def;
                             hp^.next:=procs;
                             hp^.firstpara:=tparaitem(pd^.def.Para.first);
                             if not(po_varargs in pd^.def.procoptions) then
                              begin
                                { if not all parameters are given, then skip the
                                  default parameters }
                                for i:=1 to pd^.def.maxparacount-paralength do
                                 hp^.firstpara:=tparaitem(hp^.firstPara.next);
                              end;
                             hp^.nextpara:=hp^.firstpara;
                             procs:=hp;
                          end;
                        pd:=pd^.next;
                     end;

                   { when the definition has overload directive set, we search for
                     overloaded definitions in the symtablestack. The found
                     entries are only added to the procs list and not the procsym, because
                     the list can change in every situation }
                   if (po_overload in symtableprocentry.first_procdef.procoptions) and
                      (symtableprocentry.owner.symtabletype<>objectsymtable) then
                     begin
                       srsymtable:=symtableprocentry.owner.next;
                       while assigned(srsymtable) do
                        begin
                          if srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable] then
                           begin
                             srprocsym:=tprocsym(srsymtable.speedsearch(symtableprocentry.name,symtableprocentry.speedvalue));
                             { process only visible procsyms }
                             if assigned(srprocsym) and
                                (srprocsym.typ=procsym) and
                                srprocsym.is_visible_for_proc(aktprocdef) then
                              begin
                                { if this procedure doesn't have overload we can stop
                                  searching }
                                if not(po_overload in srprocsym.first_procdef.procoptions) then
                                 break;
                                { process all overloaded definitions }
                                pd:=srprocsym.defs;
                                while assigned(pd) do
                                 begin
                                   { only when the # of parameter are supported by the
                                     procedure }
                                   if (paralength>=pd^.def.minparacount) and
                                      ((po_varargs in pd^.def.procoptions) or { varargs }
                                      (paralength<=pd^.def.maxparacount)) then
                                    begin
                                      found:=false;
                                      hp:=procs;
                                      while assigned(hp) do
                                       begin
                                         if equal_paras(hp^.data.para,pd^.def.para,cp_value_equal_const) then
                                          begin
                                            found:=true;
                                            break;
                                          end;
                                         hp:=hp^.next;
                                       end;
                                      if not found then
                                       begin
                                         new(hp);
                                         hp^.data:=pd^.def;
                                         hp^.next:=procs;
                                         hp^.firstpara:=tparaitem(pd^.def.Para.first);
                                         if not(po_varargs in pd^.def.procoptions) then
                                          begin
                                            { if not all parameters are given, then skip the
                                              default parameters }
                                            for i:=1 to pd^.def.maxparacount-paralength do
                                             hp^.firstpara:=tparaitem(hp^.firstPara.next);
                                          end;
                                         hp^.nextpara:=hp^.firstpara;
                                         procs:=hp;
                                       end;
                                    end;
                                   pd:=pd^.next;
                                 end;
                              end;
                           end;
                          srsymtable:=srsymtable.next;
                        end;
                     end;

                   { no procedures found? then there is something wrong
                     with the parameter size }
                   if not assigned(procs) then
                    begin
                      { in tp mode we can try to convert to procvar if
                        there are no parameters specified }
                      if not(assigned(left)) and
                         (m_tp_procvar in aktmodeswitches) then
                        begin
                          hpt:=cloadnode.create(tprocsym(symtableprocentry),symtableproc);
                          if (symtableprocentry.owner.symtabletype=objectsymtable) and
                             assigned(methodpointer) then
                            tloadnode(hpt).set_mp(methodpointer.getcopy);
                          resulttypepass(hpt);
                          result:=hpt;
                        end
                      else
                        begin
                          if assigned(left) then
                           aktfilepos:=left.fileinfo;
                          CGMessage(parser_e_wrong_parameter_size);
                          symtableprocentry.write_parameter_lists(nil);
                        end;
                      goto errorexit;
                    end;

                { now we can compare parameter after parameter }
                   pt:=tcallparanode(left);
                   { we start with the last parameter }
                   lastpara:=paralength+1;
                   lastparatype:=nil;
                   while assigned(pt) do
                     begin
                        dec(lastpara);
                        { walk all procedures and determine how this parameter matches and set:
                           1. pt.exact_match_found if one parameter has an exact match
                           2. exactmatch if an equal or exact match is found

                           3. Para.argconvtyp to exact,equal or convertable
                                (when convertable then also convertlevel is set)
                           4. pt.convlevel1found if there is a convertlevel=1
                           5. pt.convlevel2found if there is a convertlevel=2
                        }
                        exactmatch:=false;
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             { varargs are always equal, but not exact }
                             if (po_varargs in hp^.data.procoptions) and
                                (lastpara>hp^.data.minparacount) then
                              begin
                                hp^.nextPara.argconvtyp:=act_equal;
                                exactmatch:=true;
                              end
                             else
                              begin
                                if is_equal(pt,hp^.nextPara.paratype.def) then
                                 begin
                                   if hp^.nextPara.paratype.def=pt.resulttype.def then
                                    begin
                                       include(pt.callparaflags,cpf_exact_match_found);
                                       hp^.nextPara.argconvtyp:=act_exact;
                                    end
                                   else
                                    hp^.nextPara.argconvtyp:=act_equal;
                                   exactmatch:=true;
                                 end
                                else
                                 begin
                                   hp^.nextPara.argconvtyp:=act_convertable;
                                   hp^.nextPara.convertlevel:=isconvertable(pt.resulttype.def,hp^.nextPara.paratype.def,
                                       hcvt,pt.left.nodetype,false);
                                   case hp^.nextPara.convertlevel of
                                    1 : include(pt.callparaflags,cpf_convlevel1found);
                                    2 : include(pt.callparaflags,cpf_convlevel2found);
                                   end;
                                 end;
                              end;

                             hp:=hp^.next;
                          end;

                        { If there was an exactmatch then delete all convertables }
                        if exactmatch then
                          begin
                            hp:=procs;
                            procs:=nil;
                            while assigned(hp) do
                              begin
                                 hp2:=hp^.next;
                                 { keep if not convertable }
                                 if (hp^.nextPara.argconvtyp<>act_convertable) then
                                  begin
                                    hp^.next:=procs;
                                    procs:=hp;
                                  end
                                 else
                                  dispose(hp);
                                 hp:=hp2;
                              end;
                          end
                        else
                        { No exact match was found, remove all procedures that are
                          not convertable (convertlevel=0) }
                          begin
                            hp:=procs;
                            procs:=nil;
                            while assigned(hp) do
                              begin
                                 hp2:=hp^.next;
                                 { keep if not convertable }
                                 if (hp^.nextPara.convertlevel<>0) then
                                  begin
                                    hp^.next:=procs;
                                    procs:=hp;
                                  end
                                 else
                                  begin
                                    { save the type for nice error message }
                                    lastparatype:=hp^.nextPara.paratype.def;
                                    dispose(hp);
                                  end;
                                 hp:=hp2;
                              end;
                          end;
                        { update nextpara for all procedures }
                        hp:=procs;
                        while assigned(hp) do
                          begin
                            { only goto next para if we're out of the varargs }
                            if not(po_varargs in hp^.data.procoptions) or
                               (lastpara<=hp^.data.maxparacount) then
                              hp^.nextpara:=tparaitem(hp^.nextPara.next);
                            hp:=hp^.next;
                          end;
                        { load next parameter or quit loop if no procs left }
                        if assigned(procs) then
                          pt:=tcallparanode(pt.right)
                        else
                          break;
                     end;

                 { All parameters are checked, check if there are any
                   procedures left }
                   if not assigned(procs) then
                    begin
                      { there is an error, must be wrong type, because
                        wrong size is already checked (PFV) }
                      if (not assigned(lastparatype)) or
                         (not assigned(pt)) or
                         (not assigned(pt.resulttype.def)) then
                        internalerror(39393)
                      else
                        begin
                          aktfilepos:=pt.fileinfo;
                          CGMessage3(type_e_wrong_parameter_type,tostr(lastpara),
                            pt.resulttype.def.typename,lastparatype.typename);
                        end;
                      symtableprocentry.write_parameter_lists(nil);
                      goto errorexit;
                    end;

                   { if there are several choices left then for orddef }
                   { if a type is totally included in the other        }
                   { we don't fear an overflow ,                       }
                   { so we can do as if it is an exact match           }
                   { this will convert integer to longint              }
                   { rather than to words                              }
                   { conversion of byte to integer or longint          }
                   { would still not be solved                         }
                   if assigned(procs) and assigned(procs^.next) then
                     begin
                        hp:=procs;
                        while assigned(hp) do
                          begin
                            hp^.nextpara:=hp^.firstpara;
                            hp:=hp^.next;
                          end;
                        pt:=tcallparanode(left);
                        while assigned(pt) do
                          begin
                             { matches a parameter of one procedure exact ? }
                             exactmatch:=false;
                             def_from:=pt.resulttype.def;
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  if not is_equal(pt,hp^.nextPara.paratype.def) then
                                    begin
                                       def_to:=hp^.nextPara.paratype.def;
                                       if ((def_from.deftype=orddef) and (def_to.deftype=orddef)) and
                                         (is_in_limit(def_from,def_to) or
                                         ((hp^.nextPara.paratyp in [vs_var,vs_out]) and
                                         (def_from.size=def_to.size))) then
                                         begin
                                            exactmatch:=true;
                                            conv_to:=def_to;
                                            { there's no use in continuing the search, it will }
                                            { only result in conv_to being overwritten         }
                                            break;
                                         end;
                                    end;
                                  hp:=hp^.next;
                               end;

                             { .... if yes, del all the other procedures }
                             if exactmatch then
                               begin
                                  { the first .... }
                                  while (assigned(procs)) and not(is_in_limit(def_from,procs^.nextPara.paratype.def)) do
                                    begin
                                       hp:=procs^.next;
                                       dispose(procs);
                                       procs:=hp;
                                    end;
                                  { and the others }
                                  hp:=procs;
                                  while (assigned(hp)) and assigned(hp^.next) do
                                    begin
                                       def_to:=hp^.next^.nextPara.paratype.def;
                                       if not(is_in_limit(def_from,def_to)) then
                                         begin
                                            hp2:=hp^.next^.next;
                                            dispose(hp^.next);
                                            hp^.next:=hp2;
                                         end
                                       else
                                         begin
                                           { did we possibly find a better match? }
                                           if (conv_to.size>def_to.size) or
                                              is_in_limit(def_to,conv_to) then
                                             begin
                                                { is it the same as the previous best? }
                                                if not defbase.is_equal(def_to,conv_to) then
                                                  begin
                                                    { no -> remove all previous best matches }
                                                    hp := hp^.next;
                                                    while procs <> hp do
                                                      begin
                                                        hp2 := procs;
                                                        procs := procs^.next;
                                                        dispose(hp2);
                                                      end;
                                                    { set new match type }
                                                    conv_to:=def_to;
                                                  end
                                                { the new one matches just as well as the }
                                                { old one -> keep both                    }
                                                else
                                                  hp := hp^.next;
                                             end
                                           { not a better match -> remove }
                                           else
                                             begin
                                               hp2 := hp^.next^.next;
                                               dispose(hp^.next);
                                               hp^.next:=hp2;
                                             end;
                                         end;
                                    end;
                               end;
                             { update nextpara for all procedures }
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  hp^.nextpara:=tparaitem(hp^.nextPara.next);
                                  hp:=hp^.next;
                               end;
                             pt:=tcallparanode(pt.right);
                          end;
                     end;

                   { let's try to eliminate equal if there is an exact match
                     is there }
                   if assigned(procs) and assigned(procs^.next) then
                     begin
                        { reset nextpara for all procs left }
                        hp:=procs;
                        while assigned(hp) do
                         begin
                           hp^.nextpara:=hp^.firstpara;
                           hp:=hp^.next;
                         end;

                        pt:=tcallparanode(left);
                        while assigned(pt) do
                          begin
                             if cpf_exact_match_found in pt.callparaflags then
                               begin
                                 hp:=procs;
                                 procs:=nil;
                                 while assigned(hp) do
                                   begin
                                      hp2:=hp^.next;
                                      { keep the exact matches, dispose the others }
                                      if (hp^.nextPara.argconvtyp=act_exact) then
                                       begin
                                         hp^.next:=procs;
                                         procs:=hp;
                                       end
                                      else
                                       dispose(hp);
                                      hp:=hp2;
                                   end;
                               end;
                             { update nextpara for all procedures }
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  hp^.nextpara:=tparaitem(hp^.nextPara.next);
                                  hp:=hp^.next;
                               end;
                             pt:=tcallparanode(pt.right);
                          end;
                     end;

                   { Check if there are integer constant to integer
                     parameters then choose the best matching integer
                     parameter and remove the others, this is Delphi
                     compatible. 1 = byte, 256 = word, etc. }
                   if assigned(procs) and assigned(procs^.next) then
                     begin
                        { reset nextpara for all procs left }
                        hp:=procs;
                        while assigned(hp) do
                         begin
                           hp^.nextpara:=hp^.firstpara;
                           hp:=hp^.next;
                         end;

                        pt:=tcallparanode(left);
                        while assigned(pt) do
                          begin
                            bestord:=nil;
                            if (pt.left.nodetype=ordconstn) and
                               is_integer(pt.resulttype.def) then
                             begin
                               hp:=procs;
                               while assigned(hp) do
                                begin
                                  def_to:=hp^.nextPara.paratype.def;
                                  { to be sure, it couldn't be something else,
                                    also the defs here are all in the range
                                    so now find the closest range }
                                  if not is_integer(def_to) then
                                   internalerror(43297815);
                                  if (not assigned(bestord)) or
                                     ((torddef(def_to).low>bestord.low) or
                                      (torddef(def_to).high<bestord.high)) then
                                   bestord:=torddef(def_to);
                                  hp:=hp^.next;
                                end;
                             end;
                            { if a bestmatch is found then remove the other
                              procs which don't match the bestord }
                            if assigned(bestord) then
                             begin
                               hp:=procs;
                               procs:=nil;
                               while assigned(hp) do
                                begin
                                  hp2:=hp^.next;
                                  { keep matching bestord, dispose the others }
                                  if (torddef(hp^.nextPara.paratype.def)=bestord) then
                                   begin
                                     hp^.next:=procs;
                                     procs:=hp;
                                   end
                                  else
                                   dispose(hp);
                                  hp:=hp2;
                                end;
                             end;

                            { update nextpara for all procedures }
                            hp:=procs;
                            while assigned(hp) do
                             begin
                               hp^.nextpara:=tparaitem(hp^.nextPara.next);
                               hp:=hp^.next;
                             end;
                            pt:=tcallparanode(pt.right);
                          end;
                     end;

                   { Check if there are convertlevel 1 and 2 differences
                     left for the parameters, then discard all convertlevel
                     2 procedures. The value of convlevelXfound can still
                     be used, because all convertables are still here or
                     not }
                   if assigned(procs) and assigned(procs^.next) then
                     begin
                        { reset nextpara for all procs left }
                        hp:=procs;
                        while assigned(hp) do
                         begin
                           hp^.nextpara:=hp^.firstpara;
                           hp:=hp^.next;
                         end;

                        pt:=tcallparanode(left);
                        while assigned(pt) do
                          begin
                             if (cpf_convlevel1found in pt.callparaflags) and
                               (cpf_convlevel2found in pt.callparaflags) then
                               begin
                                 hp:=procs;
                                 procs:=nil;
                                 while assigned(hp) do
                                   begin
                                      hp2:=hp^.next;
                                      { keep all not act_convertable and all convertlevels=1 }
                                      if (hp^.nextPara.argconvtyp<>act_convertable) or
                                         (hp^.nextPara.convertlevel=1) then
                                       begin
                                         hp^.next:=procs;
                                         procs:=hp;
                                       end
                                      else
                                       dispose(hp);
                                      hp:=hp2;
                                   end;
                               end;
                             { update nextpara for all procedures }
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  hp^.nextpara:=tparaitem(hp^.nextPara.next);
                                  hp:=hp^.next;
                               end;
                             pt:=tcallparanode(pt.right);
                          end;
                     end;

                   if not(assigned(procs)) or assigned(procs^.next) then
                     begin
                        CGMessage(cg_e_cant_choose_overload_function);
                        symtableprocentry.write_parameter_lists(nil);
                        goto errorexit;
                     end;
                   if make_ref then
                     begin
                        procs^.data.lastref:=tref.create(procs^.data.lastref,@fileinfo);
                        inc(procs^.data.refcount);
                        if procs^.data.defref=nil then
                          procs^.data.defref:=procs^.data.lastref;
                     end;

                   procdefinition:=procs^.data;
                   { big error for with statements
                   symtableproc:=procdefinition.owner;
                   but neede for overloaded operators !! }
                   if symtableproc=nil then
                     symtableproc:=procdefinition.owner;
               end; { end of procedure to call determination }


              { add needed default parameters }
              if assigned(procs) and
                 (paralength<procdefinition.maxparacount) then
               begin
                 { add default parameters, just read back the skipped
                   paras starting from firstPara.previous, when not available
                   (all parameters are default) then start with the last
                   parameter and read backward (PFV) }
                 if not assigned(procs^.firstpara) then
                  pdc:=tparaitem(procs^.data.Para.last)
                 else
                  pdc:=tparaitem(procs^.firstPara.previous);
                 while assigned(pdc) do
                  begin
                    if not assigned(pdc.defaultvalue) then
                     internalerror(751349858);
                    left:=ccallparanode.create(genconstsymtree(tconstsym(pdc.defaultvalue)),left);
                    pdc:=tparaitem(pdc.previous);
                  end;
               end;
           end;

          { handle predefined procedures }
          is_const:=(po_internconst in procdefinition.procoptions) and
                    ((block_type in [bt_const,bt_type]) or
                     (assigned(left) and (tcallparanode(left).left.nodetype in [realconstn,ordconstn])));
          if (procdefinition.proccalloption=pocall_internproc) or is_const then
           begin
             if assigned(left) then
              begin
                { ptr and settextbuf needs two args }
                if assigned(tcallparanode(left).right) then
                 begin
                   hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,left);
                   left:=nil;
                 end
                else
                 begin
                   hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,tcallparanode(left).left);
                   tcallparanode(left).left:=nil;
                 end;
              end
             else
              hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,nil);
             result:=hpt;
             goto errorexit;
           end;

{$ifdef dummy}
         { Calling a message method directly ? }
         if assigned(procdefinition) and
            (po_containsself in procdefinition.procoptions) then
           message(cg_e_cannot_call_message_direct);
{$endif}

         { ensure that the result type is set }
         if not restypeset then
           resulttype:=procdefinition.rettype
         else
           resulttype:=restype;

         { modify the exit code, in case of special cases }
         if (not is_void(resulttype.def)) then
          begin
            if paramanager.ret_in_reg(resulttype.def) then
             begin
               { wide- and ansistrings are returned in EAX    }
               { but they are imm. moved to a memory location }
               if is_widestring(resulttype.def) or
                  is_ansistring(resulttype.def) then
                 begin
                   { we use ansistrings so no fast exit here }
                   if assigned(procinfo) then
                    procinfo.no_fast_exit:=true;
                 end;
             end;
          end;

         { constructors return their current class type, not the type where the
           constructor is declared, this can be different because of inheritance }
         if (procdefinition.proctypeoption=potype_constructor) then
           begin
             if assigned(methodpointer) and
                assigned(methodpointer.resulttype.def) and
                (methodpointer.resulttype.def.deftype=classrefdef) then
               resulttype:=tclassrefdef(methodpointer.resulttype.def).pointertype;
           end;

         { flag all callparanodes that belong to the varargs }
         if (po_varargs in procdefinition.procoptions) then
          begin
            pt:=tcallparanode(left);
            i:=paralength;
            while (i>procdefinition.maxparacount) do
             begin
               include(tcallparanode(pt).flags,nf_varargs_para);
               pt:=tcallparanode(pt.right);
               dec(i);
             end;
          end;

         { insert type conversions }
         if assigned(left) then
          begin
            aktcallprocdef:=procdefinition;
            tcallparanode(left).insert_typeconv(tparaitem(procdefinition.Para.first),true);
          end;

      errorexit:
         { Reset some settings back }
         if assigned(procs) then
           dispose(procs);
         aktcallprocdef:=oldcallprocdef;
      end;
{$endif}

    function tcallnode.pass_1 : tnode;
      var
         inlinecode : tnode;
         inlined : boolean;
{$ifdef m68k}
         regi : tregister;
{$endif}
         method_must_be_valid : boolean;
      label
        errorexit;
      begin
         { the default is nothing to return }
         location.loc:=LOC_INVALID;

         result:=nil;
         inlined:=false;
         inlinecode := nil;

         { work trough all parameters to get the register requirements }
         if assigned(left) then
           tcallparanode(left).det_registers;

         { return node }
         if assigned(funcretrefnode) then
           firstpass(funcretrefnode);

         if assigned(procdefinition) and
            (procdefinition.proccalloption=pocall_inline) then
           begin
              inlinecode:=right;
              if assigned(inlinecode) then
               inlined:=true;
              right:=nil;
           end;

         { procedure variable ? }
         if assigned(right) then
           begin
              firstpass(right);

              { procedure does a call }
              if not (block_type in [bt_const,bt_type]) then
                procinfo.flags:=procinfo.flags or pi_do_call;
              rg.incrementregisterpushed(all_registers);
           end
         else
         { not a procedure variable }
           begin
              { calc the correture value for the register }
              { handle predefined procedures }
              if (procdefinition.proccalloption=pocall_inline) then
                begin
                   if assigned(methodpointer) then
                     CGMessage(cg_e_unable_inline_object_methods);
                   if assigned(right) and (right.nodetype<>procinlinen) then
                     CGMessage(cg_e_unable_inline_procvar);
                   { nodetype:=procinlinen; }
                   if not assigned(right) then
                     begin
                        if assigned(tprocdef(procdefinition).code) then
                          inlinecode:=cprocinlinenode.create(tprocdef(procdefinition))
                        else
                          CGMessage(cg_e_no_code_for_inline_stored);
                        if assigned(inlinecode) then
                          begin
                             { consider it has not inlined if called
                               again inside the args }
                             procdefinition.proccalloption:=pocall_fpccall;
                             firstpass(inlinecode);
                             inlined:=true;
                          end;
                     end;
                end
              else
                begin
                  if not (block_type in [bt_const,bt_type]) then
                    procinfo.flags:=procinfo.flags or pi_do_call;
                end;

             { It doesn't hurt to calculate it already though :) (JM) }
             rg.incrementregisterpushed(tprocdef(procdefinition).usedregisters);

           end;

         { get a register for the return value }
         if (not is_void(resulttype.def)) then
           begin
             if paramanager.ret_in_param(resulttype.def) then
              begin
                location.loc:=LOC_CREFERENCE;
              end
             else
             { ansi/widestrings must be registered, so we can dispose them }
              if is_ansistring(resulttype.def) or
                 is_widestring(resulttype.def) then
               begin
                 location.loc:=LOC_CREFERENCE;
                 registers32:=1;
               end
             else
             { we have only to handle the result if it is used }
              if (nf_return_value_used in flags) then
               begin
                 case resulttype.def.deftype of
                   enumdef,
                   orddef :
                     begin
                       if (procdefinition.proctypeoption=potype_constructor) then
                        begin
                          if assigned(methodpointer) and
                             (methodpointer.resulttype.def.deftype=classrefdef) then
                           begin
                             location.loc:=LOC_REGISTER;
                             registers32:=1;
                           end
                          else
                           location.loc:=LOC_FLAGS;
                        end
                       else
                        begin
                          location.loc:=LOC_REGISTER;
                          if is_64bitint(resulttype.def) then
                            registers32:=2
                          else
                            registers32:=1;
                        end;
                     end;
                   floatdef :
                     begin
                       location.loc:=LOC_FPUREGISTER;
{$ifdef m68k}
                       if (cs_fp_emulation in aktmoduleswitches) or
                          (tfloatdef(resulttype.def).typ=s32real) then
                         registers32:=1
                       else
                         registersfpu:=1;
{$else not m68k}
                        registersfpu:=1;
{$endif not m68k}
                     end;
                   else
                     begin
                       location.loc:=LOC_REGISTER;
                       registers32:=1;
                     end;
                 end;
               end;
           end;
         { a fpu can be used in any procedure !! }
{$ifdef i386}
         registersfpu:=procdefinition.fpu_used;
{$endif i386}
         { if this is a call to a method calc the registers }
         if (methodpointer<>nil) then
           begin
              case methodpointer.nodetype of
                { but only, if this is not a supporting node }
                typen: ;
                { we need one register for new return value PM }
                hnewn : if registers32=0 then
                          registers32:=1;
                else
                  begin
                     if (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) and
                        assigned(symtableproc) and (symtableproc.symtabletype=withsymtable) and
                        not twithsymtable(symtableproc).direct_with then
                       begin
                          CGmessage(cg_e_cannot_call_cons_dest_inside_with);
                       end; { Is accepted by Delphi !! }
                     { this is not a good reason to accept it in FPC if we produce
                       wrong code for it !!! (PM) }

                     { R.Assign is not a constructor !!! }
                     { but for R^.Assign, R must be valid !! }
                     if (procdefinition.proctypeoption=potype_constructor) or
                        ((methodpointer.nodetype=loadn) and
                         ((methodpointer.resulttype.def.deftype=classrefdef) or
                          ((methodpointer.resulttype.def.deftype=objectdef) and
                           not(oo_has_virtual in tobjectdef(methodpointer.resulttype.def).objectoptions)
                          )
                         )
                        ) then
                       method_must_be_valid:=false
                     else
                       method_must_be_valid:=true;
                     firstpass(methodpointer);
                     set_varstate(methodpointer,method_must_be_valid);
                     { The object is already used ven if it is called once }
                     if (methodpointer.nodetype=loadn) and
                        (tloadnode(methodpointer).symtableentry.typ=varsym) then
                       tvarsym(tloadnode(methodpointer).symtableentry).varstate:=vs_used;

                     registersfpu:=max(methodpointer.registersfpu,registersfpu);
                     registers32:=max(methodpointer.registers32,registers32);
{$ifdef SUPPORT_MMX }
                     registersmmx:=max(methodpointer.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
                  end;
              end;
           end;

         if inlined then
           right:=inlinecode;
         { determine the registers of the procedure variable }
         { is this OK for inlined procs also ?? (PM)     }
         if assigned(right) then
           begin
              registersfpu:=max(right.registersfpu,registersfpu);
              registers32:=max(right.registers32,registers32);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(right.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
           end;
         { determine the registers of the procedure }
         if assigned(left) then
           begin
              registersfpu:=max(left.registersfpu,registersfpu);
              registers32:=max(left.registers32,registers32);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(left.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
           end;
      errorexit:
         if inlined then
           procdefinition.proccalloption:=pocall_inline;
      end;

{$ifdef state_tracking}
    function Tcallnode.track_state_pass(exec_known:boolean):boolean;

    var hp:Tcallparanode;
        value:Tnode;

    begin
        track_state_pass:=false;
        hp:=Tcallparanode(left);
        while assigned(hp) do
            begin
                if left.track_state_pass(exec_known) then
                    begin
                        left.resulttype.def:=nil;
                        do_resulttypepass(left);
                    end;
                value:=aktstate.find_fact(hp.left);
                if value<>nil then
                    begin
                        track_state_pass:=true;
                        hp.left.destroy;
                        hp.left:=value.getcopy;
                        do_resulttypepass(hp.left);
                    end;
                hp:=Tcallparanode(hp.right);
            end;
    end;
{$endif}

    function tcallnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableprocentry = tcallnode(p).symtableprocentry) and
          (symtableproc = tcallnode(p).symtableproc) and
          (procdefinition = tcallnode(p).procdefinition) and
          (methodpointer.isequal(tcallnode(p).methodpointer)) and
          ((restypeset and tcallnode(p).restypeset and
            (is_equal(restype.def,tcallnode(p).restype.def))) or
           (not restypeset and not tcallnode(p).restypeset));
      end;

{****************************************************************************
                            TPROCINLINENODE
 ****************************************************************************}

    constructor tprocinlinenode.create(p:tprocdef);

      begin
         inherited create(procinlinen);
         inlineprocdef:=p;
         retoffset:=-POINTER_SIZE; { less dangerous as zero (PM) }
         para_offset:=0;
         para_size:=0;
         { copy inlinetree }
         if assigned(p.code) then
           inlinetree:=p.code.getcopy
         else
           inlinetree:=nil;
      end;


    destructor tprocinlinenode.destroy;
      begin
        if assigned(inlinetree) then
          inlinetree.free;
        inherited destroy;
      end;


    constructor tprocinlinenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        inlineprocdef:=tprocdef(ppufile.getderef);
        inlinetree:=ppuloadnode(ppufile);
        retoffset:=-POINTER_SIZE; { less dangerous as zero (PM) }
        para_offset:=0;
        para_size:=0;
      end;


    procedure tprocinlinenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(inlineprocdef);
        ppuwritenode(ppufile,inlinetree);
      end;


    procedure tprocinlinenode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(inlinetree) then
          inlinetree.derefimpl;
        resolvedef(pointer(inlineprocdef));
      end;


    function tprocinlinenode.getcopy : tnode;

      var
         n : tprocinlinenode;

      begin
         n:=tprocinlinenode(inherited getcopy);
         n.inlineprocdef:=inlineprocdef;
         if assigned(inlinetree) then
           n.inlinetree:=inlinetree.getcopy
         else
           n.inlinetree:=nil;
         n.retoffset:=retoffset;
         n.para_offset:=para_offset;
         n.para_size:=para_size;
         getcopy:=n;
      end;

    procedure tprocinlinenode.insertintolist(l : tnodelist);

      begin
      end;


    function tprocinlinenode.det_resulttype : tnode;
      begin
         resulttype:=inlineprocdef.rettype;
         { retrieve info from inlineprocdef }
         retoffset:=-POINTER_SIZE; { less dangerous as zero (PM) }
         para_offset:=0;
         para_size:=inlineprocdef.para_size(target_info.alignment.paraalign);
         if paramanager.ret_in_param(inlineprocdef.rettype.def) then
           inc(para_size,POINTER_SIZE);
         result:=nil;
      end;


    function tprocinlinenode.pass_1 : tnode;
      begin
        firstpass(inlinetree);
        registers32:=inlinetree.registers32;
        registersfpu:=inlinetree.registersfpu;
{$ifdef SUPPORT_MMX}
        registersmmx:=inlinetree.registersmmx;
{$endif SUPPORT_MMX}
        result:=nil;
      end;

    function tprocinlinenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          inlinetree.isequal(tprocinlinenode(p).inlinetree) and
          (inlineprocdef = tprocinlinenode(p).inlineprocdef);
      end;

begin
   ccallnode:=tcallnode;
   ccallparanode:=tcallparanode;
   cprocinlinenode:=tprocinlinenode;
end.
{
  $Log$
  Revision 1.95  2002-09-03 21:32:49  daniel
    * Small bugfix for procdef selection

  Revision 1.94  2002/09/03 19:27:22  daniel
    * Activated new ncal code

  Revision 1.93  2002/09/03 16:26:26  daniel
    * Make Tprocdef.defs protected

  Revision 1.92  2002/09/01 13:28:37  daniel
   - write_access fields removed in favor of a flag

  Revision 1.91  2002/09/01 12:14:15  peter
    * remove debug line
    * containself methods can be called directly

  Revision 1.90  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.89  2002/08/23 16:13:16  peter
    * also firstpass funcretrefnode if available. This was breaking the
      asnode compilerproc code

  Revision 1.88  2002/08/20 10:31:26  daniel
   * Tcallnode.det_resulttype rewritten

  Revision 1.87  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.86  2002/08/17 22:09:44  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.85  2002/08/17 09:23:34  florian
    * first part of procinfo rewrite

  Revision 1.84  2002/08/16 14:24:57  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.83  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.82  2002/07/19 11:41:35  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.81  2002/07/15 18:03:14  florian
    * readded removed changes

  Revision 1.79  2002/07/11 14:41:27  florian
    * start of the new generic parameter handling

  Revision 1.80  2002/07/14 18:00:43  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.78  2002/07/04 20:43:00  florian
    * first x86-64 patches

  Revision 1.77  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.76  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.75  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.73  2002/05/12 16:53:06  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.72  2002/04/25 20:16:38  peter
    * moved more routines from cga/n386util

  Revision 1.71  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.70  2002/04/16 16:09:08  peter
    * allow passing the address of a procedure to a formal parameter
      in delphi mode

  Revision 1.69  2002/04/15 19:44:19  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.68  2002/04/15 18:57:22  carl
  + target_info.size_of_pointer -> POINTER_SIZE

  Revision 1.67  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.66  2002/03/31 20:26:33  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.65  2002/03/30 23:02:42  carl
  * avoid crash with inline routines

  Revision 1.64  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.63  2002/01/24 12:33:52  jonas
    * adapted ranges of native types to int64 (e.g. high cardinal is no
      longer longint($ffffffff), but just $fffffff in psystem)
    * small additional fix in 64bit rangecheck code generation for 32 bit
      processors
    * adaption of ranges required the matching talgorithm used for selecting
      which overloaded procedure to call to be adapted. It should now always
      select the closest match for ordinal parameters.
    + inttostr(qword) in sysstr.inc/sysstrh.inc
    + abs(int64), sqr(int64), sqr(qword) in systemh.inc/generic.inc (previous
      fixes were required to be able to add them)
    * is_in_limit() moved from ncal to types unit, should always be used
      instead of direct comparisons of low/high values of orddefs because
      qword is a special case

  Revision 1.62  2002/01/19 11:57:05  peter
    * fixed path appending for lib

}
