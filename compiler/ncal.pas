{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This file implements the node for sub procedure calling.

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

interface

    uses
       cutils,cclasses,
       globtype,
       paramgr,parabase,
       node,nbas,nutils,
       {$ifdef state_tracking}
       nstate,
       {$endif state_tracking}
       symbase,symtype,symsym,symdef,symtable;

    type
       tcallnodeflag = (
         cnf_restypeset,
         cnf_return_value_used,
         cnf_inherited,
         cnf_anon_inherited,
         cnf_new_call,
         cnf_dispose_call,
         cnf_member_call,        { called with implicit methodpointer tree }
         cnf_uses_varargs        { varargs are used in the declaration }
       );
       tcallnodeflags = set of tcallnodeflag;

       tcallnode = class(tbinarynode)
       private
          { info for inlining }
          inlinelocals: TList;
          { number of parameters passed from the source, this does not include the hidden parameters }
          paralength   : smallint;
          function  gen_self_tree_methodpointer:tnode;
          function  gen_self_tree:tnode;
          function  gen_vmt_tree:tnode;
          procedure bind_parasym;

          { function return node, this is used to pass the data for a
            ret_in_param return value }
          _funcretnode    : tnode;
          procedure setfuncretnode(const returnnode: tnode);
          procedure convert_carg_array_of_const;
          procedure order_parameters;

          procedure createinlineparas(var createstatement, deletestatement: tstatementnode);
          function  replaceparaload(var n: tnode; arg: pointer): foreachnoderesult;
          procedure createlocaltemps(p:TNamedIndexItem;arg:pointer);
          function  pass1_inline:tnode;
       protected
          pushedparasize : longint;
       public
          { the symbol containing the definition of the procedure }
          { to call                                               }
          symtableprocentry : tprocsym;
          symtableprocentryderef : tderef;
          { symtable where the entry was found, needed for with support }
          symtableproc   : tsymtable;
          { the definition of the procedure to call }
          procdefinition : tabstractprocdef;
          procdefinitionderef : tderef;
          { tree that contains the pointer to the object for this method }
          methodpointerinit,
          methodpointerdone : tblocknode;
          methodpointer  : tnode;
{$ifdef PASS2INLINE}
          { inline function body }
          inlinecode : tnode;
{$endif PASS2INLINE}
          { varargs parasyms }
          varargsparas : tvarargsparalist;
          { node that specifies where the result should be put for calls }
          { that return their result in a parameter                      }
          property funcretnode: tnode read _funcretnode write setfuncretnode;


          { separately specified resulttype for some compilerprocs (e.g. }
          { you can't have a function with an "array of char" resulttype }
          { the RTL) (JM)                                                }
          restype: ttype;
          callnodeflags : tcallnodeflags;

          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(l:tnode; v : tprocsym;st : tsymtable; mp: tnode; callflags:tcallnodeflags);virtual;
          constructor create_procvar(l,r:tnode);
          constructor createintern(const name: string; params: tnode);
          constructor createinternres(const name: string; params: tnode; const res: ttype);
          constructor createinternreturn(const name: string; params: tnode; returnnode : tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function  getcopy : tnode;override;
          { Goes through all symbols in a class and subclasses and calls
            verify abstract for each .
          }
          procedure verifyabstractcalls;
          { called for each definition in a class and verifies if a method
            is abstract or not, if it is abstract, give out a warning
          }
          procedure verifyabstract(p : tnamedindexitem;arg:pointer);
          procedure insertintolist(l : tnodelist);override;
          function  pass_1 : tnode;override;
          function  det_resulttype:tnode;override;
       {$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
       {$endif state_tracking}
          function  docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
          function  para_count:longint;
          function  get_load_methodpointer:tnode;
       private
          AbstractMethodsList : TStringList;
       end;
       tcallnodeclass = class of tcallnode;

       tcallparaflag = (
          cpf_is_colon_para,
          cpf_varargs_para   { belongs this para to varargs }
       );
       tcallparaflags = set of tcallparaflag;

       tcallparanode = class(tbinarynode)
       public
          callparaflags : tcallparaflags;
          parasym       : tparavarsym;
          used_by_callnode : boolean;
          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(expr,next : tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure get_paratype;
          procedure insert_typeconv(do_count : boolean);
          procedure det_registers;
          procedure firstcallparan;
          procedure secondcallparan;virtual;abstract;
          function docompare(p: tnode): boolean; override;
          procedure printnodetree(var t:text);override;
       end;
       tcallparanodeclass = class of tcallparanode;

    function reverseparameters(p: tcallparanode): tcallparanode;

    var
      ccallnode : tcallnodeclass;
      ccallparanode : tcallparanodeclass;

      { Current callnode, this is needed for having a link
       between the callparanodes and the callnode they belong to }
      aktcallnode : tcallnode;


implementation

    uses
      systems,
      verbose,globals,
      symconst,defutil,defcmp,
      htypechk,pass_1,
      ncnv,nld,ninl,nadd,ncon,nmem,
      procinfo,
      cgbase
      ;

type
     tobjectinfoitem = class(tlinkedlistitem)
       objinfo : tobjectdef;
       constructor create(def : tobjectdef);
     end;


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


    procedure maybe_load_para_in_temp(var p:tnode);

        function is_simple_node(hp:tnode):boolean;
        begin
          is_simple_node:=(hp.nodetype in [typen,loadvmtaddrn,loadn,arrayconstructorn]);
        end;

      var
        hp,
        loadp,
        refp  : tnode;
        htype : ttype;
        ptemp : ttempcreatenode;
        usederef : boolean;
        usevoidpointer : boolean;
        newinitstatement,
        newdonestatement : tstatementnode;
      begin
        if not assigned(aktcallnode) then
          internalerror(200410121);

        { Load all complex loads into a temp to prevent
          double calls to a function. We can't simply check for a hp.nodetype=calln
           }
        hp:=p;
        while assigned(hp) and
              (hp.nodetype=typeconvn) and
              (ttypeconvnode(hp).convtype=tc_equal) do
          hp:=tunarynode(hp).left;
        if assigned(hp) and
           not is_simple_node(hp) then
          begin
            if not assigned(aktcallnode.methodpointerinit) then
              begin
                aktcallnode.methodpointerinit:=internalstatements(newinitstatement);
                aktcallnode.methodpointerdone:=internalstatements(newdonestatement);
              end
            else
              begin
                newinitstatement:=laststatement(aktcallnode.methodpointerinit);
                newdonestatement:=laststatement(aktcallnode.methodpointerdone);
              end;
            { temp create }
            usederef:=(p.resulttype.def.deftype in [arraydef,recorddef]) or
                      is_shortstring(p.resulttype.def) or
                      is_object(p.resulttype.def);
            { avoid refcount increase }
            usevoidpointer:=is_interface(p.resulttype.def);
 
            if usederef then
              htype.setdef(tpointerdef.create(p.resulttype))
            else
              htype:=p.resulttype;

            if usevoidpointer then
              begin
                ptemp:=ctempcreatenode.create(voidpointertype,voidpointertype.def.size,tt_persistent,true);
                loadp := ctypeconvnode.create_internal(p,voidpointertype);
                refp:=ctypeconvnode.create_internal(ctemprefnode.create(ptemp),htype);
              end
            else
              begin
                ptemp:=ctempcreatenode.create(htype,htype.def.size,tt_persistent,true);
                if usederef then
                  begin
                    loadp:=caddrnode.create_internal(p);
                    refp:=cderefnode.create(ctemprefnode.create(ptemp));
                  end
                else
                  begin
                    loadp:=p;
                    refp:=ctemprefnode.create(ptemp)
                  end
              end;
            addstatement(newinitstatement,ptemp);
            addstatement(newinitstatement,cassignmentnode.create(
                ctemprefnode.create(ptemp),
                loadp));
            { new tree is only a temp reference }
            p:=refp;
            { temp release. We need to return a reference to the methodpointer
              otherwise the conversion from callnode to loadnode can't be done
              for the methodpointer unless the loadnode will also get a methodpointerinit and
              methodpointerdone node. For the moment we use register as temp and therefor
              don't create a temp-leak in the stackframe (PFV) }
            { the last statement should return the value as
              location and type, this is done be referencing the
              temp and converting it first from a persistent temp to
              normal temp }
            addstatement(newdonestatement,ctempdeletenode.create_normal_temp(ptemp));
            if usevoidpointer then
              addstatement(newdonestatement,ctypeconvnode.create_internal(
                ctemprefnode.create(ptemp),htype))
            else
              addstatement(newdonestatement,ctemprefnode.create(ptemp));
            { call resulttypepass for new nodes }
            resulttypepass(p);
            resulttypepass(aktcallnode.methodpointerinit);
            resulttypepass(aktcallnode.methodpointerdone);
          end;
      end;


    function gen_high_tree(var p:tnode;paradef:tdef):tnode;
      var
        temp: tnode;
        len : integer;
        loadconst : boolean;
        hightree : tnode;
      begin
        len:=-1;
        loadconst:=true;
        hightree:=nil;
        case p.resulttype.def.deftype of
          arraydef :
            begin
              if (paradef.deftype<>arraydef) then
                internalerror(200405241);
              { passing a string to an array of char }
              if (p.nodetype=stringconstn) then
                begin
                  len:=str_length(p);
                  if len>0 then
                   dec(len);
                end
              else
              { handle special case of passing an single array to an array of array }
              if compare_defs(tarraydef(paradef).elementtype.def,p.resulttype.def,nothingn)>=te_equal then
                len:=0
              else
                begin
                  { handle via a normal inline in_high_x node }
                  loadconst:=false;
                  { slice? }
                  if (p.nodetype=inlinen) and (tinlinenode(p).inlinenumber=in_slice_x) then
                    begin
                      hightree:=tcallparanode(tcallparanode(tinlinenode(p).left).right).left;
                      hightree:=caddnode.create(subn,hightree,genintconstnode(1));
                      tcallparanode(tcallparanode(tinlinenode(p).left).right).left:=nil;

                      temp:=p;
                      p:=tcallparanode(tinlinenode(p).left).left;
                      tcallparanode(tinlinenode(temp).left).left:=nil;
                      temp.free;

                      resulttypepass(hightree);
                    end
                  else
                    begin
                      maybe_load_para_in_temp(p);
                      hightree:=geninlinenode(in_high_x,false,p.getcopy);
                      resulttypepass(hightree);
                      { only substract low(array) if it's <> 0 }
                      temp:=geninlinenode(in_low_x,false,p.getcopy);
                      resulttypepass(temp);
                      if (temp.nodetype <> ordconstn) or
                         (tordconstnode(temp).value <> 0) then
                        hightree := caddnode.create(subn,hightree,temp)
                      else
                        temp.free;
                    end;
                end;
            end;
          stringdef :
            begin
              if is_open_string(paradef) then
               begin
                 maybe_load_para_in_temp(p);
                 { handle via a normal inline in_high_x node }
                 loadconst := false;
                 hightree := geninlinenode(in_high_x,false,p.getcopy);
               end
              else
               begin
                 { passing a string to an array of char }
                 if (p.nodetype=stringconstn) then
                   begin
                     len:=str_length(p);
                     if len>0 then
                      dec(len);
                   end
                 else
                   begin
                     maybe_load_para_in_temp(p);
                     hightree:=caddnode.create(subn,geninlinenode(in_length_x,false,p.getcopy),
                                               cordconstnode.create(1,s32inttype,false));
                     loadconst:=false;
                   end;
               end;
           end;
        else
          len:=0;
        end;
        if loadconst then
          hightree:=cordconstnode.create(len,s32inttype,true)
        else
          begin
            if not assigned(hightree) then
              internalerror(200304071);
            { Need to use explicit, because it can also be a enum }
            hightree:=ctypeconvnode.create_internal(hightree,s32inttype);
          end;
        result:=hightree;
      end;


{****************************************************************************
                              TOBJECTINFOITEM
 ****************************************************************************}

    constructor tobjectinfoitem.create(def : tobjectdef);
      begin
        inherited create;
        objinfo := def;
      end;


{****************************************************************************
                             TCALLPARANODE
 ****************************************************************************}

    constructor tcallparanode.create(expr,next : tnode);

      begin
         inherited create(callparan,expr,next);
         if not assigned(expr) then
           internalerror(200305091);
         expr.fileinfo:=fileinfo;
         callparaflags:=[];
      end;

    destructor tcallparanode.destroy;

      begin
         { When the node is used by callnode then
           we don't destroy left, the callnode takes care of it }
         if used_by_callnode then
          left:=nil;
         inherited destroy;
      end;


    constructor tcallparanode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getsmallset(callparaflags);
      end;


    procedure tcallparanode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putsmallset(callparaflags);
      end;


    function tcallparanode.getcopy : tnode;

      var
         n : tcallparanode;

      begin
         n:=tcallparanode(inherited getcopy);
         n.callparaflags:=callparaflags;
         n.parasym:=parasym;
         result:=n;
      end;

    procedure tcallparanode.insertintolist(l : tnodelist);

      begin
      end;


    procedure tcallparanode.get_paratype;
      var
        old_array_constructor : boolean;
      begin
         inc(parsing_para_level);
         if assigned(right) then
          tcallparanode(right).get_paratype;
         old_array_constructor:=allow_array_constructor;
         allow_array_constructor:=true;
         resulttypepass(left);
         allow_array_constructor:=old_array_constructor;
         if codegenerror then
          resulttype:=generrortype
         else
          resulttype:=left.resulttype;
         dec(parsing_para_level);
      end;


    procedure tcallparanode.insert_typeconv(do_count : boolean);
      var
        oldtype  : ttype;
        hp       : tnode;
{$ifdef extdebug}
        store_count_ref : boolean;
{$endif def extdebug}
      begin
         inc(parsing_para_level);

{$ifdef extdebug}
         if do_count then
           begin
             store_count_ref:=count_ref;
             count_ref:=true;
           end;
{$endif def extdebug}
         { Be sure to have the resulttype }
         if not assigned(left.resulttype.def) then
           resulttypepass(left);

         if (left.nodetype<>nothingn) then
           begin
             { Convert tp procvars, this is needs to be done
               here to make the change permanent. in the overload
               choosing the changes are only made temporary }
             if (left.resulttype.def.deftype=procvardef) and
                (parasym.vartype.def.deftype<>procvardef) then
               begin
                 if maybe_call_procvar(left,true) then
                   resulttype:=left.resulttype;
               end;

             { Remove implicitly inserted typecast to pointer for
               @procvar in macpas }
             if (m_mac_procvar in aktmodeswitches) and
                (parasym.vartype.def.deftype=procvardef) and
                (left.nodetype=typeconvn) and
                is_voidpointer(left.resulttype.def) and
                (ttypeconvnode(left).left.nodetype=typeconvn) and
                (ttypeconvnode(ttypeconvnode(left).left).convtype=tc_proc_2_procvar) then
               begin
                 hp:=left;
                 left:=ttypeconvnode(left).left;
                 ttypeconvnode(hp).left:=nil;
                 hp.free;
               end;

             { Handle varargs and hidden paras directly, no typeconvs or }
             { typechecking needed                                       }
             if (cpf_varargs_para in callparaflags) then
               begin
                 { convert pascal to C types }
                 case left.resulttype.def.deftype of
                   stringdef :
                     inserttypeconv(left,charpointertype);
                   floatdef :
                     inserttypeconv(left,s64floattype);
                 end;
                 set_varstate(left,vs_read,[vsf_must_be_valid]);
                 resulttype:=left.resulttype;
                 { also update parasym type to get the correct parameter location
                   for the new types }
                 parasym.vartype:=left.resulttype;
               end
             else
              if (vo_is_hidden_para in parasym.varoptions) then
               begin
                 set_varstate(left,vs_read,[vsf_must_be_valid]);
                 resulttype:=left.resulttype;
               end
             else
               begin

                 { Do we need arrayconstructor -> set conversion, then insert
                   it here before the arrayconstructor node breaks the tree
                   with its conversions of enum->ord }
                 if (left.nodetype=arrayconstructorn) and
                    (parasym.vartype.def.deftype=setdef) then
                   inserttypeconv(left,parasym.vartype);

                 { set some settings needed for arrayconstructor }
                 if is_array_constructor(left.resulttype.def) then
                  begin
                    if left.nodetype<>arrayconstructorn then
                      internalerror(200504041);
                    if is_array_of_const(parasym.vartype.def) then
                     begin
                       { force variant array }
                       include(left.flags,nf_forcevaria);
                     end
                    else
                     begin
                       include(left.flags,nf_novariaallowed);
                       { now that the resultting type is know we can insert the required
                         typeconvs for the array constructor }
                       if parasym.vartype.def.deftype=arraydef then
                         tarrayconstructornode(left).force_type(tarraydef(parasym.vartype.def).elementtype);
                     end;
                  end;

                 { check if local proc/func is assigned to procvar }
                 if left.resulttype.def.deftype=procvardef then
                   test_local_to_procvar(tprocvardef(left.resulttype.def),parasym.vartype.def);

                 { test conversions }
                 if not(is_shortstring(left.resulttype.def) and
                        is_shortstring(parasym.vartype.def)) and
                    (parasym.vartype.def.deftype<>formaldef) then
                   begin
                      { Process open parameters }
                      if paramanager.push_high_param(parasym.varspez,parasym.vartype.def,aktcallnode.procdefinition.proccalloption) then
                       begin
                         { insert type conv but hold the ranges of the array }
                         oldtype:=left.resulttype;
                         inserttypeconv(left,parasym.vartype);
                         left.resulttype:=oldtype;
                       end
                      else
                       begin
                         check_ranges(left.fileinfo,left,parasym.vartype.def);
                         inserttypeconv(left,parasym.vartype);
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
                    is_shortstring(parasym.vartype.def) and
                    (parasym.varspez in [vs_out,vs_var]) and
                    not(is_open_string(parasym.vartype.def)) and
                    not(equal_defs(left.resulttype.def,parasym.vartype.def)) then
                   begin
                     aktfilepos:=left.fileinfo;
                     CGMessage(type_e_strict_var_string_violation);
                   end;

                 { Handle formal parameters separate }
                 if (parasym.vartype.def.deftype=formaldef) then
                   begin
                     { load procvar if a procedure is passed }
                     if ((m_tp_procvar in aktmodeswitches) or
                         (m_mac_procvar in aktmodeswitches)) and
                        (left.nodetype=calln) and
                        (is_void(left.resulttype.def)) then
                       load_procvar_from_calln(left);

                     case parasym.varspez of
                       vs_var,
                       vs_out :
                         begin
                           if not valid_for_formal_var(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                         end;
                       vs_const :
                         begin
                           if not valid_for_formal_const(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                         end;
                     end;
                   end
                 else
                   begin
                     { check if the argument is allowed }
                     if (parasym.varspez in [vs_out,vs_var]) then
                       valid_for_var(left,true);
                   end;

                 if parasym.varspez in [vs_var,vs_out] then
                   set_unique(left);

                 { When the address needs to be pushed then the register is
                   not regable. Exception is when the location is also a var
                   parameter and we can pass the address transparently }
                 if (
                     not(
                         (vo_is_hidden_para in parasym.varoptions) and
                         (left.resulttype.def.deftype in [pointerdef,classrefdef])
                        ) and
                     paramanager.push_addr_param(parasym.varspez,parasym.vartype.def,
                         aktcallnode.procdefinition.proccalloption) and
                     not(
                         (left.nodetype=loadn) and
                         (tloadnode(left).is_addr_param_load)
                        )
                    ) then
                   make_not_regable(left);

                 if do_count then
                  begin
                    case parasym.varspez of
                      vs_out :
                        set_varstate(left,vs_written,[]);
                      vs_var :
                        set_varstate(left,vs_readwritten,[vsf_must_be_valid,vsf_use_hints]);
                      else
                        set_varstate(left,vs_read,[vsf_must_be_valid]);
                    end;
                  end;
                 { must only be done after typeconv PM }
                 resulttype:=parasym.vartype;
               end;
            end;

         { process next node }
         if assigned(right) then
           tcallparanode(right).insert_typeconv(do_count);

         dec(parsing_para_level);
{$ifdef extdebug}
         if do_count then
           count_ref:=store_count_ref;
{$endif def extdebug}
      end;


    procedure tcallparanode.det_registers;
      begin
         if assigned(right) then
           begin
              tcallparanode(right).det_registers;

              registersint:=right.registersint;
              registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=right.registersmmx;
{$endif}
           end;

         firstpass(left);

         if left.registersint>registersint then
           registersint:=left.registersint;
         if left.registersfpu>registersfpu then
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         if left.registersmmx>registersmmx then
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
      end;


    procedure tcallparanode.firstcallparan;
      begin
        if not assigned(left.resulttype.def) then
          get_paratype;
        det_registers;
      end;


    function tcallparanode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (callparaflags = tcallparanode(p).callparaflags)
          ;
      end;


    procedure tcallparanode.printnodetree(var t:text);
      begin
        printnodelist(t);
      end;


{****************************************************************************
                                 TCALLNODE
 ****************************************************************************}

    constructor tcallnode.create(l:tnode;v : tprocsym;st : tsymtable; mp: tnode; callflags:tcallnodeflags);
      begin
         inherited create(calln,l,nil);
         symtableprocentry:=v;
         symtableproc:=st;
         callnodeflags:=callflags+[cnf_return_value_used];
         methodpointer:=mp;
         methodpointerinit:=nil;
         methodpointerdone:=nil;
         procdefinition:=nil;
         _funcretnode:=nil;
{$ifdef PASS2INLINE}
         inlinecode:=nil;
{$endif PASS2INLINE}
         paralength:=-1;
         varargsparas:=nil;
      end;


    constructor tcallnode.create_procvar(l,r:tnode);
      begin
         inherited create(calln,l,r);
         symtableprocentry:=nil;
         symtableproc:=nil;
         methodpointer:=nil;
         methodpointerinit:=nil;
         methodpointerdone:=nil;
         procdefinition:=nil;
         callnodeflags:=[cnf_return_value_used];
         _funcretnode:=nil;
{$ifdef PASS2INLINE}
         inlinecode:=nil;
{$endif PASS2INLINE}
         paralength:=-1;
         varargsparas:=nil;
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
           Message1(cg_f_unknown_compiler,name);
         self.create(params,tprocsym(srsym),symowner,nil,[]);
       end;


    constructor tcallnode.createinternres(const name: string; params: tnode; const res: ttype);
      begin
        self.createintern(name,params);
        restype := res;
        include(callnodeflags,cnf_restypeset);
        { both the normal and specified resulttype either have to be returned via a }
        { parameter or not, but no mixing (JM)                                      }
        if paramanager.ret_in_param(restype.def,symtableprocentry.first_procdef.proccalloption) xor
           paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def,symtableprocentry.first_procdef.proccalloption) then
          internalerror(200108291);
      end;


    constructor tcallnode.createinternreturn(const name: string; params: tnode; returnnode : tnode);
      begin
        self.createintern(name,params);
        _funcretnode:=returnnode;
        if not paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def,symtableprocentry.first_procdef.proccalloption) then
          internalerror(200204247);
      end;


    procedure tcallnode.setfuncretnode(const returnnode: tnode);
      var
        para: tcallparanode;
      begin
        if assigned(_funcretnode) then
          _funcretnode.free;
        _funcretnode := returnnode;
        { if the resulttype pass hasn't occurred yet, that one will do }
        { everything                                                   }
        if assigned(resulttype.def) then
          begin
            { these are returned as values, but we can optimize their loading }
            { as well                                                         }
            if is_ansistring(resulttype.def) or
               is_widestring(resulttype.def) then
              exit;
            para := tcallparanode(left);
            while assigned(para) do
              begin
                if (vo_is_hidden_para in para.parasym.varoptions) and
                   (vo_is_funcret in tparavarsym(para.parasym).varoptions) then
                 begin
                   para.left.free;
                   para.left := _funcretnode.getcopy;
                   exit;
                 end;
                 para := tcallparanode(para.right);
              end;
            { no hidden resultpara found, error! }
            if not(po_inline in procdefinition.procoptions) then
              internalerror(200306087);
          end;
      end;


    destructor tcallnode.destroy;
      var
        i : longint;
      begin
         methodpointer.free;
         methodpointerinit.free;
         methodpointerdone.free;
         _funcretnode.free;
{$ifdef PASS2INLINE}
         inlinecode.free;
{$endif PASS2INLINE}
         if assigned(varargsparas) then
           begin
             for i:=0 to varargsparas.count-1 do
               tparavarsym(varargsparas[i]).free;
             varargsparas.free;
           end;
         inherited destroy;
      end;


    constructor tcallnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(symtableprocentryderef);
{$ifdef fpc}
{$warning FIXME: No withsymtable support}
{$endif}
        symtableproc:=nil;
        ppufile.getderef(procdefinitionderef);
        ppufile.getsmallset(callnodeflags);
        methodpointer:=ppuloadnode(ppufile);
        methodpointerinit:=tblocknode(ppuloadnode(ppufile));
        methodpointerdone:=tblocknode(ppuloadnode(ppufile));
        _funcretnode:=ppuloadnode(ppufile);
{$ifdef PASS2INLINE}
        inlinecode:=ppuloadnode(ppufile);
{$endif PASS2INLINE}
      end;


    procedure tcallnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(symtableprocentryderef);
        ppufile.putderef(procdefinitionderef);
        ppufile.putsmallset(callnodeflags);
        ppuwritenode(ppufile,methodpointer);
        ppuwritenode(ppufile,methodpointerinit);
        ppuwritenode(ppufile,methodpointerdone);
        ppuwritenode(ppufile,_funcretnode);
{$ifdef PASS2INLINE}
        ppuwritenode(ppufile,inlinecode);
{$endif PASS2INLINE}
      end;


    procedure tcallnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        symtableprocentryderef.build(symtableprocentry);
        procdefinitionderef.build(procdefinition);
        if assigned(methodpointer) then
          methodpointer.buildderefimpl;
        if assigned(methodpointerinit) then
          methodpointerinit.buildderefimpl;
        if assigned(methodpointerdone) then
          methodpointerdone.buildderefimpl;
        if assigned(_funcretnode) then
          _funcretnode.buildderefimpl;
{$ifdef PASS2INLINE}
        if assigned(inlinecode) then
          inlinecode.buildderefimpl;
{$endif PASS2INLINE}
      end;


    procedure tcallnode.derefimpl;
      var
        pt : tcallparanode;
        i  : integer;
      begin
        inherited derefimpl;
        symtableprocentry:=tprocsym(symtableprocentryderef.resolve);
        if assigned(symtableprocentry) then
          symtableproc:=symtableprocentry.owner;
        procdefinition:=tabstractprocdef(procdefinitionderef.resolve);
        if assigned(methodpointer) then
          methodpointer.derefimpl;
        if assigned(methodpointerinit) then
          methodpointerinit.derefimpl;
        if assigned(methodpointerdone) then
          methodpointerdone.derefimpl;
        if assigned(_funcretnode) then
          _funcretnode.derefimpl;
{$ifdef PASS2INLINE}
        if assigned(inlinecode) then
          inlinecode.derefimpl;
{$endif PASS2INLINE}
        { Connect parasyms }
        pt:=tcallparanode(left);
        while assigned(pt) and
              (cpf_varargs_para in pt.callparaflags) do
          pt:=tcallparanode(pt.right);
        for i:=procdefinition.paras.count-1 downto 0 do
          begin
            if not assigned(pt) then
              internalerror(200311077);
            pt.parasym:=tparavarsym(procdefinition.paras[i]);
            pt:=tcallparanode(pt.right);
          end;
        if assigned(pt) then
          internalerror(200311078);
      end;


    function tcallnode.getcopy : tnode;
      var
        n : tcallnode;
        i : integer;
        hp,hpn : tparavarsym;
        oldleft : tnode;
      begin
        { Need to use a hack here to prevent the parameters from being copied.
          The parameters must be copied between methodpointerinit/methodpointerdone because
          the can reference methodpointer }
        oldleft:=left;
        left:=nil;
        n:=tcallnode(inherited getcopy);
        left:=oldleft;
        n.symtableprocentry:=symtableprocentry;
        n.symtableproc:=symtableproc;
        n.procdefinition:=procdefinition;
        n.restype := restype;
        n.callnodeflags := callnodeflags;
        if assigned(methodpointerinit) then
         n.methodpointerinit:=tblocknode(methodpointerinit.getcopy)
        else
         n.methodpointerinit:=nil;
        { methodpointerinit is copied, now references to the temp will also be copied
          correctly. We can now copy the parameters and methodpointer }
        if assigned(left) then
         n.left:=left.getcopy
        else
         n.left:=nil;
        if assigned(methodpointer) then
         n.methodpointer:=methodpointer.getcopy
        else
         n.methodpointer:=nil;
        if assigned(methodpointerdone) then
         n.methodpointerdone:=tblocknode(methodpointerdone.getcopy)
        else
         n.methodpointerdone:=nil;
        if assigned(_funcretnode) then
         n._funcretnode:=_funcretnode.getcopy
        else
         n._funcretnode:=nil;
{$ifdef PASS2INLINE}
        if assigned(inlinecode) then
         n.inlinecode:=inlinecode.getcopy
        else
         n.inlinecode:=nil;
{$endif PASS2INLINE}
        if assigned(varargsparas) then
         begin
           n.varargsparas:=tvarargsparalist.create;
           for i:=0 to varargsparas.count-1 do
             begin
               hp:=tparavarsym(varargsparas[i]);
               hpn:=tparavarsym.create(hp.realname,hp.paranr,hp.varspez,hp.vartype,[]);
               n.varargsparas.add(hpn);
             end;
         end
        else
         n.varargsparas:=nil;
        result:=n;
      end;


    procedure tcallnode.insertintolist(l : tnodelist);

      begin
      end;


    procedure tcallnode.convert_carg_array_of_const;
      var
        hp : tarrayconstructornode;
        oldleft : tcallparanode;
      begin
        oldleft:=tcallparanode(left);
        if oldleft.left.nodetype<>arrayconstructorn then
          begin
            CGMessage1(type_e_wrong_type_in_array_constructor,oldleft.left.resulttype.def.typename);
            exit;
          end;
        include(callnodeflags,cnf_uses_varargs);
        { Get arrayconstructor node and insert typeconvs }
        hp:=tarrayconstructornode(oldleft.left);
        hp.insert_typeconvs;
        { Add c args parameters }
        { It could be an empty set }
        if assigned(hp) and
           assigned(hp.left) then
          begin
            while assigned(hp) do
              begin
                left:=ccallparanode.create(hp.left,left);
                { set callparanode resulttype and flags }
                left.resulttype:=hp.left.resulttype;
                include(tcallparanode(left).callparaflags,cpf_varargs_para);
                hp.left:=nil;
                hp:=tarrayconstructornode(hp.right);
              end;
          end;
        { Remove value of old array of const parameter, but keep it
          in the list because it is required for bind_parasym.
          Generate a nothign to keep callparanoed.left valid }
        oldleft.left.free;
        oldleft.left:=cnothingnode.create;
      end;


    procedure tcallnode.verifyabstract(p : tnamedindexitem;arg:pointer);

      var
         hp : tprocdef;
          j: integer;
      begin
         if (tsym(p).typ=procsym) then
           begin
              for j:=1 to tprocsym(p).procdef_count do
               begin
                  { index starts at 1 }
                  hp:=tprocsym(p).procdef[j];
                  { If this is an abstract method insert into the list }
                  if (po_abstractmethod in hp.procoptions) then
                     AbstractMethodsList.Insert(hp.procsym.realname)
                  else
                    { If this symbol is a virtual (includes override) method,
                      then remove it from the list }
                    if po_virtualmethod in hp.procoptions then
                      AbstractMethodsList.Remove(hp.procsym.realname);
               end;
           end;
      end;


    procedure tcallnode.verifyabstractcalls;
      var
        objectdf : tobjectdef;
        parents : tlinkedlist;
        objectinfo : tobjectinfoitem;
        stritem : tstringlistitem;
      begin
        objectdf := nil;
        { verify if trying to create an instance of a class which contains
          non-implemented abstract methods }

        { first verify this class type, no class than exit  }
        { also, this checking can only be done if the constructor is directly
          called, indirect constructor calls cannot be checked.
        }
        if assigned(methodpointer) then
          begin
            if (methodpointer.resulttype.def.deftype = objectdef) then
              objectdf:=tobjectdef(methodpointer.resulttype.def)
            else
              if (methodpointer.resulttype.def.deftype = classrefdef) and
                 (tclassrefdef(methodpointer.resulttype.def).pointertype.def.deftype = objectdef) and
                 (methodpointer.nodetype in [typen,loadvmtaddrn]) then
                objectdf:=tobjectdef(tclassrefdef(methodpointer.resulttype.def).pointertype.def);
          end;
        if not assigned(objectdf) then
          exit;

        parents := tlinkedlist.create;
        AbstractMethodsList := tstringlist.create;

        { insert all parents in this class : the first item in the
          list will be the base parent of the class .
        }
        while assigned(objectdf) do
          begin
            objectinfo:=tobjectinfoitem.create(objectdf);
            parents.insert(objectinfo);
            objectdf := objectdf.childof;
        end;
        { now all parents are in the correct order
          insert all abstract methods in the list, and remove
          those which are overriden by parent classes.
        }
        objectinfo:=tobjectinfoitem(parents.first);
        while assigned(objectinfo) do
          begin
             objectdf := objectinfo.objinfo;
             if assigned(objectdf.symtable) then
               objectdf.symtable.foreach(@verifyabstract,nil);
             objectinfo:=tobjectinfoitem(objectinfo.next);
          end;
        if assigned(parents) then
          parents.free;
        { Finally give out a warning for each abstract method still in the list }
        stritem := tstringlistitem(AbstractMethodsList.first);
        if assigned(stritem) then
          Message1(type_w_instance_with_abstract,objectdf.objrealname^);
        while assigned(stritem) do
         begin
           if assigned(stritem.fpstr) then
             Message1(sym_h_param_list,stritem.str);
           stritem := tstringlistitem(stritem.next);
         end;
        if assigned(AbstractMethodsList) then
          AbstractMethodsList.Free;
      end;


    function tcallnode.gen_self_tree_methodpointer:tnode;
      var
        hsym : tfieldvarsym;
      begin
        { find self field in methodpointer record }
        hsym:=tfieldvarsym(trecorddef(methodpointertype.def).symtable.search('self'));
        if not assigned(hsym) then
          internalerror(200305251);
        { Load tmehodpointer(right).self }
        result:=csubscriptnode.create(
                     hsym,
                     ctypeconvnode.create_internal(right.getcopy,methodpointertype));
      end;


    function tcallnode.gen_self_tree:tnode;
      var
        selftree : tnode;
      begin
        selftree:=nil;

        { inherited }
        if (cnf_inherited in callnodeflags) then
          selftree:=load_self_node
        else
          { constructors }
          if (procdefinition.proctypeoption=potype_constructor) then
            begin
              { push 0 as self when allocation is needed }
              if (methodpointer.resulttype.def.deftype=classrefdef) or
                 (cnf_new_call in callnodeflags) then
                selftree:=cpointerconstnode.create(0,voidpointertype)
              else
                begin
                  if methodpointer.nodetype=typen then
                    selftree:=load_self_node
                  else
                    selftree:=methodpointer.getcopy;
                end;
            end
        else
          { Calling a static/class method }
          if (po_classmethod in procdefinition.procoptions) or
             (po_staticmethod in procdefinition.procoptions) then
            begin
              if (procdefinition.deftype<>procdef) then
                internalerror(200305062);
              if (oo_has_vmt in tprocdef(procdefinition)._class.objectoptions) then
                begin
                  { we only need the vmt, loading self is not required and there is no
                    need to check for typen, because that will always get the
                    loadvmtaddrnode added }
                  selftree:=methodpointer.getcopy;
                  if methodpointer.resulttype.def.deftype<>classrefdef then
                    selftree:=cloadvmtaddrnode.create(selftree);
                end
              else
                selftree:=cpointerconstnode.create(0,voidpointertype);
            end
        else
          begin
            if methodpointer.nodetype=typen then
              selftree:=load_self_node
            else
              selftree:=methodpointer.getcopy;
          end;
        result:=selftree;
      end;


    function tcallnode.gen_vmt_tree:tnode;
      var
        vmttree : tnode;
      begin
        vmttree:=nil;
        if not(procdefinition.proctypeoption in [potype_constructor,potype_destructor]) then
          internalerror(200305051);

        { Handle classes and legacy objects separate to make it
          more maintainable }
        if (methodpointer.resulttype.def.deftype=classrefdef) then
          begin
            if not is_class(tclassrefdef(methodpointer.resulttype.def).pointertype.def) then
              internalerror(200501041);

            { constructor call via classreference => allocate memory }
            if (procdefinition.proctypeoption=potype_constructor) then
              begin
                vmttree:=methodpointer.getcopy;
                { Only a typenode can be passed when it is called with <class of xx>.create }
                if vmttree.nodetype=typen then
                  vmttree:=cloadvmtaddrnode.create(vmttree);
              end
            else
              begin
                { Call afterconstruction }
                vmttree:=cpointerconstnode.create(1,voidpointertype);
              end;
          end
        else
        { Class style objects }
         if is_class(methodpointer.resulttype.def) then
          begin
            { inherited call, no create/destroy }
            if (cnf_inherited in callnodeflags) then
              vmttree:=cpointerconstnode.create(0,voidpointertype)
            else
              { do not create/destroy when called from member function
                without specifying self explicit }
              if (cnf_member_call in callnodeflags) then
                begin
                  { destructor: don't release instance, vmt=0
                    constructor:
                      if called from a constructor in the same class then
                        don't call afterconstruction, vmt=0
                      else
                        call afterconstrution, vmt=1 }
                  if (procdefinition.proctypeoption=potype_destructor) then
                    vmttree:=cpointerconstnode.create(0,voidpointertype)
                  else
                    begin
                      if (current_procinfo.procdef.proctypeoption=potype_constructor) and
                         (procdefinition.proctypeoption=potype_constructor) then
                        vmttree:=cpointerconstnode.create(0,voidpointertype)
                      else
                        vmttree:=cpointerconstnode.create(1,voidpointertype);
                    end;
                end
            else
            { normal call to method like cl1.proc }
              begin
                { destructor: release instance, vmt=1
                  constructor:
                    if called from a constructor in the same class using self.create then
                      don't call afterconstruction, vmt=0
                    else
                      call afterconstrution, vmt=1 }
                if (procdefinition.proctypeoption=potype_destructor) then
                  vmttree:=cpointerconstnode.create(1,voidpointertype)
                else
                  begin
                    if (current_procinfo.procdef.proctypeoption=potype_constructor) and
                       (procdefinition.proctypeoption=potype_constructor) and
                       (nf_is_self in methodpointer.flags) then
                      vmttree:=cpointerconstnode.create(0,voidpointertype)
                    else
                      vmttree:=cpointerconstnode.create(1,voidpointertype);
                  end;
              end;
          end
        else
        { Old style object }
          begin
            { constructor with extended syntax called from new }
            if (cnf_new_call in callnodeflags) then
                vmttree:=cloadvmtaddrnode.create(ctypenode.create(methodpointer.resulttype))
            else
              { destructor with extended syntax called from dispose }
              if (cnf_dispose_call in callnodeflags) then
                vmttree:=cloadvmtaddrnode.create(methodpointer.getcopy)
            else
              { inherited call, no create/destroy }
              if (cnf_inherited in callnodeflags) then
                vmttree:=cpointerconstnode.create(0,voidpointertype)
            else
              { do not create/destroy when called from member function
                without specifying self explicit }
              if (cnf_member_call in callnodeflags) then
                begin
                  { destructor: don't release instance, vmt=0
                    constructor: don't initialize instance, vmt=0 }
                  vmttree:=cpointerconstnode.create(0,voidpointertype)
                end
            else
            { normal object call like obj.proc }
             begin
               { destructor: direct call, no dispose, vmt=0
                 constructor: initialize object, load vmt }
               if (procdefinition.proctypeoption=potype_constructor) then
                 begin
                   { old styled inherited call? }
                   if (methodpointer.nodetype=typen) then
                     vmttree:=cpointerconstnode.create(0,voidpointertype)
                   else
                     vmttree:=cloadvmtaddrnode.create(ctypenode.create(methodpointer.resulttype))
                 end
               else
                 vmttree:=cpointerconstnode.create(0,voidpointertype);
             end;
          end;
        result:=vmttree;
      end;


    procedure tcallnode.bind_parasym;
      var
        i        : integer;
        pt       : tcallparanode;
        oldppt   : ^tcallparanode;
        varargspara,
        currpara : tparavarsym;
        used_by_callnode : boolean;
        hiddentree : tnode;
        newstatement : tstatementnode;
        temp         : ttempcreatenode;
      begin
        pt:=tcallparanode(left);
        oldppt:=@left;

        { flag all callparanodes that belong to the varargs }
        i:=paralength;
        while (i>procdefinition.maxparacount) do
          begin
            include(pt.callparaflags,cpf_varargs_para);
            oldppt:=@pt.right;
            pt:=tcallparanode(pt.right);
            dec(i);
          end;

        { skip varargs that are inserted by array of const }
        while assigned(pt) and
              (cpf_varargs_para in pt.callparaflags) do
          pt:=tcallparanode(pt.right);

        { process normal parameters and insert hidden parameters }
        for i:=procdefinition.paras.count-1 downto 0 do
         begin
           currpara:=tparavarsym(procdefinition.paras[i]);
           if vo_is_hidden_para in currpara.varoptions then
            begin
              { generate hidden tree }
              used_by_callnode:=false;
              hiddentree:=nil;
              if (vo_is_funcret in currpara.varoptions) then
               begin
                 { Generate funcretnode if not specified }
                 if assigned(funcretnode) then
                  begin
                    hiddentree:=funcretnode.getcopy;
                  end
                 else
                  begin
                    hiddentree:=internalstatements(newstatement);
                    { need to use resulttype instead of procdefinition.rettype,
                      because they can be different }
                    temp:=ctempcreatenode.create(resulttype,resulttype.def.size,tt_persistent,false);
                    addstatement(newstatement,temp);
                    addstatement(newstatement,ctempdeletenode.create_normal_temp(temp));
                    addstatement(newstatement,ctemprefnode.create(temp));
                  end;
               end
              else
               if vo_is_high_para in currpara.varoptions then
                begin
                  if not assigned(pt) or
                     (i=0) then
                    internalerror(200304082);
                  { we need the information of the previous parameter }
                  hiddentree:=gen_high_tree(pt.left,tparavarsym(procdefinition.paras[i-1]).vartype.def);
                end
              else
               if vo_is_self in currpara.varoptions then
                 begin
                   if assigned(right) then
                     hiddentree:=gen_self_tree_methodpointer
                   else
                     hiddentree:=gen_self_tree;
                 end
              else
               if vo_is_vmt in currpara.varoptions then
                 begin
                   hiddentree:=gen_vmt_tree;
                 end
{$ifdef powerpc}
              else
               if vo_is_syscall_lib in currpara.varoptions then
                 begin
                   { lib parameter has no special type but proccalloptions must be a syscall }
                   hiddentree:=cloadnode.create(tprocdef(procdefinition).libsym,tprocdef(procdefinition).libsym.owner);
                 end
{$endif powerpc}
              else
               if vo_is_parentfp in currpara.varoptions then
                 begin
                   if not(assigned(procdefinition.owner.defowner)) then
                     internalerror(200309287);
                   hiddentree:=cloadparentfpnode.create(tprocdef(procdefinition.owner.defowner));
                 end;
              { add the hidden parameter }
              if not assigned(hiddentree) then
                internalerror(200304073);
              { Already insert para and let the previous node point to
                this new node }
              pt:=ccallparanode.create(hiddentree,oldppt^);
              pt.used_by_callnode:=used_by_callnode;
              oldppt^:=pt;
            end;
           if not assigned(pt) then
             internalerror(200310052);
           pt.parasym:=currpara;
           oldppt:=@pt.right;
           pt:=tcallparanode(pt.right);
         end;


        { Create parasyms for varargs, first count the number of varargs paras,
          then insert the parameters with numbering in reverse order. The SortParas
          will set the correct order at the end}
        pt:=tcallparanode(left);
        i:=0;
        while assigned(pt) do
          begin
            if cpf_varargs_para in pt.callparaflags then
              inc(i);
            pt:=tcallparanode(pt.right);
          end;
        if (i>0) then
          begin
            varargsparas:=tvarargsparalist.create;
            pt:=tcallparanode(left);
            while assigned(pt) do
              begin
                if cpf_varargs_para in pt.callparaflags then
                  begin
                    varargspara:=tparavarsym.create('va'+tostr(i),i,vs_value,pt.resulttype,[]);
                    dec(i);
                    { varargspara is left-right, use insert
                      instead of concat }
                    varargsparas.add(varargspara);
                    pt.parasym:=varargspara;
                  end;
                pt:=tcallparanode(pt.right);
              end;
            varargsparas.sortparas;
          end;
      end;


    function tcallnode.det_resulttype:tnode;
      var
        candidates : tcallcandidates;
        oldcallnode : tcallnode;
        hpt : tnode;
        pt : tcallparanode;
        lastpara : longint;
        paraidx,
        cand_cnt : integer;
        i : longint;
        is_const : boolean;
      label
        errorexit;
      begin
         result:=nil;
         candidates:=nil;

         oldcallnode:=aktcallnode;
         aktcallnode:=self;

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

         if assigned(methodpointer) then
           begin
             resulttypepass(methodpointer);
             maybe_load_para_in_temp(methodpointer);
           end;

         { procedure variable ? }
         if assigned(right) then
           begin
              set_varstate(right,vs_read,[vsf_must_be_valid]);
              resulttypepass(right);
              if codegenerror then
               exit;

              procdefinition:=tabstractprocdef(right.resulttype.def);

              { Compare parameters from right to left }
              paraidx:=procdefinition.Paras.count-1;
              { Skip default parameters }
              if not(po_varargs in procdefinition.procoptions) then
                begin
                  { ignore hidden parameters }
                  while (paraidx>=0) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
                    dec(paraidx);
                  for i:=1 to procdefinition.maxparacount-paralength do
                    begin
                      if paraidx<0 then
                        internalerror(200402261);
                      if not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym) then
                        begin
                          CGMessage(parser_e_wrong_parameter_size);
                          goto errorexit;
                        end;
                      dec(paraidx);
                    end;
                end;
              while (paraidx>=0) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
                dec(paraidx);
              pt:=tcallparanode(left);
              lastpara:=paralength;
              while (paraidx>=0) and assigned(pt) do
                begin
                  { only goto next para if we're out of the varargs }
                  if not(po_varargs in procdefinition.procoptions) or
                     (lastpara<=procdefinition.maxparacount) then
                   begin
                     repeat
                       dec(paraidx);
                     until (paraidx<0) or not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions);
                   end;
                  pt:=tcallparanode(pt.right);
                  dec(lastpara);
                end;
              if assigned(pt) or
                 ((paraidx>=0) and
                  not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym)) then
                begin
                   if assigned(pt) then
                     aktfilepos:=pt.fileinfo;
                   CGMessage(parser_e_wrong_parameter_size);
                   goto errorexit;
                end;
           end
         else
         { not a procedure variable }
           begin
              { do we know the procedure to call ? }
              if not(assigned(procdefinition)) then
                begin
                   candidates:=tcallcandidates.create(symtableprocentry,symtableproc,left,(nf_isproperty in flags),
                     { ignore possible private in delphi mode for anon. inherited (FK) }
                     (m_delphi in aktmodeswitches) and (cnf_anon_inherited in callnodeflags));

                   { no procedures found? then there is something wrong
                     with the parameter size or the procedures are
                     not accessible }
                   if candidates.count=0 then
                    begin
                      { when it's an auto inherited call and there
                        is no procedure found, but the procedures
                        were defined with overload directive and at
                        least two procedures are defined then we ignore
                        this inherited by inserting a nothingn. Only
                        do this ugly hack in Delphi mode as it looks more
                        like a bug. It's also not documented }
                      if (m_delphi in aktmodeswitches) and
                         (cnf_anon_inherited in callnodeflags) and
                         (symtableprocentry.owner.symtabletype=objectsymtable) and
                         (po_overload in symtableprocentry.first_procdef.procoptions) and
                         (symtableprocentry.procdef_count>=2) then
                        result:=cnothingnode.create
                      else
                        begin
                          { in tp mode we can try to convert to procvar if
                            there are no parameters specified. Only try it
                            when there is only one proc definition, else the
                            loadnode will give a strange error }
                          if not(assigned(left)) and
                             not(cnf_inherited in callnodeflags) and
                             ((m_tp_procvar in aktmodeswitches) or
                              (m_mac_procvar in aktmodeswitches)) and
                             (symtableprocentry.procdef_count=1) and
                             (not assigned(methodpointer) or
                              (methodpointer.nodetype <> typen)) then
                            begin
                              hpt:=cloadnode.create(tprocsym(symtableprocentry),symtableproc);
                              if assigned(methodpointer) then
                                tloadnode(hpt).set_mp(get_load_methodpointer);
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
                        end;
                      goto errorexit;
                    end;

                   { Retrieve information about the candidates }
                   candidates.get_information;
{$ifdef EXTDEBUG}
                   { Display info when multiple candidates are found }
                   if candidates.count>1 then
                     candidates.dump_info(V_Debug);
{$endif EXTDEBUG}

                   { Choose the best candidate and count the number of
                     candidates left }
                   cand_cnt:=candidates.choose_best(procdefinition);

                   { All parameters are checked, check if there are any
                     procedures left }
                   if cand_cnt>0 then
                    begin
                      { Multiple candidates left? }
                      if cand_cnt>1 then
                       begin
                         CGMessage(type_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
                         candidates.dump_info(V_Hint);
{$else EXTDEBUG}
                         candidates.list(false);
{$endif EXTDEBUG}
                         { we'll just use the first candidate to make the
                           call }
                       end;

                      { assign procdefinition }
                      if symtableproc=nil then
                        symtableproc:=procdefinition.owner;

                      { update browser information }
                      if make_ref then
                        begin
                           tprocdef(procdefinition).lastref:=tref.create(tprocdef(procdefinition).lastref,@fileinfo);
                           inc(tprocdef(procdefinition).refcount);
                           if tprocdef(procdefinition).defref=nil then
                             tprocdef(procdefinition).defref:=tprocdef(procdefinition).lastref;
                        end;
                    end
                   else
                    begin
                      { No candidates left, this must be a type error,
                        because wrong size is already checked. procdefinition
                        is filled with the first (random) definition that is
                        found. We use this definition to display a nice error
                        message that the wrong type is passed }
                      candidates.find_wrong_para;
                      candidates.list(true);
{$ifdef EXTDEBUG}
                      candidates.dump_info(V_Hint);
{$endif EXTDEBUG}

                      { We can not proceed, release all procs and exit }
                      candidates.free;
                      goto errorexit;
                    end;

                   candidates.free;
               end; { end of procedure to call determination }
           end;

          { check for hints (deprecated etc) }
          if (procdefinition.deftype = procdef) then
            check_hints(tprocdef(procdefinition).procsym,tprocdef(procdefinition).symoptions);

          { add needed default parameters }
          if assigned(procdefinition) and
             (paralength<procdefinition.maxparacount) then
           begin
             paraidx:=0;
             i:=0;
             while (i<paralength) do
              begin
                if paraidx>=procdefinition.Paras.count then
                  internalerror(200306181);
                if not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) then
                  inc(i);
                inc(paraidx);
              end;
             while (paraidx<procdefinition.paras.count) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
               inc(paraidx);
             while (paraidx<procdefinition.paras.count) do
              begin
                if not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym) then
                 internalerror(200212142);
                left:=ccallparanode.create(genconstsymtree(
                    tconstsym(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym)),left);
                { Ignore vs_hidden parameters }
                repeat
                  inc(paraidx);
                until (paraidx>=procdefinition.paras.count) or not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions);
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

         { ensure that the result type is set }
         if not(cnf_restypeset in callnodeflags) then
          begin
            { constructors return their current class type, not the type where the
              constructor is declared, this can be different because of inheritance }
            if (procdefinition.proctypeoption=potype_constructor) and
               assigned(methodpointer) and
               assigned(methodpointer.resulttype.def) and
               (methodpointer.resulttype.def.deftype=classrefdef) then
              resulttype:=tclassrefdef(methodpointer.resulttype.def).pointertype
            else
            { Member call to a (inherited) constructor from the class, the return
              value is always self, so we change it to voidtype to generate an
              error and to prevent users from generating non-working code
              when they expect to clone the current instance, see bug 3662 (PFV) }
              if (procdefinition.proctypeoption=potype_constructor) and
                 is_class(tprocdef(procdefinition)._class) and
                 assigned(methodpointer) and
                 (nf_is_self in methodpointer.flags) then
                resulttype:=voidtype
            else
              resulttype:=procdefinition.rettype;
           end
         else
           resulttype:=restype;

         {if resulttype.def.needs_inittable then
           include(current_procinfo.flags,pi_needs_implicit_finally);}

         if assigned(methodpointer) then
          begin
            { when methodpointer is a callnode we must load it first into a
              temp to prevent the processing callnode twice }
            if (methodpointer.nodetype=calln) then
              internalerror(200405121);

            { direct call to inherited abstract method, then we
              can already give a error in the compiler instead
              of a runtime error }
            if (cnf_inherited in callnodeflags) and
               (po_abstractmethod in procdefinition.procoptions) then
              CGMessage(cg_e_cant_call_abstract_method);

            { if an inherited con- or destructor should be  }
            { called in a con- or destructor then a warning }
            { will be made                                  }
            { con- and destructors need a pointer to the vmt }
            if (cnf_inherited in callnodeflags) and
               (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) and
               is_object(methodpointer.resulttype.def) and
               not(current_procinfo.procdef.proctypeoption in [potype_constructor,potype_destructor]) then
             CGMessage(cg_w_member_cd_call_from_method);

            if methodpointer.nodetype<>typen then
             begin
                { Remove all postfix operators }
                hpt:=methodpointer;
                while assigned(hpt) and (hpt.nodetype in [subscriptn,vecn]) do
                  hpt:=tunarynode(hpt).left;

               if (procdefinition.proctypeoption=potype_constructor) and
                  assigned(symtableproc) and
                  (symtableproc.symtabletype=withsymtable) and
                  (tnode(twithsymtable(symtableproc).withrefnode).nodetype=temprefn) then
                 CGmessage(cg_e_cannot_call_cons_dest_inside_with);

               { R.Init then R will be initialized by the constructor,
                 Also allow it for simple loads }
               if (procdefinition.proctypeoption=potype_constructor) or
                  ((hpt.nodetype=loadn) and
                   (
                    (methodpointer.resulttype.def.deftype=classrefdef) or
                    (
                     (methodpointer.resulttype.def.deftype=objectdef) and
                     not(oo_has_virtual in tobjectdef(methodpointer.resulttype.def).objectoptions)
                    )
                   )
                  ) then
                 set_varstate(methodpointer,vs_read,[])
               else
                 set_varstate(methodpointer,vs_read,[vsf_must_be_valid]);

               { The object is already used if it is called once }
               if (hpt.nodetype=loadn) and
                  (tloadnode(hpt).symtableentry.typ in [localvarsym,paravarsym,globalvarsym]) then
                 set_varstate(hpt,vs_read,[]);
//                 tabstractvarsym(tloadnode(hpt).symtableentry).varstate:=vs_readwritten;
             end;

            { if we are calling the constructor check for abstract
              methods. Ignore inherited and member calls, because the
              class is then already created }
            if (procdefinition.proctypeoption=potype_constructor) and
               not(cnf_inherited in callnodeflags) and
               not(cnf_member_call in callnodeflags) then
              verifyabstractcalls;
          end
         else
          begin
            { When this is method the methodpointer must be available }
            if (right=nil) and
               (procdefinition.owner.symtabletype=objectsymtable) then
              internalerror(200305061);
          end;

         { Set flag that the procedure uses varargs, also if they are not passed it is still
           needed for x86_64 to pass the number of SSE registers used }
         if po_varargs in procdefinition.procoptions then
           include(callnodeflags,cnf_uses_varargs);

         { Change loading of array of const to varargs }
         if assigned(left) and
            is_array_of_const(tparavarsym(procdefinition.paras[procdefinition.paras.count-1]).vartype.def) and
            (procdefinition.proccalloption in [pocall_cppdecl,pocall_cdecl]) then
           convert_carg_array_of_const;

         { bind parasyms to the callparanodes and insert hidden parameters }
         bind_parasym;

         { insert type conversions for parameters }
         if assigned(left) then
           tcallparanode(left).insert_typeconv(true);

      errorexit:
         aktcallnode:=oldcallnode;
      end;


    procedure tcallnode.order_parameters;
      var
        hp,hpcurr,hpnext,hpfirst,hpprev : tcallparanode;
        currloc : tcgloc;
      begin
        hpfirst:=nil;
        hpcurr:=tcallparanode(left);
        while assigned(hpcurr) do
          begin
            { pull out }
            hpnext:=tcallparanode(hpcurr.right);
            { pull in at the correct place.
              Used order:
                1. LOC_REFERENCE with smallest offset (x86 only)
                2. LOC_REFERENCE with most registers
                3. LOC_REGISTER with most registers
              For the moment we only look at the first parameter field. Combining it
              with multiple parameter fields will make things a lot complexer (PFV) }
            if not assigned(hpcurr.parasym.paraloc[callerside].location) then
              internalerror(200412152);
            currloc:=hpcurr.parasym.paraloc[callerside].location^.loc;
            hpprev:=nil;
            hp:=hpfirst;
            while assigned(hp) do
              begin
                case currloc of
                  LOC_REFERENCE :
                    begin
                      case hp.parasym.paraloc[callerside].location^.loc of
                        LOC_REFERENCE :
                          begin
                            { Offset is calculated like:
                               sub esp,12
                               mov [esp+8],para3
                               mov [esp+4],para2
                               mov [esp],para1
                               call function
                              That means the for pushes the para with the
                              highest offset (see para3) needs to be pushed first
                            }
                            if (hpcurr.registersint>hp.registersint)
{$ifdef x86}
                               or (hpcurr.parasym.paraloc[callerside].location^.reference.offset>hp.parasym.paraloc[callerside].location^.reference.offset)
{$endif x86}
                               then
                              break;
                          end;
                        LOC_REGISTER,
                        LOC_FPUREGISTER :
                          break;
                      end;
                    end;
                  LOC_FPUREGISTER,
                  LOC_REGISTER :
                    begin
                      if (hp.parasym.paraloc[callerside].location^.loc=currloc) and
                         (hpcurr.registersint>hp.registersint) then
                        break;
                    end;
                end;
                hpprev:=hp;
                hp:=tcallparanode(hp.right);
              end;
            hpcurr.right:=hp;
            if assigned(hpprev) then
              hpprev.right:=hpcurr
            else
              hpfirst:=hpcurr;
            { next }
            hpcurr:=hpnext;
          end;
        left:=hpfirst;
      end;


    function tcallnode.replaceparaload(var n: tnode; arg: pointer): foreachnoderesult;
      var
        paras: tcallparanode;
        temp: tnode;
      begin
        result := fen_false;
        n.fileinfo := pfileposinfo(arg)^;
        if (n.nodetype = loadn) then
          begin
            case tloadnode(n).symtableentry.typ of
              paravarsym :
                begin
                  paras := tcallparanode(left);
                  while assigned(paras) and
                        (paras.parasym <> tloadnode(n).symtableentry) do
                    paras := tcallparanode(paras.right);
                  if assigned(paras) then
                    begin
                      n.free;
                      n := paras.left.getcopy;
                      resulttypepass(n);
                      result := fen_true;
                    end;
                end;
              localvarsym :
                begin
                  { local? }
                  if (tloadnode(n).symtableentry.owner <> tprocdef(procdefinition).localst) then
                    exit;
                  if (tloadnode(n).symtableentry.indexnr >= inlinelocals.count) or
                     not assigned(inlinelocals[tloadnode(n).symtableentry.indexnr]) then
                    internalerror(20040720);
                  temp := tnode(inlinelocals[tloadnode(n).symtableentry.indexnr]).getcopy;
                  n.free;
                  n := temp;
                  resulttypepass(n);
                  result := fen_true;
                end;
            end;
          end;
      end;


      type
        ptempnodes = ^ttempnodes;
        ttempnodes = record
          createstatement, deletestatement: tstatementnode;
        end;

    procedure tcallnode.createlocaltemps(p:TNamedIndexItem;arg:pointer);
      var
        tempinfo: ptempnodes absolute arg;
        tempnode: ttempcreatenode;
      begin
        if (tsymentry(p).typ <> localvarsym) then
          exit;
        if (p.indexnr >= inlinelocals.count) then
          inlinelocals.count:=p.indexnr+10;
        if (vo_is_funcret in tabstractvarsym(p).varoptions) and
           assigned(funcretnode) then
          begin
            if node_complexity(funcretnode) > 1 then
              begin
                { can this happen? }
                { we may have to replace the funcretnode with the address of funcretnode }
                { loaded in a temp in this case, because the expression may e.g. contain }
                { a global variable that gets changed inside the function                }
                internalerror(2004072101);
              end;
            inlinelocals[tabstractvarsym(p).indexnr] := funcretnode.getcopy
          end
        else
          begin
            tempnode := ctempcreatenode.create(tabstractvarsym(p).vartype,tabstractvarsym(p).vartype.def.size,tt_persistent,tabstractvarsym(p).varregable<>vr_none);
            addstatement(tempinfo^.createstatement,tempnode);
            if assigned(tlocalvarsym(p).defaultconstsym) then
              begin
                { warning: duplicate from psub.pas:initializevars() -> must refactor }
                addstatement(tempinfo^.createstatement,cassignmentnode.create(
                                  ctemprefnode.create(tempnode),
                                  cloadnode.create(tlocalvarsym(p).defaultconstsym,tlocalvarsym(p).defaultconstsym.owner)));
              end;
            if (vo_is_funcret in tlocalvarsym(p).varoptions) then
              begin
                funcretnode := ctemprefnode.create(tempnode);
                addstatement(tempinfo^.deletestatement,ctempdeletenode.create_normal_temp(tempnode));
              end
            else
              addstatement(tempinfo^.deletestatement,ctempdeletenode.create(tempnode));
            inlinelocals[p.indexnr] := ctemprefnode.create(tempnode);
          end;
      end;


    procedure tcallnode.createinlineparas(var createstatement, deletestatement: tstatementnode);
      var
        para: tcallparanode;
        tempnode: ttempcreatenode;
        tempnodes: ttempnodes;
        n: tnode;
      begin
        { parameters }
        para := tcallparanode(left);
        while assigned(para) do
          begin
            if (para.parasym.typ = paravarsym) and
               { para.left will already be the same as funcretnode in the following case, so don't change }
               (not(vo_is_funcret in tparavarsym(para.parasym).varoptions) or
                (not assigned(funcretnode))) then
              begin
                { must take copy of para.left, because if it contains a       }
                { temprefn pointing to a copied temp (e.g. methodpointer),    }
                { then this parameter must be changed to point to the copy of }
                { that temp (JM)                                              }
                n := para.left.getcopy;
                para.left.free;
                para.left := n;

                { create temps for value parameters, function result and also for    }
                { const parameters which are passed by value instead of by reference }
                { we need to take care that we use the type of the defined parameter and not of the
                  passed parameter, because these can be different in case of a formaldef (PFV) }
                if
                  (
                   { the problem is that we can't take the address of a function result :( }
                   (vo_is_funcret in tparavarsym(para.parasym).varoptions) or
                   (para.parasym.varspez = vs_value) or
                   ((para.parasym.varspez = vs_const) and
                    (para.parasym.vartype.def.deftype<>formaldef) and
                   { the compiler expects that it can take the address of parameters passed by reference in
                     the case of const so we can't replace the node simply by a constant node
                     When playing with this code, ensure that
                     function f(const a,b  : longint) : longint;inline;
                       begin
                         result:=a*b;
                       end;

                     [...]
                     ...:=f(10,20));
                     [...]

                     is still folded. (FK)
                     }
                    (
                      { this must be a not ... of course }
                      not(paramanager.push_addr_param(vs_const,para.parasym.vartype.def,procdefinition.proccalloption)) or
                      (node_complexity(para.left) >= NODE_COMPLEXITY_INF)
                    ))
                   ) then
                  begin
                    { in theory, this is always regable, but ncgcall can't }
                    { handle it yet in all situations (JM)                 }
                    tempnode := ctempcreatenode.create(para.parasym.vartype,para.parasym.vartype.def.size,tt_persistent,tparavarsym(para.parasym).varregable <> vr_none);
                    addstatement(createstatement,tempnode);
                    { assign the value of the parameter to the temp, except in case of the function result }
                    { (in that case, para.left is a block containing the creation of a new temp, while we  }
                    {  only need a temprefnode, so delete the old stuff)                                   }
                    if not(vo_is_funcret in tparavarsym(para.parasym).varoptions) then
                      begin
                        addstatement(createstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                          para.left));
                        para.left := ctemprefnode.create(tempnode);
                        addstatement(deletestatement,ctempdeletenode.create(tempnode));
                      end
                    else
                      begin
                        if not(assigned(funcretnode)) then
                          funcretnode := ctemprefnode.create(tempnode);
                        para.left.free;
                        para.left := ctemprefnode.create(tempnode);
                        addstatement(deletestatement,ctempdeletenode.create_normal_temp(tempnode));
                      end
                  end
                else if (node_complexity(para.left) > 1) then
                  begin
                    tempnode := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,tt_persistent,tparavarsym(para.parasym).varregable<>vr_none);
                    addstatement(createstatement,tempnode);
                    addstatement(createstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                      caddrnode.create_internal(para.left)));
                    para.left := ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),para.left.resulttype);
                    addstatement(deletestatement,ctempdeletenode.create(tempnode));
                  end;
              end;
            para := tcallparanode(para.right);
          end;
        { local variables }
        if not assigned(tprocdef(procdefinition).localst) or
           (tprocdef(procdefinition).localst.symindex.count = 0) then
          exit;
        tempnodes.createstatement := createstatement;
        tempnodes.deletestatement := deletestatement;
        inlinelocals.count:=tprocdef(procdefinition).localst.symindex.count;
        tprocdef(procdefinition).localst.foreach(@createlocaltemps,@tempnodes);
        createstatement := tempnodes.createstatement;
        deletestatement := tempnodes.deletestatement;
      end;



    function tcallnode.pass1_inline:tnode;
      var
        createstatement,deletestatement: tstatementnode;
        createblock,deleteblock: tblocknode;
        body : tnode;
        i: longint;
      begin
        if not(assigned(tprocdef(procdefinition).inlininginfo) and
               assigned(tprocdef(procdefinition).inlininginfo^.code)) then
          internalerror(200412021);
        { inherit flags }
        current_procinfo.flags := current_procinfo.flags + ((procdefinition as tprocdef).inlininginfo^.flags*inherited_inlining_flags);
        { create blocks for loading/deleting of local data }
        createblock:=internalstatements(createstatement);
        deleteblock:=internalstatements(deletestatement);

        { add methodpointer init code to init statement                    }
        { (fini must be done later, as it will delete the hookoncopy info) }
        if assigned(methodpointerinit) then
          addstatement(createstatement,methodpointerinit.getcopy);

        inlinelocals:=tlist.create;
        { get copy of the procedure body }
        body:=tprocdef(procdefinition).inlininginfo^.code.getcopy;
        { replace complex parameters with temps }
        createinlineparas(createstatement,deletestatement);
        { replace the parameter loads with the parameter values }
        foreachnode(body,@replaceparaload,@fileinfo);

        { copy methodpointer fini code }
        if assigned(methodpointerdone) then
          addstatement(deletestatement,methodpointerdone.getcopy);

        { free the temps for the locals }
        for i := 0 to inlinelocals.count-1 do
          if assigned(inlinelocals[i]) then
            tnode(inlinelocals[i]).free;
        inlinelocals.free;
        inlinelocals:=nil;
        addstatement(createstatement,body);
        addstatement(createstatement,deleteblock);
        { set function result location if necessary }
        if assigned(funcretnode) and
           (cnf_return_value_used in callnodeflags) then
          addstatement(createstatement,funcretnode.getcopy);
        { consider it must not be inlined if called
          again inside the args or itself }
        exclude(procdefinition.procoptions,po_inline);
        firstpass(createblock);
        include(procdefinition.procoptions,po_inline);
        { return inlined block }
        result := createblock;

{$ifdef DEBUGINLINE}
        writeln;
        writeln('**************************',tprocdef(procdefinition).mangledname);
        printnode(output,result);
{$endif DEBUGINLINE}
      end;


    function tcallnode.pass_1 : tnode;
      var
        st : tsymtable;
      begin
         result:=nil;

         { Can we inline the procedure? }
         if ([po_inline,po_has_inlininginfo] <= procdefinition.procoptions) then
           begin
             { Check if we can inline the procedure when it references proc/var that
               are not in the globally available }
             st:=procdefinition.owner;
             if (st.symtabletype=objectsymtable) then
               st:=st.defowner.owner;
             if (pi_uses_static_symtable in tprocdef(procdefinition).inlininginfo^.flags) and
                (st.symtabletype=globalsymtable) and
                (not st.iscurrentunit) then
               begin
                 Comment(V_lineinfo+V_Debug,'Not inlining "'+tprocdef(procdefinition).procsym.realname+'", references static symtable');
               end
             else
               begin
                 result:=pass1_inline;
                 exit;
               end;
           end;

         { calculate the parameter info for the procdef }
         if not procdefinition.has_paraloc_info then
           begin
             procdefinition.requiredargarea:=paramanager.create_paraloc_info(procdefinition,callerside);
             procdefinition.has_paraloc_info:=true;
           end;

         { calculate the parameter size needed for this call include varargs if they are available }
         if assigned(varargsparas) then
           pushedparasize:=paramanager.create_varargs_paraloc_info(procdefinition,varargsparas)
         else
           pushedparasize:=procdefinition.requiredargarea;

         { record maximum parameter size used in this proc }
         current_procinfo.allocate_push_parasize(pushedparasize);

         { work trough all parameters to get the register requirements }
         if assigned(left) then
           tcallparanode(left).det_registers;

         { order parameters }
         order_parameters;

         if assigned(methodpointerinit) then
           firstpass(methodpointerinit);

         if assigned(methodpointerdone) then
           firstpass(methodpointerdone);

         { function result node }
         if assigned(_funcretnode) then
           firstpass(_funcretnode);

         { procedure variable ? }
         if assigned(right) then
           firstpass(right);

         if not (block_type in [bt_const,bt_type]) then
           include(current_procinfo.flags,pi_do_call);

         { implicit finally needed ? }
         if resulttype.def.needs_inittable and
            not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) and
            not assigned(funcretnode) then
           include(current_procinfo.flags,pi_needs_implicit_finally);

         { get a register for the return value }
         if (not is_void(resulttype.def)) then
           begin
              if paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
               begin
                 expectloc:=LOC_REFERENCE;
               end
             else
             { for win32 records returned in EDX:EAX, we
               move them to memory after ... }
             if (resulttype.def.deftype=recorddef) then
              begin
                expectloc:=LOC_REFERENCE;
              end
             else
             { ansi/widestrings must be registered, so we can dispose them }
              if is_ansistring(resulttype.def) or
                 is_widestring(resulttype.def) then
               begin
                 expectloc:=LOC_REFERENCE;
                 registersint:=1;
               end
             else
             { we have only to handle the result if it is used }
              if (cnf_return_value_used in callnodeflags) then
               begin
                 case resulttype.def.deftype of
                   enumdef,
                   orddef :
                     begin
                       if (procdefinition.proctypeoption=potype_constructor) then
                        begin
                          expectloc:=LOC_REGISTER;
                          registersint:=1;
                        end
                       else
                        begin
                          expectloc:=LOC_REGISTER;
                          if is_64bit(resulttype.def) then
                            registersint:=2
                          else
                            registersint:=1;
                        end;
                     end;
                   floatdef :
                     begin
                       expectloc:=LOC_FPUREGISTER;
{$ifdef cpufpemu}
                       if (cs_fp_emulation in aktmoduleswitches) then
                         registersint:=1
                       else
{$endif cpufpemu}
{$ifdef m68k}
                        if (tfloatdef(resulttype.def).typ=s32real) then
                         registersint:=1
                       else
{$endif m68k}
                         registersfpu:=1;
                     end;
                   else
                     begin
                       expectloc:=LOC_REGISTER;
                       registersint:=1;
                     end;
                 end;
               end
             else
               expectloc:=LOC_VOID;
           end
         else
           expectloc:=LOC_VOID;

{$ifdef m68k}
         { we need one more address register for virtual calls on m68k }
         if (po_virtualmethod in procdefinition.procoptions) then
           inc(registersint);
{$endif m68k}
         { a fpu can be used in any procedure !! }
{$ifdef i386}
         registersfpu:=procdefinition.fpu_used;
{$endif i386}
         { if this is a call to a method calc the registers }
         if (methodpointer<>nil) then
           begin
              if methodpointer.nodetype<>typen then
               begin
                 firstpass(methodpointer);
                 registersfpu:=max(methodpointer.registersfpu,registersfpu);
                 registersint:=max(methodpointer.registersint,registersint);
{$ifdef SUPPORT_MMX }
                 registersmmx:=max(methodpointer.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
               end;
           end;

         { determine the registers of the procedure variable }
         { is this OK for inlined procs also ?? (PM)     }
         if assigned(right) then
           begin
              registersfpu:=max(right.registersfpu,registersfpu);
              registersint:=max(right.registersint,registersint);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(right.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
           end;
         { determine the registers of the procedure }
         if assigned(left) then
           begin
              registersfpu:=max(left.registersfpu,registersfpu);
              registersint:=max(left.registersint,registersint);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(left.registersmmx,registersmmx);
{$endif SUPPORT_MMX}
           end;
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


    function tcallnode.para_count:longint;
      var
        ppn : tcallparanode;
      begin
        result:=0;
        ppn:=tcallparanode(left);
        while assigned(ppn) do
          begin
            if not(assigned(ppn.parasym) and
                   (vo_is_hidden_para in ppn.parasym.varoptions)) then
              inc(result);
            ppn:=tcallparanode(ppn.right);
          end;
      end;


    function tcallnode.get_load_methodpointer:tnode;
      var
        newstatement : tstatementnode;
      begin
        if assigned(methodpointerinit) then
          begin
            result:=internalstatements(newstatement);
            addstatement(newstatement,methodpointerinit);
            addstatement(newstatement,methodpointer);
            addstatement(newstatement,methodpointerdone);
            methodpointerinit:=nil;
            methodpointer:=nil;
            methodpointerdone:=nil;
          end
        else
          begin
            result:=methodpointer;
            methodpointer:=nil;
          end;
      end;


    function tcallnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableprocentry = tcallnode(p).symtableprocentry) and
          (procdefinition = tcallnode(p).procdefinition) and
          (methodpointer.isequal(tcallnode(p).methodpointer)) and
          (((cnf_restypeset in callnodeflags) and (cnf_restypeset in tcallnode(p).callnodeflags) and
            (equal_defs(restype.def,tcallnode(p).restype.def))) or
           (not(cnf_restypeset in callnodeflags) and not(cnf_restypeset in tcallnode(p).callnodeflags)));
      end;


    procedure tcallnode.printnodedata(var t:text);
      begin
        if assigned(procdefinition) and
           (procdefinition.deftype=procdef) then
          writeln(t,printnodeindention,'proc = ',tprocdef(procdefinition).fullprocname(true))
        else
          begin
            if assigned(symtableprocentry) then
              writeln(t,printnodeindention,'proc = ',symtableprocentry.name)
            else
              writeln(t,printnodeindention,'proc = <nil>');
          end;
        printnode(t,methodpointer);
        printnode(t,right);
        printnode(t,left);
      end;


begin
   ccallnode:=tcallnode;
   ccallparanode:=tcallparanode;
end.
