{
    $Id$
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
       globtype,cpuinfo,
       node,
       {$ifdef state_tracking}
       nstate,
       {$endif state_tracking}
       symbase,symtype,symppu,symsym,symdef,symtable;

    type
       pcandidate = ^tcandidate;
       tcandidate = record
          next        : pcandidate;
          data        : tprocdef;
          wrongpara,
          firstpara   : tparaitem;
          exact_count,
          equal_count,
          cl1_count,
          cl2_count   : integer; { should be signed }
          ordinal_distance : bestreal;
          invalid : boolean;
          wrongparanr : byte;
       end;

       tcallnode = class(tbinarynode)
       private
          paralength : smallint;
          function  candidates_find:pcandidate;
          procedure candidates_free(procs:pcandidate);
          procedure candidates_list(procs:pcandidate;all:boolean);
          procedure candidates_get_information(procs:pcandidate);
          function  candidates_choose_best(procs:pcandidate;var bestpd:tprocdef):integer;
          procedure candidates_find_wrong_para(procs:pcandidate);
       public
          { the symbol containing the definition of the procedure }
          { to call                                               }
          symtableprocentry : tprocsym;
          { symtable where the entry was found, needed for with support }
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
          procedure set_procvar(procvar:tnode);
       private
          AbstractMethodsList : TStringList;
       end;
       tcallnodeclass = class of tcallnode;

       tcallparaflags = (
          { flags used by tcallparanode }
          cpf_exact_match_found,
          cpf_convlevel1found,
          cpf_convlevel2found,
          cpf_is_colon_para
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
          procedure secondcallparan(defcoll : TParaItem;
                push_from_left_to_right:boolean;calloption:tproccalloption;
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
      systems,
      verbose,globals,
      symconst,paramgr,defutil,defcmp,
      htypechk,pass_1,cpubase,
      nbas,ncnv,nld,ninl,nadd,ncon,
      rgobj,cgbase
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


    procedure search_class_overloads(aprocsym : tprocsym);
    { searches n in symtable of pd and all anchestors }
      var
        speedvalue : cardinal;
        srsym      : tprocsym;
        s          : string;
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


      function is_better_candidate(currpd,bestpd:pcandidate):integer;
        var
          res : integer;
        begin
          {
            Return values:
              > 0 when currpd is better than bestpd
              < 0 when bestpd is better than currpd
              = 0 when both are equal

            Too choose the best candidate we use the following order:
            - Incompatible flag
            - (Smaller) Number of convertlevel 2 parameters (needs less).
            - (Smaller) Number of convertlevel 1 parameters.
            - (Bigger) Number of exact parameters.
            - (Smaller) Number of equal parameters.
            - (Smaller) Total of ordinal distance. For example, the distance of a word
              to a byte is 65535-255=65280.
          }
          if bestpd^.invalid then
           begin
             if currpd^.invalid then
              res:=0
             else
              res:=1;
           end
          else
           if currpd^.invalid then
            res:=-1
          else
           begin
             { less cl2 parameters? }
             res:=(bestpd^.cl2_count-currpd^.cl2_count);
             if (res=0) then
              begin
                { less cl1 parameters? }
                res:=(bestpd^.cl1_count-currpd^.cl1_count);
                if (res=0) then
                 begin
                   { more exact parameters? }
                   res:=(currpd^.exact_count-bestpd^.exact_count);
                   if (res=0) then
                    begin
                      { less equal parameters? }
                      res:=(bestpd^.equal_count-currpd^.equal_count);
                      if (res=0) then
                       begin
                         { smaller ordinal distance? }
                         if (currpd^.ordinal_distance<bestpd^.ordinal_distance) then
                          res:=1
                         else
                          if (currpd^.ordinal_distance>bestpd^.ordinal_distance) then
                           res:=-1
                         else
                          res:=0;
                       end;
                    end;
                 end;
              end;
           end;
          is_better_candidate:=res;
        end;


    procedure var_para_allowed(var eq:tequaltype;def_from,def_to:Tdef);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.deftype of
          formaldef :
            begin
              { all types can be passed to a formaldef }
              eq:=te_equal;
            end;
          orddef :
            begin
              { allows conversion from word to integer and
                byte to shortint, but only for TP7 compatibility }
              if (m_tp7 in aktmodeswitches) and
                 (def_from.deftype=orddef) and
                 (def_from.size=def_to.size) then
                eq:=te_convert_l1;
            end;
          pointerdef :
            begin
              { an implicit pointer conversion is allowed }
              if (def_from.deftype=pointerdef) then
                eq:=te_convert_l1;
            end;
          stringdef :
            begin
              { all shortstrings are allowed, size is not important }
              if is_shortstring(def_from) and
                 is_shortstring(def_to) then
                eq:=te_equal;
            end;
          objectdef :
            begin
              { child objects can be also passed }
              { in non-delphi mode, otherwise    }
              { they must match exactly, except  }
              { if they are objects              }
              if (def_from.deftype=objectdef) and
                 (
                  not(m_delphi in aktmodeswitches) or
                  (
                   (tobjectdef(def_from).objecttype=odt_object) and
                   (tobjectdef(def_to).objecttype=odt_object)
                  )
                 ) and
                 (tobjectdef(def_from).is_related(tobjectdef(def_to))) then
                eq:=te_convert_l1;
            end;
          filedef :
            begin
              { an implicit file conversion is also allowed }
              { from a typed file to an untyped one           }
              if (def_from.deftype=filedef) and
                 (tfiledef(def_from).filetyp = ft_typed) and
                 (tfiledef(def_to).filetyp = ft_untyped) then
                eq:=te_convert_l1;
            end;
        end;
      end;


    procedure para_allowed(var eq:tequaltype;p:tcallparanode;def_to:tdef);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.deftype of
          formaldef :
            begin
              { all types can be passed to a formaldef }
              eq:=te_equal;
            end;
          stringdef :
            begin
              { to support ansi/long/wide strings in a proper way }
              { string and string[10] are assumed as equal }
              { when searching the correct overloaded procedure   }
              if (p.resulttype.def.deftype=stringdef) and
                 (tstringdef(def_to).string_typ=tstringdef(p.resulttype.def).string_typ) then
                eq:=te_equal
              else
              { Passing a constant char to ansistring or shortstring or
                a widechar to widestring then handle it as equal. }
               if (p.left.nodetype=ordconstn) and
                  (
                   is_char(p.resulttype.def) and
                   (is_shortstring(def_to) or is_ansistring(def_to))
                  ) or
                  (
                   is_widechar(p.resulttype.def) and
                   is_widestring(def_to)
                  ) then
                eq:=te_equal
            end;
          setdef :
            begin
              { set can also be a not yet converted array constructor }
              if (p.resulttype.def.deftype=arraydef) and
                 (tarraydef(p.resulttype.def).IsConstructor) and
                 not(tarraydef(p.resulttype.def).IsVariant) then
                eq:=te_equal;
            end;
          procvardef :
            begin
              { in tp7 mode proc -> procvar is allowed }
              if (m_tp_procvar in aktmodeswitches) and
                 (p.left.nodetype=calln) and
                 (proc_to_procvar_equal(tprocdef(tcallnode(p.left).procdefinition),tprocvardef(def_to))>=te_equal) then
               eq:=te_equal;
            end;
        end;
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


    procedure tcallparanode.insert_typeconv(defcoll : tparaitem;do_count : boolean);
      var
        oldtype     : ttype;
{$ifdef extdebug}
        store_count_ref : boolean;
{$endif def extdebug}
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
                  (aktcallprocdef.proccalloption in [pocall_cppdecl,pocall_cdecl]) then
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
         if paramanager.push_high_param(defcoll.paratype.def,aktcallprocdef.proccalloption) then
           gen_high_tree(is_open_string(defcoll.paratype.def));

         { test conversions }
         if not(is_shortstring(left.resulttype.def) and
                is_shortstring(defcoll.paratype.def)) and
            (defcoll.paratype.def.deftype<>formaldef) then
           begin
              { Process open parameters }
              if paramanager.push_high_param(defcoll.paratype.def,aktcallprocdef.proccalloption) then
               begin
                 { insert type conv but hold the ranges of the array }
                 oldtype:=left.resulttype;
                 inserttypeconv(left,defcoll.paratype);
                 left.resulttype:=oldtype;
               end
              else
               begin
                 { for ordinals, floats and enums, verify if we might cause
                   some range-check errors. }
                 if (left.resulttype.def.deftype in [enumdef,orddef,floatdef]) and
                    (left.nodetype in [vecn,loadn,calln]) then
                   begin
                      if (left.resulttype.def.size > defcoll.paratype.def.size) then
                        begin
                          if (cs_check_range in aktlocalswitches) then
                             Message(type_w_smaller_possible_range_check)
                          else
                             Message(type_h_smaller_possible_range_check);
                        end;
                   end;
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
            not(equal_defs(left.resulttype.def,defcoll.paratype.def)) then
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
               load_procvar_from_calln(left);

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
                                               cordconstnode.create(1,s32bittype,false));
                     loadconst:=false;
                   end;
               end;
           end;
        else
          len:=0;
        end;
        if loadconst then
          hightree:=cordconstnode.create(len,s32bittype,true)
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
         paralength:=-1;
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
{$ifdef EXTDEBUG}
             Comment(V_Error,'unknown compilerproc '+name);
{$endif EXTDEBUG}
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
        if paramanager.ret_in_param(restype.def,pocall_compilerproc) xor
           paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def,symtableprocentry.first_procdef.proccalloption) then
          internalerror(200108291);
      end;


    constructor tcallnode.createinternreturn(const name: string; params: tnode; returnnode : tnode);
      begin
        self.createintern(name,params);
        funcretrefnode:=returnnode;
        if not paramanager.ret_in_param(symtableprocentry.first_procdef.rettype.def,symtableprocentry.first_procdef.proccalloption) then
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
{$ifdef fpc}
{$warning FIXME: No withsymtable support}
{$endif}
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
                     AbstractMethodsList.Insert(hp.procsym.name)
                  else
                    { If this symbol is already in the list, and it is
                      an overriding method or dynamic, then remove it from the list
                    }
                    begin
                       { symbol was found }
                       if AbstractMethodsList.Find(hp.procsym.name) <> nil then
                         begin
                            if po_overridingmethod in hp.procoptions then
                              AbstractMethodsList.Remove(hp.procsym.name);
                         end;

                  end;
               end;
           end;
      end;


    procedure tcallnode.verifyabstractcalls;
      var
        objectdf : tobjectdef;
        parents : tlinkedlist;
        objectinfo : tobjectinfoitem;
        stritem : tstringlistitem;
        _classname : string;
      begin
        objectdf := nil;
        { verify if trying to create an instance of a class which contains
          non-implemented abstract methods }

        { first verify this class type, no class than exit  }
        { also, this checking can only be done if the constructor is directly
          called, indirect constructor calls cannot be checked.
        }
        if assigned(methodpointer) and assigned(methodpointer.resulttype.def) then
            if (methodpointer.resulttype.def.deftype = classrefdef) and
              (methodpointer.nodetype in [typen,loadvmtn]) then
              begin
                if (tclassrefdef(methodpointer.resulttype.def).pointertype.def.deftype = objectdef) then
                    objectdf := tobjectdef(tclassrefdef(methodpointer.resulttype.def).pointertype.def);

              end;
        if not assigned(objectdf) then exit;
        if assigned(objectdf.symtable.name) then
          _classname := objectdf.symtable.name^
        else
          _classname := '';

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
               objectdf.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}verifyabstract,nil);
             objectinfo:=tobjectinfoitem(objectinfo.next);
          end;
        if assigned(parents) then
          parents.free;
        { Finally give out a warning for each abstract method still in the list }
        stritem := tstringlistitem(AbstractMethodsList.first);
        while assigned(stritem) do
         begin
           if assigned(stritem.fpstr) then
              Message2(type_w_instance_with_abstract,lower(_classname),lower(stritem.fpstr^));
           stritem := tstringlistitem(stritem.next);
         end;
        if assigned(AbstractMethodsList) then
          AbstractMethodsList.Free;
      end;


    function Tcallnode.candidates_find:pcandidate;

      var
        j          : integer;
        pd         : tprocdef;
        procs,hp   : pcandidate;
        found,
        has_overload_directive : boolean;
        srsymtable : tsymtable;
        srprocsym  : tprocsym;

        procedure proc_add(pd:tprocdef);
        var
          i : integer;
        begin
          { generate new candidate entry }
          new(hp);
          fillchar(hp^,sizeof(tcandidate),0);
          hp^.data:=pd;
          hp^.next:=procs;
          procs:=hp;
          { Setup first parameter, skip all default parameters
            that are not passed. Ignore this skipping for varargs }
          hp^.firstpara:=tparaitem(pd.Para.first);
          if not(po_varargs in pd.procoptions) then
           begin
             for i:=1 to pd.maxparacount-paralength do
              hp^.firstpara:=tparaitem(hp^.firstPara.next);
           end;
        end;

      begin
        procs:=nil;

        { when the definition has overload directive set, we search for
          overloaded definitions in the class, this only needs to be done once
          for class entries as the tree keeps always the same }
        if (not symtableprocentry.overloadchecked) and
           (po_overload in symtableprocentry.first_procdef.procoptions) and
           (symtableprocentry.owner.symtabletype=objectsymtable) then
         search_class_overloads(symtableprocentry);

        { link all procedures which have the same # of parameters }
        for j:=1 to symtableprocentry.procdef_count do
          begin
            pd:=symtableprocentry.procdef[j];
            { only when the # of parameter are supported by the
              procedure }
            if (paralength>=pd.minparacount) and
               ((po_varargs in pd.procoptions) or { varargs }
                (paralength<=pd.maxparacount)) then
              proc_add(pd);
           end;

        { remember if the procedure is declared with the overload directive,
          it's information is still needed also after all procs are removed }
        has_overload_directive:=(po_overload in symtableprocentry.first_procdef.procoptions);

        { when the definition has overload directive set, we search for
          overloaded definitions in the symtablestack. The found
          entries are only added to the procs list and not the procsym, because
          the list can change in every situation }
        if has_overload_directive and
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
                     for j:=1 to srprocsym.procdef_count do
                      begin
                        pd:=srprocsym.procdef[j];
                        { only when the # of parameter are supported by the
                          procedure }
                        if (paralength>=pd.minparacount) and
                           ((po_varargs in pd.procoptions) or { varargs }
                           (paralength<=pd.maxparacount)) then
                         begin
                           found:=false;
                           hp:=procs;
                           while assigned(hp) do
                            begin
                              if compare_paras(hp^.data.para,pd.para,cp_value_equal_const,false)>=te_equal then
                               begin
                                 found:=true;
                                 break;
                               end;
                              hp:=hp^.next;
                            end;
                           if not found then
                             proc_add(pd);
                         end;
                      end;
                   end;
                end;
               srsymtable:=srsymtable.next;
             end;
          end;
        candidates_find:=procs;
      end;


    procedure tcallnode.candidates_free(procs:pcandidate);
      var
        hpnext,
        hp : pcandidate;
      begin
        hp:=procs;
        while assigned(hp) do
         begin
           hpnext:=hp^.next;
           dispose(hp);
           hp:=hpnext;
         end;
      end;


    procedure tcallnode.candidates_list(procs:pcandidate;all:boolean);
      var
        hp : pcandidate;
      begin
        hp:=procs;
        while assigned(hp) do
         begin
           if all or
              (not hp^.invalid) then
             MessagePos1(hp^.data.fileinfo,sym_b_param_list,hp^.data.fullprocname);
           hp:=hp^.next;
         end;
      end;


    procedure Tcallnode.candidates_get_information(procs:pcandidate);
      var
        hp       : pcandidate;
        currpara : tparaitem;
        currparanr : byte;
        def_from,
        def_to   : tdef;
        pt       : tcallparanode;
        eq       : tequaltype;
        convtype : tconverttype;
        pdoper   : tprocdef;
      begin
        { process all procs }
        hp:=procs;
        while assigned(hp) do
         begin
           currparanr:=paralength;
           currpara:=hp^.firstpara;
           pt:=tcallparanode(left);
           while assigned(pt) do
            begin
              { retrieve current parameter definitions to compares }
              def_from:=pt.resulttype.def;
              def_to:=currpara.paratype.def;
{$ifdef extdebug}
              if not(assigned(def_from)) then
               internalerror(200212091);
              if not(
                     assigned(def_to) or
                     ((po_varargs in hp^.data.procoptions) and
                      (currparanr>hp^.data.minparacount))
                    ) then
               internalerror(200212092);
{$endif extdebug}

              { varargs are always equal, but not exact }
              if (po_varargs in hp^.data.procoptions) and
                 (currparanr>hp^.data.minparacount) then
               begin
                 inc(hp^.equal_count);
               end
              else
              { same definition -> exact }
               if (def_from=def_to) then
                begin
                  inc(hp^.exact_count);
                end
              else
              { for value and const parameters check if a integer is constant or
                included in other integer -> equal and calc ordinal_distance }
               if not(currpara.paratyp in [vs_var,vs_out]) and
                  is_integer(def_from) and
                  is_integer(def_to) and
                  is_in_limit(def_from,def_to) then
                 begin
                   inc(hp^.equal_count);
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_from).low-torddef(def_to).low));
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_to).high-torddef(def_from).high));
                 end
              else
              { generic type comparision }
               begin
                 eq:=compare_defs_ext(def_from,def_to,pt.left.nodetype,
                                      false,true,convtype,pdoper);

                 { when the types are not equal we need to check
                   some special case for parameter passing }
                 if (eq<te_equal) then
                  begin
                    if currpara.paratyp in [vs_var,vs_out] then
                      begin
                        { para requires an equal type so the previous found
                          match was not good enough, reset to incompatible }
                        eq:=te_incompatible;
                        { var_para_allowed will return te_equal and te_convert_l1 to
                          make a difference for best matching }
                        var_para_allowed(eq,pt.resulttype.def,currpara.paratype.def)
                      end
                    else
                      para_allowed(eq,pt,def_to);
                  end;

                 case eq of
                   te_exact :
                     internalerror(200212071); { already checked }
                   te_equal :
                     inc(hp^.equal_count);
                   te_convert_l1 :
                     inc(hp^.cl1_count);
                   te_convert_l2,
                   te_convert_operator :
                     inc(hp^.cl2_count);
                   te_incompatible :
                     hp^.invalid:=true;
                   else
                     internalerror(200212072);
                 end;
               end;

              { stop checking when an incompatible parameter is found }
              if hp^.invalid then
               begin
                 { store the current parameter info for
                   a nice error message when no procedure is found }
                 hp^.wrongpara:=currpara;
                 hp^.wrongparanr:=currparanr;
                 break;
               end;

              { next parameter in the call tree }
              pt:=tcallparanode(pt.right);

              { next parameter for definition, only goto next para
                if we're out of the varargs }
              if not(po_varargs in hp^.data.procoptions) or
                 (currparanr<=hp^.data.maxparacount) then
                currpara:=tparaitem(currpara.next);
              dec(currparanr);
            end;

           { next candidate }
           hp:=hp^.next;
         end;
      end;


    function Tcallnode.candidates_choose_best(procs:pcandidate;var bestpd:tprocdef):integer;
      var
        besthpstart,
        hp       : pcandidate;
        cntpd,
        res      : integer;
      begin
        {
          Returns the number of candidates left and the
          first candidate is returned in pdbest
        }
        { Setup the first procdef as best, only count it as a result
          when it is valid }
        bestpd:=procs^.data;
        if procs^.invalid then
         cntpd:=0
        else
         cntpd:=1;
        if assigned(procs^.next) then
         begin
           besthpstart:=procs;
           hp:=procs^.next;
           while assigned(hp) do
            begin
              res:=is_better_candidate(hp,besthpstart);
              if (res>0) then
               begin
                 { hp is better, flag all procs to be incompatible }
                 while (besthpstart<>hp) do
                  begin
                    besthpstart^.invalid:=true;
                    besthpstart:=besthpstart^.next;
                  end;
                 { besthpstart is already set to hp }
                 bestpd:=besthpstart^.data;
                 cntpd:=1;
               end
              else
               if (res<0) then
                begin
                  { besthpstart is better, flag current hp to be incompatible }
                  hp^.invalid:=true;
                end
              else
               begin
                 { res=0, both are valid }
                 if not hp^.invalid then
                   inc(cntpd);
               end;
              hp:=hp^.next;
            end;
         end;

        candidates_choose_best:=cntpd;
      end;


    procedure tcallnode.candidates_find_wrong_para(procs:pcandidate);
      var
        currparanr : smallint;
        hp : pcandidate;
        pt : tcallparanode;
      begin
        { Only process the first overloaded procdef }
        hp:=procs;
        { Find callparanode corresponding to the argument }
        pt:=tcallparanode(left);
        currparanr:=paralength;
        while assigned(pt) and
              (currparanr>hp^.wrongparanr) do
         begin
           pt:=tcallparanode(pt.right);
           dec(currparanr);
         end;
        if (currparanr<>hp^.wrongparanr) or
           not assigned(pt) then
          internalerror(200212094);
        { Show error message, when it was a var or out parameter
          guess that it is a missing typeconv }
        if hp^.wrongpara.paratyp in [vs_var,vs_out] then
          CGMessagePos2(left.fileinfo,parser_e_call_by_ref_without_typeconv,
            pt.resulttype.def.typename,hp^.wrongpara.paratype.def.typename)
        else
          CGMessagePos3(pt.fileinfo,type_e_wrong_parameter_type,
            tostr(hp^.wrongparanr),pt.resulttype.def.typename,hp^.wrongpara.paratype.def.typename);
      end;


    function tcallnode.det_resulttype:tnode;
      var
        procs : pcandidate;
        oldcallprocdef : tabstractprocdef;
        hpt : tnode;
        pt : tcallparanode;
        lastpara : longint;
        pdc : tparaitem;
        cand_cnt : integer;
        i : longint;
        is_const : boolean;
      label
        errorexit;
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
                   procs:=candidates_find;

                   { no procedures found? then there is something wrong
                     with the parameter size }
                   if not assigned(procs) then
                    begin
                      { when it's an auto inherited call and there
                        is no procedure found, but the procedures
                        were defined with overload directive and at
                        least two procedures are defined then we ignore
                        this inherited by inserting a nothingn. Only
                        do this ugly hack in Delphi mode as it looks more
                        like a bug. It's also not documented }
                      if (m_delphi in aktmodeswitches) and
                         (nf_auto_inherited in flags) and
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
                             (m_tp_procvar in aktmodeswitches) and
                             (symtableprocentry.procdef_count=1) then
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
                        end;
                      goto errorexit;
                    end;

                   { Retrieve information about the candidates }
                   candidates_get_information(procs);

                   { Choose the best candidate and count the number of
                     candidates left }
                   cand_cnt:=candidates_choose_best(procs,tprocdef(procdefinition));

                   { All parameters are checked, check if there are any
                     procedures left }
                   if cand_cnt>0 then
                    begin
                      { Multiple candidates left? }
                      if cand_cnt>1 then
                       begin
                         CGMessage(cg_e_cant_choose_overload_function);
                         candidates_list(procs,false);
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
                      candidates_find_wrong_para(procs);
                      candidates_list(procs,true);

                      { We can not proceed, release all procs and exit }
                      candidates_free(procs);
                      goto errorexit;
                    end;

                   candidates_free(procs);
               end; { end of procedure to call determination }

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
         if not restypeset then
           resulttype:=procdefinition.rettype
         else
           resulttype:=restype;

         { modify the exit code, in case of special cases }
         if (not is_void(resulttype.def)) then
          begin
            if paramanager.ret_in_reg(resulttype.def,procdefinition.proccalloption) then
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
         aktcallprocdef:=oldcallprocdef;
      end;


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
                   if not assigned(inlinecode) then
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
             { for win32 records returned in EDX:EAX, we
               move them to memory after ... }
             if (resulttype.def.deftype=recorddef) then
              begin
                location.loc:=LOC_CREFERENCE;
              end
             else
              if paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
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
{$ifdef cpufpemu}
                       if (cs_fp_emulation in aktmoduleswitches) then
                         registers32:=1
                       else
{$endif cpufpemu}
{$ifdef m68k}
                        if (tfloatdef(resulttype.def).typ=s32real) then
                         registers32:=1
                       else
{$endif m68k}
                         registersfpu:=1;
                     end;
                   else
                     begin
                       location.loc:=LOC_REGISTER;
                       registers32:=1;
                     end;
                 end;
               end;
           end;
{$ifdef m68k}
         { we need one more address register for virtual calls on m68k }
         if (po_virtualmethod in procdefinition.procoptions) then
           inc(registers32);
{$endif m68k}
         { a fpu can be used in any procedure !! }
{$ifdef i386}
         registersfpu:=procdefinition.fpu_used;
{$endif i386}
         { if this is a call to a method calc the registers }
         if (methodpointer<>nil) then
           begin
              { if we are calling the constructor }
              if procdefinition.proctypeoption in [potype_constructor] then
                verifyabstractcalls;

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
          (procdefinition = tcallnode(p).procdefinition) and
          (methodpointer.isequal(tcallnode(p).methodpointer)) and
          ((restypeset and tcallnode(p).restypeset and
            (equal_defs(restype.def,tcallnode(p).restype.def))) or
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
      var
        storesymtablelevel : longint;
        storeparasymtable,
        storelocalsymtable : tsymtabletype;
        oldprocdef : tprocdef;
        oldprocinfo : tprocinfo;
        oldinlining_procedure : boolean;
      begin
         result:=nil;
         oldinlining_procedure:=inlining_procedure;
         oldprocdef:=aktprocdef;
         oldprocinfo:=procinfo;
         { we're inlining a procedure }
         inlining_procedure:=true;
         aktprocdef:=inlineprocdef;

         { clone procinfo, but not the asmlists }
         procinfo:=tprocinfo(cprocinfo.newinstance);
         move(pointer(oldprocinfo)^,pointer(procinfo)^,cprocinfo.InstanceSize);
         procinfo.aktentrycode:=nil;
         procinfo.aktexitcode:=nil;
         procinfo.aktproccode:=nil;
         procinfo.aktlocaldata:=nil;

         { set new procinfo }
         procinfo.return_offset:=retoffset;
         procinfo.para_offset:=para_offset;
         procinfo.no_fast_exit:=false;

        { set it to the same lexical level }
        storesymtablelevel:=aktprocdef.localst.symtablelevel;
        storelocalsymtable:=aktprocdef.localst.symtabletype;
        storeparasymtable:=aktprocdef.parast.symtabletype;
        aktprocdef.localst.symtablelevel:=oldprocdef.localst.symtablelevel;
        aktprocdef.localst.symtabletype:=inlinelocalsymtable;
        aktprocdef.parast.symtabletype:=inlineparasymtable;

                                                { pass inlinetree }
         resulttypepass(inlinetree);
         resulttype:=inlineprocdef.rettype;

         { retrieve info from inlineprocdef }
         retoffset:=-POINTER_SIZE; { less dangerous as zero (PM) }
         para_offset:=0;
         para_size:=inlineprocdef.para_size(target_info.alignment.paraalign);
         if paramanager.ret_in_param(inlineprocdef.rettype.def,inlineprocdef.proccalloption) then
           inc(para_size,POINTER_SIZE);

         { restore procinfo }
         procinfo.free;
         procinfo:=oldprocinfo;
         { restore symtable }
         aktprocdef.localst.symtablelevel:=storesymtablelevel;
         aktprocdef.localst.symtabletype:=storelocalsymtable;
         aktprocdef.parast.symtabletype:=storeparasymtable;
         { restore }
         aktprocdef:=oldprocdef;
         inlining_procedure:=oldinlining_procedure;
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
  Revision 1.117  2002-12-11 22:42:28  peter
    * tcallnode.det_resulttype rewrite, merged code from nice_ncal and
      the old code. The new code collects the information about possible
      candidates only once resultting in much less calls to type compare
      routines

  Revision 1.116  2002/12/07 14:27:07  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.115  2002/12/06 17:51:10  peter
    * merged cdecl and array fixes

  Revision 1.114  2002/12/06 16:56:58  peter
    * only compile cs_fp_emulation support when cpufpuemu is defined
    * define cpufpuemu for m68k only

  Revision 1.113  2002/11/27 20:04:38  peter
    * cdecl array of const fixes

  Revision 1.112  2002/11/27 15:33:46  peter
    * the never ending story of tp procvar hacks

  Revision 1.111  2002/11/27 02:31:17  peter
    * fixed inlinetree parsing in det_resulttype

  Revision 1.110  2002/11/25 18:43:32  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.109  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.108  2002/11/18 17:31:54  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.107  2002/11/15 01:58:50  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.106  2002/10/14 18:20:30  carl
    * var parameter checking for classes and interfaces in Delphi mode

  Revision 1.105  2002/10/06 21:02:17  peter
    * fixed limit checking for qword

  Revision 1.104  2002/10/05 15:15:45  peter
    * Write unknwon compiler proc using Comment and only in Extdebug

  Revision 1.103  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.102  2002/10/05 00:48:57  peter
    * support inherited; support for overload as it is handled by
      delphi. This is only for delphi mode as it is working is
      undocumented and hard to predict what is done

  Revision 1.101  2002/09/16 14:11:12  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.100  2002/09/15 17:49:59  peter
    * don't have strict var parameter checking for procedures in the
      system unit

  Revision 1.99  2002/09/09 19:30:34  peter
    * don't allow convertable parameters for var and out parameters in
      delphi and tp mode

  Revision 1.98  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.97  2002/09/07 12:16:05  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.96  2002/09/05 14:53:41  peter
    * fixed old callnode.det_resulttype code
    * old ncal code is default again

  Revision 1.95  2002/09/03 21:32:49  daniel
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

}
