{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Type checking and register allocation for load/assignment nodes

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
unit nld;

{$i defines.inc}

interface

    uses
       node,
       symconst,symbase,symtype,symsym,symdef;

    type
       tloadnode = class(tunarynode)
          symtableentry : tsym;
          symtable : tsymtable;
          procdeflist : tprocdef;
          constructor create(v : tsym;st : tsymtable);virtual;
          constructor create_procvar(v : tsym;d:tprocdef;st : tsymtable);virtual;
          procedure set_mp(p:tnode);
          function  getcopy : tnode;override;
          function  pass_1 : tnode;override;
          function  det_resulttype:tnode;override;
          function  docompare(p: tnode): boolean; override;
       end;
       tloadnodeclass = class of tloadnode;

       { different assignment types }
       tassigntype = (at_normal,at_plus,at_minus,at_star,at_slash);

       tassignmentnode = class(tbinarynode)
          assigntype : tassigntype;
          constructor create(l,r : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tassignmentnodeclass = class of tassignmentnode;

       tfuncretnode = class(tnode)
          funcretsym : tfuncretsym;
          constructor create(v:tsym);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tfuncretnodeclass = class of tfuncretnode;

       tarrayconstructorrangenode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tarrayconstructorrangenodeclass = class of tarrayconstructorrangenode;

       tarrayconstructornode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
          procedure force_type(tt:ttype);
       end;
       tarrayconstructornodeclass = class of tarrayconstructornode;

       ttypenode = class(tnode)
          allowed : boolean;
          restype : ttype;
          constructor create(t : ttype);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       ttypenodeclass = class of ttypenode;

       trttinode = class(tnode)
          l1,l2  : longint;
          rttitype : trttitype;
          rttidef : tstoreddef;
          constructor create(def:tstoreddef;rt:trttitype);virtual;
          function  getcopy : tnode;override;
          function pass_1 : tnode;override;
          procedure pass_2;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       trttinodeclass = class of trttinode;

    var
       cloadnode : tloadnodeclass;
       cassignmentnode : tassignmentnodeclass;
       cfuncretnode : tfuncretnodeclass;
       carrayconstructorrangenode : tarrayconstructorrangenodeclass;
       carrayconstructornode : tarrayconstructornodeclass;
       ctypenode : ttypenodeclass;
       crttinode : trttinodeclass;


implementation

    uses
      cutils,verbose,globtype,globals,systems,
      symtable,types,
      htypechk,pass_1,
      ncnv,nmem,ncal,cpubase,rgobj,cginfo,cgbase
      ;


{*****************************************************************************
                             TLOADNODE
*****************************************************************************}

    constructor tloadnode.create(v : tsym;st : tsymtable);
      begin
         inherited create(loadn,nil);
         if not assigned(v) then
          internalerror(200108121);
         symtableentry:=v;
         symtable:=st;
         procdeflist:=nil;
      end;

    constructor tloadnode.create_procvar(v : tsym;d:tprocdef;st : tsymtable);
      begin
         inherited create(loadn,nil);
         if not assigned(v) then
          internalerror(200108121);
         symtableentry:=v;
         symtable:=st;
         procdeflist:=d;
      end;

    procedure tloadnode.set_mp(p:tnode);
      begin
        left:=p;
      end;


    function tloadnode.getcopy : tnode;
      var
         n : tloadnode;

      begin
         n:=tloadnode(inherited getcopy);
         n.symtable:=symtable;
         n.symtableentry:=symtableentry;
         result:=n;
      end;


    function tloadnode.det_resulttype:tnode;
      var
        p1 : tnode;
        p  : pprocinfo;
      begin
         result:=nil;
         { optimize simple with loadings }
         if (symtable.symtabletype=withsymtable) and
            (twithsymtable(symtable).direct_with) and
            (symtableentry.typ=varsym) then
           begin
              p1:=tnode(twithsymtable(symtable).withrefnode).getcopy;
              p1:=csubscriptnode.create(tvarsym(symtableentry),p1);
              left:=nil;
              result:=p1;
              exit;
           end;
         { handle first absolute as it will replace the symtableentry }
         if symtableentry.typ=absolutesym then
           begin
             { force the resulttype to the type of the absolute }
             resulttype:=tabsolutesym(symtableentry).vartype;
             { replace the symtableentry when it points to a var, else
               we are finished }
             if tabsolutesym(symtableentry).abstyp=tovar then
              begin
                symtableentry:=tabsolutesym(symtableentry).ref;
                symtable:=symtableentry.owner;
                include(flags,nf_absolute);
              end
             else
              exit;
           end;
         case symtableentry.typ of
            funcretsym :
              begin
                { find the main funcret for the function }
                p:=procinfo;
                while assigned(p) do
                 begin
                   if assigned(p^.procdef.funcretsym) and
                      ((tfuncretsym(symtableentry)=p^.procdef.resultfuncretsym) or
                       (tfuncretsym(symtableentry)=p^.procdef.funcretsym)) then
                     begin
                       symtableentry:=p^.procdef.funcretsym;
                       break;
                     end;
                    p:=p^.parent;
                  end;
                { generate funcretnode }
                p1:=cfuncretnode.create(symtableentry);
                resulttypepass(p1);
                { if it's refered as absolute then we need to have the
                  type of the absolute instead of the function return,
                  the function return is then also assigned }
                if nf_absolute in flags then
                 begin
                   tfuncretsym(symtableentry).funcretstate:=vs_assigned;
                   p1.resulttype:=resulttype;
                 end;
                left:=nil;
                result:=p1;
              end;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   resulttype:=cansistringtype
                 else
                   internalerror(22799);
              end;
            varsym :
                begin
                  { if it's refered by absolute then it's used }
                  if nf_absolute in flags then
                   tvarsym(symtableentry).varstate:=vs_used
                  else
                   resulttype:=tvarsym(symtableentry).vartype;
                end;
            typedconstsym :
                if not(nf_absolute in flags) then
                  resulttype:=ttypedconstsym(symtableentry).typedconsttype;
            procsym :
                begin
                   if not assigned(procdeflist) then
                    begin
                      if assigned(tprocsym(symtableentry).defs^.next) then
                       CGMessage(parser_e_no_overloaded_procvars);
                      resulttype.setdef(tprocsym(symtableentry).defs^.def);
                    end
                   else
                    resulttype.setdef(procdeflist);
                   { if the owner of the procsym is a object,  }
                   { left must be set, if left isn't set       }
                   { it can be only self                       }
                   { this code is only used in TP procvar mode }
                   if (m_tp_procvar in aktmodeswitches) and
                      not(assigned(left)) and
                      (tprocsym(symtableentry).owner.symtabletype=objectsymtable) then
                    begin
                      left:=cselfnode.create(tobjectdef(symtableentry.owner.defowner));
                    end;

                   { process methodpointer }
                   if assigned(left) then
                    begin
                      resulttypepass(left);

                      { turn on the allowed flag, the secondpass
                        will handle the typen itself }
                      if left.nodetype=typen then
                       ttypenode(left).allowed:=true;
                    end;
                end;
           else
             internalerror(200104141);
         end;
      end;


    function tloadnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_REFERENCE;
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         case symtableentry.typ of
            absolutesym :
              ;
            funcretsym :
              internalerror(200104142);
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      { we use ansistrings so no fast exit here }
                      if assigned(procinfo) then
                        procinfo^.no_fast_exit:=true;
                      location.loc:=LOC_CREFERENCE;
                   end;
              end;
            varsym :
                begin
                  if (symtable.symtabletype in [parasymtable,localsymtable]) and
                      (lexlevel>symtable.symtablelevel) then
                     begin
                       { if the variable is in an other stackframe then we need
                         a register to dereference }
                       if (symtable.symtablelevel)>0 then
                        begin
                          registers32:=1;
                          { further, the variable can't be put into a register }
                          tvarsym(symtableentry).varoptions:=
                            tvarsym(symtableentry).varoptions-[vo_fpuregable,vo_regable];
                        end;
                     end;
                   if (tvarsym(symtableentry).varspez=vs_const) then
                     location.loc:=LOC_CREFERENCE;
                   { we need a register for call by reference parameters }
                   if (tvarsym(symtableentry).varspez in [vs_var,vs_out]) or
                      ((tvarsym(symtableentry).varspez=vs_const) and
                      push_addr_param(tvarsym(symtableentry).vartype.def)) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(tvarsym(symtableentry).vartype.def) then
                     registers32:=1;
                   if symtable.symtabletype=withsymtable then
                     inc(registers32);

                   if ([vo_is_thread_var,vo_is_dll_var]*tvarsym(symtableentry).varoptions)<>[] then
                     registers32:=1;
                   { count variable references }

                     { this will create problem with local var set by
                     under_procedures
                     if (assigned(tvarsym(symtableentry).owner) and assigned(aktprocsym)
                       and ((tvarsym(symtableentry).owner = aktprocdef.localst)
                       or (tvarsym(symtableentry).owner = aktprocdef.localst))) then }
                   if rg.t_times<1 then
                     inc(tvarsym(symtableentry).refs)
                   else
                     inc(tvarsym(symtableentry).refs,rg.t_times);
                end;
            typedconstsym :
                ;
            procsym :
                begin
                   { method pointer ? }
                   if assigned(left) then
                     begin
                        firstpass(left);
                        registers32:=max(registers32,left.registers32);
                        registersfpu:=max(registersfpu,left.registersfpu);
 {$ifdef SUPPORT_MMX}
                        registersmmx:=max(registersmmx,left.registersmmx);
 {$endif SUPPORT_MMX}
                     end;
                end;
           else
             internalerror(200104143);
         end;
      end;


    function tloadnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableentry = tloadnode(p).symtableentry) and
          (symtable = tloadnode(p).symtable);
      end;


{*****************************************************************************
                             TASSIGNMENTNODE
*****************************************************************************}

    constructor tassignmentnode.create(l,r : tnode);

      begin
         inherited create(assignn,l,r);
         assigntype:=at_normal;
      end;

    function tassignmentnode.getcopy : tnode;

      var
         n : tassignmentnode;

      begin
         n:=tassignmentnode(inherited getcopy);
         n.assigntype:=assigntype;
         getcopy:=n;
      end;


    function tassignmentnode.det_resulttype:tnode;
      var
        hp,hp2 : tnode;
      begin
        result:=nil;
        resulttype:=voidtype;

        { must be made unique }
        if assigned(left) then
          begin
             set_unique(left);

             { set we the function result? }
             set_funcret_is_valid(left);
          end;

        resulttypepass(left);
        resulttypepass(right);
        set_varstate(left,false);
        set_varstate(right,true);
        if codegenerror then
          exit;

        { assignments to open arrays aren't allowed }
        if is_open_array(left.resulttype.def) then
          CGMessage(type_e_mismatch);

        { assigning nil to a dynamic array clears the array }
        if is_dynamic_array(left.resulttype.def) and
           (right.nodetype=niln) then
         begin
           hp := ctypeconvnode.create(left,voidpointertype);
           hp.toggleflag(nf_explizit);
           hp2 := crttinode.create(tstoreddef(left.resulttype.def),initrtti);
           hp := ccallparanode.create(hp2,ccallparanode.create(hp,nil));
           left:=nil;
           result := ccallnode.createintern('fpc_dynarray_clear',hp);
           exit;
         end;

        { some string functions don't need conversion, so treat them separatly }
        if not (
                is_shortstring(left.resulttype.def) and
                (
                 is_shortstring(right.resulttype.def) or
                 is_ansistring(right.resulttype.def) or
                 is_char(right.resulttype.def)
                )
               ) then
         inserttypeconv(right,left.resulttype);

        { test if node can be assigned, properties are allowed }
        valid_for_assignment(left);

        { check if local proc/func is assigned to procvar }
        if right.resulttype.def.deftype=procvardef then
          test_local_to_procvar(tprocvardef(right.resulttype.def),left.resulttype.def);
      end;


    function tassignmentnode.pass_1 : tnode;
      begin
         result:=nil;

         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { some string functions don't need conversion, so treat them separatly }
         if is_shortstring(left.resulttype.def) and
            (
             is_shortstring(right.resulttype.def) or
             is_ansistring(right.resulttype.def) or
             is_char(right.resulttype.def)
            ) then
          begin
            { we call STRCOPY }
            procinfo^.flags:=procinfo^.flags or pi_do_call;
            { test for s:=s+anything ... }
            { the problem is for
              s:=s+s+s;
              this is broken here !! }
{$ifdef newoptimizations2}
            { the above is fixed now, but still problem with s := s + f(); if }
            { f modifies s (bad programming, so only enable if uncertain      }
            { optimizations are on) (JM)                                      }
            if (cs_UncertainOpts in aktglobalswitches) then
              begin
                hp := right;
                while hp.treetype=addn do hp:=hp.left;
                if equal_trees(left,hp) and
                   not multiple_uses(left,right) then
                  begin
                    concat_string:=true;
                    hp:=right;
                    while hp.treetype=addn do
                      begin
                        hp.use_strconcat:=true;
                        hp:=hp.left;
                      end;
                  end;
              end;
{$endif newoptimizations2}
          end;

         registers32:=left.registers32+right.registers32;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
      end;

    function tassignmentnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (assigntype = tassignmentnode(p).assigntype);
      end;


{*****************************************************************************
                                 TFUNCRETNODE
*****************************************************************************}

    constructor tfuncretnode.create(v:tsym);

      begin
         inherited create(funcretn);
         funcretsym:=tfuncretsym(v);
      end;


    function tfuncretnode.getcopy : tnode;
      var
         n : tfuncretnode;
      begin
         n:=tfuncretnode(inherited getcopy);
         n.funcretsym:=funcretsym;
         getcopy:=n;
      end;


    function tfuncretnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=funcretsym.returntype;
      end;


    function tfuncretnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_REFERENCE;
         if ret_in_param(resulttype.def) or
            (lexlevel<>funcretsym.owner.symtablelevel) then
           registers32:=1;
      end;


    function tfuncretnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (funcretsym = tfuncretnode(p).funcretsym);
      end;


{*****************************************************************************
                           TARRAYCONSTRUCTORRANGENODE
*****************************************************************************}

    constructor tarrayconstructorrangenode.create(l,r : tnode);

      begin
         inherited create(arrayconstructorrangen,l,r);
      end;

    function tarrayconstructorrangenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        resulttypepass(right);
        set_varstate(left,true);
        set_varstate(right,true);
        if codegenerror then
         exit;
        resulttype:=left.resulttype;
      end;


    function tarrayconstructorrangenode.pass_1 : tnode;
      begin
        firstpass(left);
        firstpass(right);
        location.loc := LOC_CREFERENCE;
        calcregisters(self,0,0,0);
        result:=nil;
      end;


{****************************************************************************
                            TARRAYCONSTRUCTORNODE
*****************************************************************************}

    constructor tarrayconstructornode.create(l,r : tnode);
      begin
         inherited create(arrayconstructorn,l,r);
      end;


    function tarrayconstructornode.getcopy : tnode;
      var
         n : tarrayconstructornode;
      begin
         n:=tarrayconstructornode(inherited getcopy);
         result:=n;
      end;


    function tarrayconstructornode.det_resulttype:tnode;
      var
        htype : ttype;
        hp    : tarrayconstructornode;
        len   : longint;
        varia : boolean;
      begin
        result:=nil;

      { are we allowing array constructor? Then convert it to a set }
        if not allow_array_constructor then
         begin
           hp:=tarrayconstructornode(getcopy);
           arrayconstructor_to_set(hp);
           result:=hp;
           exit;
         end;

      { only pass left tree, right tree contains next construct if any }
        htype.reset;
        len:=0;
        varia:=false;
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              resulttypepass(hp.left);
              set_varstate(hp.left,true);
              if (htype.def=nil) then
               htype:=hp.left.resulttype
              else
               begin
                 if ((nf_novariaallowed in flags) or (not varia)) and
                    (not is_equal(htype.def,hp.left.resulttype.def)) then
                  begin
                    varia:=true;
                  end;
               end;
              inc(len);
              hp:=tarrayconstructornode(hp.right);
            end;
         end;
         if not assigned(htype.def) then
          htype:=voidtype;
         resulttype.setdef(tarraydef.create(0,len-1,s32bittype));
         tarraydef(resulttype.def).elementtype:=htype;
         tarraydef(resulttype.def).IsConstructor:=true;
         tarraydef(resulttype.def).IsVariant:=varia;
      end;


    procedure tarrayconstructornode.force_type(tt:ttype);
      var
        hp : tarrayconstructornode;
      begin
        tarraydef(resulttype.def).elementtype:=tt;
        tarraydef(resulttype.def).IsConstructor:=true;
        tarraydef(resulttype.def).IsVariant:=false;
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              inserttypeconv(hp.left,tt);
              hp:=tarrayconstructornode(hp.right);
            end;
         end;
      end;


    function tarrayconstructornode.pass_1 : tnode;
      var
        thp,
        chp,
        hp        : tarrayconstructornode;
        dovariant : boolean;
        htype     : ttype;
        orgflags  : tnodeflagset;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        result:=nil;
        { only pass left tree, right tree contains next construct if any }
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              firstpass(hp.left);
              { Insert typeconvs for array of const }
              if dovariant then
               begin
                 case hp.left.resulttype.def.deftype of
                   enumdef :
                     begin
                       hp.left:=ctypeconvnode.create(hp.left,s32bittype);
                       firstpass(hp.left);
                     end;
                   orddef :
                     begin
                       if is_integer(hp.left.resulttype.def) and
                         not(is_64bitint(hp.left.resulttype.def)) then
                        begin
                          hp.left:=ctypeconvnode.create(hp.left,s32bittype);
                          firstpass(hp.left);
                        end;
                     end;
                   floatdef :
                     begin
                       hp.left:=ctypeconvnode.create(hp.left,pbestrealtype^);
                       firstpass(hp.left);
                     end;
                   stringdef :
                     begin
                       if nf_cargs in flags then
                        begin
                          hp.left:=ctypeconvnode.create(hp.left,charpointertype);
                          firstpass(hp.left);
                        end;
                     end;
                   procvardef :
                     begin
                       hp.left:=ctypeconvnode.create(hp.left,voidpointertype);
                       firstpass(hp.left);
                     end;
                   pointerdef,
                   classrefdef,
                   objectdef : ;
                   else
                     CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype.def.typename);
                 end;
               end;
              hp:=tarrayconstructornode(hp.right);
            end;
         { swap the tree for cargs }
           if (nf_cargs in flags) and (not(nf_cargswap in flags)) then
            begin
              chp:=nil;
              { save resulttype }
              htype:=resulttype;
              { we need a copy here, because self is destroyed }
              { by firstpass later                             }
              hp:=tarrayconstructornode(getcopy);
              { we also need a copy of the nf_ forcevaria flag to restore }
              { later) (JM)                                               }
              orgflags := flags * [nf_forcevaria];
              while assigned(hp) do
               begin
                 thp:=tarrayconstructornode(hp.right);
                 hp.right:=chp;
                 chp:=hp;
                 hp:=thp;
               end;
              chp.flags := chp.flags+orgflags;
              include(chp.flags,nf_cargswap);
              chp.location.loc:=LOC_CREFERENCE;
              calcregisters(chp,0,0,0);
              chp.resulttype:=htype;
              result:=chp;
              exit;
            end;
         end;
        { C Arguments are pushed on the stack and
          are not accesible after the push }
        if not(nf_cargs in flags) then
         location.loc:=LOC_CREFERENCE
        else
         location.loc:=LOC_INVALID;
        calcregisters(self,0,0,0);
      end;


    function tarrayconstructornode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
      end;


{*****************************************************************************
                              TTYPENODE
*****************************************************************************}

    constructor ttypenode.create(t : ttype);
      begin
         inherited create(typen);
         restype:=t;
         allowed:=false;
      end;


    function ttypenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
        { check if it's valid }
        if restype.def.deftype = errordef then
          CGMessage(cg_e_illegal_expression);
      end;


    function ttypenode.pass_1 : tnode;
      begin
         result:=nil;
         { a typenode can't generate code, so we give here
           an error. Else it'll be an abstract error in pass_2.
           Only when the allowed flag is set we don't generate
           an error }
         if not allowed then
          Message(parser_e_no_type_not_allowed_here);
      end;


    function ttypenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
      end;


{*****************************************************************************
                              TRTTINODE
*****************************************************************************}


    constructor trttinode.create(def:tstoreddef;rt:trttitype);
      begin
         inherited create(rttin);
         rttidef:=def;
         rttitype:=rt;
      end;


    function trttinode.getcopy : tnode;
      var
         n : trttinode;
      begin
         n:=trttinode(inherited getcopy);
         n.rttidef:=rttidef;
         n.rttitype:=rttitype;
         result:=n;
      end;


    function trttinode.det_resulttype:tnode;
      begin
        { rtti information will be returned as a void pointer }
        result:=nil;
        resulttype:=voidpointertype;
      end;


    function trttinode.pass_1 : tnode;
      begin
        result:=nil;
        location.loc:=LOC_CREFERENCE;
      end;


    function trttinode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (rttidef = trttinode(p).rttidef) and
          (rttitype = trttinode(p).rttitype);
      end;


    procedure trttinode.pass_2;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        location.reference.symbol:=rttidef.get_rtti_label(rttitype);
      end;


begin
   cloadnode:=tloadnode;
   cassignmentnode:=tassignmentnode;
   cfuncretnode:=tfuncretnode;
   carrayconstructorrangenode:=tarrayconstructorrangenode;
   carrayconstructornode:=tarrayconstructornode;
   ctypenode:=ttypenode;
   crttinode:=trttinode;
end.
{
  $Log$
  Revision 1.35  2002-04-21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.34  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.33  2002/03/31 20:26:34  jonas
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

  Revision 1.32  2002/01/19 11:52:32  peter
    * dynarr:=nil support added

  Revision 1.31  2001/12/28 15:02:00  jonas
    * fixed web bug 1684 (it already didn't crash anymore, but it also didn't
      generate an error) ("merged")

  Revision 1.30  2001/11/07 13:52:52  jonas
    * only save/restore nf_forcevaria flag when reversing order of
      arrayconstructor elements, since the other flags are element specific

  Revision 1.29  2001/11/02 22:58:02  peter
    * procsym definition rewrite

  Revision 1.28  2001/10/31 17:34:20  jonas
    * fixed web bug 1651

  Revision 1.27  2001/10/28 17:22:25  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.26  2001/10/12 13:51:51  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.25  2001/09/02 21:12:07  peter
    * move class of definitions into type section for delphi

  Revision 1.24  2001/08/30 15:48:34  jonas
    * fix from Peter for getting correct symtableentry for funcret loads

  Revision 1.23  2001/08/26 13:36:41  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.22  2001/08/12 22:11:52  peter
    * errordef.typesym is not updated anymore

  Revision 1.21  2001/08/06 21:40:47  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.20  2001/07/30 20:52:25  peter
    * fixed array constructor passing with type conversions

  Revision 1.19  2001/06/04 18:07:47  peter
    * remove unused typenode for procvar load. Don't know what happened why
      this code was not there already with revision 1.17.

  Revision 1.18  2001/06/04 11:48:01  peter
    * better const to var checking

  Revision 1.17  2001/05/19 21:19:57  peter
    * remove unused typenode for procvars to prevent error
    * typenode.allowed flag to allow a typenode

  Revision 1.16  2001/05/09 19:57:51  peter
    * typenode doesn't generate code, give error in pass_1 instead of
      getting an abstract methode runtime error

  Revision 1.15  2001/04/14 14:06:31  peter
    * move more code from loadnode.pass_1 to det_resulttype

  Revision 1.14  2001/04/13 01:22:10  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/05 21:03:08  peter
    * array constructor fix

  Revision 1.12  2001/04/04 22:42:40  peter
    * move constant folding into det_resulttype

  Revision 1.11  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.10  2000/12/31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.9  2000/11/29 00:30:33  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.8  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.7  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.4  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.3  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.2  2000/09/25 15:37:14  florian
    * more fixes

  Revision 1.1  2000/09/25 14:55:05  florian
    * initial revision
}
