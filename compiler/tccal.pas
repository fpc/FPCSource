{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Type checking and register allocation for call nodes

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
unit tccal;
interface

    uses
      symtable,tree;


    procedure gen_high_tree(p:ptree;openstring:boolean);

    procedure firstcallparan(var p : ptree;defcoll : pparaitem;do_count : boolean);
    procedure firstcalln(var p : ptree);
    procedure firstprocinline(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,aasm,types,
      hcodegen,htypechk,pass_1,cpubase
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif m68k}
      ;

{*****************************************************************************
                             FirstCallParaN
*****************************************************************************}

    procedure gen_high_tree(p:ptree;openstring:boolean);
      var
        len : longint;
        st  : psymtable;
        loadconst : boolean;
      begin
        if assigned(p^.hightree) then
         exit;
        len:=-1;
        loadconst:=true;
        case p^.left^.resulttype^.deftype of
          arraydef :
            begin
              if is_open_array(p^.left^.resulttype) or
                 is_array_of_const(p^.left^.resulttype) then
               begin
                 st:=p^.left^.symtable;
                 getsymonlyin(st,'high'+pvarsym(p^.left^.symtableentry)^.name);
                 p^.hightree:=genloadnode(pvarsym(srsym),st);
                 loadconst:=false;
               end
              else
                begin
                  { this is an empty constructor }
                  len:=parraydef(p^.left^.resulttype)^.highrange-
                       parraydef(p^.left^.resulttype)^.lowrange;
                end;
            end;
          stringdef :
            begin
              if openstring then
               begin
                 if is_open_string(p^.left^.resulttype) then
                  begin
                    st:=p^.left^.symtable;
                    getsymonlyin(st,'high'+pvarsym(p^.left^.symtableentry)^.name);
                    p^.hightree:=genloadnode(pvarsym(srsym),st);
                    loadconst:=false;
                  end
                 else
                  len:=pstringdef(p^.left^.resulttype)^.len;
               end
              else
             { passing a string to an array of char }
               begin
                 if (p^.left^.treetype=stringconstn) then
                   begin
                     len:=str_length(p^.left);
                     if len>0 then
                      dec(len);
                   end
                 else
                   begin
                     p^.hightree:=gennode(subn,geninlinenode(in_length_string,false,getcopy(p^.left)),
                                               genordinalconstnode(1,s32bitdef));
                     firstpass(p^.hightree);
                     p^.hightree:=gentypeconvnode(p^.hightree,s32bitdef);
                     loadconst:=false;
                   end;
               end;
           end;
        else
          len:=0;
        end;
        if loadconst then
          p^.hightree:=genordinalconstnode(len,s32bitdef);
        firstpass(p^.hightree);
      end;


    procedure firstcallparan(var p : ptree;defcoll : pparaitem;do_count : boolean);
      var
        old_get_para_resulttype : boolean;
        old_array_constructor : boolean;
        oldtype     : pdef;
{$ifdef extdebug}
        store_count_ref : boolean;
{$endif def extdebug}
        {convtyp     : tconverttype;}
      begin
         inc(parsing_para_level);
{$ifdef extdebug}
         if do_count then
           begin
             store_count_ref:=count_ref;
             count_ref:=true;
           end;
{$endif def extdebug}
         if assigned(p^.right) then
           begin
              if defcoll=nil then
                firstcallparan(p^.right,nil,do_count)
              else
                firstcallparan(p^.right,pparaitem(defcoll^.next),do_count);
              p^.registers32:=p^.right^.registers32;
              p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.right^.registersmmx;
{$endif}
           end;
         if defcoll=nil then
           begin
              old_array_constructor:=allow_array_constructor;
              old_get_para_resulttype:=get_para_resulttype;
              get_para_resulttype:=true;
              allow_array_constructor:=true;
              firstpass(p^.left);
              get_para_resulttype:=old_get_para_resulttype;
              allow_array_constructor:=old_array_constructor;
              if codegenerror then
                begin
                   dec(parsing_para_level);
                   exit;
                end;
              p^.resulttype:=p^.left^.resulttype;
           end
         { if we know the routine which is called, then the type }
         { conversions are inserted                           }
         else
           begin
              { Do we need arrayconstructor -> set conversion, then insert
                it here before the arrayconstructor node breaks the tree
                with its conversions of enum->ord }
              if (p^.left^.treetype=arrayconstructn) and
                 (defcoll^.paratype.def^.deftype=setdef) then
                p^.left:=gentypeconvnode(p^.left,defcoll^.paratype.def);

              { set some settings needed for arrayconstructor }
              if is_array_constructor(p^.left^.resulttype) then
               begin
                 if is_array_of_const(defcoll^.paratype.def) then
                  begin
                    if assigned(aktcallprocsym) and
                       (pocall_cdecl in aktcallprocsym^.definition^.proccalloptions) and
                       (po_external in aktcallprocsym^.definition^.procoptions) then
                      p^.left^.cargs:=true;
                    { force variant array }
                    p^.left^.forcevaria:=true;
                  end
                 else
                  begin
                    p^.left^.novariaallowed:=true;
                    p^.left^.constructdef:=parraydef(defcoll^.paratype.def)^.elementtype.def;
                  end;
               end;

              if do_count then
               begin
                 { not completly proper, but avoids some warnings }
                 if (defcoll^.paratyp=vs_var) then
                   set_funcret_is_valid(p^.left);

                 { protected has nothing to do with read/write
                 if (defcoll^.paratyp=vs_var) then
                   test_protected(p^.left);
                 }
                 set_varstate(p^.left,defcoll^.paratyp<>vs_var);
                 { only process typeconvn and arrayconstructn, else it will
                   break other trees }
                 { But this is need to get correct varstate !! PM }
                 old_array_constructor:=allow_array_constructor;
                 old_get_para_resulttype:=get_para_resulttype;
                 allow_array_constructor:=true;
                 get_para_resulttype:=false;
                  if (p^.left^.treetype in [arrayconstructn,typeconvn]) then
                   firstpass(p^.left);
                 if not assigned(p^.resulttype) then
                   p^.resulttype:=p^.left^.resulttype;
                 get_para_resulttype:=old_get_para_resulttype;
                 allow_array_constructor:=old_array_constructor;
               end;
              { check if local proc/func is assigned to procvar }
              if p^.left^.resulttype^.deftype=procvardef then
                test_local_to_procvar(pprocvardef(p^.left^.resulttype),defcoll^.paratype.def);
              { property is not allowed as var parameter }
              if (defcoll^.paratyp=vs_var) and
                 (p^.left^.isproperty) then
                CGMessagePos(p^.left^.fileinfo,type_e_argument_cant_be_assigned);
              { generate the high() value tree }
              if push_high_param(defcoll^.paratype.def) then
                gen_high_tree(p,is_open_string(defcoll^.paratype.def));
              if not(is_shortstring(p^.left^.resulttype) and
                     is_shortstring(defcoll^.paratype.def)) and
                     (defcoll^.paratype.def^.deftype<>formaldef) then
                begin
                   if (defcoll^.paratyp=vs_var) and
                   { allows conversion from word to integer and
                     byte to shortint }
                     (not(
                        (p^.left^.resulttype^.deftype=orddef) and
                        (defcoll^.paratype.def^.deftype=orddef) and
                        (p^.left^.resulttype^.size=defcoll^.paratype.def^.size)
                         ) and
                   { an implicit pointer conversion is allowed }
                     not(
                        (p^.left^.resulttype^.deftype=pointerdef) and
                        (defcoll^.paratype.def^.deftype=pointerdef)
                         ) and
                   { child classes can be also passed }
                     not(
                        (p^.left^.resulttype^.deftype=objectdef) and
                        (defcoll^.paratype.def^.deftype=objectdef) and
                        pobjectdef(p^.left^.resulttype)^.is_related(pobjectdef(defcoll^.paratype.def))
                        ) and
                   { passing a single element to a openarray of the same type }
                     not(
                        (is_open_array(defcoll^.paratype.def) and
                        is_equal(parraydef(defcoll^.paratype.def)^.elementtype.def,p^.left^.resulttype))
                        ) and
                   { an implicit file conversion is also allowed }
                   { from a typed file to an untyped one           }
                     not(
                        (p^.left^.resulttype^.deftype=filedef) and
                        (defcoll^.paratype.def^.deftype=filedef) and
                        (pfiledef(defcoll^.paratype.def)^.filetyp = ft_untyped) and
                        (pfiledef(p^.left^.resulttype)^.filetyp = ft_typed)
                         ) and
                     not(is_equal(p^.left^.resulttype,defcoll^.paratype.def))) then
                       begin
                          CGMessagePos2(p^.left^.fileinfo,parser_e_call_by_ref_without_typeconv,
                            p^.left^.resulttype^.typename,defcoll^.paratype.def^.typename);
                       end;
                   { Process open parameters }
                   if push_high_param(defcoll^.paratype.def) then
                    begin
                      { insert type conv but hold the ranges of the array }
                      oldtype:=p^.left^.resulttype;
                      p^.left:=gentypeconvnode(p^.left,defcoll^.paratype.def);
                      firstpass(p^.left);
                      p^.left^.resulttype:=oldtype;
                    end
                   else
                    begin
                      p^.left:=gentypeconvnode(p^.left,defcoll^.paratype.def);
                      firstpass(p^.left);
                    end;
                   if codegenerror then
                     begin
                        dec(parsing_para_level);
                        exit;
                     end;
                end;
              { check var strings }
              if (cs_strict_var_strings in aktlocalswitches) and
                 is_shortstring(p^.left^.resulttype) and
                 is_shortstring(defcoll^.paratype.def) and
                 (defcoll^.paratyp=vs_var) and
                 not(is_open_string(defcoll^.paratype.def)) and
                 not(is_equal(p^.left^.resulttype,defcoll^.paratype.def)) then
                 begin
                    aktfilepos:=p^.left^.fileinfo;
                    CGMessage(type_e_strict_var_string_violation);
                 end;

              { Variablen for call by reference may not be copied }
              { into a register }
              { is this usefull here ? }
              { this was missing in formal parameter list   }
              if (defcoll^.paratype.def=pdef(cformaldef)) then
                begin
                  if defcoll^.paratyp=vs_var then
                    begin
                      if not valid_for_formal_var(p^.left) then
                        begin
                           aktfilepos:=p^.left^.fileinfo;
                           CGMessage(parser_e_illegal_parameter_list);
                        end;
                    end;
                  if defcoll^.paratyp=vs_const then
                    begin
                      if not valid_for_formal_const(p^.left) then
                        begin
                           aktfilepos:=p^.left^.fileinfo;
                           CGMessage(parser_e_illegal_parameter_list);
                        end;
                    end;
                end;

              if defcoll^.paratyp=vs_var then
                begin
                   set_unique(p^.left);
                   make_not_regable(p^.left);
                end;

              p^.resulttype:=defcoll^.paratype.def;
           end;
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         dec(parsing_para_level);
{$ifdef extdebug}
         if do_count then
           count_ref:=store_count_ref;
{$endif def extdebug}
      end;


{*****************************************************************************
                             FirstCallN
*****************************************************************************}

    procedure firstcalln(var p : ptree);
      type
         pprocdefcoll = ^tprocdefcoll;
         tprocdefcoll = record
            data      : pprocdef;
            nextpara  : pparaitem;
            firstpara : pparaitem;
            next      : pprocdefcoll;
         end;
      var
         hp,procs,hp2 : pprocdefcoll;
         pd : pprocdef;
         oldcallprocsym : pprocsym;
         nextprocsym : pprocsym;
         def_from,def_to,conv_to : pdef;
         hpt,pt,inlinecode : ptree;
         exactmatch,inlined : boolean;
         paralength,lastpara : longint;
         lastparatype : pdef;
         pdc : pparaitem;
{$ifdef TEST_PROCSYMS}
         symt : psymtable;
{$endif TEST_PROCSYMS}

         { only Dummy }
         hcvt : tconverttype;
         regi : tregister;
         method_must_be_valid : boolean;
      label
        errorexit;

      { check if the resulttype from tree p is equal with def, needed
        for stringconstn and formaldef }
      function is_equal(p:ptree;def:pdef) : boolean;

        begin
           { safety check }
           if not (assigned(def) or assigned(p^.resulttype)) then
            begin
              is_equal:=false;
              exit;
            end;
           { all types can be passed to a formaldef }
           is_equal:=(def^.deftype=formaldef) or
             (types.is_equal(p^.resulttype,def))
           { to support ansi/long/wide strings in a proper way }
           { string and string[10] are assumed as equal }
           { when searching the correct overloaded procedure   }
             or
             (
              (def^.deftype=stringdef) and (p^.resulttype^.deftype=stringdef) and
              (pstringdef(def)^.string_typ=pstringdef(p^.resulttype)^.string_typ)
             )
             or
             (
              (p^.left^.treetype=stringconstn) and
              (is_ansistring(p^.resulttype) and is_pchar(def))
             )
             or
             (
              (p^.left^.treetype=ordconstn) and
              (is_char(p^.resulttype) and (is_shortstring(def) or is_ansistring(def)))
             )
           { set can also be a not yet converted array constructor }
             or
             (
              (def^.deftype=setdef) and (p^.resulttype^.deftype=arraydef) and
              (parraydef(p^.resulttype)^.IsConstructor) and not(parraydef(p^.resulttype)^.IsVariant)
             )
           { in tp7 mode proc -> procvar is allowed }
             or
             (
              (m_tp_procvar in aktmodeswitches) and
              (def^.deftype=procvardef) and (p^.left^.treetype=calln) and
              (proc_to_procvar_equal(pprocdef(p^.left^.procdefinition),pprocvardef(def)))
             )
             ;
        end;

      function is_in_limit(def_from,def_to : pdef) : boolean;

        begin
           is_in_limit:=(def_from^.deftype = orddef) and
                        (def_to^.deftype = orddef) and
                        (porddef(def_from)^.low>porddef(def_to)^.low) and
                        (porddef(def_from)^.high<porddef(def_to)^.high);
        end;

      var
        is_const : boolean;
      begin
         { release registers! }
         { if procdefinition<>nil then we called firstpass already }
         { it seems to be bad because of the registers }
         { at least we can avoid the overloaded search !! }
         procs:=nil;
         { made this global for disposing !! }

         oldcallprocsym:=aktcallprocsym;
         aktcallprocsym:=nil;

         inlined:=false;
         if assigned(p^.procdefinition) and
            (pocall_inline in p^.procdefinition^.proccalloptions) then
           begin
              inlinecode:=p^.right;
              if assigned(inlinecode) then
                begin
                   inlined:=true;
{$ifdef INCLUDEOK}
                   exclude(p^.procdefinition^.proccalloptions,pocall_inline);
{$else}
                   p^.procdefinition^.proccalloptions:=p^.procdefinition^.proccalloptions-[pocall_inline];
{$endif}
                end;
              p^.right:=nil;
           end;
         if assigned(p^.procdefinition) and
            (po_containsself in p^.procdefinition^.procoptions) then
           message(cg_e_cannot_call_message_direct);

         { procedure variable ? }
         if assigned(p^.right) then
           begin
              { procedure does a call }
              procinfo^.flags:=procinfo^.flags or pi_do_call;

              { calc the correture value for the register }
{$ifdef i386}
              for regi:=R_EAX to R_EDI do
                inc(reg_pushes[regi],t_times*2);
{$endif}
{$ifdef m68k}
              for regi:=R_D0 to R_A6 do
                inc(reg_pushes[regi],t_times*2);
{$endif}
              { calculate the type of the parameters }
              if assigned(p^.left) then
                begin
                   firstcallparan(p^.left,nil,false);
                   if codegenerror then
                     goto errorexit;
                end;
              firstpass(p^.right);
              set_varstate(p^.right,true);

              { check the parameters }
              pdc:=pparaitem(pprocvardef(p^.right^.resulttype)^.para^.first);
              pt:=p^.left;
              while assigned(pdc) and assigned(pt) do
                begin
                   pt:=pt^.right;
                   pdc:=pparaitem(pdc^.next);
                end;
              if assigned(pt) or assigned(pdc) then
                begin
                   if assigned(pt) then
                     aktfilepos:=pt^.fileinfo;
                   CGMessage(parser_e_illegal_parameter_list);
                end;
              { insert type conversions }
              if assigned(p^.left) then
                begin
                   firstcallparan(p^.left,pparaitem(pprocvardef(p^.right^.resulttype)^.para^.first),true);
                   if codegenerror then
                     goto errorexit;
                end;
              p^.resulttype:=pprocvardef(p^.right^.resulttype)^.rettype.def;

              { this was missing, leads to a bug below if
                the procvar is a function }
              p^.procdefinition:=pabstractprocdef(p^.right^.resulttype);
           end
         else
         { not a procedure variable }
           begin
              { determine the type of the parameters }
              if assigned(p^.left) then
                begin
                   firstcallparan(p^.left,nil,false);
                   if codegenerror then
                     goto errorexit;
                end;

              aktcallprocsym:=pprocsym(p^.symtableprocentry);
              { do we know the procedure to call ? }
              if not(assigned(p^.procdefinition)) then
                begin
{$ifdef TEST_PROCSYMS}
                 if (p^.unit_specific) or
                    assigned(p^.methodpointer) then
                   nextprocsym:=nil
                 else while not assigned(procs) do
                  begin
                     symt:=p^.symtableproc;
                     srsym:=nil;
                     while assigned(symt^.next) and not assigned(srsym) do
                       begin
                          symt:=symt^.next;
                          getsymonlyin(symt,actprocsym^.name);
                          if assigned(srsym) then
                            if srsym^.typ<>procsym then
                              begin
                                 { reject all that is not a procedure }
                                 srsym:=nil;
                                 { don't search elsewhere }
                                 while assigned(symt^.next) do
                                   symt:=symt^.next;
                              end;
                       end;
                     nextprocsym:=srsym;
                  end;
{$else TEST_PROCSYMS}
                nextprocsym:=nil;
{$endif TEST_PROCSYMS}
                   { determine length of parameter list }
                   pt:=p^.left;
                   paralength:=0;
                   while assigned(pt) do
                     begin
                        inc(paralength);
                        pt:=pt^.right;
                     end;

                   { link all procedures which have the same # of parameters }
                   pd:=aktcallprocsym^.definition;
                   while assigned(pd) do
                     begin
                        { only when the # of parameter are equal }
                        if (pd^.para^.count=paralength) then
                          begin
                             new(hp);
                             hp^.data:=pd;
                             hp^.next:=procs;
                             hp^.nextpara:=pparaitem(pd^.para^.first);
                             hp^.firstpara:=pparaitem(pd^.para^.first);
                             procs:=hp;
                          end;
                        pd:=pd^.nextoverloaded;
                     end;

                   { no procedures found? then there is something wrong
                     with the parameter size }
                   if not assigned(procs) then
                    begin
                      { in tp mode we can try to convert to procvar if
                        there are no parameters specified }
                      if not(assigned(p^.left)) and
                         (m_tp_procvar in aktmodeswitches) then
                        begin
                          if (p^.symtableprocentry^.owner^.symtabletype=objectsymtable) and
                             (pobjectdef(p^.symtableprocentry^.owner^.defowner)^.is_class) then
                           hpt:=genloadmethodcallnode(pprocsym(p^.symtableprocentry),p^.symtableproc,
                                 getcopy(p^.methodpointer))
                          else
                           hpt:=genloadcallnode(pprocsym(p^.symtableprocentry),p^.symtableproc);
                          disposetree(p);
                          firstpass(hpt);
                          p:=hpt;
                        end
                      else
                        begin
                          if assigned(p^.left) then
                           aktfilepos:=p^.left^.fileinfo;
                          CGMessage(parser_e_wrong_parameter_size);
                          aktcallprocsym^.write_parameter_lists;
                        end;
                      goto errorexit;
                    end;

                { now we can compare parameter after parameter }
                   pt:=p^.left;
                   { we start with the last parameter }
                   lastpara:=paralength+1;
                   lastparatype:=nil;
                   while assigned(pt) do
                     begin
                        dec(lastpara);
                        { walk all procedures and determine how this parameter matches and set:
                           1. pt^.exact_match_found if one parameter has an exact match
                           2. exactmatch if an equal or exact match is found

                           3. para^.argconvtyp to exact,equal or convertable
                                (when convertable then also convertlevel is set)
                           4. pt^.convlevel1found if there is a convertlevel=1
                           5. pt^.convlevel2found if there is a convertlevel=2
                        }
                        exactmatch:=false;
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             if is_equal(pt,hp^.nextpara^.paratype.def) then
                               begin
                                  if hp^.nextpara^.paratype.def=pt^.resulttype then
                                    begin
                                       pt^.exact_match_found:=true;
                                       hp^.nextpara^.argconvtyp:=act_exact;
                                    end
                                  else
                                    hp^.nextpara^.argconvtyp:=act_equal;
                                  exactmatch:=true;
                               end
                             else
                               begin
                                 hp^.nextpara^.argconvtyp:=act_convertable;
                                 hp^.nextpara^.convertlevel:=isconvertable(pt^.resulttype,hp^.nextpara^.paratype.def,
                                     hcvt,pt^.left^.treetype,false);
                                 case hp^.nextpara^.convertlevel of
                                  1 : pt^.convlevel1found:=true;
                                  2 : pt^.convlevel2found:=true;
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
                                 if (hp^.nextpara^.argconvtyp<>act_convertable) then
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
                                 if (hp^.nextpara^.convertlevel<>0) then
                                  begin
                                    hp^.next:=procs;
                                    procs:=hp;
                                  end
                                 else
                                  begin
                                    { save the type for nice error message }
                                    lastparatype:=hp^.nextpara^.paratype.def;
                                    dispose(hp);
                                  end;
                                 hp:=hp2;
                              end;
                          end;
                        { update nextpara for all procedures }
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             hp^.nextpara:=pparaitem(hp^.nextpara^.next);
                             hp:=hp^.next;
                          end;
                        { load next parameter or quit loop if no procs left }
                        if assigned(procs) then
                          pt:=pt^.right
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
                         (not assigned(pt^.resulttype)) then
                        internalerror(39393)
                      else
                        begin
                          aktfilepos:=pt^.fileinfo;
                          CGMessage3(type_e_wrong_parameter_type,tostr(lastpara),
                            pt^.resulttype^.typename,lastparatype^.typename);
                        end;
                      aktcallprocsym^.write_parameter_lists;
                      goto errorexit;
                    end;

                   { if there are several choices left then for orddef }
                   { if a type is totally included in the other }
                   { we don't fear an overflow ,                       }
                   { so we can do as if it is an exact match       }
                   { this will convert integer to longint             }
                   { rather than to words                             }
                   { conversion of byte to integer or longint     }
                   {would still not be solved                     }
                   if assigned(procs) and assigned(procs^.next) then
                     begin
                        hp:=procs;
                        while assigned(hp) do
                          begin
                            hp^.nextpara:=hp^.firstpara;
                            hp:=hp^.next;
                          end;
                        pt:=p^.left;
                        while assigned(pt) do
                          begin
                             { matches a parameter of one procedure exact ? }
                             exactmatch:=false;
                             def_from:=pt^.resulttype;
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  if not is_equal(pt,hp^.nextpara^.paratype.def) then
                                    begin
                                       def_to:=hp^.nextpara^.paratype.def;
                                       if ((def_from^.deftype=orddef) and (def_to^.deftype=orddef)) and
                                         (is_in_limit(def_from,def_to) or
                                         ((hp^.nextpara^.paratyp=vs_var) and
                                         (def_from^.size=def_to^.size))) then
                                         begin
                                            exactmatch:=true;
                                            conv_to:=def_to;
                                         end;
                                    end;
                                  hp:=hp^.next;
                               end;

                             { .... if yes, del all the other procedures }
                             if exactmatch then
                               begin
                                  { the first .... }
                                  while (assigned(procs)) and not(is_in_limit(def_from,procs^.nextpara^.paratype.def)) do
                                    begin
                                       hp:=procs^.next;
                                       dispose(procs);
                                       procs:=hp;
                                    end;
                                  { and the others }
                                  hp:=procs;
                                  while (assigned(hp)) and assigned(hp^.next) do
                                    begin
                                       if not(is_in_limit(def_from,hp^.next^.nextpara^.paratype.def)) then
                                         begin
                                            hp2:=hp^.next^.next;
                                            dispose(hp^.next);
                                            hp^.next:=hp2;
                                         end
                                       else
                                         begin
                                           def_to:=hp^.next^.nextpara^.paratype.def;
                                           if (conv_to^.size>def_to^.size) or
                                              ((porddef(conv_to)^.low<porddef(def_to)^.low) and
                                              (porddef(conv_to)^.high>porddef(def_to)^.high)) then
                                             begin
                                                hp2:=procs;
                                                procs:=hp;
                                                conv_to:=def_to;
                                                dispose(hp2);
                                             end
                                           else
                                             hp:=hp^.next;
                                         end;
                                    end;
                               end;
                             { update nextpara for all procedures }
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  hp^.nextpara:=pparaitem(hp^.nextpara^.next);
                                  hp:=hp^.next;
                               end;
                             pt:=pt^.right;
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

                        pt:=p^.left;
                        while assigned(pt) do
                          begin
                             if pt^.exact_match_found then
                               begin
                                 hp:=procs;
                                 procs:=nil;
                                 while assigned(hp) do
                                   begin
                                      hp2:=hp^.next;
                                      { keep the exact matches, dispose the others }
                                      if (hp^.nextpara^.argconvtyp=act_exact) then
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
                                  hp^.nextpara:=pparaitem(hp^.nextpara^.next);
                                  hp:=hp^.next;
                               end;
                             pt:=pt^.right;
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

                        pt:=p^.left;
                        while assigned(pt) do
                          begin
                             if pt^.convlevel1found and pt^.convlevel2found then
                               begin
                                 hp:=procs;
                                 procs:=nil;
                                 while assigned(hp) do
                                   begin
                                      hp2:=hp^.next;
                                      { keep all not act_convertable and all convertlevels=1 }
                                      if (hp^.nextpara^.argconvtyp<>act_convertable) or
                                         (hp^.nextpara^.convertlevel=1) then
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
                                  hp^.nextpara:=pparaitem(hp^.nextpara^.next);
                                  hp:=hp^.next;
                               end;
                             pt:=pt^.right;
                          end;
                     end;

                   if not(assigned(procs)) or assigned(procs^.next) then
                     begin
                        CGMessage(cg_e_cant_choose_overload_function);
                        aktcallprocsym^.write_parameter_lists;
                        goto errorexit;
                     end;
{$ifdef TEST_PROCSYMS}
                   if (procs=nil) and assigned(nextprocsym) then
                     begin
                        p^.symtableprocentry:=nextprocsym;
                        p^.symtableproc:=symt;
                     end;
                 end ; { of while assigned(p^.symtableprocentry) do }
{$endif TEST_PROCSYMS}
                   if make_ref then
                     begin
                        procs^.data^.lastref:=new(pref,init(procs^.data^.lastref,@p^.fileinfo));
                        inc(procs^.data^.refcount);
                        if procs^.data^.defref=nil then
                          procs^.data^.defref:=procs^.data^.lastref;
                     end;

                   p^.procdefinition:=procs^.data;
                   p^.resulttype:=procs^.data^.rettype.def;
                   { big error for with statements
                   p^.symtableproc:=p^.procdefinition^.owner;
                   but neede for overloaded operators !! }
                   if p^.symtableproc=nil then
                     p^.symtableproc:=p^.procdefinition^.owner;

                   p^.location.loc:=LOC_MEM;
{$ifdef CHAINPROCSYMS}
                   { object with method read;
                     call to read(x) will be a usual procedure call }
                   if assigned(p^.methodpointer) and
                     (p^.procdefinition^._class=nil) then
                     begin
                        { not ok for extended }
                        case p^.methodpointer^.treetype of
                           typen,hnewn : fatalerror(no_para_match);
                        end;
                        disposetree(p^.methodpointer);
                        p^.methodpointer:=nil;
                     end;
{$endif CHAINPROCSYMS}
               end; { end of procedure to call determination }

              is_const:=(pocall_internconst in p^.procdefinition^.proccalloptions) and
                        ((block_type=bt_const) or
                         (assigned(p^.left) and (p^.left^.left^.treetype in [realconstn,ordconstn])));
              { handle predefined procedures }
              if (pocall_internproc in p^.procdefinition^.proccalloptions) or is_const then
                begin
                   if assigned(p^.left) then
                     begin
                     { settextbuf needs two args }
                       if assigned(p^.left^.right) then
                         pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,is_const,p^.left)
                       else
                         begin
                           pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,is_const,p^.left^.left);
                           putnode(p^.left);
                         end;
                     end
                   else
                     begin
                       pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,is_const,nil);
                     end;
                   putnode(p);
                   firstpass(pt);
                   p:=pt;
                   goto errorexit;
                end
              else
                { no intern procedure => we do a call }
              { calc the correture value for the register }
              { handle predefined procedures }
              if (pocall_inline in p^.procdefinition^.proccalloptions) then
                begin
                   if assigned(p^.methodpointer) then
                     CGMessage(cg_e_unable_inline_object_methods);
                   if assigned(p^.right) and (p^.right^.treetype<>procinlinen) then
                     CGMessage(cg_e_unable_inline_procvar);
                   { p^.treetype:=procinlinen; }
                   if not assigned(p^.right) then
                     begin
                        if assigned(pprocdef(p^.procdefinition)^.code) then
                          inlinecode:=genprocinlinenode(p,ptree(pprocdef(p^.procdefinition)^.code))
                        else
                          CGMessage(cg_e_no_code_for_inline_stored);
                        if assigned(inlinecode) then
                          begin
                             { consider it has not inlined if called
                               again inside the args }
{$ifdef INCLUDEOK}
                             exclude(p^.procdefinition^.proccalloptions,pocall_inline);
{$else}
                             p^.procdefinition^.proccalloptions:=p^.procdefinition^.proccalloptions-[pocall_inline];
{$endif}
                             firstpass(inlinecode);
                             inlined:=true;
                          end;
                     end;
                end
              else
                procinfo^.flags:=procinfo^.flags or pi_do_call;

              if (po_interrupt in p^.procdefinition^.procoptions) then
                CGmessage1(cg_e_no_call_to_interrupt,p^.symtableprocentry^.name);
              { work trough all parameters to insert the type conversions }
              { !!! done now after internproc !! (PM) }
              if assigned(p^.left) then
                begin
                   firstcallparan(p^.left,pparaitem(p^.procdefinition^.para^.first),true);
                end;
{$ifdef i386}
              for regi:=R_EAX to R_EDI do
                begin
                   if (pprocdef(p^.procdefinition)^.usedregisters and ($80 shr word(regi)))<>0 then
                     inc(reg_pushes[regi],t_times*2);
                end;
{$endif}
{$ifdef m68k}
             for regi:=R_D0 to R_A6 do
               begin
                  if (pprocdef(p^.procdefinition)^.usedregisters and ($800 shr word(regi)))<>0 then
                    inc(reg_pushes[regi],t_times*2);
               end;
{$endif}
           end;
         { ensure that the result type is set }
         p^.resulttype:=p^.procdefinition^.rettype.def;
         { get a register for the return value }
         if (p^.resulttype<>pdef(voiddef)) then
           begin
              if (p^.procdefinition^.proctypeoption=potype_constructor) then
                begin
                   { extra handling of classes }
                   { p^.methodpointer should be assigned! }
                   if assigned(p^.methodpointer) and assigned(p^.methodpointer^.resulttype) and
                     (p^.methodpointer^.resulttype^.deftype=classrefdef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        p^.registers32:=1;
                        { the result type depends on the classref }
                        p^.resulttype:=pclassrefdef(p^.methodpointer^.resulttype)^.pointertype.def;
                     end
                  { a object constructor returns the result with the flags }
                   else
                     p^.location.loc:=LOC_FLAGS;
                end
              else
                begin
{$ifdef SUPPORT_MMX}
                   if (cs_mmx in aktlocalswitches) and
                     is_mmx_able_array(p^.resulttype) then
                     begin
                        p^.location.loc:=LOC_MMXREGISTER;
                        p^.registersmmx:=1;
                     end
                   else
{$endif SUPPORT_MMX}
                   if ret_in_acc(p^.resulttype) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        if is_64bitint(p^.resulttype) then
                          p^.registers32:=2
                        else
                          p^.registers32:=1;

                        { wide- and ansistrings are returned in EAX    }
                        { but they are imm. moved to a memory location }
                        if is_widestring(p^.resulttype) or
                          is_ansistring(p^.resulttype) then
                          begin
                             p^.location.loc:=LOC_MEM;
                             { this is wrong we still need one register  PM
                             p^.registers32:=0; }
                             { we use ansistrings so no fast exit here }
                             procinfo^.no_fast_exit:=true;
                             p^.registers32:=1;
                          end;
                     end
                   else if (p^.resulttype^.deftype=floatdef) then
                     begin
                        p^.location.loc:=LOC_FPU;
                        p^.registersfpu:=1;
                     end
                   else
                     p^.location.loc:=LOC_MEM;
                end;
           end;
         { a fpu can be used in any procedure !! }
         p^.registersfpu:=p^.procdefinition^.fpu_used;
         { if this is a call to a method calc the registers }
         if (p^.methodpointer<>nil) then
           begin
              case p^.methodpointer^.treetype of
                { but only, if this is not a supporting node }
                typen: ;
                { we need one register for new return value PM }
                hnewn : if p^.registers32=0 then
                          p^.registers32:=1;
                else
                  begin
                     if (p^.procdefinition^.proctypeoption in [potype_constructor,potype_destructor]) and
                        assigned(p^.symtable) and (p^.symtable^.symtabletype=withsymtable) and
                        not pwithsymtable(p^.symtable)^.direct_with then
                       begin
                          CGmessage(cg_e_cannot_call_cons_dest_inside_with);
                       end; { Is accepted by Delphi !! }
                     { this is not a good reason to accept it in FPC if we produce
                       wrong code for it !!! (PM) }

                     { R.Assign is not a constructor !!! }
                     { but for R^.Assign, R must be valid !! }
                     if (p^.procdefinition^.proctypeoption=potype_constructor) or
                        ((p^.methodpointer^.treetype=loadn) and
                        (not(oo_has_virtual in pobjectdef(p^.methodpointer^.resulttype)^.objectoptions))) then
                       method_must_be_valid:=false
                     else
                       method_must_be_valid:=true;
                     firstpass(p^.methodpointer);
                     set_varstate(p^.methodpointer,method_must_be_valid);
                     { The object is already used ven if it is called once }
                     if (p^.methodpointer^.treetype=loadn) and
                        (p^.methodpointer^.symtableentry^.typ=varsym) then
                       pvarsym(p^.methodpointer^.symtableentry)^.varstate:=vs_used;

                     p^.registersfpu:=max(p^.methodpointer^.registersfpu,p^.registersfpu);
                     p^.registers32:=max(p^.methodpointer^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
                     p^.registersmmx:=max(p^.methodpointer^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
                  end;
              end;
           end;

         if inlined then
           p^.right:=inlinecode;
         { determine the registers of the procedure variable }
         { is this OK for inlined procs also ?? (PM)     }
         if assigned(p^.right) then
           begin
              p^.registersfpu:=max(p^.right^.registersfpu,p^.registersfpu);
              p^.registers32:=max(p^.right^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.right^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { determine the registers of the procedure }
         if assigned(p^.left) then
           begin
              p^.registersfpu:=max(p^.left^.registersfpu,p^.registersfpu);
              p^.registers32:=max(p^.left^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.left^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      errorexit:
         { Reset some settings back }
         if assigned(procs) then
           dispose(procs);
         if inlined then
{$ifdef INCLUDEOK}
           include(p^.procdefinition^.proccalloptions,pocall_inline);
{$else}
           p^.procdefinition^.proccalloptions:=p^.procdefinition^.proccalloptions+[pocall_inline];
{$endif}
         aktcallprocsym:=oldcallprocsym;
      end;


{*****************************************************************************
                             FirstProcInlineN
*****************************************************************************}

    procedure firstprocinline(var p : ptree);
      begin
        { left contains the code in tree form }
        { but it has already been firstpassed }
        { so firstpass(p^.left); does not seem required }
        { might be required later if we change the arg handling !! }
      end;

end.
{
  $Log$
  Revision 1.76  1999-12-19 15:13:56  peter
    * constant array type conversion fixed

  Revision 1.75  1999/12/09 23:18:04  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.74  1999/11/30 10:40:57  peter
    + ttype, tsymlist

  Revision 1.73  1999/11/18 15:34:49  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.72  1999/11/17 17:05:07  pierre
   * Notes/hints changes

  Revision 1.71  1999/11/06 14:34:29  peter
    * truncated log to 20 revs

  Revision 1.70  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.69  1999/10/22 14:37:30  peter
    * error when properties are passed to var parameters

  Revision 1.68  1999/10/13 10:35:27  peter
    * var must match exactly error msg extended with got and expected type
    * array constructor type check now gives error on wrong types

  Revision 1.67  1999/10/12 15:50:54  pierre
   * error if calling interrupt procedure

  Revision 1.66  1999/09/27 23:45:00  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.65  1999/09/16 23:05:56  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.64  1999/09/14 07:59:48  florian
    * finally!? fixed
         with <function with result in temp> do
      My last and also Peter's fix before were wrong :(

  Revision 1.63  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo^.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.62  1999/08/23 23:42:52  pierre
   * hnewn reg allocation corrected

  Revision 1.61  1999/08/17 13:26:08  peter
    * arrayconstructor -> arrayofconst fixed when arraycosntructor was not
      variant.

  Revision 1.60  1999/08/16 23:23:39  peter
    * arrayconstructor -> openarray type conversions for element types

  Revision 1.59  1999/08/13 21:33:16  peter
    * support for array constructors extended and more error checking

  Revision 1.58  1999/08/12 14:34:28  peter
    * tp_procvar mode call->loadn fixed

  Revision 1.57  1999/08/05 16:53:19  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.56  1999/08/04 00:23:34  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.55  1999/08/03 22:03:27  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.54  1999/07/01 21:33:58  peter
    * merged

  Revision 1.53  1999/06/29 14:02:33  peter
    * merged file pos fixes

  Revision 1.52  1999/06/29 12:16:22  pierre
   * mereg from fixes-0_99_12

  Revision 1.51.2.3  1999/07/01 21:32:01  peter
    * procvar fixes again

}
