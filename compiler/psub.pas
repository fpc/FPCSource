{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Daniel Mantione

    Does the parsing and codegeneration at subroutine level

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
unit psub;

{$i defines.inc}
{$ifdef powerpc}
  {$define newcg}
{$endif powerpc}

interface

    procedure compile_proc_body(make_global,parent_has_class:boolean);

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;


implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,verbose,comphook,
       systems,
       { aasm }
       cpubase,cpuinfo,aasm,
       { symtable }
       symconst,symbase,symdef,symsym,symtype,symtable,types,
       ppu,fmodule,
       { pass 1 }
       node,
       nbas,
       { pass 2 }
{$ifndef NOPASS2}
       pass_1,pass_2,
{$endif}
       { parser }
       scanner,
       pbase,pstatmnt,pdecl,pdecsub,pexports,
       { codegen }
       tgobj,cgbase,rgobj,
       rgcpu,
       cga
       {$ifndef NOOPT}
         {$ifdef i386}
           ,aopt386
         {$else i386}
           ,aoptcpu
         {$endif i386}
       {$endif}
       ;


{****************************************************************************
                      PROCEDURE/FUNCTION BODY PARSING
****************************************************************************}

    function block(islibrary : boolean) : tnode;
      var
         storepos : tfileposinfo;
      begin
         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
          include(aktprocdef.procoptions,po_assembler);

         { Handle assembler block different }
         if (po_assembler in aktprocdef.procoptions) then
          begin
            read_declarations(false);
            block:=assembler_block;
            exit;
          end;

         if not is_void(aktprocdef.rettype.def) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              storepos:=akttokenpos;
              akttokenpos:=aktprocsym.fileinfo;
              aktprocdef.funcretsym:=tfuncretsym.create(aktprocsym.name,aktprocdef.rettype);
              { insert in local symtable }
              symtablestack.insert(aktprocdef.funcretsym);
              akttokenpos:=storepos;
              if ret_in_acc(aktprocdef.rettype.def) or
                 (aktprocdef.rettype.def.deftype=floatdef) then
                procinfo^.return_offset:=-tfuncretsym(aktprocdef.funcretsym).address;
              { insert result also if support is on }
              if (m_result in aktmodeswitches) then
               begin
                 aktprocdef.resultfuncretsym:=tfuncretsym.create('RESULT',aktprocdef.rettype);
                 symtablestack.insert(aktprocdef.resultfuncretsym);
               end;
           end;
         read_declarations(islibrary);

         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack.symtabletype=localsymtable) then
           procinfo^.firsttemp_offset := -symtablestack.datasize
         else
           procinfo^.firsttemp_offset := 0;

         { space for the return value }
         { !!!!!   this means that we can not set the return value
         in a subfunction !!!!! }
         { because we don't know yet where the address is }
         if not is_void(aktprocdef.rettype.def) then
           begin
              if ret_in_acc(aktprocdef.rettype.def) or (aktprocdef.rettype.def.deftype=floatdef) then
                begin
                   { the space has been set in the local symtable }
                   procinfo^.return_offset:=-tfuncretsym(aktprocdef.funcretsym).address;
                   if ((procinfo^.flags and pi_operator)<>0) and
                      assigned(otsym) then
                     otsym.address:=-procinfo^.return_offset;
                   { eax is modified by a function }
                   include(rg.usedinproc,accumulator);
                   if (sizeof(aword) < 8) and
                      (is_64bitint(aktprocdef.rettype.def)) then
                     include(rg.usedinproc,accumulatorhigh);
                end;
           end;

         {Unit initialization?.}
         if (lexlevel=unit_init_level) and (current_module.is_unit)
            or islibrary then
           begin
             if (token=_END) then
                begin
                   consume(_END);
                   { We need at least a node, else the entry/exit code is not
                     generated and thus no PASCALMAIN symbol which we need (PFV) }
                   if islibrary then
                    block:=cnothingnode.create
                   else
                    block:=nil;
                end
              else
                begin
                   if token=_INITIALIZATION then
                     begin
                        current_module.flags:=current_module.flags or uf_init;
                        block:=statement_block(_INITIALIZATION);
                     end
                   else if (token=_FINALIZATION) then
                     begin
                        if (current_module.flags and uf_finalize)<>0 then
                          block:=statement_block(_FINALIZATION)
                        else
                          begin
                          { can we allow no INITIALIZATION for DLL ??
                            I think it should work PM }
                             block:=nil;
                             exit;
                          end;
                     end
                   else
                     begin
                        current_module.flags:=current_module.flags or uf_init;
                        block:=statement_block(_BEGIN);
                     end;
                end;
            end
         else
            block:=statement_block(_BEGIN);
      end;


{****************************************************************************
                       PROCEDURE/FUNCTION COMPILING
****************************************************************************}

    procedure compile_proc_body(make_global,parent_has_class:boolean);
      {
        Compile the body of a procedure
      }
      var
         oldexitlabel,oldexit2label : tasmlabel;
         oldfaillabel,oldquickexitlabel:tasmlabel;
         _class,hp:tobjectdef;
         { switches can change inside the procedure }
         entryswitches, exitswitches : tlocalswitches;
         oldaktmaxfpuregisters,localmaxfpuregisters : longint;
         { code for the subroutine as tree }
         code:tnode;
         { size of the local strackframe }
         stackframe:longint;
         { true when no stackframe is required }
         nostackframe:boolean;
         { number of bytes which have to be cleared by RET }
         parasize:longint;
         { filepositions }
         entrypos,
         savepos,
         exitpos   : tfileposinfo;
      begin
         { calculate the lexical level }
         inc(lexlevel);
         if lexlevel>32 then
          Message(parser_e_too_much_lexlevel);

         { static is also important for local procedures !! }
         if (po_staticmethod in aktprocdef.procoptions) then
           allow_only_static:=true
         else if (lexlevel=normal_function_level) then
           allow_only_static:=false;

         { save old labels }
         oldexitlabel:=aktexitlabel;
         oldexit2label:=aktexit2label;
         oldquickexitlabel:=quickexitlabel;
         oldfaillabel:=faillabel;
         { get new labels }
         getlabel(aktexitlabel);
         getlabel(aktexit2label);
         { exit for fail in constructors }
         if (aktprocdef.proctypeoption=potype_constructor) then
           begin
             getlabel(faillabel);
             getlabel(quickexitlabel);
           end;
         { reset break and continue labels }
         block_type:=bt_general;
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;

         { insert symtables for the class, by only if it is no nested function }
         if assigned(procinfo^._class) and not(parent_has_class) then
           begin
             { insert them in the reverse order ! }
             hp:=nil;
             repeat
               _class:=procinfo^._class;
               while _class.childof<>hp do
                 _class:=_class.childof;
               hp:=_class;
               _class.symtable.next:=symtablestack;
               symtablestack:=_class.symtable;
             until hp=procinfo^._class;
           end;

         { insert parasymtable in symtablestack}
         { only if lexlevel > 1 !!! global symtable should be right after staticsymtazble
           for checking of same names used in interface and implementation !! }
         if lexlevel>=normal_function_level then
           begin
              aktprocdef.parast.next:=symtablestack;
              symtablestack:=aktprocdef.parast;
              symtablestack.symtablelevel:=lexlevel;
           end;
         { insert localsymtable in symtablestack}
         aktprocdef.localst.next:=symtablestack;
         symtablestack:=aktprocdef.localst;
         symtablestack.symtablelevel:=lexlevel;
         { constant symbols are inserted in this symboltable }
         constsymtable:=symtablestack;

         { reset the temporary memory }
         rg.cleartempgen;

         rg.usedinproc:=[];
         { save entry info }
         entrypos:=aktfilepos;
         entryswitches:=aktlocalswitches;
         localmaxfpuregisters:=aktmaxfpuregisters;
         { parse the code ... }
         code:=block(current_module.islibrary);
         { get a better entry point }
         if assigned(code) then
           entrypos:=code.fileinfo;
         { save exit info }
         exitswitches:=aktlocalswitches;
         exitpos:=last_endtoken_filepos;
         { save current filepos }
         savepos:=aktfilepos;

         {When we are called to compile the body of a unit, aktprocsym should
          point to the unit initialization. If the unit has no initialization,
          aktprocsym=nil. But in that case code=nil. hus we should check for
          code=nil, when we use aktprocsym.}

         { set the start offset to the start of the temp area in the stack }
         tg.setfirsttemp(procinfo^.firsttemp_offset);

         { ... and generate assembler }
         { but set the right switches for entry !! }
         aktlocalswitches:=entryswitches;
         oldaktmaxfpuregisters:=aktmaxfpuregisters;
         aktmaxfpuregisters:=localmaxfpuregisters;
         if assigned(code) then
          begin
            { the procedure is now defined }
            aktprocdef.forwarddef:=false;

             { only generate the code if no type errors are found, else
               finish at least the type checking pass }
{$ifndef NOPASS2}
            if (status.errorcount=0) then
              begin
                generatecode(code);
                aktprocdef.code:=code;
                stackframe:=tg.gettempsize;

                { first generate entry code with the correct position and switches }
                aktfilepos:=entrypos;
                aktlocalswitches:=entryswitches;
{$ifdef newcg}
                cg^.g_entrycode(procinfo^.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$else newcg}
                genentrycode(procinfo^.aktentrycode,make_global,stackframe,parasize,nostackframe,false);
{$endif newcg}

                { FPC_POPADDRSTACK destroys all registers (JM) }
                if (procinfo^.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0 then
                 begin
                   rg.usedinproc := ALL_REGISTERS;
                 end;

                { now generate exit code with the correct position and switches }
                aktfilepos:=exitpos;
                aktlocalswitches:=exitswitches;
{$ifdef newcg}
                cg^.g_exitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$else newcg}
                genexitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$endif newcg}

                { now all the registers used are known }
                aktprocdef.usedregisters:=rg.usedinproc;
                procinfo^.aktproccode.insertlist(procinfo^.aktentrycode);
                procinfo^.aktproccode.concatlist(procinfo^.aktexitcode);
{$ifdef i386}
   {$ifndef NoOpt}
                if (cs_optimize in aktglobalswitches) and
                { do not optimize pure assembler procedures }
                   ((procinfo^.flags and pi_is_assembler)=0)  then
                  Optimize(procinfo^.aktproccode);
   {$endif NoOpt}
{$endif i386}
                { save local data (casetable) also in the same file }
                if assigned(procinfo^.aktlocaldata) and
                   (not procinfo^.aktlocaldata.empty) then
                 begin
                   procinfo^.aktproccode.concat(Tai_section.Create(sec_data));
                   procinfo^.aktproccode.concatlist(procinfo^.aktlocaldata);
                   procinfo^.aktproccode.concat(Tai_section.Create(sec_code));
                end;

                { add the procedure to the codesegment }
                if (cs_create_smart in aktmoduleswitches) then
                 codeSegment.concat(Tai_cut.Create);
                codeSegment.concatlist(procinfo^.aktproccode);
              end
            else
              do_resulttypepass(code);
{$else NOPASS2}
            do_resulttypepass(code);
{$endif NOPASS2}
          end;

         { ... remove symbol tables }
         if lexlevel>=normal_function_level then
           symtablestack:=symtablestack.next.next
         else
           symtablestack:=symtablestack.next;

         { ... check for unused symbols      }
         { but only if there is no asm block }
         if assigned(code) then
           begin
             if (Errorcount=0) then
               begin
                 { check if forwards are resolved }
                 tstoredsymtable(aktprocdef.localst).check_forwards;
                 { check if all labels are used }
                 tstoredsymtable(aktprocdef.localst).checklabels;
                 { remove cross unit overloads }
                 tstoredsymtable(aktprocdef.localst).unchain_overloaded;
               end;
             if (procinfo^.flags and pi_uses_asm)=0 then
               begin
                  { not for unit init, becuase the var can be used in finalize,
                    it will be done in proc_unit }
                  if not(aktprocdef.proctypeoption
                     in [potype_proginit,potype_unitinit,potype_unitfinalize]) then
                     tstoredsymtable(aktprocdef.localst).allsymbolsused;
                  tstoredsymtable(aktprocdef.parast).allsymbolsused;
               end;
           end;

         { the local symtables can be deleted, but the parast   }
         { doesn't, (checking definitons when calling a        }
         { function                                        }
         { not for a inline procedure !!               (PM)   }
         { at lexlevel = 1 localst is the staticsymtable itself }
         { so no dispose here !!                              }
         if assigned(code) and
            not(cs_browser in aktmoduleswitches) and
            (aktprocdef.proccalloption<>pocall_inline) then
           begin
             if lexlevel>=normal_function_level then
               aktprocdef.localst.free;
             aktprocdef.localst:=nil;
           end;

         { all registers can be used again }
         rg.resetusableregisters;
         { only now we can remove the temps }
         tg.resettempgen;

         { remove code tree, if not inline procedure }
         if assigned(code) and (aktprocdef.proccalloption<>pocall_inline) then
           code.free;

         { remove class member symbol tables }
         while symtablestack.symtabletype=objectsymtable do
           symtablestack:=symtablestack.next;

         aktmaxfpuregisters:=oldaktmaxfpuregisters;

         { restore filepos, the switches are already set }
         aktfilepos:=savepos;
         { restore labels }
         aktexitlabel:=oldexitlabel;
         aktexit2label:=oldexit2label;
         quickexitlabel:=oldquickexitlabel;
         faillabel:=oldfaillabel;

         { reset to normal non static function }
         if (lexlevel=normal_function_level) then
           allow_only_static:=false;
         { previous lexlevel }
         dec(lexlevel);
      end;


{****************************************************************************
                        PROCEDURE/FUNCTION PARSING
****************************************************************************}

    procedure checkvaluepara(p:tnamedindexitem);
      var
        vs : tvarsym;
        s  : string;
      begin
        with tvarsym(p) do
         begin
           if copy(name,1,3)='val' then
            begin
              s:=Copy(name,4,255);
              if not(po_assembler in aktprocdef.procoptions) then
               begin
                 vs:=tvarsym.create(s,vartype);
                 vs.fileinfo:=fileinfo;
                 vs.varspez:=varspez;
                 aktprocdef.localst.insert(vs);
                 include(vs.varoptions,vo_is_local_copy);
                 vs.varstate:=vs_assigned;
                 localvarsym:=vs;
                 inc(refs); { the para was used to set the local copy ! }
                 { warnings only on local copy ! }
                 varstate:=vs_used;
               end
              else
               begin
                 aktprocdef.parast.rename(name,s);
               end;
            end;
         end;
      end;


    procedure read_proc;
      {
        Parses the procedure directives, then parses the procedure body, then
        generates the code for it
      }
      var
        oldprefix        : string;
        oldprocsym       : tprocsym;
        oldprocdef       : tprocdef;
        oldprocinfo      : pprocinfo;
        oldconstsymtable : tsymtable;
        oldfilepos       : tfileposinfo;
        pdflags          : word;
      begin
      { save old state }
         oldprocdef:=aktprocdef;
         oldprocsym:=aktprocsym;
         oldprefix:=procprefix;
         oldconstsymtable:=constsymtable;
         oldprocinfo:=procinfo;
      { create a new procedure }
         codegen_newprocedure;
         with procinfo^ do
          begin
            parent:=oldprocinfo;
          { clear flags }
            flags:=0;
          { standard frame pointer }
            framepointer:=frame_pointer;
          { is this a nested function of a method ? }
            if assigned(oldprocinfo) then
              _class:=oldprocinfo^._class;
          end;

         parse_proc_dec;

         procinfo^.procdef:=aktprocdef;

         { set the default function options }
         if parse_only then
          begin
            aktprocdef.forwarddef:=true;
            { set also the interface flag, for better error message when the
              implementation doesn't much this header }
            aktprocdef.interfacedef:=true;
            pdflags:=pd_interface;
          end
         else
          begin
            pdflags:=pd_body;
            if current_module.in_implementation then
             pdflags:=pdflags or pd_implemen;
            if (not current_module.is_unit) or (cs_create_smart in aktmoduleswitches) then
             pdflags:=pdflags or pd_global;
            procinfo^.exported:=false;
            aktprocdef.forwarddef:=false;
          end;

         { parse the directives that may follow }
         inc(lexlevel);
         parse_proc_directives(pdflags);
         dec(lexlevel);

         { hint directives, these can be separated by semicolons here,
           that need to be handled here with a loop (PFV) }
         while try_consume_hintdirective(aktprocsym.symoptions) do
          Consume(_SEMICOLON);

         { set aktfilepos to the beginning of the function declaration }
         oldfilepos:=aktfilepos;
         aktfilepos:=aktprocdef.fileinfo;

         { For varargs directive also cdecl and external must be defined }
         if (po_varargs in aktprocdef.procoptions) then
          begin
            { check first for external in the interface, if available there
              then the cdecl must also be there since there is no implementation
              available to contain it }
            if parse_only then
             begin
               { if external is available, then cdecl must also be available }
               if (po_external in aktprocdef.procoptions) and
                  not(aktprocdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
             end
            else
             begin
               { both must be defined now }
               if not(po_external in aktprocdef.procoptions) or
                  not(aktprocdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
             end;
          end;

         { search for forward declarations }
         if not proc_add_definition(aktprocsym,aktprocdef) then
           begin
             { A method must be forward defined (in the object declaration) }
             if assigned(procinfo^._class) and
                (not assigned(oldprocinfo^._class)) then
              begin
                Message1(parser_e_header_dont_match_any_member,aktprocdef.fullprocname);
                aktprocsym.write_parameter_lists(aktprocdef);
              end
             else
              begin
                { Give a better error if there is a forward def in the interface and only
                  a single implementation }
                if (not aktprocdef.forwarddef) and
                   assigned(aktprocsym.defs^.next) and
                   aktprocsym.defs^.def.forwarddef and
                   aktprocsym.defs^.def.interfacedef and
                   not(assigned(aktprocsym.defs^.next^.next)) then
                 begin
                   Message1(parser_e_header_dont_match_forward,aktprocdef.fullprocname);
                   aktprocsym.write_parameter_lists(aktprocdef);
                 end
                else
                 begin
                   { check the global flag, for delphi this is not
                     required }
                   if not(m_delphi in aktmodeswitches) and
                      ((procinfo^.flags and pi_is_global)<>0) then
                     Message(parser_e_overloaded_must_be_all_global);
                 end;
              end;
           end;

         { update procinfo, because the aktprocdef can be
           changed by check_identical_proc (PFV) }
         procinfo^.procdef:=aktprocdef;

{$ifdef i386}
         { add implicit pushes for interrupt routines }
         if (po_interrupt in aktprocdef.procoptions) then
           begin
             { we push Flags and CS as long
               to cope with the IRETD
               and we save 6 register + 4 selectors }
             inc(procinfo^.para_offset,8+6*4+4*2);
           end;
{$endif i386}

         { pointer to the return value ? }
         if ret_in_param(aktprocdef.rettype.def) then
          begin
            procinfo^.return_offset:=procinfo^.para_offset;
            inc(procinfo^.para_offset,pointer_size);
          end;
         { allows to access the parameters of main functions in nested functions }
         aktprocdef.parast.address_fixup:=procinfo^.para_offset;

         { when it is a value para and it needs a local copy then rename
           the parameter and insert a copy in the localst. This is not done
           for assembler procedures }
         if (not parse_only) and (not aktprocdef.forwarddef) then
           aktprocdef.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}checkvaluepara);

         { restore file pos }
         aktfilepos:=oldfilepos;

         { compile procedure when a body is needed }
         if (pdflags and pd_body)<>0 then
           begin
             Message1(parser_p_procedure_start,
                      aktprocdef.fullprocname);
             aktprocdef.aliasnames.insert(aktprocdef.mangledname);
            { set _FAIL as keyword if constructor }
            if (aktprocdef.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=m_all;
            if assigned(aktprocdef._class) then
              tokeninfo^[_SELF].keyword:=m_all;

             compile_proc_body(((pdflags and pd_global)<>0),assigned(oldprocinfo^._class));

            { reset _FAIL as normal }
            if (aktprocdef.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=m_none;
            if assigned(aktprocdef._class) and (lexlevel=main_program_level) then
              tokeninfo^[_SELF].keyword:=m_none;
             consume(_SEMICOLON);
           end;
         { close }
         codegen_doneprocedure;
         { Restore old state }
         constsymtable:=oldconstsymtable;
         { from now on all refernece to mangledname means
           that the function is already used }
         aktprocdef.count:=true;
{$ifdef notused}
         { restore the interface order to maintain CRC values PM }
         if assigned(prevdef) and assigned(aktprocdef.nextoverloaded) then
           begin
             stdef:=aktprocdef;
             aktprocdef:=stdef.nextoverloaded;
             stdef.nextoverloaded:=prevdef.nextoverloaded;
             prevdef.nextoverloaded:=stdef;
           end;
{$endif notused}
         aktprocsym:=oldprocsym;
         aktprocdef:=oldprocdef;
         procprefix:=oldprefix;
         procinfo:=oldprocinfo;
         otsym:=nil;
      end;


{****************************************************************************
                             DECLARATION PARSING
****************************************************************************}

    { search in symtablestack for not complete classes }
    procedure check_forward_class(p : tnamedindexitem);
      begin
        if (tsym(p).typ=typesym) and
           (ttypesym(p).restype.def.deftype=objectdef) and
           (oo_is_forward in tobjectdef(ttypesym(p).restype.def).objectoptions) then
          MessagePos1(tsym(p).fileinfo,sym_e_forward_type_not_resolved,tsym(p).realname);
      end;


    procedure read_declarations(islibrary : boolean);

        procedure Not_supported_for_inline(t : ttoken);
        begin
           if assigned(aktprocsym) and
              (aktprocdef.proccalloption=pocall_inline) then
             Begin
                Message1(parser_w_not_supported_for_inline,tokenstring(t));
                Message(parser_w_inlining_disabled);
                aktprocdef.proccalloption:=pocall_fpccall;
             End;
        end;

      begin
         repeat
           case token of
              _LABEL:
                begin
                   Not_supported_for_inline(token);
                   label_dec;
                end;
              _CONST:
                begin
                   Not_supported_for_inline(token);
                   const_dec;
                end;
              _TYPE:
                begin
                   Not_supported_for_inline(token);
                   type_dec;
                end;
              _VAR:
                var_dec;
              _THREADVAR:
                threadvar_dec;
              _CONSTRUCTOR,_DESTRUCTOR,
              _FUNCTION,_PROCEDURE,_OPERATOR,_CLASS:
                begin
                   Not_supported_for_inline(token);
                   read_proc;
                end;
              _RESOURCESTRING:
                resourcestring_dec;
              _EXPORTS:
                begin
                   Not_supported_for_inline(token);
                   { here we should be at lexlevel 1, no ? PM }
                   if (lexlevel<>main_program_level) or
                      (current_module.is_unit) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or (target_info.target in [target_i386_WIN32,target_i386_wdosx,target_i386_Netware])
                   then  // AD
                     read_exports;
                end
              else break;
           end;
         until false;
         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
          symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_forward_class);
      end;


    procedure read_interface_declarations;
      begin
         {Since the body is now parsed at lexlevel 1, and the declarations
          must be parsed at the same lexlevel we increase the lexlevel.}
         inc(lexlevel);
         repeat
           case token of
             _CONST :
               const_dec;
             _TYPE :
               type_dec;
             _VAR :
               var_dec;
             _THREADVAR :
               threadvar_dec;
             _RESOURCESTRING:
               resourcestring_dec;
             _FUNCTION,
             _PROCEDURE,
             _OPERATOR :
               read_proc;
             else
               break;
           end;
         until false;
         dec(lexlevel);
         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
          symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_forward_class);
      end;

end.
{
  $Log$
  Revision 1.47  2002-04-15 19:01:28  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.46  2002/04/04 18:45:19  carl
  + added wdosx support (patch from Pavel)

  Revision 1.45  2002/03/31 20:26:36  jonas
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

  Revision 1.44  2002/01/19 15:37:24  peter
    * commited the wrong file :(

  Revision 1.43  2002/01/19 15:20:09  peter
    * also check at the end of the implementation for incomplete classes

  Revision 1.42  2002/01/19 15:12:34  peter
    * check for unresolved forward classes in the interface

  Revision 1.41  2001/11/02 22:58:06  peter
    * procsym definition rewrite

  Revision 1.40  2001/10/25 21:22:37  peter
    * calling convention rewrite

  Revision 1.39  2001/10/22 21:20:46  peter
    * overloaded functions don't need to be global in kylix

  Revision 1.38  2001/10/01 13:38:45  jonas
    * allow self parameter for normal procedures again (because Kylix allows
      it too) ("merged")

  Revision 1.37  2001/09/10 10:26:26  jonas
    * fixed web bug 1593
    * writing of procvar headers is more complete (mention var/const/out for
      paras, add "of object" if applicable)
    + error if declaring explicit self para as var/const
    * fixed mangled name of procedures which contain an explicit self para
    * parsing para's should be slightly faster because mangled name of
      procedure is only updated once instead of after parsing each para
      (all merged from fixes)

  Revision 1.36  2001/08/26 13:36:46  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.35  2001/08/06 21:40:47  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.34  2001/06/04 11:53:13  peter
    + varargs directive

  Revision 1.33  2001/06/03 21:57:37  peter
    + hint directive parsing support

  Revision 1.32  2001/04/21 12:03:12  peter
    * m68k updates merged from fixes branch

  Revision 1.31  2001/04/18 22:01:57  peter
    * registration of targets and assemblers

  Revision 1.30  2001/04/14 14:05:47  peter
    * better skipping of secondpass if error

  Revision 1.29  2001/04/13 23:49:24  peter
    * when errors are found don't generate code, but still run the
      resulttype pass

  Revision 1.28  2001/04/13 17:59:03  peter
    * don't generate code when there is already an error

  Revision 1.27  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.26  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.25  2001/02/26 19:44:53  peter
    * merged generic m68k updates from fixes branch

  Revision 1.24  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.23  2000/11/29 00:30:37  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.22  2000/11/08 16:38:24  jonas
    * if a procedure uses exceptions (be it implicit or explicit), the
      usedregisters are set to all (because FPC_POPADDRSTACK doesn't save
      any registers) ("merged", fixes make cycle woth -Or)

  Revision 1.21  2000/11/01 23:04:38  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.20  2000/10/31 22:02:50  peter
    * symtable splitted, no real code changes

  Revision 1.19  2000/10/24 22:21:25  peter
    * set usedregisters after writing entry and exit code (merged)

  Revision 1.18  2000/10/21 18:16:12  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.17  2000/10/15 07:47:51  peter
    * unit names and procedure names are stored mixed case

  Revision 1.16  2000/10/14 10:14:52  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.15  2000/09/24 21:33:47  peter
    * message updates merges

  Revision 1.14  2000/09/24 21:19:51  peter
    * delphi compile fixes

  Revision 1.13  2000/09/24 15:06:24  peter
    * use defines.inc

  Revision 1.12  2000/09/10 20:11:07  peter
    * overload checking in implementation removed (merged)

  Revision 1.11  2000/09/04 20:15:19  peter
    * fixed operator overloading

  Revision 1.10  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.9  2000/08/16 18:33:54  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/13 12:54:56  peter
    * class member decl wrong then no other error after it
    * -vb has now also line numbering
    * -vb is also used for interface/implementation different decls and
      doesn't list the current function (merged)

  Revision 1.7  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

  Revision 1.6  2000/08/06 19:39:28  peter
    * default parameters working !

  Revision 1.5  2000/08/06 14:17:15  peter
    * overload fixes (merged)

  Revision 1.4  2000/07/30 17:04:43  peter
    * merged fixes

  Revision 1.3  2000/07/13 12:08:27  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:46  michael
  + removed logs
}
