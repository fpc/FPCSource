{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Daniel Mantione

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

{$i fpcdefs.inc}

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
       cpubase,cpuinfo,aasmbase,aasmtai,
       { symtable }
       symconst,symbase,symdef,symsym,symtype,symtable,defutil,
       paramgr,
       ppu,fmodule,
       { pass 1 }
       node,
       nbas,nld,
       pass_1,
    {$ifdef state_tracking}
       nstate,
    {$endif state_tracking}
       { pass 2 }
{$ifndef NOPASS2}
       pass_2,
{$endif}
       { parser }
       scanner,
       pbase,pstatmnt,pdecl,pdecsub,pexports,
       { codegen }
       tgobj,cgbase,rgobj,rgcpu,
       ncgutil
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

    procedure initializevars(p:tnamedindexitem;arg:pointer);
      var
        b : tblocknode;
      begin
        if tsym(p).typ<>varsym then
         exit;
        with tvarsym(p) do
         begin
           if assigned(defaultconstsym) then
            begin
              b:=tblocknode(arg);
              b.left:=cstatementnode.create(
                        cassignmentnode.create(
                            cloadnode.create(tsym(p),tsym(p).owner),
                            cloadnode.create(defaultconstsym,defaultconstsym.owner)),
                        b.left);
            end;
         end;
      end;


    function block(islibrary : boolean) : tnode;
      var
         storepos : tfileposinfo;
      begin
         if not is_void(aktprocdef.rettype.def) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              storepos:=akttokenpos;
              akttokenpos:=aktprocsym.fileinfo;
              aktprocdef.funcretsym:=tfuncretsym.create(aktprocsym.name,aktprocdef.rettype);
              { insert in local symtable }
              symtablestack.insert(aktprocdef.funcretsym);
              symtablestack.insertvardata(aktprocdef.funcretsym);
              akttokenpos:=storepos;

              procinfo.set_result_offset;
              { insert result also if support is on }
              if (m_result in aktmodeswitches) then
               begin
                 aktprocdef.resultfuncretsym:=tfuncretsym.create('RESULT',aktprocdef.rettype);
                 symtablestack.insert(aktprocdef.resultfuncretsym);
               end;
           end;
         { parse const,types and vars }
         read_declarations(islibrary);

         procinfo.handle_body_start;

         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
          include(aktprocdef.procoptions,po_assembler);

         { Handle assembler block different }
         if (po_assembler in aktprocdef.procoptions) then
          begin
            block:=assembler_block;
            exit;
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
                        { The library init code is already called and does not
                          need to be in the initfinal table (PFV) }
                        if not islibrary then
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
                        { The library init code is already called and does not
                          need to be in the initfinal table (PFV) }
                        if not islibrary then
                          current_module.flags:=current_module.flags or uf_init;
                        block:=statement_block(_BEGIN);
                     end;
                end;
            end
         else
            begin
               block:=statement_block(_BEGIN);
               if symtablestack.symtabletype=localsymtable then
                 symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}initializevars,block);
            end;
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
         if lexlevel>maxnesting then
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
         objectlibrary.getlabel(aktexitlabel);
         objectlibrary.getlabel(aktexit2label);
         { exit for fail in constructors }
         if (aktprocdef.proctypeoption=potype_constructor) then
           begin
             objectlibrary.getlabel(faillabel);
             objectlibrary.getlabel(quickexitlabel);
           end;
         { reset break and continue labels }
         block_type:=bt_general;
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;
    {$ifdef state_tracking}
{    aktstate:=Tstate_storage.create;}
    {$endif state_tracking}

         { insert symtables for the class, by only if it is no nested function }
         if assigned(procinfo._class) and not(parent_has_class) then
           begin
             { insert them in the reverse order ! }
             hp:=nil;
             repeat
               _class:=procinfo._class;
               while _class.childof<>hp do
                 _class:=_class.childof;
               hp:=_class;
               _class.symtable.next:=symtablestack;
               symtablestack:=_class.symtable;
             until hp=procinfo._class;
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
         { create a local symbol table for this routine }
         if not assigned(aktprocdef.localst) then
            aktprocdef.insert_localst;
         { insert localsymtable in symtablestack}
         aktprocdef.localst.next:=symtablestack;
         symtablestack:=aktprocdef.localst;
         symtablestack.symtablelevel:=lexlevel;
         { constant symbols are inserted in this symboltable }
         constsymtable:=symtablestack;

         { reset the temporary memory }
         rg.cleartempgen;
         rg.usedinproc:=[];
         rg.usedbyproc:=[];

         { save entry info }
         entrypos:=aktfilepos;
         entryswitches:=aktlocalswitches;
         localmaxfpuregisters:=aktmaxfpuregisters;
         { parse the code ... }
         code:=block(current_module.islibrary);
         { store a copy of the original tree for inline, for
           normal procedures only store a reference to the
           current tree }
         if (aktprocdef.proccalloption=pocall_inline) then
           aktprocdef.code:=code.getcopy
         else
           aktprocdef.code:=code;
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
         tg.setfirsttemp(procinfo.firsttemp_offset);

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
                { first generate entry code with the correct position and switches }
                aktfilepos:=entrypos;
                aktlocalswitches:=entryswitches;
                genentrycode(procinfo.aktentrycode,make_global,0,parasize,nostackframe,false);

                { FPC_POPADDRSTACK destroys all registers (JM) }
                if (procinfo.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0 then
                 begin
                   rg.usedinproc := ALL_REGISTERS;
                 end;

                { now generate exit code with the correct position and switches }
                aktfilepos:=exitpos;
                aktlocalswitches:=exitswitches;
                genexitcode(procinfo.aktexitcode,parasize,nostackframe,false);

                { now all the registers used are known }
                aktprocdef.usedregisters:=rg.usedinproc;
                procinfo.aktproccode.insertlist(procinfo.aktentrycode);
                procinfo.aktproccode.concatlist(procinfo.aktexitcode);
{$ifdef i386}
                procinfo.aktproccode.convert_registers;
{$endif}                
{$ifndef NoOpt}
                if (cs_optimize in aktglobalswitches) and
                { do not optimize pure assembler procedures }
                   ((procinfo.flags and pi_is_assembler)=0)  then
                  optimize(procinfo.aktproccode);
{$endif NoOpt}
                { save local data (casetable) also in the same file }
                if assigned(procinfo.aktlocaldata) and
                   (not procinfo.aktlocaldata.empty) then
                 begin
                   procinfo.aktproccode.concat(Tai_section.Create(sec_data));
                   procinfo.aktproccode.concatlist(procinfo.aktlocaldata);
                   procinfo.aktproccode.concat(Tai_section.Create(sec_code));
                end;

                { add the procedure to the codesegment }
                if (cs_create_smart in aktmoduleswitches) then
                 codeSegment.concat(Tai_cut.Create);
                codeSegment.concatlist(procinfo.aktproccode);
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
             if (procinfo.flags and pi_uses_asm)=0 then
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
              begin
               aktprocdef.localst.free;
              end;
             aktprocdef.localst:=nil;
           end;

         { all registers can be used again }
         rg.resetusableregisters;
         { only now we can remove the temps }
         tg.resettempgen;

         { remove code tree, if not inline procedure }
         if assigned(code) then
          begin
            { the inline procedure has already got a copy of the tree
              stored in aktprocdef.code }
            code.free;
            if (aktprocdef.proccalloption<>pocall_inline) then
              aktprocdef.code:=nil;
          end;

         { remove class member symbol tables }
         while symtablestack.symtabletype=objectsymtable do
           symtablestack:=symtablestack.next;

         aktmaxfpuregisters:=oldaktmaxfpuregisters;

    {$ifdef state_tracking}
{    aktstate.destroy;}
    {$endif state_tracking}
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

    procedure checkvaluepara(p:tnamedindexitem;arg:pointer);
      var
        vs : tvarsym;
        s  : string;
      begin
        if tsym(p).typ<>varsym then
         exit;
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
                 if not assigned(aktprocdef.localst) then
                    aktprocdef.insert_localst;
                 aktprocdef.localst.insert(vs);
                 aktprocdef.localst.insertvardata(vs);
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
        oldprocsym       : tprocsym;
        oldprocdef       : tprocdef;
        oldprocinfo      : tprocinfo;
        oldconstsymtable : tsymtable;
        oldfilepos       : tfileposinfo;
        pdflags          : word;
      begin
      { save old state }
         oldprocdef:=aktprocdef;
         oldprocsym:=aktprocsym;
         oldconstsymtable:=constsymtable;
         oldprocinfo:=procinfo;
      { create a new procedure }
         codegen_newprocedure;
         with procinfo do
          begin
            parent:=oldprocinfo;
          { clear flags }
            flags:=0;
          { standard frame pointer }
            framepointer.enum:=frame_pointer_reg;
          { is this a nested function of a method ? }
            if assigned(oldprocinfo) then
              _class:=oldprocinfo._class;
          end;

         parse_proc_dec;

         procinfo.procdef:=aktprocdef;

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
            if (not current_module.in_interface) then
             pdflags:=pdflags or pd_implemen;
            if (not current_module.is_unit) or (cs_create_smart in aktmoduleswitches) then
             pdflags:=pdflags or pd_global;
            procinfo.exported:=false;
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
             if assigned(procinfo._class) and
                (not assigned(oldprocinfo._class)) then
              begin
                Message1(parser_e_header_dont_match_any_member,aktprocdef.fullprocname);
                aktprocsym.write_parameter_lists(aktprocdef);
              end
             else
              begin
                { Give a better error if there is a forward def in the interface and only
                  a single implementation }
                if (not aktprocdef.forwarddef) and
                   (not aktprocdef.interfacedef) and
                   (aktprocsym.procdef_count>1) and
                   aktprocsym.first_procdef.forwarddef and
                   aktprocsym.first_procdef.interfacedef and
                   not(aktprocsym.procdef_count>2) then
                 begin
                   Message1(parser_e_header_dont_match_forward,aktprocdef.fullprocname);
                   aktprocsym.write_parameter_lists(aktprocdef);
                 end
                else
                 begin
                   { check the global flag, for delphi this is not
                     required }
                   if not(m_delphi in aktmodeswitches) and
                      ((procinfo.flags and pi_is_global)<>0) then
                     Message(parser_e_overloaded_must_be_all_global);
                 end;
              end;
           end;

         { update procinfo, because the aktprocdef can be
           changed by check_identical_proc (PFV) }
         procinfo.procdef:=aktprocdef;


{$ifdef i386}
         { add implicit pushes for interrupt routines }
         if (po_interrupt in aktprocdef.procoptions) then
           procinfo.allocate_interrupt_stackframe;
{$endif i386}

         { pointer to the return value ? }
         if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption)
{$ifdef m68k}
            and not (aktprocdef.proccalloption in [pocall_cdecl])
{$endif m68k}
            then
          begin
            procinfo.return_offset:=procinfo.para_offset;
            inc(procinfo.para_offset,pointer_size);
          end;
         { allows to access the parameters of main functions in nested functions }
         aktprocdef.parast.address_fixup:=procinfo.para_offset;

         { when it is a value para and it needs a local copy then rename
           the parameter and insert a copy in the localst. This is not done
           for assembler procedures }
         if (not parse_only) and (not aktprocdef.forwarddef) then
           aktprocdef.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}checkvaluepara,nil);

         procinfo.after_header;

         { restore file pos }
         aktfilepos:=oldfilepos;

         { compile procedure when a body is needed }
         if (pdflags and pd_body)<>0 then
           begin
             Message1(parser_p_procedure_start,
                      aktprocdef.fullprocname);

             if assigned(aktprocsym.owner) then
               aktprocdef.aliasnames.insert(aktprocdef.mangledname);
            { set _FAIL as keyword if constructor }
            if (aktprocdef.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=m_all;
            if assigned(aktprocdef._class) then
              tokeninfo^[_SELF].keyword:=m_all;

             compile_proc_body(((pdflags and pd_global)<>0),assigned(oldprocinfo._class));

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
         { release procsym when it was not stored in the symtable }
         if not assigned(aktprocsym.owner) then
          begin
            aktprocsym.free;
            aktprocdef.procsym:=nil;
          end;
         aktprocsym:=oldprocsym;
         aktprocdef:=oldprocdef;
         procinfo:=oldprocinfo;
         otsym:=nil;
      end;


{****************************************************************************
                             DECLARATION PARSING
****************************************************************************}

    { search in symtablestack for not complete classes }
    procedure check_forward_class(p : tnamedindexitem;arg:pointer);
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
                   else if islibrary or (target_info.system in [system_i386_WIN32,system_i386_wdosx,system_i386_Netware])
                   then  // AD
                     read_exports;
                end
              else break;
           end;
         until false;
         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
          symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_forward_class,nil);
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
          symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_forward_class,nil);
      end;

end.
{
  $Log$
  Revision 1.90  2003-01-09 20:40:59  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.89  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.88  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.87  2003/01/03 20:35:08  peter
    * check also interfacedef when checking for matching forwarddef

  Revision 1.86  2003/01/02 11:14:02  michael
  + Patch from peter to support initial values for local variables

  Revision 1.85  2002/12/29 18:59:34  peter
    * fixed parsing of declarations before asm statement

  Revision 1.84  2002/12/29 18:25:18  peter
    * parse declarations before check _ASM token

  Revision 1.83  2002/12/29 14:57:50  peter
    * unit loading changed to first register units and load them
      afterwards. This is needed to support uses xxx in yyy correctly
    * unit dependency check fixed

  Revision 1.82  2002/12/25 01:26:56  peter
    * duplicate procsym-unitsym fix

  Revision 1.81  2002/12/15 13:37:15  peter
    * don't include uf_init for library. The code is already called and
      does not need to be in the initfinal table

  Revision 1.80  2002/12/07 14:27:09  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.79  2002/11/25 18:43:32  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.78  2002/11/25 17:43:23  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.77  2002/11/23 22:50:06  carl
    * some small speed optimizations
    + added several new warnings/hints

  Revision 1.76  2002/11/18 17:31:58  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.75  2002/11/17 16:31:57  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.74  2002/11/15 01:58:53  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.73  2002/11/09 15:32:30  carl
    * noopt for non-i386 targets

  Revision 1.72  2002/09/10 20:31:48  florian
    * call to procinfo.after_header added

  Revision 1.71  2002/09/07 15:25:07  peter
    * old logs removed and tabs fixed

  Revision 1.70  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.69  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.68  2002/08/17 09:23:41  florian
    * first part of procinfo rewrite

  Revision 1.67  2002/08/16 14:24:59  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.66  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.65  2002/08/11 13:24:13  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.64  2002/08/09 19:14:28  carl
    * fixed stackframe parameter (should only contain local size),
      set to zero currently

  Revision 1.63  2002/08/06 20:55:22  florian
    * first part of ppc calling conventions fix

  Revision 1.62  2002/07/26 21:15:41  florian
    * rewrote the system handling

  Revision 1.61  2002/07/20 11:57:56  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.60  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.59  2002/07/15 18:03:15  florian
    * readded removed changes

  Revision 1.57  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.58  2002/07/14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.56  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.55  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.54  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.53  2002/05/18 13:34:14  peter
    * readded missing revisions

  Revision 1.52  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.51  2002/05/14 19:34:49  peter
    * removed old logs and updated copyright year

}
