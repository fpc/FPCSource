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

interface

    uses
       cobjects;

    procedure compile_proc_body(const proc_names:Tstringcontainer;
                                make_global,parent_has_class:boolean);

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;


implementation

    uses
       { common }
       cutils,
       { global }
       globtype,globals,tokens,verbose,
       systems,cpuinfo,
       { aasm }
       cpubase,aasm,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,symtable,types,
       ppu,fmodule,
       { pass 1 }
       node,pass_1,
       nbas,
       { pass 2 }
{$ifndef NOPASS2}
       pass_2,
{$endif}
       { parser }
       scanner,
       pbase,pexpr,pstatmnt,pdecl,pdecsub,pexports,
       { codegen }
{$ifdef newcg}
       cgbase,
       tgcpu,cgobj,
       {$ifndef NOOPT}
        ,aopt
       {$endif}
{$else}
       hcodegen,
       temp_gen
       {$ifdef i386}
         ,tgeni386
         ,cgai386
         {$ifndef NOOPT}
           ,aopt386
         {$endif}
       {$endif}
{$endif}
       ;


{****************************************************************************
                      PROCEDURE/FUNCTION BODY PARSING
****************************************************************************}

    function block(islibrary : boolean) : tnode;
      var
         funcretsym : pfuncretsym;
         storepos : tfileposinfo;
      begin
         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
          include(aktprocsym^.definition^.procoptions,po_assembler);

         { Handle assembler block different }
         if (po_assembler in aktprocsym^.definition^.procoptions) then
          begin
            read_declarations(false);
            block:=assembler_block;
            exit;
          end;

         if procinfo^.returntype.def<>pdef(voiddef) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              storepos:=akttokenpos;
              akttokenpos:=aktprocsym^.fileinfo;
              funcretsym:=new(pfuncretsym,init(aktprocsym^.name,procinfo));
              { insert in local symtable }
              symtablestack^.insert(funcretsym);
              akttokenpos:=storepos;
              if ret_in_acc(procinfo^.returntype.def) or (procinfo^.returntype.def^.deftype=floatdef) then
                procinfo^.return_offset:=-funcretsym^.address;
              procinfo^.funcretsym:=funcretsym;
              { insert result also if support is on }
              if (m_result in aktmodeswitches) then
               begin
                 procinfo^.resultfuncretsym:=new(pfuncretsym,init('RESULT',procinfo));
                 symtablestack^.insert(procinfo^.resultfuncretsym);
               end;
           end;
         read_declarations(islibrary);

         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack^.symtabletype=localsymtable) then
           procinfo^.firsttemp_offset := -symtablestack^.datasize
         else
           procinfo^.firsttemp_offset := 0;

         { space for the return value }
         { !!!!!   this means that we can not set the return value
         in a subfunction !!!!! }
         { because we don't know yet where the address is }
         if procinfo^.returntype.def<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo^.returntype.def) or (procinfo^.returntype.def^.deftype=floatdef) then
              { if (procinfo^.retdef^.deftype=orddef) or
                 (procinfo^.retdef^.deftype=pointerdef) or
                 (procinfo^.retdef^.deftype=enumdef) or
                 (procinfo^.retdef^.deftype=procvardef) or
                 (procinfo^.retdef^.deftype=floatdef) or
                 (
                   (procinfo^.retdef^.deftype=setdef) and
                   (psetdef(procinfo^.retdef)^.settype=smallset)
                 ) then  }
                begin
                   { the space has been set in the local symtable }
                   procinfo^.return_offset:=-funcretsym^.address;
                   if ((procinfo^.flags and pi_operator)<>0) and
                     assigned(opsym) then
                     {opsym^.address:=procinfo^.para_offset; is wrong PM }
                     opsym^.address:=-procinfo^.return_offset;
                   { eax is modified by a function }
{$ifndef newcg}
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX));

                   if is_64bitint(procinfo^.returntype.def) then
                     usedinproc:=usedinproc or ($80 shr byte(R_EDX))
{$endif}
{$ifdef m68k}
                   usedinproc:=usedinproc or ($800 shr word(R_D0));

                   if is_64bitint(procinfo^.retdef) then
                     usedinproc:=usedinproc or ($800 shr byte(R_D1))
{$endif}
{$endif newcg}
                end;
           end;

         {Unit initialization?.}
         if (lexlevel=unit_init_level) and (current_module^.is_unit)
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
                        current_module^.flags:=current_module^.flags or uf_init;
                        block:=statement_block(_INITIALIZATION);
                     end
                   else if (token=_FINALIZATION) then
                     begin
                        if (current_module^.flags and uf_finalize)<>0 then
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
                        current_module^.flags:=current_module^.flags or uf_init;
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

    procedure compile_proc_body(const proc_names:Tstringcontainer;
                                make_global,parent_has_class:boolean);
      {
        Compile the body of a procedure
      }
      var
         oldexitlabel,oldexit2label : pasmlabel;
         oldfaillabel,oldquickexitlabel:Pasmlabel;
         _class,hp:Pobjectdef;
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
         if (po_staticmethod in aktprocsym^.definition^.procoptions) then
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
         if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
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
               while _class^.childof<>hp do
                 _class:=_class^.childof;
               hp:=_class;
               _class^.symtable^.next:=symtablestack;
               symtablestack:=_class^.symtable;
             until hp=procinfo^._class;
           end;

         { insert parasymtable in symtablestack}
         { only if lexlevel > 1 !!! global symtable should be right after staticsymtazble
           for checking of same names used in interface and implementation !! }
         if lexlevel>=normal_function_level then
           begin
              aktprocsym^.definition^.parast^.next:=symtablestack;
              symtablestack:=aktprocsym^.definition^.parast;
              symtablestack^.symtablelevel:=lexlevel;
           end;
         { insert localsymtable in symtablestack}
         aktprocsym^.definition^.localst^.next:=symtablestack;
         symtablestack:=aktprocsym^.definition^.localst;
         symtablestack^.symtablelevel:=lexlevel;
         { constant symbols are inserted in this symboltable }
         constsymtable:=symtablestack;

         { reset the temporary memory }
         cleartempgen;

{$ifdef newcg}
         tg.usedinproc:=[];
{$else newcg}
         { no registers are used }
         usedinproc:=0;
{$endif newcg}
         { save entry info }
         entrypos:=aktfilepos;
         entryswitches:=aktlocalswitches;
         localmaxfpuregisters:=aktmaxfpuregisters;
         { parse the code ... }
         code:=block(current_module^.islibrary);
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

         { set the framepointer to esp for assembler functions }
         { but only if the are no local variables           }
         { already done in assembler_block }
{$ifdef newcg}
         tg.setfirsttemp(procinfo^.firsttemp_offset);
{$else newcg}
         setfirsttemp(procinfo^.firsttemp_offset);
{$endif newcg}

         { ... and generate assembler }
         { but set the right switches for entry !! }
         aktlocalswitches:=entryswitches;
         oldaktmaxfpuregisters:=aktmaxfpuregisters;
         aktmaxfpuregisters:=localmaxfpuregisters;
{$ifndef NOPASS2}
         if assigned(code) then
           generatecode(code);
         { set switches to status at end of procedure }
         aktlocalswitches:=exitswitches;

         if assigned(code) then
           begin
              aktprocsym^.definition^.code:=code;

              { the procedure is now defined }
              aktprocsym^.definition^.forwarddef:=false;
           end;

{$ifdef newcg}
         stackframe:=tg.gettempsize;
{$else newcg}
         stackframe:=gettempsize;
{$endif newcg}

         { first generate entry code with the correct position and switches }
         aktfilepos:=entrypos;
         aktlocalswitches:=entryswitches;
{$ifdef newcg}
         if assigned(code) then
           cg^.g_entrycode(procinfo^.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$else newcg}
         if assigned(code) then
           genentrycode(procinfo^.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$endif newcg}

         { now generate exit code with the correct position and switches }
         aktfilepos:=exitpos;
         aktlocalswitches:=exitswitches;
         if assigned(code) then
           begin
{$ifdef newcg}
             cg^.g_exitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$else newcg}
             genexitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$endif newcg}
             { now all the registers used are known }
{$ifdef newcg}
             aktprocsym^.definition^.usedregisters:=tg.usedinproc;
{$else newcg}
             aktprocsym^.definition^.usedregisters:=usedinproc;
{$endif newcg}
             procinfo^.aktproccode^.insertlist(procinfo^.aktentrycode);
             procinfo^.aktproccode^.concatlist(procinfo^.aktexitcode);
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
                (not procinfo^.aktlocaldata^.empty) then
               begin
                  procinfo^.aktproccode^.concat(new(pai_section,init(sec_data)));
                  procinfo^.aktproccode^.concatlist(procinfo^.aktlocaldata);
                  procinfo^.aktproccode^.concat(new(pai_section,init(sec_code)));
               end;
             { now we can insert a cut }
             if (cs_create_smart in aktmoduleswitches) then
               codesegment^.concat(new(pai_cut,init));

             { add the procedure to the codesegment }
             codesegment^.concatlist(procinfo^.aktproccode);
           end;
{$else NOPASS2}
         if assigned(code) then
          firstpass(code);
{$endif NOPASS2}

         { ... remove symbol tables, for the browser leave the static table }
      {    if (cs_browser in aktmoduleswitches) and (symtablestack^.symtabletype=staticsymtable) then
          symtablestack^.next:=symtablestack^.next^.next
         else }
         if lexlevel>=normal_function_level then
           symtablestack:=symtablestack^.next^.next
         else
           symtablestack:=symtablestack^.next;

         { ... check for unused symbols      }
         { but only if there is no asm block }
         if assigned(code) then
           begin
             if (Errorcount=0) then
               begin
                 pstoredsymtable(aktprocsym^.definition^.localst)^.check_forwards;
                 pstoredsymtable(aktprocsym^.definition^.localst)^.checklabels;
               end;
             if (procinfo^.flags and pi_uses_asm)=0 then
               begin
                  { not for unit init, becuase the var can be used in finalize,
                    it will be done in proc_unit }
                  if not(aktprocsym^.definition^.proctypeoption
                     in [potype_proginit,potype_unitinit,potype_unitfinalize]) then
                     pstoredsymtable(aktprocsym^.definition^.localst)^.allsymbolsused;
                  pstoredsymtable(aktprocsym^.definition^.parast)^.allsymbolsused;
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
            not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
           begin
             if lexlevel>=normal_function_level then
               dispose(aktprocsym^.definition^.localst,done);
             aktprocsym^.definition^.localst:=nil;
           end;

{$ifdef newcg}
         { all registers can be used again }
         tg.resetusableregisters;
         { only now we can remove the temps }
         tg.resettempgen;
{$else newcg}
         { all registers can be used again }
         resetusableregisters;
         { only now we can remove the temps }
         resettempgen;
{$endif newcg}

         { remove code tree, if not inline procedure }
         if assigned(code) and not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
           code.free;

         { remove class member symbol tables }
         while symtablestack^.symtabletype=objectsymtable do
           symtablestack:=symtablestack^.next;

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

      procedure checkvaluepara(p:pnamedindexobject);
      var
        vs : pvarsym;
        s  : string;
      begin
        with pvarsym(p)^ do
         begin
           if copy(name,1,3)='val' then
            begin
              s:=Copy(name,4,255);
              if not(po_assembler in aktprocsym^.definition^.procoptions) then
               begin
                 vs:=new(Pvarsym,initdef(s,vartype.def));
                 vs^.fileinfo:=fileinfo;
                 vs^.varspez:=varspez;
                 aktprocsym^.definition^.localst^.insert(vs);
                 include(vs^.varoptions,vo_is_local_copy);
                 vs^.varstate:=vs_assigned;
                 localvarsym:=vs;
                 inc(refs); { the para was used to set the local copy ! }
                 { warnings only on local copy ! }
                 varstate:=vs_used;
               end
              else
               begin
                 aktprocsym^.definition^.parast^.rename(name,s);
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
        oldprefix     : string;
        oldprocsym       : Pprocsym;
        oldprocinfo      : pprocinfo;
        oldconstsymtable : Psymtable;
        oldfilepos       : tfileposinfo;
        names           : Pstringcontainer;
        pdflags         : word;
        prevdef,stdef   : pprocdef;
      begin
      { save old state }
         oldprocsym:=aktprocsym;
         oldprefix:=procprefix;
         oldconstsymtable:=constsymtable;
         oldprocinfo:=procinfo;
      { create a new procedure }
         new(names,init);
{$ifdef fixLeaksOnError}
         strContStack.push(names);
{$endif fixLeaksOnError}
         codegen_newprocedure;
         with procinfo^ do
          begin
            parent:=oldprocinfo;
          { clear flags }
            flags:=0;
          { standard frame pointer }
            framepointer:=frame_pointer;
            { funcret_is_valid:=false; }
            funcret_state:=vs_declared;
          { is this a nested function of a method ? }
            if assigned(oldprocinfo) then
              _class:=oldprocinfo^._class;
          end;

         parse_proc_dec;

         procinfo^.sym:=aktprocsym;
         procinfo^.def:=aktprocsym^.definition;

      { set the default function options }
         if parse_only then
          begin
            aktprocsym^.definition^.forwarddef:=true;
            { set also the interface flag, for better error message when the
              implementation doesn't much this header }
            aktprocsym^.definition^.interfacedef:=true;
            pdflags:=pd_interface;
          end
         else
          begin
            pdflags:=pd_body;
            if current_module^.in_implementation then
             pdflags:=pdflags or pd_implemen;
            if (not current_module^.is_unit) or (cs_create_smart in aktmoduleswitches) then
             pdflags:=pdflags or pd_global;
            procinfo^.exported:=false;
            aktprocsym^.definition^.forwarddef:=false;
          end;

      { parse the directives that may follow }
         inc(lexlevel);
         parse_proc_directives(names,pdflags);
         dec(lexlevel);

      { set aktfilepos to the beginning of the function declaration }
         oldfilepos:=aktfilepos;
         aktfilepos:=aktprocsym^.definition^.fileinfo;

      { search for forward declarations }
         if not check_identical_proc(prevdef) then
           begin
           { A method must be forward defined (in the object declaration) }
             if assigned(procinfo^._class) and (not assigned(oldprocinfo^._class)) then
              begin
                Message1(parser_e_header_dont_match_any_member,
                         aktprocsym^.definition^.fullprocname);
                aktprocsym^.write_parameter_lists(aktprocsym^.definition);
              end
             else
              begin
                { Give a better error if there is a forward def in the interface and only
                  a single implementation }
                if (not aktprocsym^.definition^.forwarddef) and
                   assigned(aktprocsym^.definition^.nextoverloaded) and
                   aktprocsym^.definition^.nextoverloaded^.forwarddef and
                   aktprocsym^.definition^.nextoverloaded^.interfacedef and
                   not(assigned(aktprocsym^.definition^.nextoverloaded^.nextoverloaded)) then
                 begin
                   Message1(parser_e_header_dont_match_forward,
                            aktprocsym^.definition^.fullprocname);
                   aktprocsym^.write_parameter_lists(aktprocsym^.definition);
                 end
                else
                 begin
                 { check the global flag }
                   if (procinfo^.flags and pi_is_global)<>0 then
                     Message(parser_e_overloaded_must_be_all_global);
                 end;
              end;
           end;

         { set return type here, becuase the aktprocsym^.definition can be
           changed by check_identical_proc (PFV) }
         procinfo^.returntype.def:=aktprocsym^.definition^.rettype.def;

{$ifdef i386}
         if (po_interrupt in aktprocsym^.definition^.procoptions) then
           begin
             { we push Flags and CS as long
               to cope with the IRETD
               and we save 6 register + 4 selectors }
             inc(procinfo^.para_offset,8+6*4+4*2);
           end;
{$endif i386}

         { pointer to the return value ? }
         if ret_in_param(procinfo^.returntype.def) then
          begin
            procinfo^.return_offset:=procinfo^.para_offset;
            inc(procinfo^.para_offset,target_os.size_of_pointer);
          end;
         { allows to access the parameters of main functions in nested functions }
         aktprocsym^.definition^.parast^.address_fixup:=procinfo^.para_offset;

         { when it is a value para and it needs a local copy then rename
           the parameter and insert a copy in the localst. This is not done
           for assembler procedures }
         if (not parse_only) and (not aktprocsym^.definition^.forwarddef) then
           aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}checkvaluepara);

      { restore file pos }
         aktfilepos:=oldfilepos;

      { compile procedure when a body is needed }
         if (pdflags and pd_body)<>0 then
           begin
             Message1(parser_p_procedure_start,
                      aktprocsym^.definition^.fullprocname);
             names^.insert(aktprocsym^.definition^.mangledname);
            { set _FAIL as keyword if constructor }
            if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=m_all;
            if assigned(aktprocsym^.definition^._class) then
              tokeninfo^[_SELF].keyword:=m_all;

             compile_proc_body(names^,((pdflags and pd_global)<>0),assigned(oldprocinfo^._class));

            { reset _FAIL as normal }
            if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=m_none;
            if assigned(aktprocsym^.definition^._class) and (lexlevel=main_program_level) then
              tokeninfo^[_SELF].keyword:=m_none;
             consume(_SEMICOLON);
           end;
      { close }
{$ifdef fixLeaksOnError}
         if names <> strContStack.pop then
           writeln('problem with strContStack in psub!');
{$endif fixLeaksOnError}
         dispose(names,done);
         codegen_doneprocedure;
      { Restore old state }
         constsymtable:=oldconstsymtable;
         { from now on all refernece to mangledname means
           that the function is already used }
         aktprocsym^.definition^.count:=true;
         { restore the interface order to maintain CRC values PM }
         if assigned(prevdef) and assigned(aktprocsym^.definition^.nextoverloaded) then
           begin
             stdef:=aktprocsym^.definition;
             aktprocsym^.definition:=stdef^.nextoverloaded;
             stdef^.nextoverloaded:=prevdef^.nextoverloaded;
             prevdef^.nextoverloaded:=stdef;
           end;
         aktprocsym:=oldprocsym;
         procprefix:=oldprefix;
         procinfo:=oldprocinfo;
         opsym:=nil;
      end;


{****************************************************************************
                             DECLARATION PARSING
****************************************************************************}

    procedure read_declarations(islibrary : boolean);

        procedure Not_supported_for_inline(t : ttoken);
        begin
           if assigned(aktprocsym) and
              (pocall_inline in aktprocsym^.definition^.proccalloptions) then
             Begin
                Message1(parser_w_not_supported_for_inline,tokenstring(t));
                Message(parser_w_inlining_disabled);
                exclude(aktprocsym^.definition^.proccalloptions,pocall_inline);
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
                      (current_module^.is_unit) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or (target_info.target=target_i386_WIN32)
                   or (target_info.target=target_i386_Netware) then  // AD
                     read_exports;
                end
              else break;
           end;
         until false;
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
      end;

end.
{
  $Log$
  Revision 1.21  2000-11-01 23:04:38  peter
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