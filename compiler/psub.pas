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

    uses
      symdef;

    procedure printnode_reset;

    procedure compile_proc_body(pd:tprocdef;make_global,parent_has_class:boolean);

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
       symconst,symbase,symsym,symtype,symtable,defutil,
       paramgr,
       ppu,fmodule,
       { pass 1 }
       node,
       nutils,nbas,nld,ncal,ncon,nflw,nadd,ncnv,nmem,
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
      begin
         { parse const,types and vars }
         read_declarations(islibrary);

         current_procinfo.handle_body_start;

         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
          include(current_procdef.procoptions,po_assembler);

         { Handle assembler block different }
         if (po_assembler in current_procdef.procoptions) then
          begin
            block:=assembler_block;
            exit;
          end;

         {Unit initialization?.}
         if (
             assigned(current_procdef.localst) and
             (current_procdef.localst.symtablelevel=main_program_level) and
             (current_module.is_unit)
            ) or
            islibrary then
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

    procedure printnode_reset;
      begin
        assign(printnodefile,treelogfilename);
        {$I-}
         rewrite(printnodefile);
        {$I+}
        if ioresult<>0 then
         begin
           Comment(V_Error,'Error creating '+treelogfilename);
           exit;
         end;
        close(printnodefile);
      end;


    procedure printnode_procdef(pd:tprocdef);
      begin
        assign(printnodefile,treelogfilename);
        {$I-}
         append(printnodefile);
         if ioresult<>0 then
          rewrite(printnodefile);
        {$I+}
        if ioresult<>0 then
         begin
           Comment(V_Error,'Error creating '+treelogfilename);
           exit;
         end;
        writeln(printnodefile);
        writeln(printnodefile,'*******************************************************************************');
        writeln(printnodefile,current_procdef.fullprocname(false));
        writeln(printnodefile,'*******************************************************************************');
        printnode(printnodefile,pd.code);
        close(printnodefile);
      end;


    function generate_initialize_block:tnode;
      var
        srsym        : tsym;
        para         : tcallparanode;
        newstatement : tstatementnode;
        htype        : ttype;
      begin
        result:=internalstatements(newstatement,true);

        if assigned(current_procdef._class) then
          begin
            { a constructor needs a help procedure }
            if (current_procdef.proctypeoption=potype_constructor) then
              begin
                if is_class(current_procdef._class) then
                  begin
                    if (cs_implicit_exceptions in aktmoduleswitches) then
                      include(current_procinfo.flags,pi_needs_implicit_finally);
                    srsym:=search_class_member(current_procdef._class,'NEWINSTANCE');
                    if assigned(srsym) and
                       (srsym.typ=procsym) then
                      begin
                        { if vmt<>0 then newinstance }
                        addstatement(newstatement,cifnode.create(
                            caddnode.create(unequaln,
                                load_vmt_pointer_node,
                                cnilnode.create),
                            cassignmentnode.create(
                                ctypeconvnode.create_explicit(
                                    load_self_pointer_node,
                                    voidpointertype),
                                ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_vmt_pointer_node)),
                            nil));
                      end
                    else
                      internalerror(200305108);
                  end
                else
                  if is_object(current_procdef._class) then
                    begin
                      htype.setdef(current_procdef._class);
                      htype.setdef(tpointerdef.create(htype));
                      { parameter 3 : vmt_offset }
                      { parameter 2 : address of pointer to vmt,
                        this is required to allow setting the vmt to -1 to indicate
                        that memory was allocated }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(current_procdef._class.vmt_offset,s32bittype,false),
                            ccallparanode.create(
                                ctypeconvnode.create_explicit(
                                    load_vmt_pointer_node,
                                    voidpointertype),
                            ccallparanode.create(
                                ctypeconvnode.create_explicit(
                                    load_self_pointer_node,
                                    voidpointertype),
                            nil)));
                      addstatement(newstatement,cassignmentnode.create(
                          ctypeconvnode.create_explicit(
                              load_self_pointer_node,
                              voidpointertype),
                          ccallnode.createintern('fpc_help_constructor',para)));
                    end
                else
                  internalerror(200305103);
                { if self=nil then exit
                  calling fail instead of exit is useless because
                  there is nothing to dispose (PFV) }
                addstatement(newstatement,cifnode.create(
                    caddnode.create(equaln,
                        load_self_pointer_node,
                        cnilnode.create),
                    cexitnode.create(nil),
                    nil));
              end;

            { maybe call BeforeDestruction for classes }
            if (current_procdef.proctypeoption=potype_destructor) and
               is_class(current_procdef._class) then
              begin
                srsym:=search_class_member(current_procdef._class,'BEFOREDESTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    { if vmt<>0 then beforedestruction }
                    addstatement(newstatement,cifnode.create(
                        caddnode.create(unequaln,
                            load_vmt_pointer_node,
                            cnilnode.create),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node),
                        nil));
                  end
                else
                  internalerror(200305104);
              end;
          end;
      end;


    function generate_finalize_block:tnode;
      begin
        result:=cnothingnode.create;
      end;


    function generate_entry_block:tnode;
      begin
        result:=cnothingnode.create;
      end;


    function generate_exit_block:tnode;
      var
        srsym : tsym;
        para : tcallparanode;
        newstatement : tstatementnode;
      begin
        generate_exit_block:=internalstatements(newstatement,true);

        if assigned(current_procdef._class) then
          begin
            { maybe call AfterConstruction for classes }
            if (current_procdef.proctypeoption=potype_constructor) and
               is_class(current_procdef._class) then
              begin
                srsym:=search_class_member(current_procdef._class,'AFTERCONSTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    { if vmt<>0 then afterconstruction }
                    addstatement(newstatement,cifnode.create(
                        caddnode.create(unequaln,
                            load_vmt_pointer_node,
                            cnilnode.create),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node),
                        nil));
                  end
                else
                  internalerror(200305106);
              end;

            { a destructor needs a help procedure }
            if (current_procdef.proctypeoption=potype_destructor) then
              begin
                if is_class(current_procdef._class) then
                  begin
                    srsym:=search_class_member(current_procdef._class,'FREEINSTANCE');
                    if assigned(srsym) and
                       (srsym.typ=procsym) then
                      begin
                        { if self<>0 and vmt=1 then freeinstance }
                        addstatement(newstatement,cifnode.create(
                            caddnode.create(andn,
                                caddnode.create(unequaln,
                                    load_self_pointer_node,
                                    cnilnode.create),
                                caddnode.create(equaln,
                                    ctypeconvnode.create(
                                        load_vmt_pointer_node,
                                        voidpointertype),
                                    cpointerconstnode.create(1,voidpointertype))),
                            ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node),
                            nil));
                      end
                    else
                      internalerror(200305108);
                  end
                else
                  if is_object(current_procdef._class) then
                    begin
                      { finalize object data }
                      if current_procdef._class.needs_inittable then
                        addstatement(newstatement,finalize_data_node(load_self_node));
                      { parameter 3 : vmt_offset }
                      { parameter 2 : pointer to vmt }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(current_procdef._class.vmt_offset,s32bittype,false),
                            ccallparanode.create(
                                ctypeconvnode.create_explicit(
                                    load_vmt_pointer_node,
                                    voidpointertype),
                            ccallparanode.create(
                                ctypeconvnode.create_explicit(
                                    load_self_pointer_node,
                                    voidpointertype),
                            nil)));
                      addstatement(newstatement,
                          ccallnode.createintern('fpc_help_destructor',para));
                    end
                else
                  internalerror(200305105);
              end;
          end;
      end;


    function generate_except_block:tnode;
      var
        pd : tprocdef;
        newstatement : tstatementnode;
      begin
        generate_except_block:=internalstatements(newstatement,true);

        { a constructor needs call destructor (if available) when it
          is not inherited }
        if assigned(current_procdef._class) and
           (current_procdef.proctypeoption=potype_constructor) then
          begin
            pd:=current_procdef._class.searchdestructor;
            if assigned(pd) then
              begin
                { if vmt<>0 then call destructor }
                addstatement(newstatement,cifnode.create(
                    caddnode.create(unequaln,
                        load_vmt_pointer_node,
                        cnilnode.create),
                    ccallnode.create(nil,tprocsym(pd.procsym),pd.procsym.owner,load_self_node),
                    nil));
              end;
          end
        else
          begin
            { no constructor }
            { must be the return value finalized before reraising the exception? }
            if (not is_void(current_procdef.rettype.def)) and
               (current_procdef.rettype.def.needs_inittable) and
               (not is_class(current_procdef.rettype.def)) then
              finalize_data_node(load_result_node);
          end;
      end;


    procedure add_entry_exit_code(var code:tnode;const entrypos,exitpos:tfileposinfo);
      var
        initializecode,
        finalizecode,
        entrycode,
        exitcode,
        exceptcode  : tnode;
        codeblock,
        newblock     : tblocknode;
        codestatement,
        newstatement : tstatementnode;
        oldfilepos   : tfileposinfo;
      begin
        oldfilepos:=aktfilepos;
        { Generate entry,exit and init,final blocks }
        aktfilepos:=entrypos;
        initializecode:=generate_initialize_block;
        entrycode:=generate_entry_block;
        aktfilepos:=exitpos;
        exitcode:=generate_exit_block;
        finalizecode:=generate_finalize_block;
        exceptcode:=generate_except_block;

        { Generate body of the procedure by combining entry+body+exit }
        codeblock:=internalstatements(codestatement,true);
        addstatement(codestatement,entrycode);
        addstatement(codestatement,code);
        addstatement(codestatement,exitcode);

        { Generate procedure by combining init+body+final,
          depending on the implicit finally we need to add
          an try...finally...end wrapper }
        newblock:=internalstatements(newstatement,true);
        if (pi_needs_implicit_finally in current_procinfo.flags) and
           { but it's useless in init/final code of units }
           not(current_procdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
          begin
            addstatement(newstatement,initializecode);
            aktfilepos:=entrypos;
            addstatement(newstatement,ctryfinallynode.create_implicit(
               codeblock,
               finalizecode,
               exceptcode));
          end
        else
          begin
            addstatement(newstatement,initializecode);
            addstatement(newstatement,codeblock);
            addstatement(newstatement,finalizecode);
          end;
        resulttypepass(newblock);
        code:=newblock;
        aktfilepos:=oldfilepos;
      end;


    procedure compile_proc_body(pd:tprocdef;make_global,parent_has_class:boolean);
      {
        Compile the body of a procedure
      }
      var
         oldexitlabel,oldexit2label : tasmlabel;
         oldquickexitlabel:tasmlabel;
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
         oldprocdef : tprocdef;
      begin
         oldprocdef:=current_procdef;
         current_procdef:=pd;

         { calculate the lexical level }
         if current_procdef.parast.symtablelevel>maxnesting then
           Message(parser_e_too_much_lexlevel);

         { static is also important for local procedures !! }
         if (po_staticmethod in current_procdef.procoptions) then
           allow_only_static:=true
         else if (current_procdef.parast.symtablelevel=normal_function_level) then
           allow_only_static:=false;

         { save old labels }
         oldexitlabel:=aktexitlabel;
         oldexit2label:=aktexit2label;
         oldquickexitlabel:=quickexitlabel;
         { get new labels }
         objectlibrary.getlabel(aktexitlabel);
         objectlibrary.getlabel(aktexit2label);
         { exit for fail in constructors }
         if (current_procdef.proctypeoption=potype_constructor) then
           objectlibrary.getlabel(quickexitlabel);
         { reset break and continue labels }
         block_type:=bt_general;
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;
    {$ifdef state_tracking}
{    aktstate:=Tstate_storage.create;}
    {$endif state_tracking}

         { insert symtables for the class, but only if it is no nested function }
         if assigned(current_procdef._class) and not(parent_has_class) then
           begin
             { insert them in the reverse order }
             hp:=nil;
             repeat
               _class:=current_procdef._class;
               while _class.childof<>hp do
                 _class:=_class.childof;
               hp:=_class;
               _class.symtable.next:=symtablestack;
               symtablestack:=_class.symtable;
             until hp=current_procdef._class;
           end;

         { insert parasymtable in symtablestack when parsing
           a function }
         if current_procdef.parast.symtablelevel>=normal_function_level then
           begin
              current_procdef.parast.next:=symtablestack;
              symtablestack:=current_procdef.parast;
           end;
         { create a local symbol table for this routine }
         if not assigned(current_procdef.localst) then
            current_procdef.insert_localst;
         { insert localsymtable in symtablestack}
         current_procdef.localst.next:=symtablestack;
         symtablestack:=current_procdef.localst;
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
         { get a better entry point }
         if assigned(code) then
           entrypos:=code.fileinfo;
         { save exit info }
         exitswitches:=aktlocalswitches;
         exitpos:=last_endtoken_filepos;
         { save current filepos }
         savepos:=aktfilepos;
         { add implicit entry and exit code }
         if assigned(code) then
           add_entry_exit_code(code,entrypos,exitpos);
         { store a copy of the original tree for inline, for
           normal procedures only store a reference to the
           current tree }
         if (current_procdef.proccalloption=pocall_inline) then
           current_procdef.code:=code.getcopy
         else
           current_procdef.code:=code;

         {When we are called to compile the body of a unit, aktprocsym should
          point to the unit initialization. If the unit has no initialization,
          aktprocsym=nil. But in that case code=nil. Thus we should check for
          code=nil, when we use aktprocsym.}

         { set the start offset to the start of the temp area in the stack }
         tg.setfirsttemp(current_procinfo.firsttemp_offset);

         { ... and generate assembler }
         { but set the right switches for entry !! }
         aktlocalswitches:=entryswitches;
         oldaktmaxfpuregisters:=aktmaxfpuregisters;
         aktmaxfpuregisters:=localmaxfpuregisters;
         if assigned(code) then
          begin
            { the procedure is now defined }
            current_procdef.forwarddef:=false;

            if paraprintnodetree=1 then
              printnode_procdef(current_procdef);

            { only generate the code if no type errors are found, else
              finish at least the type checking pass }
{$ifndef NOPASS2}
            if (status.errorcount=0) then
              begin
                generatecode(code);
                { first generate entry code with the correct position and switches }
                aktfilepos:=entrypos;
                aktlocalswitches:=entryswitches;
                genentrycode(current_procinfo.aktentrycode,make_global,0,parasize,nostackframe,false);

                { FPC_POPADDRSTACK destroys all registers (JM) }
                if (pi_needs_implicit_finally in current_procinfo.flags) or
                   (pi_uses_exceptions in current_procinfo.flags) then
                 begin
                   rg.usedinproc := ALL_REGISTERS;
                 end;

                { now generate exit code with the correct position and switches }
                aktfilepos:=exitpos;
                aktlocalswitches:=exitswitches;
                genexitcode(current_procinfo.aktexitcode,parasize,nostackframe,false);

                { now all the registers used are known }
                current_procdef.usedintregisters:=rg.usedintinproc;
                current_procdef.usedotherregisters:=rg.usedinproc;
                current_procinfo.aktproccode.insertlist(current_procinfo.aktentrycode);
                current_procinfo.aktproccode.concatlist(current_procinfo.aktexitcode);
{$ifdef newra}
{                rg.writegraph;}
{$endif}
                if not(cs_no_regalloc in aktglobalswitches) then
                  begin
{$ifdef newra}
                    {Do register allocation.}
                    repeat
                      rg.prepare_colouring;
                      rg.colour_registers;
                      rg.epilogue_colouring;
                      {Are there spilled registers? We cannot do that yet.}
                      if rg.spillednodes<>'' then
                        internalerror(200304221);
                      {if not try_fast_spill(rg) then
                        slow_spill(rg);
                      }
                    until rg.spillednodes='';
                    current_procinfo.aktproccode.translate_registers(rg.colour);
                    current_procinfo.aktproccode.convert_registers;
{$else newra}
                    current_procinfo.aktproccode.convert_registers;
{$ifndef NoOpt}
                    if (cs_optimize in aktglobalswitches) and
                    { do not optimize pure assembler procedures }
                       not(pi_is_assembler in current_procinfo.flags)  then
                      optimize(current_procinfo.aktproccode);
{$endif NoOpt}
{$endif newra}
                  end;
                { save local data (casetable) also in the same file }
                if assigned(current_procinfo.aktlocaldata) and
                   (not current_procinfo.aktlocaldata.empty) then
                 begin
                   current_procinfo.aktproccode.concat(Tai_section.Create(sec_data));
                   current_procinfo.aktproccode.concatlist(current_procinfo.aktlocaldata);
                   current_procinfo.aktproccode.concat(Tai_section.Create(sec_code));
                end;

                { add the procedure to the codesegment }
                if (cs_create_smart in aktmoduleswitches) then
                 codeSegment.concat(Tai_cut.Create);
                codeSegment.concatlist(current_procinfo.aktproccode);
              end
            else
              do_resulttypepass(code);
{$else NOPASS2}
            do_resulttypepass(code);
{$endif NOPASS2}
          end;

         { ... remove symbol tables }
         if current_procdef.parast.symtablelevel>=normal_function_level then
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
                 tstoredsymtable(current_procdef.localst).check_forwards;
                 { check if all labels are used }
                 tstoredsymtable(current_procdef.localst).checklabels;
                 { remove cross unit overloads }
                 tstoredsymtable(current_procdef.localst).unchain_overloaded;
               end;
             if not(pi_uses_asm in current_procinfo.flags) then
               begin
                  { not for unit init, becuase the var can be used in finalize,
                    it will be done in proc_unit }
                  if not(current_procdef.proctypeoption
                     in [potype_proginit,potype_unitinit,potype_unitfinalize]) then
                     tstoredsymtable(current_procdef.localst).allsymbolsused;
                  tstoredsymtable(current_procdef.parast).allsymbolsused;
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
            (current_procdef.proccalloption<>pocall_inline) then
           begin
             if current_procdef.parast.symtablelevel>=normal_function_level then
               current_procdef.localst.free;
             current_procdef.localst:=nil;
           end;

         { all registers can be used again }
         rg.resetusableregisters;
         { only now we can remove the temps }
         tg.resettempgen;

         { remove code tree, if not inline procedure }
         if assigned(code) then
          begin
            { the inline procedure has already got a copy of the tree
              stored in current_procdef.code }
            code.free;
            if (current_procdef.proccalloption<>pocall_inline) then
              current_procdef.code:=nil;
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

         { reset to normal non static function }
         if (current_procdef.parast.symtablelevel=normal_function_level) then
           allow_only_static:=false;

         current_procdef:=oldprocdef;
      end;


{****************************************************************************
                        PROCEDURE/FUNCTION PARSING
****************************************************************************}

    procedure insert_local_value_para(p:tnamedindexitem;arg:pointer);
      var
        vs : tvarsym;
        pd : tprocdef;
      begin
        if tsym(p).typ<>varsym then
         exit;
        with tvarsym(p) do
         begin
           if copy(name,1,3)='val' then
            begin
              pd:=tprocdef(owner.defowner);
              vs:=tvarsym.create(Copy(name,4,255),varspez,vartype);
              vs.fileinfo:=fileinfo;
              if not assigned(pd.localst) then
                pd.insert_localst;
              pd.localst.insert(vs);
              pd.localst.insertvardata(vs);
              include(vs.varoptions,vo_is_local_copy);
              vs.varstate:=vs_assigned;
              localvarsym:=vs;
              inc(refs); { the para was used to set the local copy ! }
              { warnings only on local copy ! }
              varstate:=vs_used;
            end;
         end;
      end;


    procedure read_proc;
      {
        Parses the procedure directives, then parses the procedure body, then
        generates the code for it
      }
      var
        oldprocdef       : tprocdef;
        old_current_procinfo : tprocinfo;
        oldconstsymtable : tsymtable;
        oldselftokenmode,
        oldfailtokenmode : tmodeswitch;
        pdflags          : word;
        pd               : tprocdef;
      begin
         { save old state }
         oldprocdef:=current_procdef;
         oldconstsymtable:=constsymtable;
         old_current_procinfo:=current_procinfo;

         { reset current_procdef to nil to be sure that nothing is writing
           to an other procdef }
         current_procdef:=nil;

         { create a new procedure }
         current_procinfo:=cprocinfo.create(old_current_procinfo);
         current_module.procinfo:=current_procinfo;

         { parse procedure declaration }
         if assigned(current_procinfo.parent) and
            assigned(current_procinfo.parent.procdef) then
          pd:=parse_proc_dec(current_procinfo.parent.procdef._class)
         else
          pd:=parse_proc_dec(nil);
         current_procinfo.procdef:=pd;

         { set the default function options }
         if parse_only then
          begin
            pd.forwarddef:=true;
            { set also the interface flag, for better error message when the
              implementation doesn't much this header }
            pd.interfacedef:=true;
            pdflags:=pd_interface;
          end
         else
          begin
            pdflags:=pd_body;
            if (not current_module.in_interface) then
             pdflags:=pdflags or pd_implemen;
            if (not current_module.is_unit) or (cs_create_smart in aktmoduleswitches) then
             pdflags:=pdflags or pd_global;
            pd.forwarddef:=false;
          end;

         { parse the directives that may follow }
         parse_proc_directives(pd,pdflags);

         { hint directives, these can be separated by semicolons here,
           that needs to be handled here with a loop (PFV) }
         while try_consume_hintdirective(pd.symoptions) do
          Consume(_SEMICOLON);

         { everything of the proc definition is known, we can now
           calculate the parameters }
         calc_parast(pd);

         { search for forward declarations }
         if not proc_add_definition(pd) then
           begin
             { A method must be forward defined (in the object declaration) }
             if assigned(pd._class) and
                (not assigned(current_procinfo.parent.procdef._class)) then
              begin
                Message1(parser_e_header_dont_match_any_member,pd.fullprocname(false));
                tprocsym(pd.procsym).write_parameter_lists(pd);
              end
             else
              begin
                { Give a better error if there is a forward def in the interface and only
                  a single implementation }
                if (not pd.forwarddef) and
                   (not pd.interfacedef) and
                   (tprocsym(pd.procsym).procdef_count>1) and
                   tprocsym(pd.procsym).first_procdef.forwarddef and
                   tprocsym(pd.procsym).first_procdef.interfacedef and
                   not(tprocsym(pd.procsym).procdef_count>2) then
                 begin
                   Message1(parser_e_header_dont_match_forward,pd.fullprocname(false));
                   tprocsym(pd.procsym).write_parameter_lists(pd);
                 end
                else
                 begin
                   { check the global flag, for delphi this is not
                     required }
                   if not(m_delphi in aktmodeswitches) and
                      (pi_is_global in current_procinfo.flags) then
                     Message(parser_e_overloaded_must_be_all_global);
                 end;
              end;
           end;

         { update procinfo, because the procdef can be
           changed by check_identical_proc (PFV) }
         current_procinfo.procdef:=pd;

         { compile procedure when a body is needed }
         if (pdflags and pd_body)<>0 then
          begin
            Message1(parser_d_procedure_start,pd.fullprocname(false));
            pd.aliasnames.insert(pd.mangledname);

            { Insert result variables in the localst }
            insert_funcret_local(pd);

            { Insert local copies for value para }
            pd.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}insert_local_value_para,nil);

{$ifdef i386}
            { add implicit pushes for interrupt routines }
            if (po_interrupt in pd.procoptions) then
              current_procinfo.allocate_interrupt_stackframe;
{$endif i386}

{$ifdef powerpc}
            { temp hack for nested procedures on ppc }

            { Calculate offsets }
            current_procinfo.after_header;

            { Update parameter information }
            current_procinfo.allocate_implicit_parameter;
{$else powerpc}
            { Update parameter information }
            current_procinfo.allocate_implicit_parameter;

            { Calculate offsets }
            current_procinfo.after_header;
{$endif powerpc}

            { set _FAIL as keyword if constructor }
            if (pd.proctypeoption=potype_constructor) then
             begin
               oldfailtokenmode:=tokeninfo^[_FAIL].keyword;
               tokeninfo^[_FAIL].keyword:=m_all;
             end;
            { set _SELF as keyword if methods }
            if assigned(pd._class) then
             begin
               oldselftokenmode:=tokeninfo^[_SELF].keyword;
               tokeninfo^[_SELF].keyword:=m_all;
             end;

            compile_proc_body(pd,((pdflags and pd_global)<>0),assigned(current_procinfo.parent.procdef._class));

            { reset _FAIL as _SELF normal }
            if (pd.proctypeoption=potype_constructor) then
              tokeninfo^[_FAIL].keyword:=oldfailtokenmode;
            if assigned(pd._class) then
              tokeninfo^[_SELF].keyword:=oldselftokenmode;
             consume(_SEMICOLON);
          end;

         { release procinfo }
         if tprocinfo(current_module.procinfo)<>current_procinfo then
          internalerror(200304274);
         current_module.procinfo:=current_procinfo.parent;
         current_procinfo.free;

         { Restore old state }
         constsymtable:=oldconstsymtable;

         current_procdef:=oldprocdef;
         current_procinfo:=old_current_procinfo;
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
           if (current_procdef.proccalloption=pocall_inline) then
             Begin
                Message1(parser_w_not_supported_for_inline,tokenstring(t));
                Message(parser_w_inlining_disabled);
                current_procdef.proccalloption:=pocall_fpccall;
             End;
        end;

      begin
         repeat
           if not assigned(current_procdef) then
             internalerror(200304251);
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
                   if not(assigned(current_procdef.localst)) or
                      (current_procdef.localst.symtablelevel>main_program_level) or
                      (current_module.is_unit) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or
                           (target_info.system in [system_i386_WIN32,system_i386_wdosx,system_i386_Netware]) then
                     read_exports
                   else
                     begin
                        Message(parser_w_unsupported_feature);
                        consume(_BEGIN);
                     end;
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
         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
          symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_forward_class,nil);
      end;

end.
{
  $Log$
  Revision 1.114  2003-05-16 20:00:39  jonas
    * powerpc nested procedure fixes, should work completely now if all
      local variables of the parent procedure are declared before the
      nested procedures are declared

  Revision 1.113  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.112  2003/05/13 21:26:38  peter
    * only call destructor in except block when there is a destructor
      available

  Revision 1.111  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.110  2003/05/13 15:18:49  peter
    * fixed various crashes

  Revision 1.109  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.108  2003/05/09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.107  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.106  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.105  2003/04/26 00:31:42  peter
    * set return_offset moved to after_header

  Revision 1.104  2003/04/25 20:59:34  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.103  2003/04/24 13:03:01  florian
    * comp is now written with its bit pattern to the ppu instead as an extended

  Revision 1.102  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.101  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.100  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.99  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.98  2003/04/17 07:50:24  daniel
    * Some work on interference graph construction

  Revision 1.97  2003/04/16 09:26:55  jonas
    * assembler procedures now again get a stackframe if they have local
      variables. No space is reserved for a function result however.
      Also, the register parameters aren't automatically saved on the stack
      anymore in assembler procedures.

  Revision 1.96  2003/04/05 21:09:31  jonas
    * several ppc/generic result offset related fixes. The "normal" result
      offset seems now to be calculated correctly and a lot of duplicate
      calculations have been removed. Nested functions accessing the parent's
      function result don't work at all though :(

  Revision 1.95  2003/04/02 16:11:34  peter
    * give error when exports is not supported

  Revision 1.94  2003/03/12 22:43:38  jonas
    * more powerpc and generic fixes related to the new register allocator

  Revision 1.93  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.92  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.91  2003/01/09 21:52:37  peter
    * merged some verbosity options.
    * V_LineInfo is a verbosity flag to include line info

  Revision 1.90  2003/01/09 20:40:59  daniel
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
    * call to current_procinfo.after_header added

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
