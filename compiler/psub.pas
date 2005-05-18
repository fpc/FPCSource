{
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
      cclasses,globals,
      node,nbas,
      symdef,procinfo;

    type
      tcgprocinfo = class(tprocinfo)
      private
        procedure add_entry_exit_code;
      public
        { code for the subroutine as tree }
        code : tnode;
        { positions in the tree for init/final }
        entry_asmnode,
        loadpara_asmnode,
        exitlabel_asmnode,
        stackcheck_asmnode,
        init_asmnode,
        final_asmnode : tasmnode;
        { list to store the procinfo's of the nested procedures }
        nestedprocs : tlinkedlist;
        constructor create(aparent:tprocinfo);override;
        destructor  destroy;override;
        procedure printproc;
        procedure generate_code;
        procedure resetprocdef;
        procedure add_to_symtablestack;
        procedure remove_from_symtablestack;
        procedure parse_body;
      end;


    procedure printnode_reset;

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;



implementation

    uses
       { common }
       cutils,
       { global }
       globtype,tokens,verbose,comphook,
       systems,
       { aasm }
       cpubase,aasmbase,aasmtai,
       { symtable }
       symconst,symbase,symsym,symtype,symtable,defutil,
       paramgr,
       ppu,fmodule,
       { pass 1 }
       nutils,nld,ncal,ncon,nflw,nadd,ncnv,nmem,
       pass_1,
    {$ifdef state_tracking}
       nstate,
    {$endif state_tracking}
       { pass 2 }
{$ifndef NOPASS2}
       pass_2,
{$endif}
       { parser }
       scanner,import,gendef,
       pbase,pstatmnt,pdecl,pdecsub,pexports,
       { codegen }
       tgobj,cgobj,
       ncgutil,regvars
{$ifdef arm}
       ,aasmcpu
{$endif arm}
       {$ifndef NOOPT}
         {$ifdef i386}
           ,aopt386
         {$else i386}
           ,aopt
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
        if not (tsym(p).typ in [localvarsym,globalvarsym]) then
         exit;
        with tabstractnormalvarsym(p) do
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


    procedure check_finalize_paras(p : tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ=paravarsym) and
           (tparavarsym(p).varspez=vs_value) and
           not is_class(tparavarsym(p).vartype.def) and
           tparavarsym(p).vartype.def.needs_inittable then
          include(current_procinfo.flags,pi_needs_implicit_finally);
      end;


    procedure check_finalize_locals(p : tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ=localvarsym) and
           (tlocalvarsym(p).refs>0) and
           not(vo_is_funcret in tlocalvarsym(p).varoptions) and
           not(is_class(tlocalvarsym(p).vartype.def)) and
           tlocalvarsym(p).vartype.def.needs_inittable then
          include(current_procinfo.flags,pi_needs_implicit_finally);
      end;


    function block(islibrary : boolean) : tnode;
      begin
         { parse const,types and vars }
         read_declarations(islibrary);

         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in aktmodeswitches) then
          include(current_procinfo.procdef.procoptions,po_assembler);

         { Handle assembler block different }
         if (po_assembler in current_procinfo.procdef.procoptions) then
          begin
            block:=assembler_block;
            exit;
          end;

         {Unit initialization?.}
         if (
             assigned(current_procinfo.procdef.localst) and
             (current_procinfo.procdef.localst.symtablelevel=main_program_level) and
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
                 symtablestack.foreach_static(@initializevars,block);
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


    function generate_bodyentry_block:tnode;
      var
        srsym        : tsym;
        para         : tcallparanode;
        newstatement : tstatementnode;
        htype        : ttype;
      begin
        result:=internalstatements(newstatement);

        if assigned(current_procinfo.procdef._class) then
          begin
            { a constructor needs a help procedure }
            if (current_procinfo.procdef.proctypeoption=potype_constructor) then
              begin
                if is_class(current_procinfo.procdef._class) then
                  begin
                    include(current_procinfo.flags,pi_needs_implicit_finally);
                    srsym:=search_class_member(current_procinfo.procdef._class,'NEWINSTANCE');
                    if assigned(srsym) and
                       (srsym.typ=procsym) then
                      begin
                        { if vmt>1 then newinstance }
                        addstatement(newstatement,cifnode.create(
                            caddnode.create(gtn,
                                ctypeconvnode.create_internal(
                                    load_vmt_pointer_node,
                                    voidpointertype),
                                cpointerconstnode.create(1,voidpointertype)),
                            cassignmentnode.create(
                                ctypeconvnode.create_internal(
                                    load_self_pointer_node,
                                    voidpointertype),
                                ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_vmt_pointer_node,[])),
                            nil));
                      end
                    else
                      internalerror(200305108);
                  end
                else
                  if is_object(current_procinfo.procdef._class) then
                    begin
                      htype.setdef(current_procinfo.procdef._class);
                      htype.setdef(tpointerdef.create(htype));
                      { parameter 3 : vmt_offset }
                      { parameter 2 : address of pointer to vmt,
                        this is required to allow setting the vmt to -1 to indicate
                        that memory was allocated }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(current_procinfo.procdef._class.vmt_offset,s32inttype,false),
                            ccallparanode.create(
                                ctypeconvnode.create_internal(
                                    load_vmt_pointer_node,
                                    voidpointertype),
                            ccallparanode.create(
                                ctypeconvnode.create_internal(
                                    load_self_pointer_node,
                                    voidpointertype),
                            nil)));
                      addstatement(newstatement,cassignmentnode.create(
                          ctypeconvnode.create_internal(
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
            if (current_procinfo.procdef.proctypeoption=potype_destructor) and
               is_class(current_procinfo.procdef._class) then
              begin
                srsym:=search_class_member(current_procinfo.procdef._class,'BEFOREDESTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    { if vmt<>0 then beforedestruction }
                    addstatement(newstatement,cifnode.create(
                        caddnode.create(unequaln,
                            load_vmt_pointer_node,
                            cnilnode.create),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                        nil));
                  end
                else
                  internalerror(200305104);
              end;
          end;
      end;


    function generate_bodyexit_block:tnode;
      var
        srsym : tsym;
        para : tcallparanode;
        newstatement : tstatementnode;
      begin
        result:=internalstatements(newstatement);

        if assigned(current_procinfo.procdef._class) then
          begin
            { maybe call AfterConstruction for classes }
            if (current_procinfo.procdef.proctypeoption=potype_constructor) and
               is_class(current_procinfo.procdef._class) then
              begin
                srsym:=search_class_member(current_procinfo.procdef._class,'AFTERCONSTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    { Self can be nil when fail is called }
                    { if self<>nil and vmt<>nil then afterconstruction }
                    addstatement(newstatement,cifnode.create(
                        caddnode.create(andn,
                            caddnode.create(unequaln,
                                load_self_pointer_node,
                                cnilnode.create),
                            caddnode.create(unequaln,
                                load_vmt_pointer_node,
                                cnilnode.create)),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                        nil));
                  end
                else
                  internalerror(200305106);
              end;

            { a destructor needs a help procedure }
            if (current_procinfo.procdef.proctypeoption=potype_destructor) then
              begin
                if is_class(current_procinfo.procdef._class) then
                  begin
                    srsym:=search_class_member(current_procinfo.procdef._class,'FREEINSTANCE');
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
                            ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                            nil));
                      end
                    else
                      internalerror(200305108);
                  end
                else
                  if is_object(current_procinfo.procdef._class) then
                    begin
                      { finalize object data }
                      if current_procinfo.procdef._class.needs_inittable then
                        addstatement(newstatement,finalize_data_node(load_self_node));
                      { parameter 3 : vmt_offset }
                      { parameter 2 : pointer to vmt }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(current_procinfo.procdef._class.vmt_offset,s32inttype,false),
                            ccallparanode.create(
                                ctypeconvnode.create_internal(
                                    load_vmt_pointer_node,
                                    voidpointertype),
                            ccallparanode.create(
                                ctypeconvnode.create_internal(
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
        generate_except_block:=internalstatements(newstatement);

        { a constructor needs call destructor (if available) when it
          is not inherited }
        if assigned(current_procinfo.procdef._class) and
           (current_procinfo.procdef.proctypeoption=potype_constructor) then
          begin
            pd:=current_procinfo.procdef._class.searchdestructor;
            if assigned(pd) then
              begin
                { if vmt<>0 then call destructor }
                addstatement(newstatement,cifnode.create(
                    caddnode.create(unequaln,
                        load_vmt_pointer_node,
                        cnilnode.create),
                    ccallnode.create(nil,tprocsym(pd.procsym),pd.procsym.owner,load_self_node,[]),
                    nil));
              end;
          end
        else
          begin
            { no constructor }
            { must be the return value finalized before reraising the exception? }
            if (not is_void(current_procinfo.procdef.rettype.def)) and
               (current_procinfo.procdef.rettype.def.needs_inittable) and
               (not is_class(current_procinfo.procdef.rettype.def)) then
              addstatement(newstatement,finalize_data_node(load_result_node));
          end;
      end;


{****************************************************************************
                                  TCGProcInfo
****************************************************************************}

    constructor tcgprocinfo.create(aparent:tprocinfo);
      begin
        inherited Create(aparent);
        nestedprocs:=tlinkedlist.create;
      end;


     destructor tcgprocinfo.destroy;
       begin
         nestedprocs.free;
         if assigned(code) then
           code.free;
         inherited destroy;
       end;


    procedure tcgprocinfo.printproc;
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
        writeln(printnodefile,procdef.fullprocname(false));
        writeln(printnodefile,'*******************************************************************************');
        printnode(printnodefile,code);
        close(printnodefile);
      end;


    procedure tcgprocinfo.add_entry_exit_code;
      var
        finalcode,
        bodyentrycode,
        bodyexitcode,
        exceptcode   : tnode;
        newblock     : tblocknode;
        codestatement,
        newstatement : tstatementnode;
        oldfilepos   : tfileposinfo;
      begin
        oldfilepos:=aktfilepos;
        { Generate code/locations used at start of proc }
        aktfilepos:=entrypos;
        entry_asmnode:=casmnode.create_get_position;
        loadpara_asmnode:=casmnode.create_get_position;
        stackcheck_asmnode:=casmnode.create_get_position;
        init_asmnode:=casmnode.create_get_position;
        bodyentrycode:=generate_bodyentry_block;
        { Generate code/locations used at end of proc }
        aktfilepos:=exitpos;
        exitlabel_asmnode:=casmnode.create_get_position;
        final_asmnode:=casmnode.create_get_position;
        bodyexitcode:=generate_bodyexit_block;

        { Generate procedure by combining init+body+final,
          depending on the implicit finally we need to add
          an try...finally...end wrapper }
        newblock:=internalstatements(newstatement);
        if (cs_implicit_exceptions in aktmoduleswitches) and
           (pi_needs_implicit_finally in flags) and
           { but it's useless in init/final code of units }
           not(procdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
          begin
            { Generate special exception block only needed when
              implicit finaly is used }
            aktfilepos:=exitpos;
            exceptcode:=generate_except_block;
            { Generate code that will be in the try...finally }
            finalcode:=internalstatements(codestatement);
            addstatement(codestatement,bodyexitcode);
            addstatement(codestatement,final_asmnode);
            { Initialize before try...finally...end frame }
            addstatement(newstatement,loadpara_asmnode);
            addstatement(newstatement,stackcheck_asmnode);
            addstatement(newstatement,entry_asmnode);
            addstatement(newstatement,init_asmnode);
            addstatement(newstatement,bodyentrycode);
            aktfilepos:=entrypos;
            addstatement(newstatement,ctryfinallynode.create_implicit(
               code,
               finalcode,
               exceptcode));
            addstatement(newstatement,exitlabel_asmnode);
            { set flag the implicit finally has been generated }
            include(flags,pi_has_implicit_finally);
          end
        else
          begin
            addstatement(newstatement,loadpara_asmnode);
            addstatement(newstatement,stackcheck_asmnode);
            addstatement(newstatement,entry_asmnode);
            addstatement(newstatement,init_asmnode);
            addstatement(newstatement,bodyentrycode);
            addstatement(newstatement,code);
            addstatement(newstatement,exitlabel_asmnode);
            addstatement(newstatement,bodyexitcode);
            addstatement(newstatement,final_asmnode);
          end;
        do_firstpass(newblock);
        code:=newblock;
        aktfilepos:=oldfilepos;
      end;


    procedure clearrefs(p : tnamedindexitem;arg:pointer);
      begin
         if (tsym(p).typ in [localvarsym,paravarsym,globalvarsym]) then
           if tabstractvarsym(p).refs>1 then
             tabstractvarsym(p).refs:=1;
      end;


    procedure tcgprocinfo.generate_code;
      var
        oldprocinfo : tprocinfo;
        oldaktmaxfpuregisters : longint;
        oldfilepos : tfileposinfo;
        templist : Taasmoutput;
        headertai : tai;
      begin
        { the initialization procedure can be empty, then we
          don't need to generate anything. When it was an empty
          procedure there would be at least a blocknode }
        if not assigned(code) then
          exit;

        { We need valid code }
        if Errorcount<>0 then
          exit;

        { The RA and Tempgen shall not be available yet }
        if assigned(tg) then
          internalerror(200309201);

        oldprocinfo:=current_procinfo;
        oldfilepos:=aktfilepos;
        oldaktmaxfpuregisters:=aktmaxfpuregisters;

        current_procinfo:=self;
        aktfilepos:=entrypos;

        { get new labels }
        aktbreaklabel:=nil;
        aktcontinuelabel:=nil;
        templist:=Taasmoutput.create;

        { add parast/localst to symtablestack }
        add_to_symtablestack;

        { when size optimization only count occurrence }
        if cs_littlesize in aktglobalswitches then
          cg.t_times:=1
        else
          { reference for repetition is 100 }
          cg.t_times:=100;

        { clear register count }
        symtablestack.foreach_static(@clearrefs,nil);
        symtablestack.next.foreach_static(@clearrefs,nil);

        { there's always a call to FPC_INITIALIZEUNITS/FPC_DO_EXIT in the main program }
        if (procdef.localst.symtablelevel=main_program_level) and
           (not current_module.is_unit) then
          include(flags,pi_do_call);

        { set implicit_finally flag when there are locals/paras to be finalized }
        current_procinfo.procdef.parast.foreach_static(@check_finalize_paras,nil);
        current_procinfo.procdef.localst.foreach_static(@check_finalize_locals,nil);

        { firstpass everything }
        flowcontrol:=[];
        do_firstpass(code);
        if code.registersfpu>0 then
          include(current_procinfo.flags,pi_uses_fpu);

        { add implicit entry and exit code }
        add_entry_exit_code;

        { only do secondpass if there are no errors }
        if ErrorCount=0 then
          begin
            { set the start offset to the start of the temp area in the stack }
            tg:=ttgobj.create;

            { Create register allocator }
            cg.init_register_allocators;

            set_first_temp_offset;
            generate_parameter_info;

            { Allocate space in temp/registers for parast and localst }
            aktfilepos:=entrypos;
            gen_alloc_symtable(aktproccode,procdef.parast);
            gen_alloc_symtable(aktproccode,procdef.localst);

            { Store temp offset for information about 'real' temps }
            tempstart:=tg.lasttemp;

            { Generate code to load register parameters in temps and insert local
              copies for values parameters. This must be done before the code for the
              body is generated because the localloc is updated.
              Note: The generated code will be inserted after the code generation of
              the body is finished, because only then the position is known }
{$ifdef oldregvars}
            assign_regvars(code);
{$endif oldreg}
            aktfilepos:=entrypos;
            gen_load_para_value(templist);

            { caller paraloc info is also necessary in the stackframe_entry
              code of the ppc (and possibly other processors)               }
            if not procdef.has_paraloc_info then
              begin
                procdef.requiredargarea:=paramanager.create_paraloc_info(procdef,callerside);
                procdef.has_paraloc_info:=true;
              end;

            { generate code for the node tree }
            do_secondpass(code);
            aktproccode.concatlist(exprasmlist);
{$ifdef i386}
            procdef.fpu_used:=code.registersfpu;
{$endif i386}

            { The position of the loadpara_asmnode is now known }
            aktproccode.insertlistafter(loadpara_asmnode.currenttai,templist);

            { first generate entry and initialize code with the correct
              position and switches }
            aktfilepos:=entrypos;
            aktlocalswitches:=entryswitches;
            gen_entry_code(templist);
            aktproccode.insertlistafter(entry_asmnode.currenttai,templist);
            gen_initialize_code(templist);
            aktproccode.insertlistafter(init_asmnode.currenttai,templist);

            { now generate finalize and exit code with the correct position
              and switches }
            aktfilepos:=exitpos;
            aktlocalswitches:=exitswitches;
            gen_finalize_code(templist);
            { the finalcode must be concated if there was no position available,
              using insertlistafter will result in an insert at the start
              when currentai=nil }
            if assigned(final_asmnode.currenttai) then
              aktproccode.insertlistafter(final_asmnode.currenttai,templist)
            else
              aktproccode.concatlist(templist);
            { insert exit label at the correct position }
            cg.a_label(templist,aktexitlabel);
            if assigned(exitlabel_asmnode.currenttai) then
              aktproccode.insertlistafter(exitlabel_asmnode.currenttai,templist)
            else
              aktproccode.concatlist(templist);
            { exit code }
            gen_exit_code(templist);
            aktproccode.concatlist(templist);

{$ifdef OLDREGVARS}
            { note: this must be done only after as much code as possible has  }
            {   been generated. The result is that when you ungetregister() a  }
            {   regvar, it will actually free the regvar (and alse free the    }
            {   the regvars at the same time). Doing this too early will       }
            {   confuse the register allocator, as the regvars will still be   }
            {   used. It should be done before loading the result regs (so     }
            {   they don't conflict with the regvars) and before               }
            {   gen_entry_code (that one has to be able to allocate the        }
            {   regvars again) (JM)                                            }
            free_regvars(aktproccode);
{$endif OLDREGVARS}

            { add code that will load the return value, this is not done
              for assembler routines when they didn't reference the result
              variable }
            gen_load_return_value(templist);
            aktproccode.concatlist(templist);

            { generate symbol and save end of header position }
            aktfilepos:=entrypos;
            gen_proc_symbol(templist);
            headertai:=tai(templist.last);
            { insert symbol }
            aktproccode.insertlist(templist);

            { Free space in temp/registers for parast and localst, must be
              done after gen_entry_code }
            aktfilepos:=exitpos;
            gen_free_symtable(aktproccode,procdef.localst);
            gen_free_symtable(aktproccode,procdef.parast);

            { Already reserve all registers for stack checking code and
              generate the call to the helper function }
            if (cs_check_stack in entryswitches) and
               not(po_assembler in procdef.procoptions) and
               (current_procinfo.procdef.proctypeoption<>potype_proginit) then
              begin
                aktfilepos:=entrypos;
                gen_stack_check_call(templist);
                aktproccode.insertlistafter(stackcheck_asmnode.currenttai,templist)
              end;

            { The procedure body is finished, we can now
              allocate the registers }
            cg.do_register_allocation(aktproccode,headertai);

            { Add save and restore of used registers }
            aktfilepos:=entrypos;
            gen_save_used_regs(templist);
            aktproccode.insertlistafter(headertai,templist);
            aktfilepos:=exitpos;
            gen_restore_used_regs(aktproccode);
            { We know the size of the stack, now we can generate the
              parameter that is passed to the stack checking code }
            if (cs_check_stack in entryswitches) and
               not(po_assembler in procdef.procoptions) and
               (current_procinfo.procdef.proctypeoption<>potype_proginit) then
              begin
                aktfilepos:=entrypos;
                gen_stack_check_size_para(templist);
                aktproccode.insertlistafter(stackcheck_asmnode.currenttai,templist)
              end;
            { Add entry code (stack allocation) after header }
            aktfilepos:=entrypos;
            gen_proc_entry_code(templist);
            aktproccode.insertlistafter(headertai,templist);
            { Add exit code at the end }
            aktfilepos:=exitpos;
            gen_proc_exit_code(templist);
            aktproccode.concatlist(templist);

            { check if the implicit finally has been generated. The flag
              should already be set in pass1 }
            if (cs_implicit_exceptions in aktmoduleswitches) and
               not(procdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) and
               (pi_needs_implicit_finally in flags) and
               not(pi_has_implicit_finally in flags) then
             internalerror(200405231);

{$ifndef NoOpt}
            if not(cs_no_regalloc in aktglobalswitches) then
              begin
                if (cs_optimize in aktglobalswitches) and
                   { do not optimize pure assembler procedures }
                   not(pi_is_assembler in flags)  then
                  optimize(aktproccode);
              end;
{$endif NoOpt}

            { Add end symbol and debug info }
            aktfilepos:=exitpos;
            gen_proc_symbol_end(templist);
            aktproccode.concatlist(templist);

{$ifdef ARM}
            insertpcrelativedata(aktproccode,aktlocaldata);
{$endif ARM}

            { save local data (casetable) also in the same file }
            if assigned(aktlocaldata) and
               (not aktlocaldata.empty) then
             begin
               { because of the limited constant size of the arm, all data access is done pc relative }
               if target_info.cpu=cpu_arm then
                 aktproccode.concatlist(aktlocaldata)
               else
                 begin
                   new_section(aktproccode,sec_data,lower(procdef.mangledname),0);
                   aktproccode.concatlist(aktlocaldata);
                 end;
            end;

            { add the procedure to the codesegment }
            maybe_new_object_file(codesegment);
            new_section(codesegment,sec_code,lower(procdef.mangledname),aktalignment.procalign);
            codesegment.concatlist(aktproccode);

            { only now we can remove the temps }
            tg.resettempgen;

            { stop tempgen and ra }
            tg.free;
            cg.done_register_allocators;
            tg:=nil;
          end;

        { restore symtablestack }
        remove_from_symtablestack;

        { restore }
        templist.free;
        aktmaxfpuregisters:=oldaktmaxfpuregisters;
        aktfilepos:=oldfilepos;
        current_procinfo:=oldprocinfo;
      end;


    procedure tcgprocinfo.add_to_symtablestack;
      var
        _class,hp : tobjectdef;
      begin
        { insert symtables for the class, but only if it is no nested function }
        if assigned(procdef._class) and
           not(assigned(parent) and
               assigned(parent.procdef) and
               assigned(parent.procdef._class)) then
          begin
            { insert them in the reverse order }
            hp:=nil;
            repeat
              _class:=procdef._class;
              while _class.childof<>hp do
                _class:=_class.childof;
              hp:=_class;
              _class.symtable.next:=symtablestack;
              symtablestack:=_class.symtable;
            until hp=procdef._class;
          end;

        { insert parasymtable in symtablestack when parsing
          a function }
        if procdef.parast.symtablelevel>=normal_function_level then
          begin
             procdef.parast.next:=symtablestack;
             symtablestack:=procdef.parast;
          end;

        procdef.localst.next:=symtablestack;
        symtablestack:=procdef.localst;
      end;


    procedure tcgprocinfo.remove_from_symtablestack;
      begin
        { remove localst/parast }
        if procdef.parast.symtablelevel>=normal_function_level then
          symtablestack:=symtablestack.next.next
        else
          symtablestack:=symtablestack.next;

        { remove class member symbol tables }
        while symtablestack.symtabletype=objectsymtable do
          symtablestack:=symtablestack.next;
      end;


    procedure tcgprocinfo.resetprocdef;
      begin
         { remove code tree, if not inline procedure }
         if assigned(code) then
          begin
            { the inline procedure has already got a copy of the tree
              stored in procdef.inlininginfo }
            code.free;
            code:=nil;
          end;
       end;


    function checknodeinlining(procdef: tprocdef): boolean;
      var
        i : integer;
        currpara : tparavarsym;
      begin
        result := false;
        if (pi_has_assembler_block in current_procinfo.flags) then
          exit;
        for i:=0 to procdef.paras.count-1 do
          begin
            currpara:=tparavarsym(procdef.paras[i]);
            { we can't handle formaldefs and special arrays (the latter may need a    }
            { re-basing of the index, i.e. if you pass an array[1..10] as open array, }
            { you have to add 1 to all index operations if you directly inline it     }
            if ((currpara.varspez in [vs_out,vs_var]) and
                (currpara.vartype.def.deftype=formaldef)) or
               is_special_array(currpara.vartype.def)  then
              exit;
          end;
        result:=true;
      end;


    procedure tcgprocinfo.parse_body;
      var
         oldprocinfo : tprocinfo;
         oldblock_type : tblock_type;
      begin
         oldprocinfo:=current_procinfo;
         oldblock_type:=block_type;
         { reset break and continue labels }
         block_type:=bt_body;

         current_procinfo:=self;

         { calculate the lexical level }
         if procdef.parast.symtablelevel>maxnesting then
           Message(parser_e_too_much_lexlevel);

         { static is also important for local procedures !! }
         if (po_staticmethod in procdef.procoptions) then
           allow_only_static:=true
         else if (procdef.parast.symtablelevel=normal_function_level) then
           allow_only_static:=false;

    {$ifdef state_tracking}
{    aktstate:=Tstate_storage.create;}
    {$endif state_tracking}

         { create a local symbol table for this routine }
         if not assigned(procdef.localst) then
           procdef.insert_localst;

         { add parast/localst to symtablestack }
         add_to_symtablestack;

         { constant symbols are inserted in this symboltable }
         constsymtable:=symtablestack;

         { save entry info }
         entrypos:=aktfilepos;
         entryswitches:=aktlocalswitches;

         { parse the code ... }
         code:=block(current_module.islibrary);
         { save exit info }
         exitswitches:=aktlocalswitches;
         exitpos:=last_endtoken_filepos;

         { the procedure is now defined }
         procdef.forwarddef:=false;

         if assigned(code) then
           begin
             { get a better entry point }
             entrypos:=code.fileinfo;

             { Finish type checking pass }
             do_resulttypepass(code);
           end;

         { Check for unused labels, forwards, symbols for procedures. Static
           symtable is checked in pmodules.
           The check must be done after the resulttypepass }
         if (Errorcount=0) and
            (tstoredsymtable(procdef.localst).symtabletype<>staticsymtable) then
           begin
             { check if forwards are resolved }
             tstoredsymtable(procdef.localst).check_forwards;
             { check if all labels are used }
             tstoredsymtable(procdef.localst).checklabels;
             { remove cross unit overloads }
             tstoredsymtable(procdef.localst).unchain_overloaded;
             { check for unused symbols, but only if there is no asm block }
             if not(pi_has_assembler_block in flags) then
               begin
                 tstoredsymtable(procdef.localst).allsymbolsused;
                 tstoredsymtable(procdef.parast).allsymbolsused;
               end;
           end;

         if (procdef.proccalloption=pocall_inline) then
           begin
             { Can we inline this procedure? }
             if checknodeinlining(procdef) then
               begin
                 new(procdef.inlininginfo);
                 include(procdef.procoptions,po_has_inlininginfo);
                 procdef.inlininginfo^.code:=code.getcopy;
                 procdef.inlininginfo^.flags:=current_procinfo.flags;
                 { The blocknode needs to set an exit label }
                 if procdef.inlininginfo^.code.nodetype=blockn then
                   include(procdef.inlininginfo^.code.flags,nf_block_with_exit);
               end;
           end;

         { Print the node to tree.log }
         if paraprintnodetree=1 then
           printproc;

         { ... remove symbol tables }
         remove_from_symtablestack;

    {$ifdef state_tracking}
{    aktstate.destroy;}
    {$endif state_tracking}

         { reset to normal non static function }
         if (procdef.parast.symtablelevel=normal_function_level) then
           allow_only_static:=false;
         current_procinfo:=oldprocinfo;

         block_type:=oldblock_type;
      end;


{****************************************************************************
                        PROCEDURE/FUNCTION PARSING
****************************************************************************}


    procedure check_init_paras(p:tnamedindexitem;arg:pointer);
      begin
        if tsym(p).typ<>paravarsym then
         exit;
        with tparavarsym(p) do
          if (not is_class(vartype.def) and
             vartype.def.needs_inittable and
             (varspez in [vs_value,vs_out])) then
            include(current_procinfo.flags,pi_do_call);
      end;


    procedure read_proc;
      {
        Parses the procedure directives, then parses the procedure body, then
        generates the code for it
      }

      procedure do_generate_code(pi:tcgprocinfo);
        var
          hpi : tcgprocinfo;
        begin
          { generate code for this procedure }
          pi.generate_code;
          { process nested procs }
          hpi:=tcgprocinfo(pi.nestedprocs.first);
          while assigned(hpi) do
           begin
             do_generate_code(hpi);
             hpi:=tcgprocinfo(hpi.next);
           end;
          pi.resetprocdef;
        end;

      var
        old_current_procinfo : tprocinfo;
        oldconstsymtable : tsymtable;
        oldfailtokenmode : tmodeswitch;
        pdflags          : tpdflags;
        pd               : tprocdef;
        isnestedproc     : boolean;
        s                : string;
      begin
         { save old state }
         oldconstsymtable:=constsymtable;
         old_current_procinfo:=current_procinfo;

         { reset current_procinfo.procdef to nil to be sure that nothing is writing
           to an other procdef }
         current_procinfo:=nil;

         { parse procedure declaration }
         if assigned(old_current_procinfo) and
            assigned(old_current_procinfo.procdef) then
          pd:=parse_proc_dec(old_current_procinfo.procdef._class)
         else
          pd:=parse_proc_dec(nil);

         { set the default function options }
         if parse_only then
          begin
            pd.forwarddef:=true;
            { set also the interface flag, for better error message when the
              implementation doesn't much this header }
            pd.interfacedef:=true;
            include(pd.procoptions,po_global);
            pdflags:=[pd_interface];
          end
         else
          begin
            pdflags:=[pd_body];
            if (not current_module.in_interface) then
              include(pdflags,pd_implemen);
            if (not current_module.is_unit) or
               maybe_smartlink_symbol then
              include(pd.procoptions,po_global);
            pd.forwarddef:=false;
          end;

         { parse the directives that may follow }
         parse_proc_directives(pd,pdflags);

         { hint directives, these can be separated by semicolons here,
           that needs to be handled here with a loop (PFV) }
         while try_consume_hintdirective(pd.symoptions) do
          Consume(_SEMICOLON);

         { Set calling convention }
         handle_calling_convention(pd);

         { search for forward declarations }
         if not proc_add_definition(pd) then
           begin
             { A method must be forward defined (in the object declaration) }
             if assigned(pd._class) and
                (not assigned(old_current_procinfo.procdef._class)) then
              begin
                MessagePos1(pd.fileinfo,parser_e_header_dont_match_any_member,pd.fullprocname(false));
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
                   MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,pd.fullprocname(false));
                   tprocsym(pd.procsym).write_parameter_lists(pd);
                 end;
              end;
           end;

         { Set mangled name }
         proc_set_mangledname(pd);

         { compile procedure when a body is needed }
         if (pd_body in pdflags) then
           begin
             Message1(parser_d_procedure_start,pd.fullprocname(false));

             { create a new procedure }
             current_procinfo:=cprocinfo.create(old_current_procinfo);
             current_module.procinfo:=current_procinfo;
             current_procinfo.procdef:=pd;
             isnestedproc:=(current_procinfo.procdef.parast.symtablelevel>normal_function_level);

             { Insert mangledname }
             pd.aliasnames.insert(pd.mangledname);

             { Handle Export of this procedure }
             if (po_exports in pd.procoptions) and
                (target_info.system in [system_i386_os2,system_i386_emx]) then
               begin
                 pd.aliasnames.insert(pd.procsym.realname);
                 if cs_link_deffile in aktglobalswitches then
                   deffile.AddExport(pd.mangledname);
               end;

             { Insert result variables in the localst }
             insert_funcret_local(pd);

             { check if there are para's which require initing -> set }
             { pi_do_call (if not yet set)                            }
             if not(pi_do_call in current_procinfo.flags) then
               pd.parast.foreach_static(@check_init_paras,nil);

             { set _FAIL as keyword if constructor }
             if (pd.proctypeoption=potype_constructor) then
              begin
                oldfailtokenmode:=tokeninfo^[_FAIL].keyword;
                tokeninfo^[_FAIL].keyword:=m_all;
              end;

             tcgprocinfo(current_procinfo).parse_body;

             { When it's a nested procedure then defer the code generation,
               when back at normal function level then generate the code
               for all defered nested procedures and the current procedure }
             if isnestedproc then
               tcgprocinfo(current_procinfo.parent).nestedprocs.insert(current_procinfo)
             else
               begin
                 { We can't support inlining for procedures that have nested
                   procedures because the nested procedures use a fixed offset
                   for accessing locals in the parent procedure (PFV) }
                 if (current_procinfo.procdef.proccalloption=pocall_inline) and
                    (tcgprocinfo(current_procinfo).nestedprocs.count>0) then
                   begin
                     Message1(parser_w_not_supported_for_inline,'nested procedures');
                     Message(parser_w_inlining_disabled);
                     current_procinfo.procdef.proccalloption:=pocall_default;
                   end;
                 do_generate_code(tcgprocinfo(current_procinfo));
               end;

             { reset _FAIL as _SELF normal }
             if (pd.proctypeoption=potype_constructor) then
               tokeninfo^[_FAIL].keyword:=oldfailtokenmode;

             { release procinfo }
             if tprocinfo(current_module.procinfo)<>current_procinfo then
               internalerror(200304274);
             current_module.procinfo:=current_procinfo.parent;
             if not isnestedproc then
               current_procinfo.free;

             consume(_SEMICOLON);
           end
         else
           begin
             { Handle imports }
             if (po_external in pd.procoptions) then
               begin
                 { External declared in implementation, and there was already a
                   forward (or interface) declaration then we need to generate
                   a stub that calls the external routine }
                 if (not pd.forwarddef) and
                    (pd.hasforward) and
                    not(
                        assigned(pd.import_dll) and
                        (target_info.system in [system_i386_win32,system_i386_wdosx,
                                                system_i386_emx,system_i386_os2])
                       ) then
                   begin
                     s:=proc_get_importname(pd);
                     if s<>'' then
                       gen_external_stub(codesegment,pd,s);
                   end;

                 { Import DLL specified? }
                 if assigned(pd.import_dll) then
                   begin
                     { create importlib if not already done }
                     if not(current_module.uses_imports) then
                       begin
                         current_module.uses_imports:=true;
                         importlib.preparelib(current_module.realmodulename^);
                       end;

                     if assigned(pd.import_name) then
                       importlib.importprocedure(pd,pd.import_dll^,pd.import_nr,pd.import_name^)
                     else
                       importlib.importprocedure(pd,pd.import_dll^,pd.import_nr,'');
                   end
                 else
                   begin
                     { add import name to external list for DLL scanning }
                     if target_info.DllScanSupported then
                       current_module.externals.insert(tExternalsItem.create(proc_get_importname(pd)));
                   end;
               end;
           end;

         { Restore old state }
         constsymtable:=oldconstsymtable;

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
      begin
         repeat
           if not assigned(current_procinfo) then
             internalerror(200304251);
           case token of
              _LABEL:
                label_dec;
              _CONST:
                const_dec;
              _TYPE:
                type_dec;
              _VAR:
                var_dec;
              _THREADVAR:
                threadvar_dec;
              _CONSTRUCTOR,
              _DESTRUCTOR,
              _FUNCTION,
              _PROCEDURE,
              _OPERATOR,
              _CLASS:
                read_proc;
              _EXPORTS:
                begin
                   if not(assigned(current_procinfo.procdef.localst)) or
                      (current_procinfo.procdef.localst.symtablelevel>main_program_level) or
                      (current_module.is_unit) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or
                           (target_info.system in [system_i386_WIN32,system_i386_wdosx,system_i386_Netware,system_i386_netwlibc]) then
                     read_exports
                   else
                     begin
                        Message(parser_w_unsupported_feature);
                        consume(_BEGIN);
                     end;
                end
              else
                begin
                  case idtoken of
                    _RESOURCESTRING :
                      begin
                        { m_class is needed, because the resourcestring
                          loading is in the ObjPas unit }
                        if (m_class in aktmodeswitches) then
                          resourcestring_dec
                        else
                          break;
                      end;
                    _PROPERTY:
                      begin
                        if (m_fpc in aktmodeswitches) then
                          property_dec
                        else
                          break;
                      end;
                    else
                      break;
                  end;
                end;
           end;
         until false;

         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
           symtablestack.foreach_static(@check_forward_class,nil);
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
             _FUNCTION,
             _PROCEDURE,
             _OPERATOR :
               read_proc;
             else
               begin
                 case idtoken of
                   _RESOURCESTRING :
                     resourcestring_dec;
                   _PROPERTY:
                     begin
                       if (m_fpc in aktmodeswitches) then
                         property_dec
                       else
                         break;
                     end;
                   else
                     break;
                 end;
               end;
           end;
         until false;
         { check for incomplete class definitions, this is only required
           for fpc modes }
         if (m_fpc in aktmodeswitches) then
          symtablestack.foreach_static(@check_forward_class,nil);
      end;


end.
