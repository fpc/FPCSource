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
      symdef,procinfo,optdfa;

    type
      tcgprocinfo = class(tprocinfo)
      private
        procedure maybe_add_constructor_wrapper(var tocode: tnode; withexceptblock: boolean);
        procedure add_entry_exit_code;
        procedure setup_tempgen;
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
        dfabuilder : TDFABuilder;
        destructor  destroy;override;
        procedure printproc(pass:string);
        procedure generate_code;
        procedure generate_code_tree;
        procedure generate_exceptfilter(nestedpi: tcgprocinfo);
        procedure resetprocdef;
        procedure add_to_symtablestack;
        procedure remove_from_symtablestack;
        procedure parse_body;

        function has_assembler_child : boolean;
      end;


    procedure printnode_reset;

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;

    procedure generate_specialization_procs;


implementation

    uses
       sysutils,
       { common }
       cutils,
       { global }
       globtype,tokens,verbose,comphook,constexp,
       systems,
       { aasm }
       cpuinfo,cpubase,aasmbase,aasmtai,aasmdata,
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
       tgobj,cgbase,cgobj,cgcpu,dbgbase,
       ncgutil,regvars,
       optbase,
       opttail,
       optcse,optloop,
       optutils
{$if defined(arm) or defined(powerpc) or defined(powerpc64) or defined(avr)}
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

    procedure initializevars(p:TObject;arg:pointer);
      var
        b : tblocknode;
      begin
        if not (tsym(p).typ in [localvarsym,staticvarsym]) then
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


    procedure check_finalize_paras(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ=paravarsym) and
           tparavarsym(p).needs_finalization then
          include(current_procinfo.flags,pi_needs_implicit_finally);
      end;


    procedure check_finalize_locals(p:TObject;arg:pointer);
      begin
        { include the result: it needs to be finalized in case an exception }
        { occurs                                                            }
        if (tsym(p).typ=localvarsym) and
           (tlocalvarsym(p).refs>0) and
           is_managed_type(tlocalvarsym(p).vardef) then
          include(current_procinfo.flags,pi_needs_implicit_finally);
      end;


    function block(islibrary : boolean) : tnode;
      var
        oldfilepos: tfileposinfo;
      begin
         { parse const,types and vars }
         read_declarations(islibrary);

         { do we have an assembler block without the po_assembler?
           we should allow this for Delphi compatibility (PFV) }
         if (token=_ASM) and (m_delphi in current_settings.modeswitches) then
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
             (current_module.is_unit or islibrary)
            ) then
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
                        block:=statement_block(_INITIALIZATION);
                     end
                   else if token=_FINALIZATION then
                     begin
                       { when a unit has only a finalization section, we can come to this
                         point when we try to read the nonh existing initalization section
                         so we've to check if we are really try to parse the finalization }
                       if current_procinfo.procdef.proctypeoption=potype_unitfinalize then
                         block:=statement_block(_FINALIZATION)
                       else
                         block:=nil;
                     end
                   else
                     block:=statement_block(_BEGIN);
                end;
            end
         else
            begin
               block:=statement_block(_BEGIN);
               if current_procinfo.procdef.localst.symtabletype=localsymtable then
                 begin
                   { initialization of local variables with their initial
                     values: part of function entry }
                   oldfilepos:=current_filepos;
                   current_filepos:=current_procinfo.entrypos;
                   current_procinfo.procdef.localst.SymList.ForEachCall(@initializevars,block);
                   current_filepos:=oldfilepos;
                 end;
            end;
      end;


{****************************************************************************
                       PROCEDURE/FUNCTION COMPILING
****************************************************************************}

    procedure printnode_reset;
      begin
        assign(printnodefile,treelogfilename);
        {$push}{$I-}
         rewrite(printnodefile);
        {$pop}
        if ioresult<>0 then
         begin
           Comment(V_Error,'Error creating '+treelogfilename);
           exit;
         end;
        close(printnodefile);
      end;


    procedure add_label_init(p:TObject;arg:pointer);
      begin
        if tstoredsym(p).typ=labelsym then
          begin
            addstatement(tstatementnode(arg^),
              cifnode.create(caddnode.create(equaln,
                ccallnode.createintern('fpc_setjmp',
                  ccallparanode.create(cloadnode.create(tlabelsym(p).jumpbuf,tlabelsym(p).jumpbuf.owner),nil)),
                cordconstnode.create(1,sinttype,true))
              ,cgotonode.create(tlabelsym(p)),nil)
            );
          end;
      end;


    function generate_bodyentry_block:tnode;
      var
        srsym        : tsym;
        para         : tcallparanode;
        newstatement : tstatementnode;
      begin
        result:=internalstatements(newstatement);

        if assigned(current_structdef) then
          begin
            { a constructor needs a help procedure }
            if (current_procinfo.procdef.proctypeoption=potype_constructor) then
              begin
                if is_class(current_structdef) then
                  begin
                    srsym:=search_struct_member(current_structdef,'NEWINSTANCE');
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
                  if is_object(current_structdef) then
                    begin
                      { parameter 3 : vmt_offset }
                      { parameter 2 : address of pointer to vmt,
                        this is required to allow setting the vmt to -1 to indicate
                        that memory was allocated }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(tobjectdef(current_structdef).vmt_offset,s32inttype,false),
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
                  if not is_record(current_structdef) then
                  internalerror(200305103);
                { if self=nil then exit
                  calling fail instead of exit is useless because
                  there is nothing to dispose (PFV) }
                if is_class_or_object(current_structdef) then
                addstatement(newstatement,cifnode.create(
                    caddnode.create(equaln,
                        load_self_pointer_node,
                        cnilnode.create),
                    cexitnode.create(nil),
                    nil));
              end;

            { maybe call BeforeDestruction for classes }
            if (current_procinfo.procdef.proctypeoption=potype_destructor) and
               is_class(current_structdef) then
              begin
                srsym:=search_struct_member(current_structdef,'BEFOREDESTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    { if vmt>0 then beforedestruction }
                    addstatement(newstatement,cifnode.create(
                        caddnode.create(gtn,
                            ctypeconvnode.create_internal(
                              load_vmt_pointer_node,ptrsinttype),
                            ctypeconvnode.create_internal(
                              cnilnode.create,ptrsinttype)),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                        nil));
                  end
                else
                  internalerror(200305104);
              end;
          end;
        if m_non_local_goto in current_settings.modeswitches then
          tsymtable(current_procinfo.procdef.localst).SymList.ForEachCall(@add_label_init,@newstatement);
      end;


    function generate_bodyexit_block:tnode;
      var
        srsym : tsym;
        para : tcallparanode;
        newstatement : tstatementnode;
        oldlocalswitches: tlocalswitches;
      begin
        result:=internalstatements(newstatement);

        if assigned(current_structdef) then
          begin
            { Don't test self and the vmt here. The reason is that  }
            { a constructor already checks whether these are valid  }
            { before. Further, in case of TThread the thread may    }
            { free the class instance right after AfterConstruction }
            { has been called, so it may no longer be valid (JM)    }
            oldlocalswitches:=current_settings.localswitches;
            current_settings.localswitches:=oldlocalswitches-[cs_check_object,cs_check_range];

            { a destructor needs a help procedure }
            if (current_procinfo.procdef.proctypeoption=potype_destructor) then
              begin
                if is_class(current_structdef) then
                  begin
                    srsym:=search_struct_member(current_structdef,'FREEINSTANCE');
                    if assigned(srsym) and
                       (srsym.typ=procsym) then
                      begin
                        { if self<>0 and vmt<>0 then freeinstance }
                        addstatement(newstatement,cifnode.create(
                            caddnode.create(andn,
                                caddnode.create(unequaln,
                                    load_self_pointer_node,
                                    cnilnode.create),
                                caddnode.create(unequaln,
                                    ctypeconvnode.create(
                                        load_vmt_pointer_node,
                                        voidpointertype),
                                    cpointerconstnode.create(0,voidpointertype))),
                            ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                            nil));
                      end
                    else
                      internalerror(200305108);
                  end
                else
                  if is_object(current_structdef) then
                    begin
                      { finalize object data, but only if not in inherited call }
                      if is_managed_type(current_structdef) then
                        begin
                          addstatement(newstatement,cifnode.create(
                            caddnode.create(unequaln,
                              ctypeconvnode.create_internal(load_vmt_pointer_node,voidpointertype),
                              cnilnode.create),
                            finalize_data_node(load_self_node),
                            nil));
                        end;
                      { parameter 3 : vmt_offset }
                      { parameter 2 : pointer to vmt }
                      { parameter 1 : self pointer }
                      para:=ccallparanode.create(
                                cordconstnode.create(tobjectdef(current_structdef).vmt_offset,s32inttype,false),
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
            current_settings.localswitches:=oldlocalswitches;
          end;
      end;


    function generate_except_block:tnode;
      var
        newstatement : tstatementnode;
      begin
        generate_except_block:=internalstatements(newstatement);

        { a constructor needs call destructor (if available) when it
          is not inherited }
        if not assigned(current_structdef) or
           (current_procinfo.procdef.proctypeoption<>potype_constructor) then
          begin
            { no constructor }
            { must be the return value finalized before reraising the exception? }
            if (not is_void(current_procinfo.procdef.returndef)) and
               is_managed_type(current_procinfo.procdef.returndef) and
               (not paramanager.ret_in_param(current_procinfo.procdef.returndef, current_procinfo.procdef.proccalloption)) and
               (not is_class(current_procinfo.procdef.returndef)) then
              addstatement(newstatement,finalize_data_node(load_result_node));
          end;
      end;


{****************************************************************************
                                  TCGProcInfo
****************************************************************************}

     destructor tcgprocinfo.destroy;
       begin
         if assigned(code) then
           code.free;
         inherited destroy;
       end;


    procedure tcgprocinfo.printproc(pass:string);
      begin
        assign(printnodefile,treelogfilename);
        {$push}{$I-}
         append(printnodefile);
         if ioresult<>0 then
          rewrite(printnodefile);
        {$pop}
        if ioresult<>0 then
         begin
           Comment(V_Error,'Error creating '+treelogfilename);
           exit;
         end;
        writeln(printnodefile);
        writeln(printnodefile,'*******************************************************************************');
        writeln(printnodefile, pass);
        writeln(printnodefile,procdef.fullprocname(false));
        writeln(printnodefile,'*******************************************************************************');
        printnode(printnodefile,code);
        close(printnodefile);
      end;


    procedure tcgprocinfo.maybe_add_constructor_wrapper(var tocode: tnode; withexceptblock: boolean);
      var
        oldlocalswitches: tlocalswitches;
        srsym: tsym;
        afterconstructionblock,
        exceptblock,
        newblock: tblocknode;
        newstatement: tstatementnode;
        pd: tprocdef;
      begin
        if assigned(procdef.struct) and
           (procdef.proctypeoption=potype_constructor) then
          begin
            { Don't test self and the vmt here. See generate_bodyexit_block }
            { why (JM)                                                      }
            oldlocalswitches:=current_settings.localswitches;
            current_settings.localswitches:=oldlocalswitches-[cs_check_object,cs_check_range];

            { call AfterConstruction for classes }
            if is_class(procdef.struct) then
              begin
                srsym:=search_struct_member(procdef.struct,'AFTERCONSTRUCTION');
                if assigned(srsym) and
                   (srsym.typ=procsym) then
                  begin
                    current_filepos:=exitpos;
                    afterconstructionblock:=internalstatements(newstatement);
                    { first execute all constructor code. If no exception
                      occurred then we will execute afterconstruction,
                      otherwise we won't (the exception will jump over us) }
                    addstatement(newstatement,tocode);
                    { if implicit finally node wasn't created, then exit label and
                      finalization code must be handled here and placed before
                      afterconstruction }
                    if not ((pi_needs_implicit_finally in flags) and
                      (cs_implicit_exceptions in current_settings.moduleswitches)) then
                      begin
                        include(tocode.flags,nf_block_with_exit);
                        addstatement(newstatement,final_asmnode);
                      end;

                    { Self can be nil when fail is called }
                    { if self<>nil and vmt<>nil then afterconstruction }
                    addstatement(newstatement,cifnode.create(
                      caddnode.create(andn,
                        caddnode.create(unequaln,
                          load_self_node,
                          cnilnode.create),
                        caddnode.create(unequaln,
                          load_vmt_pointer_node,
                          cnilnode.create)),
                        ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                        nil));
                    tocode:=afterconstructionblock;
                  end
                else
                  internalerror(200305106);
              end;

            if withexceptblock and (procdef.struct.typ=objectdef) then
              begin
                { Generate the implicit "fail" code for a constructor (destroy
                  in case an exception happened) }
                pd:=tobjectdef(procdef.struct).find_destructor;
                { this will always be the case for classes, since tobject has
                  a destructor }
                if assigned(pd) then
                  begin
                    current_filepos:=exitpos;
                    exceptblock:=internalstatements(newstatement);
                    { first free the instance if non-nil }
                    { if vmt<>0 then call destructor }
                    addstatement(newstatement,cifnode.create(
                      caddnode.create(unequaln,
                        load_vmt_pointer_node,
                        cnilnode.create),
                      { cnf_create_failed -> don't call BeforeDestruction }
                      ccallnode.create(nil,tprocsym(pd.procsym),pd.procsym.owner,load_self_node,[cnf_create_failed]),
                      nil));
                    { then re-raise the exception }
                    addstatement(newstatement,craisenode.create(nil,nil,nil));
                    current_filepos:=entrypos;
                    newblock:=internalstatements(newstatement);
                    { try
                        tocode
                      except
                        exceptblock
                      end
                    }
                    addstatement(newstatement,ctryexceptnode.create(
                      tocode,
                      nil,
                      exceptblock));
                    tocode:=newblock;
                  end;
              end;
            current_settings.localswitches:=oldlocalswitches;
          end;
      end;


    procedure tcgprocinfo.add_entry_exit_code;
      var
        finalcode,
        bodyentrycode,
        bodyexitcode,
        exceptcode,
        wrappedbody: tnode;
        newblock     : tblocknode;
        codestatement,
        newstatement : tstatementnode;
        oldfilepos   : tfileposinfo;
        is_constructor: boolean;
      begin
        is_constructor:=assigned(procdef.struct) and
          (procdef.proctypeoption=potype_constructor);

        oldfilepos:=current_filepos;
        { Generate code/locations used at start of proc }
        current_filepos:=entrypos;
        entry_asmnode:=casmnode.create_get_position;
        loadpara_asmnode:=casmnode.create_get_position;
        stackcheck_asmnode:=casmnode.create_get_position;
        init_asmnode:=casmnode.create_get_position;
        bodyentrycode:=generate_bodyentry_block;
        { Generate code/locations used at end of proc }
        current_filepos:=exitpos;
        exitlabel_asmnode:=casmnode.create_get_position;
        final_asmnode:=casmnode.create_get_position;
        bodyexitcode:=generate_bodyexit_block;

        { Generate procedure by combining init+body+final,
          depending on the implicit finally we need to add
          an try...finally...end wrapper }
        newblock:=internalstatements(newstatement);
        { initialization is common for all cases }
        addstatement(newstatement,loadpara_asmnode);
        addstatement(newstatement,stackcheck_asmnode);
        addstatement(newstatement,entry_asmnode);
        addstatement(newstatement,init_asmnode);
        addstatement(newstatement,bodyentrycode);

        if (cs_implicit_exceptions in current_settings.moduleswitches) and
           (pi_needs_implicit_finally in flags) and
           { but it's useless in init/final code of units }
           not(procdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) and
           not(po_assembler in procdef.procoptions) then
          begin
            { Generate special exception block only needed when
              implicit finaly is used }
            current_filepos:=exitpos;
            exceptcode:=generate_except_block;
            { Generate code that will be in the try...finally }
            finalcode:=internalstatements(codestatement);
            addstatement(codestatement,final_asmnode);

            current_filepos:=entrypos;
            wrappedbody:=ctryfinallynode.create_implicit(
               code,
               finalcode,
               exceptcode);
            { afterconstruction must be called after final_asmnode, because it
               has to execute after the temps have been finalised in case of a
               refcounted class (afterconstruction decreases the refcount
               without freeing the instance if the count becomes nil, while
               the finalising of the temps can free the instance) }
            maybe_add_constructor_wrapper(wrappedbody,true);
            addstatement(newstatement,wrappedbody);
            addstatement(newstatement,exitlabel_asmnode);
            addstatement(newstatement,bodyexitcode);
            { set flag the implicit finally has been generated }
            include(flags,pi_has_implicit_finally);
          end
        else
          begin
            { constructors need destroy-on-exception code even if they don't
              have managed variables/temps }
            maybe_add_constructor_wrapper(code,
              cs_implicit_exceptions in current_settings.moduleswitches);
            addstatement(newstatement,code);
            addstatement(newstatement,exitlabel_asmnode);
            addstatement(newstatement,bodyexitcode);
            if not is_constructor then
              addstatement(newstatement,final_asmnode);
          end;
        do_firstpass(tnode(newblock));
        code:=newblock;
        current_filepos:=oldfilepos;
      end;


    procedure clearrefs(p:TObject;arg:pointer);
      begin
         if (tsym(p).typ in [localvarsym,paravarsym,staticvarsym]) then
           if tabstractvarsym(p).refs>1 then
             tabstractvarsym(p).refs:=1;
      end;


    procedure translate_registers(p:TObject;list:pointer);
      begin
         if (tsym(p).typ in [localvarsym,paravarsym,staticvarsym]) and
            (tabstractnormalvarsym(p).localloc.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_MMREGISTER,
              LOC_CMMREGISTER,LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
           begin
             if not(cs_no_regalloc in current_settings.globalswitches) then
               cg.translate_register(tabstractnormalvarsym(p).localloc.register);
             if cs_asm_source in current_settings.globalswitches then
               TAsmList(list).concat(Tai_comment.Create(strpnew('Var '+tabstractnormalvarsym(p).realname+' located in register '+
                 std_regname(tabstractnormalvarsym(p).localloc.register))))
           end;
      end;

    procedure tcgprocinfo.setup_tempgen;
      begin
        tg:=ttgobj.create;

{$if defined(x86) or defined(arm)}
        { try to strip the stack frame }
        { set the framepointer to esp if:
          - no assembler directive, those are handled in assembler_block
            in pstatment.pas (for cases not caught by the Delphi
            exception below)
          - no exceptions are used
          - no pushes are used/esp modifications, could be:
            * outgoing parameters on the stack
            * incoming parameters on the stack
            * open arrays
          - no inline assembler
         or
          - Delphi mode
          - assembler directive
          - no pushes are used/esp modifications, could be:
            * outgoing parameters on the stack
            * incoming parameters on the stack
            * open arrays
          - no local variables
        }
        if ((po_assembler in procdef.procoptions) and
           (m_delphi in current_settings.modeswitches) and
           { localst at main_program_level is a staticsymtable }
            (procdef.localst.symtablelevel<>main_program_level) and
            (tabstractlocalsymtable(procdef.localst).count_locals = 0)) or
           ((cs_opt_stackframe in current_settings.optimizerswitches) and
            not(cs_generate_stackframes in current_settings.localswitches) and
            not(po_assembler in procdef.procoptions) and
            ((flags*[pi_has_assembler_block,pi_uses_exceptions,pi_is_assembler,
                    pi_needs_implicit_finally,pi_has_implicit_finally,pi_has_stackparameter,
                    pi_needs_stackframe])=[])
           )
        then
          begin
            { we need the parameter info here to determine if the procedure gets
              parameters on the stack

              calling generate_parameter_info doesn't hurt but it costs time
              (necessary to init para_stack_size)
            }
            generate_parameter_info;
            if not(procdef.stack_tainting_parameter(calleeside)) and
               not(has_assembler_child) and (para_stack_size=0) then
              begin
                { Only need to set the framepointer }
                framepointer:=NR_STACK_POINTER_REG;
                tg.direction:=1;
              end;
          end;

{$endif}
        { set the start offset to the start of the temp area in the stack }
        set_first_temp_offset;
      end;

    function tcgprocinfo.has_assembler_child : boolean;
      var
        hp : tprocinfo;
      begin
        result:=false;
        hp:=get_first_nestedproc;
        while assigned(hp) do
          begin
            if (hp.flags*[pi_has_assembler_block,pi_is_assembler])<>[] then
              begin
                result:=true;
                exit;
              end;
            hp:=tprocinfo(hp.next);
          end;
      end;

    procedure tcgprocinfo.generate_code_tree;
      var
        hpi : tcgprocinfo;
      begin
        { generate code for this procedure }
        generate_code;
        { process nested procedures }
        hpi:=tcgprocinfo(get_first_nestedproc);
        while assigned(hpi) do
          begin
            hpi.generate_code_tree;
            hpi:=tcgprocinfo(hpi.next);
          end;
        resetprocdef;
      end;

    procedure tcgprocinfo.generate_exceptfilter(nestedpi: tcgprocinfo);
      var
        saved_cg: tcg;
      begin
        if nestedpi.procdef.proctypeoption<>potype_exceptfilter then
          InternalError(201201141);
        { flush code generated this far }
        aktproccode.concatlist(current_asmdata.CurrAsmList);
        { save the codegen }
        saved_cg:=cg;
        cg:=nil;
        nestedpi.generate_code;
        { prevents generating code the second time when processing nested procedures }
        nestedpi.resetprocdef;
        cg:=saved_cg;
        add_reg_instruction_hook:=@cg.add_reg_instruction;
      end;

    procedure tcgprocinfo.generate_code;
      var
        old_current_procinfo : tprocinfo;
        oldmaxfpuregisters : longint;
        oldfilepos : tfileposinfo;
        old_current_structdef : tabstractrecorddef;
        templist : TAsmList;
        headertai : tai;
        i : integer;
        varsym : tabstractnormalvarsym;
        {RedoDFA : boolean;}
      begin
        { the initialization procedure can be empty, then we
          don't need to generate anything. When it was an empty
          procedure there would be at least a blocknode }
        if not assigned(code) then
          exit;

        { We need valid code }
        if Errorcount<>0 then
          exit;

        { No code can be generated for generic template }
        if (df_generic in procdef.defoptions) then
          internalerror(200511152);

        { For regular procedures the RA and Tempgen shall not be available yet,
          but exception filters reuse Tempgen of parent }
        if assigned(tg)<>(procdef.proctypeoption=potype_exceptfilter) then
          internalerror(200309201);

        old_current_procinfo:=current_procinfo;
        oldfilepos:=current_filepos;
        old_current_structdef:=current_structdef;
        oldmaxfpuregisters:=current_settings.maxfpuregisters;

        current_procinfo:=self;
        current_filepos:=entrypos;
        current_structdef:=procdef.struct;

        templist:=TAsmList.create;

        { add parast/localst to symtablestack }
        add_to_symtablestack;

        { clear register count }
        procdef.localst.SymList.ForEachCall(@clearrefs,nil);
        procdef.parast.SymList.ForEachCall(@clearrefs,nil);

        { there's always a call to FPC_INITIALIZEUNITS/FPC_DO_EXIT in the main program }
        if (procdef.localst.symtablelevel=main_program_level) and
           (not current_module.is_unit) then
          include(flags,pi_do_call);

        { set implicit_finally flag when there are locals/paras to be finalized }
        procdef.parast.SymList.ForEachCall(@check_finalize_paras,nil);
        procdef.localst.SymList.ForEachCall(@check_finalize_locals,nil);

{$if defined(x86) or defined(arm)}
        { set implicit_finally flag for if procedure is safecall }
        if (tf_safecall_exceptions in target_info.flags) and
           (procdef.proccalloption=pocall_safecall) then
          include(flags, pi_needs_implicit_finally);
{$endif}
        { firstpass everything }
        flowcontrol:=[];
        do_firstpass(code);
{$ifdef i386}
        procdef.fpu_used:=node_resources_fpu(code);
        if procdef.fpu_used>0 then
          include(flags,pi_uses_fpu);
{$endif i386}

        { Print the node to tree.log }
        if paraprintnodetree=1 then
          printproc( 'after the firstpass');

        { do this before adding the entry code else the tail recursion recognition won't work,
          if this causes troubles, it must be if'ed
        }
        if (cs_opt_tailrecursion in current_settings.optimizerswitches) and
          (pi_is_recursive in flags) then
          do_opttail(code,procdef);

        if (cs_opt_nodedfa in current_settings.optimizerswitches) and
          { creating dfa is not always possible }
          ((flags*[pi_has_assembler_block,pi_uses_exceptions,pi_is_assembler,
                  pi_needs_implicit_finally,pi_has_implicit_finally])=[]) then
          begin
            dfabuilder:=TDFABuilder.Create;
            dfabuilder.createdfainfo(code);
            { when life info is available, we can give more sophisticated warning about unintialized
              variables }

            { iterate through life info of the first node }
            for i:=0 to dfabuilder.nodemap.count-1 do
              begin
                if DFASetIn(code.optinfo^.life,i) then
                  case tnode(dfabuilder.nodemap[i]).nodetype of
                    loadn:
                      begin
                        varsym:=tabstractnormalvarsym(tloadnode(dfabuilder.nodemap[i]).symtableentry);

                        { Give warning/note for living locals }
                        if assigned(varsym.owner) and
                          not(vo_is_external in varsym.varoptions) then
                          begin
                            if (vo_is_funcret in varsym.varoptions) then
                              CGMessage(sym_w_function_result_uninitialized)
                            else
                              begin
                                if (varsym.owner=procdef.localst) and not (vo_is_typed_const in varsym.varoptions) then
                                  CGMessage1(sym_w_uninitialized_local_variable,varsym.realname);
                              end;
                          end;
                      end;
                  end;
              end;
            include(flags,pi_dfaavailable);
          end;

        if (cs_opt_loopstrength in current_settings.optimizerswitches)
          { our induction variable strength reduction doesn't like
            for loops with more than one entry }
          and not(pi_has_label in flags) then
          begin
            {RedoDFA:=}OptimizeInductionVariables(code);
          end;

        if cs_opt_nodecse in current_settings.optimizerswitches then
          do_optcse(code);

        { add implicit entry and exit code }
        add_entry_exit_code;

        { only do secondpass if there are no errors }
        if (ErrorCount=0) then
          begin
            create_codegen;

            if (procdef.proctypeoption<>potype_exceptfilter) then
              setup_tempgen;

            { Create register allocator, must come after framepointer is known }
            cg.init_register_allocators;

            generate_parameter_info;

            { allocate got register if needed }
            allocate_got_register(aktproccode);

            { Allocate space in temp/registers for parast and localst }
            current_filepos:=entrypos;
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
            current_filepos:=entrypos;

            gen_load_para_value(templist);

            { caller paraloc info is also necessary in the stackframe_entry
              code of the ppc (and possibly other processors)               }
            procdef.init_paraloc_info(callerside);

            { generate code for the node tree }
            do_secondpass(code);
            aktproccode.concatlist(current_asmdata.CurrAsmList);

            { The position of the loadpara_asmnode is now known }
            aktproccode.insertlistafter(loadpara_asmnode.currenttai,templist);

            { first generate entry and initialize code with the correct
              position and switches }
            current_filepos:=entrypos;
            current_settings.localswitches:=entryswitches;

            cg.set_regalloc_live_range_direction(rad_backwards);

            gen_entry_code(templist);
            aktproccode.insertlistafter(entry_asmnode.currenttai,templist);
            gen_initialize_code(templist);
            aktproccode.insertlistafter(init_asmnode.currenttai,templist);

            { now generate finalize and exit code with the correct position
              and switches }
            current_filepos:=exitpos;
            current_settings.localswitches:=exitswitches;

            cg.set_regalloc_live_range_direction(rad_forward);

            if assigned(finalize_procinfo) then
              generate_exceptfilter(tcgprocinfo(finalize_procinfo))
            else
              begin
                gen_finalize_code(templist);
                { the finalcode must be concated if there was no position available,
                  using insertlistafter will result in an insert at the start
                  when currentai=nil }
                if assigned(final_asmnode) and assigned(final_asmnode.currenttai) then
                  aktproccode.insertlistafter(final_asmnode.currenttai,templist)
                else
                  aktproccode.concatlist(templist);
              end;
            { insert exit label at the correct position }
            cg.a_label(templist,CurrExitLabel);
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

            { generate symbol and save end of header position }
            current_filepos:=entrypos;
            gen_proc_symbol(templist);
            headertai:=tai(templist.last);
            { insert symbol }
            aktproccode.insertlist(templist);

            { Free space in temp/registers for parast and localst, must be
              done after gen_entry_code }
            current_filepos:=exitpos;

            { make sure the got/pic register doesn't get freed in the }
            { middle of a loop                                        }
            if (cs_create_pic in current_settings.moduleswitches) and
               (pi_needs_got in flags) and
               (got<>NR_NO) then
              cg.a_reg_sync(aktproccode,got);

            gen_free_symtable(aktproccode,procdef.localst);
            gen_free_symtable(aktproccode,procdef.parast);

            { add code that will load the return value, this is not done
              for assembler routines when they didn't reference the result
              variable }
            gen_load_return_value(templist);
            aktproccode.concatlist(templist);

            { Already reserve all registers for stack checking code and
              generate the call to the helper function }
            if not(tf_no_generic_stackcheck in target_info.flags) and
               (cs_check_stack in entryswitches) and
               not(po_assembler in procdef.procoptions) and
               (procdef.proctypeoption<>potype_proginit) then
              begin
                current_filepos:=entrypos;
                gen_stack_check_call(templist);
                aktproccode.insertlistafter(stackcheck_asmnode.currenttai,templist)
              end;

            { this code (got loading) comes before everything which has }
            { already been generated, so reset the info about already   }
            { backwards extended registers (so their live range can be  }
            { extended backwards even further if needed)                }
            { This code must be                                         }
            {  a) generated after do_secondpass has been called         }
            {     (because pi_needs_got may be set there)               }
            {  b) generated before register allocation, because the     }
            {     got/pic register can be a virtual one                 }
            {  c) inserted before the entry code, because the entry     }
            {     code may need global symbols such as init rtti        }
            {  d) inserted after the stackframe allocation, because     }
            {     this register may have to be spilled                  }
            cg.set_regalloc_live_range_direction(rad_backwards_reinit);
            current_filepos:=entrypos;
            { load got if necessary }
            cg.g_maybe_got_init(templist);

            aktproccode.insertlistafter(headertai,templist);

            { re-enable if more code at the end is ever generated here
            cg.set_regalloc_live_range_direction(rad_forward);
            }

            { The procedure body is finished, we can now
              allocate the registers }
            cg.do_register_allocation(aktproccode,headertai);

            { translate imag. register to their real counter parts
              this is necessary for debuginfo and verbose assembler output
              when SSA will be implented, this will be more complicated because we've to
              maintain location lists }
            procdef.parast.SymList.ForEachCall(@translate_registers,templist);
            procdef.localst.SymList.ForEachCall(@translate_registers,templist);
            if (cs_create_pic in current_settings.moduleswitches) and
               (pi_needs_got in flags) and
               not(cs_no_regalloc in current_settings.globalswitches) and
               (got<>NR_NO) then
              cg.translate_register(got);

            { Add save and restore of used registers }
            current_filepos:=entrypos;
            gen_save_used_regs(templist);
            { Remember the last instruction of register saving block
              (may be =nil for e.g. assembler procedures) }
            endprologue_ai:=templist.last;
            aktproccode.insertlistafter(headertai,templist);
            current_filepos:=exitpos;
            gen_restore_used_regs(aktproccode);
            { We know the size of the stack, now we can generate the
              parameter that is passed to the stack checking code }
            if not(tf_no_generic_stackcheck in target_info.flags) and
               (cs_check_stack in entryswitches) and
               not(po_assembler in procdef.procoptions) and
               (procdef.proctypeoption<>potype_proginit) then
              begin
                current_filepos:=entrypos;
                gen_stack_check_size_para(templist);
                aktproccode.insertlistafter(stackcheck_asmnode.currenttai,templist)
              end;
            { Add entry code (stack allocation) after header }
            current_filepos:=entrypos;
            gen_proc_entry_code(templist);
            aktproccode.insertlistafter(headertai,templist);
{$if defined(x86) or defined(arm)}
            { Set return value of safecall procedure if implicit try/finally blocks are disabled }
            if not (cs_implicit_exceptions in current_settings.moduleswitches) and
               (tf_safecall_exceptions in target_info.flags) and
               (procdef.proccalloption=pocall_safecall) then
              cg.a_load_const_reg(aktproccode,OS_ADDR,0,NR_FUNCTION_RETURN_REG);
{$endif}
            { Add exit code at the end }
            current_filepos:=exitpos;
            gen_proc_exit_code(templist);
            aktproccode.concatlist(templist);

            { check if the implicit finally has been generated. The flag
              should already be set in pass1 }
            if (cs_implicit_exceptions in current_settings.moduleswitches) and
               not(procdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) and
               (pi_needs_implicit_finally in flags) and
               not(po_assembler in procdef.procoptions) and
               not(pi_has_implicit_finally in flags) then
             internalerror(200405231);

{$ifndef NoOpt}
            if not(cs_no_regalloc in current_settings.globalswitches) then
              begin
                if (cs_opt_level1 in current_settings.optimizerswitches) and
                   { do not optimize pure assembler procedures }
                   not(pi_is_assembler in flags)  then
                  optimize(aktproccode);
              end;
{$endif NoOpt}


{$ifdef ARM}
            { because of the limited constant size of the arm, all data access is done pc relative }
            finalizearmcode(aktproccode,aktlocaldata);
{$endif ARM}

{$ifdef AVR}
            { because of the limited branch distance of cond. branches, they must be replaced
              somtimes by normal jmps and an inverse branch }
            finalizeavrcode(aktproccode);
{$endif AVR}

            { Add end symbol and debug info }
            { this must be done after the pcrelativedata is appended else the distance calculation of
              insertpcrelativedata will be wrong, further the pc indirect data is part of the procedure
              so it should be inserted before the end symbol (FK)
            }
            current_filepos:=exitpos;
            gen_proc_symbol_end(templist);
            aktproccode.concatlist(templist);
{$if defined(POWERPC) or defined(POWERPC64)}
            fixup_jmps(aktproccode);
{$endif}
            { insert line debuginfo }
            if (cs_debuginfo in current_settings.moduleswitches) or
               (cs_use_lineinfo in current_settings.globalswitches) then
              current_debuginfo.insertlineinfo(aktproccode);

            { add the procedure to the al_procedures }
            maybe_new_object_file(current_asmdata.asmlists[al_procedures]);
            new_section(current_asmdata.asmlists[al_procedures],sec_code,lower(procdef.mangledname),getprocalign);
            current_asmdata.asmlists[al_procedures].concatlist(aktproccode);
            { save local data (casetable) also in the same file }
            if assigned(aktlocaldata) and
               (not aktlocaldata.empty) then
              current_asmdata.asmlists[al_procedures].concatlist(aktlocaldata);

            { only now we can remove the temps }
            if (procdef.proctypeoption<>potype_exceptfilter) then
              begin
                tg.resettempgen;
                tg.free;
                tg:=nil;
              end;
            { stop tempgen and ra }
            cg.done_register_allocators;
            destroy_codegen;
          end;

        dfabuilder.free;

        { restore symtablestack }
        remove_from_symtablestack;

        { restore }
        templist.free;
        current_settings.maxfpuregisters:=oldmaxfpuregisters;
        current_filepos:=oldfilepos;
        current_structdef:=old_current_structdef;
        current_procinfo:=old_current_procinfo;
      end;


    procedure tcgprocinfo.add_to_symtablestack;
      begin
        { insert symtables for the class, but only if it is no nested function }
        if assigned(procdef.struct) and
           not(assigned(parent) and
               assigned(parent.procdef) and
               assigned(parent.procdef.struct)) then
          push_nested_hierarchy(procdef.struct);

        { insert parasymtable in symtablestack when parsing
          a function }
        if procdef.parast.symtablelevel>=normal_function_level then
          symtablestack.push(procdef.parast);

        { insert localsymtable, except for the main procedure
          (in that case the localst is the unit's static symtable,
           which is already on the stack) }
        if procdef.localst.symtablelevel>=normal_function_level then
          symtablestack.push(procdef.localst);
      end;


    procedure tcgprocinfo.remove_from_symtablestack;
      begin
        { remove localsymtable }
        if procdef.localst.symtablelevel>=normal_function_level then
          symtablestack.pop(procdef.localst);

        { remove parasymtable }
        if procdef.parast.symtablelevel>=normal_function_level then
          symtablestack.pop(procdef.parast);

        { remove symtables for the class, but only if it is no nested function }
        if assigned(procdef.struct) and
           not(assigned(parent) and
               assigned(parent.procdef) and
               assigned(parent.procdef.struct)) then
          pop_nested_hierarchy(procdef.struct);
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
          begin
            Message1(parser_w_not_supported_for_inline,'assembler');
            Message(parser_w_inlining_disabled);
            exit;
          end;
        for i:=0 to procdef.paras.count-1 do
          begin
            currpara:=tparavarsym(procdef.paras[i]);
            case currpara.vardef.typ of
              formaldef :
                begin
                  if (currpara.varspez in [vs_out,vs_var,vs_const,vs_constref]) then
                    begin
                      Message1(parser_w_not_supported_for_inline,'formal parameter');
                      Message(parser_w_inlining_disabled);
                      exit;
                    end;
                end;
              arraydef :
                begin
                  if is_array_of_const(currpara.vardef) or
                     is_variant_array(currpara.vardef) then
                    begin
                      Message1(parser_w_not_supported_for_inline,'array of const');
                      Message(parser_w_inlining_disabled);
                      exit;
                    end;
                  { open arrays might need re-basing of the index, i.e. if you pass
                    an array[1..10] as open array, you have to add 1 to all index operations
                    if you directly inline it }
                  if is_open_array(currpara.vardef) then
                    begin
                      Message1(parser_w_not_supported_for_inline,'open array');
                      Message(parser_w_inlining_disabled);
                      exit;
                    end;
                end;
            end;
        end;
        result:=true;
      end;


    procedure tcgprocinfo.parse_body;
      var
         old_current_procinfo : tprocinfo;
         old_block_type : tblock_type;
         st : TSymtable;
         old_current_structdef: tabstractrecorddef;
         old_current_genericdef,
         old_current_specializedef: tstoreddef;
         old_parse_generic: boolean;
      begin
         old_current_procinfo:=current_procinfo;
         old_block_type:=block_type;
         old_current_structdef:=current_structdef;
         old_current_genericdef:=current_genericdef;
         old_current_specializedef:=current_specializedef;
         old_parse_generic:=parse_generic;

         current_procinfo:=self;
         current_structdef:=procdef.struct;
         if assigned(current_structdef) and (df_generic in current_structdef.defoptions) then
           begin
             current_genericdef:=current_structdef;
             parse_generic:=true;
           end;
         if assigned(current_structdef) and (df_specialization in current_structdef.defoptions) then
           current_specializedef:=current_structdef;

         { calculate the lexical level }
         if procdef.parast.symtablelevel>maxnesting then
           Message(parser_e_too_much_lexlevel);
         block_type:=bt_body;

    {$ifdef state_tracking}
{    aktstate:=Tstate_storage.create;}
    {$endif state_tracking}

         { allocate the symbol for this procedure }
         alloc_proc_symbol(procdef);

         { add parast/localst to symtablestack }
         add_to_symtablestack;

         { save entry info }
         entrypos:=current_filepos;
         entryswitches:=current_settings.localswitches;

         if (df_generic in procdef.defoptions) then
           begin
             { start token recorder for generic template }
             procdef.initgeneric;
             current_scanner.startrecordtokens(procdef.generictokenbuf);
           end;

         { parse the code ... }
         code:=block(current_module.islibrary);

         if (df_generic in procdef.defoptions) then
           begin
             { stop token recorder for generic template }
             current_scanner.stoprecordtokens;

             { Give an error for accesses in the static symtable that aren't visible
               outside the current unit }
             st:=procdef.owner;
             while (st.symtabletype=ObjectSymtable) do
               st:=st.defowner.owner;
             if (pi_uses_static_symtable in flags) and
                (st.symtabletype<>staticsymtable) then
               Comment(V_Error,'Global Generic template references static symtable');
           end;

         { save exit info }
         exitswitches:=current_settings.localswitches;
         exitpos:=last_endtoken_filepos;

         { the procedure is now defined }
         procdef.forwarddef:=false;

         if assigned(code) then
           begin
             { get a better entry point }
             entrypos:=code.fileinfo;

             { Finish type checking pass }
             do_typecheckpass(code);
           end;

         { Check for unused labels, forwards, symbols for procedures. Static
           symtable is checked in pmodules.
           The check must be done after the typecheckpass }
         if (Errorcount=0) and
            (tstoredsymtable(procdef.localst).symtabletype<>staticsymtable) then
           begin
             { check if forwards are resolved }
             tstoredsymtable(procdef.localst).check_forwards;
             { check if all labels are used }
             tstoredsymtable(procdef.localst).checklabels;
             { check for unused symbols, but only if there is no asm block }
             if not(pi_has_assembler_block in flags) then
               begin
                 tstoredsymtable(procdef.localst).allsymbolsused;
                 tstoredsymtable(procdef.parast).allsymbolsused;
               end;
           end;

         if (po_inline in procdef.procoptions) then
           begin
             { Can we inline this procedure? }
             if checknodeinlining(procdef) then
               begin
                 new(procdef.inlininginfo);
                 include(procdef.procoptions,po_has_inlininginfo);
                 procdef.inlininginfo^.code:=code.getcopy;
                 procdef.inlininginfo^.flags:=flags;
                 { The blocknode needs to set an exit label }
                 if procdef.inlininginfo^.code.nodetype=blockn then
                   include(procdef.inlininginfo^.code.flags,nf_block_with_exit);
               end;
           end;

         { Print the node to tree.log }
         if paraprintnodetree=1 then
           printproc( 'after parsing');

         { ... remove symbol tables }
         remove_from_symtablestack;

    {$ifdef state_tracking}
{    aktstate.destroy;}
    {$endif state_tracking}

         current_structdef:=old_current_structdef;
         current_genericdef:=old_current_genericdef;
         current_specializedef:=old_current_specializedef;
         current_procinfo:=old_current_procinfo;
         parse_generic:=old_parse_generic;

         { Restore old state }
         block_type:=old_block_type;
      end;


{****************************************************************************
                        PROCEDURE/FUNCTION PARSING
****************************************************************************}


    procedure check_init_paras(p:TObject;arg:pointer);
      begin
        if tsym(p).typ<>paravarsym then
         exit;
        with tparavarsym(p) do
          if is_managed_type(vardef) and
             (varspez in [vs_value,vs_out]) then
            include(current_procinfo.flags,pi_do_call);
      end;



    procedure read_proc_body(old_current_procinfo:tprocinfo;pd:tprocdef);
      {
        Parses the procedure directives, then parses the procedure body, then
        generates the code for it
      }

      var
        oldfailtokenmode : tmodeswitch;
        isnestedproc     : boolean;
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
            if cs_link_deffile in current_settings.globalswitches then
              deffile.AddExport(pd.mangledname);
          end;

        { Insert result variables in the localst }
        insert_funcret_local(pd);

        { check if there are para's which require initing -> set }
        { pi_do_call (if not yet set)                            }
        if not(pi_do_call in current_procinfo.flags) then
          pd.parast.SymList.ForEachCall(@check_init_paras,nil);

        { set _FAIL as keyword if constructor }
        if (pd.proctypeoption=potype_constructor) then
         begin
           oldfailtokenmode:=tokeninfo^[_FAIL].keyword;
           tokeninfo^[_FAIL].keyword:=m_all;
         end;

        tcgprocinfo(current_procinfo).parse_body;

        { We can't support inlining for procedures that have nested
          procedures because the nested procedures use a fixed offset
          for accessing locals in the parent procedure (PFV) }
        if current_procinfo.has_nestedprocs then
          begin
            if (df_generic in current_procinfo.procdef.defoptions) then
              Comment(V_Error,'Generic methods cannot have nested procedures')
            else
             if (po_inline in current_procinfo.procdef.procoptions) then
              begin
                Message1(parser_w_not_supported_for_inline,'nested procedures');
                Message(parser_w_inlining_disabled);
                exclude(current_procinfo.procdef.procoptions,po_inline);
              end;
          end;

        { When it's a nested procedure then defer the code generation,
          when back at normal function level then generate the code
          for all defered nested procedures and the current procedure }
        if not isnestedproc then
          begin
            if not(df_generic in current_procinfo.procdef.defoptions) then
              tcgprocinfo(current_procinfo).generate_code_tree;
          end;

        { reset _FAIL as _SELF normal }
        if (pd.proctypeoption=potype_constructor) then
          tokeninfo^[_FAIL].keyword:=oldfailtokenmode;

        { release procinfo }
        if tprocinfo(current_module.procinfo)<>current_procinfo then
          internalerror(200304274);
        current_module.procinfo:=current_procinfo.parent;

        { For specialization we didn't record the last semicolon. Moving this parsing
          into the parse_body routine is not done because of having better file position
          information available }
        if not(df_specialization in current_procinfo.procdef.defoptions) then
          consume(_SEMICOLON);

        if not isnestedproc then
          { current_procinfo is checked for nil later on }
          freeandnil(current_procinfo);
      end;


    procedure read_proc(isclassmethod:boolean);
      {
        Parses the procedure directives, then parses the procedure body, then
        generates the code for it
      }

      var
        old_current_procinfo : tprocinfo;
        old_current_structdef: tabstractrecorddef;
        old_current_genericdef,
        old_current_specializedef: tstoreddef;
        pdflags    : tpdflags;
        pd,firstpd : tprocdef;
        s          : string;
      begin
         { save old state }
         old_current_procinfo:=current_procinfo;
         old_current_structdef:=current_structdef;
         old_current_genericdef:=current_genericdef;
         old_current_specializedef:=current_specializedef;

         { reset current_procinfo.procdef to nil to be sure that nothing is writing
           to another procdef }
         current_procinfo:=nil;
         current_structdef:=nil;
         current_genericdef:=nil;
         current_specializedef:=nil;

         { parse procedure declaration }
         pd:=parse_proc_dec(isclassmethod,old_current_structdef);

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
               create_smartlink or
              {
                taking addresses of static procedures goes wrong
                if they aren't global when pic is used (FK)
              }
              (cs_create_pic in current_settings.moduleswitches) then
              include(pd.procoptions,po_global);
            pd.forwarddef:=false;
          end;

         { parse the directives that may follow }
         parse_proc_directives(pd,pdflags);

         { hint directives, these can be separated by semicolons here,
           that needs to be handled here with a loop (PFV) }
         while try_consume_hintdirective(pd.symoptions,pd.deprecatedmsg) do
          Consume(_SEMICOLON);

         { Set calling convention }
         handle_calling_convention(pd);

         { search for forward declarations }
         if not proc_add_definition(pd) then
           begin
             { A method must be forward defined (in the object declaration) }
             if assigned(pd.struct) and
                (not assigned(old_current_structdef)) then
              begin
                MessagePos1(pd.fileinfo,parser_e_header_dont_match_any_member,pd.fullprocname(false));
                tprocsym(pd.procsym).write_parameter_lists(pd);
              end
             else
              begin
                { Give a better error if there is a forward def in the interface and only
                  a single implementation }
                firstpd:=tprocdef(tprocsym(pd.procsym).ProcdefList[0]);
                if (not pd.forwarddef) and
                   (not pd.interfacedef) and
                   (tprocsym(pd.procsym).ProcdefList.Count>1) and
                   firstpd.forwarddef and
                   firstpd.interfacedef and
                   not(tprocsym(pd.procsym).ProcdefList.Count>2) and
                   { don't give an error if it may be an overload }
                   not(m_fpc in current_settings.modeswitches) and
                   (not(po_overload in pd.procoptions) or
                    not(po_overload in firstpd.procoptions)) then
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
             read_proc_body(old_current_procinfo,pd);
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
                    (pd.hasforward)
                    { it is unclear to me what's the use of the following condition,
                      so commented out, see also issue #18371 (FK)
                    and
                    not(
                        assigned(pd.import_dll) and
                        (target_info.system in [system_i386_wdosx,
                                                system_arm_wince,system_i386_wince])
                       ) } then
                   begin
                     s:=proc_get_importname(pd);
                     if s<>'' then
                       gen_external_stub(current_asmdata.asmlists[al_procedures],pd,s);
                   end;

                 { Import DLL specified? }
                 if assigned(pd.import_dll) then
                   begin
                     if assigned (pd.import_name) then
                       current_module.AddExternalImport(pd.import_dll^,
                         pd.import_name^,proc_get_importname(pd),
                         pd.import_nr,false,false)
                     else
                       current_module.AddExternalImport(pd.import_dll^,
                         proc_get_importname(pd),proc_get_importname(pd),
                         pd.import_nr,false,true);
                   end
                 else
                   begin
                     { add import name to external list for DLL scanning }
                     if tf_has_dllscanner in target_info.flags then
                       current_module.dllscannerinputlist.Add(proc_get_importname(pd),pd);
                   end;
               end;
           end;

         { make sure that references to forward-declared functions are not }
         { treated as references to external symbols, needed for darwin.   }

         { make sure we don't change the binding of real external symbols }
         if not(po_external in pd.procoptions) then
           begin
             if (po_global in pd.procoptions) or
                (cs_profile in current_settings.moduleswitches) then
               current_asmdata.DefineAsmSymbol(pd.mangledname,AB_GLOBAL,AT_FUNCTION)
             else
               current_asmdata.DefineAsmSymbol(pd.mangledname,AB_LOCAL,AT_FUNCTION);
           end;

         current_structdef:=old_current_structdef;
         current_genericdef:=old_current_genericdef;
         current_specializedef:=old_current_specializedef;
         current_procinfo:=old_current_procinfo;
      end;


{****************************************************************************
                             DECLARATION PARSING
****************************************************************************}

    { search in symtablestack for not complete classes }
    procedure check_forward_class(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ=typesym) and
           (ttypesym(p).typedef.typ=objectdef) and
           (oo_is_forward in tobjectdef(ttypesym(p).typedef).objectoptions) then
          MessagePos1(tsym(p).fileinfo,sym_e_forward_type_not_resolved,tsym(p).realname);
      end;


    procedure read_declarations(islibrary : boolean);
      var
        is_classdef:boolean;
      begin
        is_classdef:=false;
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
              _CLASS:
                begin
                  is_classdef:=false;
                  if try_to_consume(_CLASS) then
                   begin
                     { class modifier is only allowed for procedures, functions, }
                     { constructors, destructors, fields and properties          }
                     if not(token in [_FUNCTION,_PROCEDURE,_PROPERTY,_VAR,_CONSTRUCTOR,_DESTRUCTOR,_OPERATOR]) and
                        not((token=_ID) and (idtoken=_OPERATOR)) then
                       Message(parser_e_procedure_or_function_expected);

                     if is_interface(current_structdef) then
                       Message(parser_e_no_static_method_in_interfaces)
                     else
                       { class methods are also allowed for Objective-C protocols }
                       is_classdef:=true;
                   end;
                end;
              _CONSTRUCTOR,
              _DESTRUCTOR,
              _FUNCTION,
              _PROCEDURE,
              _OPERATOR:
                begin
                  read_proc(is_classdef);
                  is_classdef:=false;
                end;
              _EXPORTS:
                begin
                   if (current_procinfo.procdef.localst.symtablelevel>main_program_level) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or
                     (target_info.system in systems_unit_program_exports) then
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
                    _RESOURCESTRING:
                      begin
                        { m_class is needed, because the resourcestring
                          loading is in the ObjPas unit }
{                        if (m_class in current_settings.modeswitches) then}
                          resourcestring_dec
{                        else
                          break;}
                      end;
                    _OPERATOR:
                      begin
                        if is_classdef then
                          begin
                            read_proc(is_classdef);
                            is_classdef:=false;
                          end
                        else
                          break;
                      end;
                    _PROPERTY:
                      begin
                        if (m_fpc in current_settings.modeswitches) then
                        begin
                          property_dec(is_classdef);
                          is_classdef:=false;
                        end
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
         if (m_fpc in current_settings.modeswitches) then
           current_procinfo.procdef.localst.SymList.ForEachCall(@check_forward_class,nil);
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
               read_proc(false);
             else
               begin
                 case idtoken of
                   _RESOURCESTRING :
                     resourcestring_dec;
                   _PROPERTY:
                     begin
                       if (m_fpc in current_settings.modeswitches) then
                         property_dec(false)
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
         if (m_fpc in current_settings.modeswitches) then
          symtablestack.top.SymList.ForEachCall(@check_forward_class,nil);
      end;


{****************************************************************************
                      SPECIALIZATION BODY GENERATION
****************************************************************************}


    procedure specialize_objectdefs(p:TObject;arg:pointer);
      var
        oldcurrent_filepos : tfileposinfo;
        oldsymtablestack   : tsymtablestack;
        oldextendeddefs    : TFPHashObjectList;
        pu : tused_unit;
        hmodule : tmodule;
        specobj : tabstractrecorddef;
        unitsyms : TFPHashObjectList;
        sym : tsym;
        i : Integer;

      procedure process_abstractrecorddef(def:tabstractrecorddef);
        var
          i  : longint;
          hp : tdef;
        begin
          for i:=0 to def.symtable.DefList.Count-1 do
            begin
              hp:=tdef(def.symtable.DefList[i]);
              if hp.typ=procdef then
               begin
                 { only generate the code if we need a body }
                 if assigned(tprocdef(hp).struct) and not tprocdef(hp).forwarddef then
                   continue;
                 if assigned(tprocdef(hp).genericdef) and
                   (tprocdef(hp).genericdef.typ=procdef) and
                   assigned(tprocdef(tprocdef(hp).genericdef).generictokenbuf) then
                   begin
                     oldcurrent_filepos:=current_filepos;
                     current_filepos:=tprocdef(tprocdef(hp).genericdef).fileinfo;
                     { use the index the module got from the current compilation process }
                     current_filepos.moduleindex:=hmodule.unit_index;
                     current_tokenpos:=current_filepos;
                     current_scanner.startreplaytokens(tprocdef(tprocdef(hp).genericdef).generictokenbuf,
                       tprocdef(tprocdef(hp).genericdef).change_endian);
                     read_proc_body(nil,tprocdef(hp));
                     current_filepos:=oldcurrent_filepos;
                   end
                 else
                   MessagePos1(tprocdef(hp).fileinfo,sym_e_forward_not_resolved,tprocdef(hp).fullprocname(false));
               end
             else
               if hp.typ in [objectdef,recorddef] then
                 { generate code for subtypes as well }
                 process_abstractrecorddef(tabstractrecorddef(hp));
           end;
        end;

      begin
        if not((tsym(p).typ=typesym) and
               (ttypesym(p).typedef.typesym=tsym(p)) and
               (ttypesym(p).typedef.typ in [objectdef,recorddef]) and
               (df_specialization in ttypesym(p).typedef.defoptions)
              ) then
          exit;

        { Setup symtablestack a definition time }
        specobj:=tabstractrecorddef(ttypesym(p).typedef);

        if not (is_class_or_object(specobj) or is_record(specobj)) then
          exit;

        oldsymtablestack:=symtablestack;
        oldextendeddefs:=current_module.extendeddefs;
        current_module.extendeddefs:=TFPHashObjectList.create(true);
        symtablestack:=tdefawaresymtablestack.create;
        if not assigned(specobj.genericdef) then
          internalerror(200705151);
        hmodule:=find_module_from_symtable(specobj.genericdef.owner);
        if hmodule=nil then
          internalerror(200705152);
        { collect all unit syms in the generic's unit as we need to establish
          their unitsym.module link again so that unit identifiers can be used }
        unitsyms:=tfphashobjectlist.create(false);
        if (hmodule<>current_module) and assigned(hmodule.globalsymtable) then
          for i:=0 to hmodule.globalsymtable.symlist.count-1 do
            begin
              sym:=tsym(hmodule.globalsymtable.symlist[i]);
              if sym.typ=unitsym then
                unitsyms.add(upper(sym.realname),sym);
            end;
        pu:=tused_unit(hmodule.used_units.first);
        while assigned(pu) do
          begin
            if not assigned(pu.u.globalsymtable) then
              internalerror(200705153);
            symtablestack.push(pu.u.globalsymtable);
            sym:=tsym(unitsyms.find(pu.u.modulename^));
            if assigned(sym) and not assigned(tunitsym(sym).module) then
              tunitsym(sym).module:=pu.u;
            pu:=tused_unit(pu.next);
          end;
        unitsyms.free;
        if assigned(hmodule.globalsymtable) then
          symtablestack.push(hmodule.globalsymtable);
        if assigned(hmodule.localsymtable) then
          symtablestack.push(hmodule.localsymtable);

        { procedure definitions for classes or objects }
        process_abstractrecorddef(specobj);

        { Restore symtablestack }
        current_module.extendeddefs.free;
        current_module.extendeddefs:=oldextendeddefs;
        symtablestack.free;
        symtablestack:=oldsymtablestack;
      end;


    procedure generate_specialization_procs;
      begin
        if assigned(current_module.globalsymtable) then
          current_module.globalsymtable.SymList.ForEachCall(@specialize_objectdefs,nil);
        if assigned(current_module.localsymtable) then
          current_module.localsymtable.SymList.ForEachCall(@specialize_objectdefs,nil);
      end;

end.
