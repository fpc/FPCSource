{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit does the parsing process

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
unit parser;

{$i fpcdefs.inc}

interface

{$ifdef PREPROCWRITE}
    procedure preprocess(const filename:string);
{$endif PREPROCWRITE}
    procedure compile(const filename:string);
    procedure initparser;
    procedure doneparser;

implementation

    uses
      cutils,cclasses,
      globtype,version,tokens,systems,globals,verbose,
      symbase,symtable,symdef,symsym,
      finput,fmodule,fppu,
      aasmbase,aasmtai,
      cpubase,cgbase,
      script,gendef,
{$ifdef BrowserLog}
      browlog,
{$endif BrowserLog}
{$ifdef UseExcept}
      tpexcept,
{$endif UseExcept}
{$ifdef GDB}
      gdb,
{$endif GDB}
      comphook,
      scanner,scandir,
      pbase,ptype,psystem,pmodules,psub,cresstr,cpuinfo;


    procedure initparser;
      begin
         { ^M means a string or a char, because we don't parse a }
         { type declaration                                      }
         ignore_equal:=false;

         { we didn't parse a object or class declaration }
         { and no function header                        }
         testcurobject:=0;

         { Symtable }
         current_procdef:=nil;

         objectlibrary:=nil;
         current_module:=nil;
         compiled_module:=nil;
         current_procinfo:=nil;

         loaded_units:=TLinkedList.Create;

         usedunits:=TLinkedList.Create;

         { global switches }
         aktglobalswitches:=initglobalswitches;

         aktsourcecodepage:=initsourcecodepage;

         { initialize scanner }
         InitScanner;
         InitScannerDirectives;

         { scanner }
         c:=#0;
         pattern:='';
         orgpattern:='';
         current_scanner:=nil;

         { register all nodes and tais }
         registernodes;
         registertais;

         { memory sizes }
         if heapsize=0 then
          heapsize:=target_info.heapsize;
         if stacksize=0 then
          stacksize:=target_info.stacksize;

         { open assembler response }
         GenerateAsmRes(outputexedir+'ppas');

         { open deffile }
         DefFile:=TDefFile.Create(outputexedir+inputfile+target_info.defext);

         { list of generated .o files, so the linker can remove them }
         SmartLinkOFiles:=TStringList.Create;

         { codegen }
         if paraprintnodetree<>0 then
           printnode_reset;
      end;


    procedure doneparser;
      begin
         { unload units }
         loaded_units.free;
         usedunits.free;

         { if there was an error in the scanner, the scanner is
           still assinged }
         if assigned(current_scanner) then
          begin
            current_scanner.free;
            current_scanner:=nil;
          end;

         { close scanner }
         DoneScanner;

         { close ppas,deffile }
         asmres.free;
         deffile.free;

         { free list of .o files }
         SmartLinkOFiles.Free;
      end;


    procedure default_macros;
      var
        hp : tstringlistitem;
      begin
      { commandline }
        hp:=tstringlistitem(initdefines.first);
        while assigned(hp) do
         begin
           current_scanner.def_macro(hp.str);
           hp:=tstringlistitem(hp.next);
         end;
      { set macros for version checking }
        current_scanner.set_macro('FPC_VERSION',version_nr);
        current_scanner.set_macro('FPC_RELEASE',release_nr);
        current_scanner.set_macro('FPC_PATCH',patch_nr);
      end;


{$ifdef PREPROCWRITE}
    procedure preprocess(const filename:string);
      var
        i : longint;
      begin
         new(preprocfile,init('pre'));
       { default macros }
         current_scanner^.macros:=new(pdictionary,init);
         default_macros;
       { initialize a module }
         current_module:=new(pmodule,init(filename,false));
         main_module:=current_module;
       { startup scanner, and save in current_module }
         current_scanner:=new(pscannerfile,Init(filename));
         current_module.scanner:=current_scanner;
       { loop until EOF is found }
         repeat
           current_scanner^.readtoken;
           preprocfile^.AddSpace;
           case token of
             _ID :
               begin
                 preprocfile^.Add(orgpattern);
               end;
             _REALNUMBER,
             _INTCONST :
               preprocfile^.Add(pattern);
             _CSTRING :
               begin
                 i:=0;
                 while (i<length(pattern)) do
                  begin
                    inc(i);
                    if pattern[i]='''' then
                     begin
                       insert('''',pattern,i);
                       inc(i);
                     end;
                  end;
                 preprocfile^.Add(''''+pattern+'''');
               end;
             _CCHAR :
               begin
                 case pattern[1] of
                   #39 :
                     pattern:='''''''';
                   #0..#31,
                   #128..#255 :
                     begin
                       str(ord(pattern[1]),pattern);
                       pattern:='#'+pattern;
                     end;
                   else
                     pattern:=''''+pattern[1]+'''';
                 end;
                 preprocfile^.Add(pattern);
               end;
             _EOF :
               break;
             else
               preprocfile^.Add(tokeninfo^[token].str)
           end;
         until false;
       { free scanner }
         dispose(current_scanner,done);
         current_scanner:=nil;
       { close }
         dispose(preprocfile,done);
      end;
{$endif PREPROCWRITE}


    procedure compile(const filename:string);
      type
        polddata=^tolddata;
        tolddata=record
        { scanner }
          oldidtoken,
          oldtoken       : ttoken;
          oldtokenpos    : tfileposinfo;
          oldc           : char;
          oldpattern,
          oldorgpattern  : string;
          old_block_type : tblock_type;
        { symtable }
          oldrefsymtable,
          olddefaultsymtablestack,
          oldsymtablestack : tsymtable;
          oldaktprocsym    : tprocsym;
          oldcurrent_procdef    : tprocdef;
          oldoverloaded_operators : toverloaded_operators;
        { cg }
          oldparse_only  : boolean;
        { asmlists }
          oldimports,
          oldexports,
          oldresource,
          oldrttilist,
          oldresourcestringlist,
          oldbsssegment,
          olddatasegment,
          oldcodesegment,
          oldexprasmlist,
          olddebuglist,
          oldwithdebuglist,
          oldconsts     : taasmoutput;
          oldobjectlibrary : tasmlibrarydata;
        { resourcestrings }
          OldResourceStrings : tResourceStrings;
        { akt.. things }
          oldaktlocalswitches  : tlocalswitches;
          oldaktmoduleswitches : tmoduleswitches;
          oldaktfilepos      : tfileposinfo;
          oldaktpackenum,oldaktmaxfpuregisters : longint;
          oldaktalignment  : talignmentinfo;
          oldaktoutputformat : tasm;
          oldaktspecificoptprocessor,
          oldaktoptprocessor : tprocessors;
          oldaktasmmode      : tasmmode;
          oldaktinterfacetype: tinterfacetypes;
          oldaktmodeswitches : tmodeswitches;
          old_compiled_module : tmodule;
          oldaktdefproccall : tproccalloption;
          oldsourcecodepage : tcodepagestring;
          oldstatement_level : integer;
{$ifdef GDB}
          store_dbx : plongint;
{$endif GDB}
        end;

      var
         olddata : polddata;
{$ifdef USEEXCEPT}
{$ifndef Delphi}
         recoverpos    : jmp_buf;
         oldrecoverpos : pjmp_buf;
{$endif Delphi}
{$endif useexcept}
       begin
         inc(compile_level);
         parser_current_file:=filename;
         { Uses heap memory instead of placing everything on the
           stack. This is needed because compile() can be called
           recursively }
         new(olddata);
         with olddata^ do
          begin
            old_compiled_module:=compiled_module;
          { save symtable state }
            oldsymtablestack:=symtablestack;
            olddefaultsymtablestack:=defaultsymtablestack;
            oldrefsymtable:=refsymtable;
            oldcurrent_procdef:=current_procdef;
            oldaktdefproccall:=aktdefproccall;
            move(overloaded_operators,oldoverloaded_operators,sizeof(toverloaded_operators));
          { save scanner state }
            oldc:=c;
            oldpattern:=pattern;
            oldorgpattern:=orgpattern;
            oldtoken:=token;
            oldidtoken:=idtoken;
            old_block_type:=block_type;
            oldtokenpos:=akttokenpos;
            oldsourcecodepage:=aktsourcecodepage;
          { save cg }
            oldparse_only:=parse_only;
          { save assembler lists }
            olddatasegment:=datasegment;
            oldbsssegment:=bsssegment;
            oldcodesegment:=codesegment;
            olddebuglist:=debuglist;
            oldwithdebuglist:=withdebuglist;
            oldconsts:=consts;
            oldrttilist:=rttilist;
            oldexprasmlist:=exprasmlist;
            oldimports:=importssection;
            oldexports:=exportssection;
            oldresource:=resourcesection;
            oldresourcestringlist:=resourcestringlist;
            oldobjectlibrary:=objectlibrary;
            OldResourceStrings:=ResourceStrings;
          { save akt... state }
          { handle the postponed case first }
           if localswitcheschanged then
             begin
               aktlocalswitches:=nextaktlocalswitches;
               localswitcheschanged:=false;
             end;
            oldaktlocalswitches:=aktlocalswitches;
            oldaktmoduleswitches:=aktmoduleswitches;
            oldaktalignment:=aktalignment;
            oldaktpackenum:=aktpackenum;
            oldaktmaxfpuregisters:=aktmaxfpuregisters;
            oldaktoutputformat:=aktoutputformat;
            oldaktoptprocessor:=aktoptprocessor;
            oldaktspecificoptprocessor:=aktspecificoptprocessor;
            oldaktasmmode:=aktasmmode;
            oldaktinterfacetype:=aktinterfacetype;
            oldaktfilepos:=aktfilepos;
            oldaktmodeswitches:=aktmodeswitches;
            oldstatement_level:=statement_level;
{$ifdef GDB}
            store_dbx:=dbx_counter;
            dbx_counter:=nil;
{$endif GDB}
          end;
       { show info }
         Message1(parser_i_compiling,filename);

       { reset symtable }
         symtablestack:=nil;
         defaultsymtablestack:=nil;
         systemunit:=nil;
         refsymtable:=nil;
         aktdefproccall:=initdefproccall;
         registerdef:=true;
         statement_level:=0;
         aktexceptblock:=0;
         exceptblockcounter:=0;
         aktmaxfpuregisters:=-1;
         fillchar(overloaded_operators,sizeof(toverloaded_operators),0);
       { reset the unit or create a new program }
         if not assigned(current_module) then
          begin
            current_module:=tppumodule.create(nil,filename,'',false);
            main_module:=current_module;
            current_module.state:=ms_compile;
          end;
         if not(current_module.state in [ms_compile,ms_second_compile]) then
           internalerror(200212281);

         { a unit compiled at command line must be inside the loaded_unit list }
         if (compile_level=1) then
           loaded_units.insert(current_module);

         { Set the module to use for verbose }
         compiled_module:=current_module;
         SetCompileModule(current_module);
         Fillchar(aktfilepos,0,sizeof(aktfilepos));

         { Load current state from the init values }
         aktlocalswitches:=initlocalswitches;
         aktmoduleswitches:=initmoduleswitches;
         aktmodeswitches:=initmodeswitches;
         {$IFDEF Testvarsets}
         aktsetalloc:=initsetalloc;
         {$ENDIF}
         aktalignment:=initalignment;
         aktpackenum:=initpackenum;
         aktoutputformat:=initoutputformat;
         set_target_asm(aktoutputformat);
         aktoptprocessor:=initoptprocessor;
         aktspecificoptprocessor:=initspecificoptprocessor;
         aktasmmode:=initasmmode;
         aktinterfacetype:=initinterfacetype;

       { startup scanner and load the first file }
         current_scanner:=tscannerfile.Create(filename);
         current_scanner.firstfile;
         current_module.scanner:=current_scanner;
       { macros }
         default_macros;
       { read the first token }
         current_scanner.readtoken;

       { init code generator for a new module }
         codegen_newmodule;

         { If the compile level > 1 we get a nice "unit expected" error
           message if we are trying to use a program as unit.}
{$ifdef USEEXCEPT}
         if setjmp(recoverpos)=0 then
          begin
            oldrecoverpos:=recoverpospointer;
            recoverpospointer:=@recoverpos;
{$endif USEEXCEPT}

            if (token=_UNIT) or (compile_level>1) then
              begin
                current_module.is_unit:=true;
                proc_unit;
              end
            else
              proc_program(token=_LIBRARY);
{$ifdef USEEXCEPT}
            recoverpospointer:=oldrecoverpos;
          end
         else
          begin
            recoverpospointer:=oldrecoverpos;
            longjump_used:=true;
          end;
{$endif USEEXCEPT}

       { clear memory }
{$ifdef Splitheap}
         if testsplit then
           begin
           { temp heap should be empty after that !!!}
             codegen_donemodule;
             Releasetempheap;
           end;
{$endif Splitheap}

       { restore old state, close trees, > 0.99.5 has heapblocks, so
         it's the default to release the trees }
         codegen_donemodule;

         if assigned(current_module) then
          begin
            { free ppu }
            if assigned(tppumodule(current_module).ppufile) then
             begin
               tppumodule(current_module).ppufile.free;
               tppumodule(current_module).ppufile:=nil;
             end;

            { free scanner }
            if assigned(current_module.scanner) then
             begin
               if current_scanner=tscannerfile(current_module.scanner) then
                 current_scanner:=nil;
               tscannerfile(current_module.scanner).free;
               current_module.scanner:=nil;
             end;
          end;

         if (compile_level>1) then
           begin
              with olddata^ do
               begin
                 { restore scanner }
                 c:=oldc;
                 pattern:=oldpattern;
                 orgpattern:=oldorgpattern;
                 token:=oldtoken;
                 idtoken:=oldidtoken;
                 akttokenpos:=oldtokenpos;
                 block_type:=old_block_type;
                 { restore cg }
                 parse_only:=oldparse_only;
                 { restore asmlists }
                 exprasmlist:=oldexprasmlist;
                 datasegment:=olddatasegment;
                 bsssegment:=oldbsssegment;
                 codesegment:=oldcodesegment;
                 consts:=oldconsts;
                 debuglist:=olddebuglist;
                 withdebuglist:=oldwithdebuglist;
                 importssection:=oldimports;
                 exportssection:=oldexports;
                 resourcesection:=oldresource;
                 rttilist:=oldrttilist;
                 resourcestringlist:=oldresourcestringlist;
                 { object data }
                 ResourceStrings:=OldResourceStrings;
                 objectlibrary:=oldobjectlibrary;
                 { restore previous scanner }
                 if assigned(old_compiled_module) then
                   current_scanner:=tscannerfile(old_compiled_module.scanner)
                 else
                   current_scanner:=nil;
                 if assigned(current_scanner) then
                   parser_current_file:=current_scanner.inputfile.name^;
                 { restore symtable state }
                 refsymtable:=oldrefsymtable;
                 symtablestack:=oldsymtablestack;
                 defaultsymtablestack:=olddefaultsymtablestack;
                 aktdefproccall:=oldaktdefproccall;
                 current_procdef:=oldcurrent_procdef;
                 move(oldoverloaded_operators,overloaded_operators,sizeof(toverloaded_operators));
                 aktsourcecodepage:=oldsourcecodepage;
                 aktlocalswitches:=oldaktlocalswitches;
                 aktmoduleswitches:=oldaktmoduleswitches;
                 aktalignment:=oldaktalignment;
                 aktpackenum:=oldaktpackenum;
                 aktmaxfpuregisters:=oldaktmaxfpuregisters;
                 aktoutputformat:=oldaktoutputformat;
                 set_target_asm(aktoutputformat);
                 aktoptprocessor:=oldaktoptprocessor;
                 aktspecificoptprocessor:=oldaktspecificoptprocessor;
                 aktasmmode:=oldaktasmmode;
                 aktinterfacetype:=oldaktinterfacetype;
                 aktfilepos:=oldaktfilepos;
                 aktmodeswitches:=oldaktmodeswitches;
                 statement_level:=oldstatement_level;
                 aktexceptblock:=0;
                 exceptblockcounter:=0;
{$ifdef GDB}
                 dbx_counter:=store_dbx;
{$endif GDB}
               end;
           end
         else
           begin
             parser_current_file:='';
             { Shut down things when the last file is compiled }
             if (compile_level=1) then
              begin
                { Close script }
                if (not AsmRes.Empty) then
                 begin
                   Message1(exec_i_closing_script,AsmRes.Fn);
                   AsmRes.WriteToDisk;
                 end;

{$ifdef USEEXCEPT}
                if not longjump_used then
{$endif USEEXCEPT}
                 begin
                   { do not create browsers on errors !! }
                   if status.errorcount=0 then
                    begin
{$ifdef BrowserLog}
                      { Write Browser Log }
                      if (cs_browser_log in aktglobalswitches) and
                         (cs_browser in aktmoduleswitches) then
                       begin
                         if browserlog.elements_to_list.empty then
                          begin
                            Message1(parser_i_writing_browser_log,browserlog.Fname);
                            WriteBrowserLog;
                          end
                         else
                          browserlog.list_elements;
                       end;
{$endif BrowserLog}

                      { Write Browser Collections }
                      do_extractsymbolinfo{$ifdef FPC}(){$endif};
                    end;
                 end;

{$ifdef dummy}
                if current_module.in_second_compile then
                 begin
                   current_module.in_second_compile:=false;
                   current_module.in_compile:=true;
                 end
                else
                 current_module.in_compile:=false;
{$endif dummy}
              end;
           end;

         dec(compile_level);
         compiled_module:=olddata^.old_compiled_module;

         dispose(olddata);

{$ifdef USEEXCEPT}
         if longjump_used then
           longjmp(recoverpospointer^,1);
{$endif USEEXCEPT}
      end;

end.
{
  $Log$
  Revision 1.52  2003-04-27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.51  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.50  2003/04/26 00:30:52  peter
    * reset aktfilepos when setting new module for compile

  Revision 1.49  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.48  2002/12/29 14:57:50  peter
    * unit loading changed to first register units and load them
      afterwards. This is needed to support uses xxx in yyy correctly
    * unit dependency check fixed

  Revision 1.47  2002/12/24 23:32:19  peter
    * fixed crash when old_compiled_module was nil

  Revision 1.46  2002/11/20 12:36:24  mazen
  * $UNITPATH directive is now working

  Revision 1.45  2002/10/07 19:29:52  peter
    * Place old data in compile() in the heap to save stack

  Revision 1.44  2002/09/05 19:27:06  peter
    * fixed crash when current_module becomes nil

  Revision 1.43  2002/08/18 19:58:28  peter
    * more current_scanner fixes

  Revision 1.42  2002/08/16 15:31:08  peter
    * fixed possible crashes with current_scanner

  Revision 1.41  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.40  2002/08/12 16:46:04  peter
    * tscannerfile is now destroyed in tmodule.reset and current_scanner
      is updated accordingly. This removes all the loading and saving of
      the old scanner and the invalid flag marking

  Revision 1.39  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.38  2002/08/11 14:28:19  peter
    * TScannerFile.SetInvalid added that will also reset inputfile

  Revision 1.37  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.36  2002/08/09 19:15:41  carl
     - removed newcg define

  Revision 1.35  2002/07/20 17:16:03  florian
    + source code page support

  Revision 1.34  2002/07/01 18:46:24  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.33  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.32  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.30  2002/04/21 18:57:23  peter
    * fixed memleaks when file can't be opened

  Revision 1.29  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.28  2002/04/19 15:46:02  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.27  2002/01/29 19:43:11  peter
    * update target_asm according to outputformat

}
