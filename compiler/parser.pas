{
    $Id$
    Copyright (c) 1993-99 by Florian Klaempfl

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
{$ifdef tp}
  {$E+,N+,D+,F+}
{$endif}
unit parser;

{ Use exception catching so the compiler goes futher after a Stop }
{$ifdef i386}
  {$define USEEXCEPT}
{$endif}

{$ifdef TP}
  {$ifdef DPMI}
    {$undef USEEXCEPT}
  {$endif}
{$endif}

  interface

    procedure preprocess(const filename:string);
    procedure compile(const filename:string;compile_system:boolean);
    procedure initparser;
    procedure doneparser;

  implementation

    uses
      globtype,version,tokens,systems,
      cobjects,globals,verbose,
      symtable,files,aasm,
{$ifndef newcg}
      hcodegen,
{$endif newcg}
      assemble,link,script,gendef,
{$ifdef BrowserLog}
      browlog,
{$endif BrowserLog}
{$ifdef BrowserCol}
      browcol,
{$endif BrowserCol}
{$ifdef UseExcept}
      tpexcept,compiler,
{$endif UseExcept}
{$ifdef newcg}
      cgobj,
      cgcpu,
      { cgbase must be after hcodegen to use the correct procinfo !!! }
      cgbase,
{$endif newcg}
{$ifdef GDB}
       gdb,
{$endif GDB}
      comphook,tree,scanner,pbase,ptype,psystem,pmodules,cresstr;


    procedure initparser;
      begin
         { ^M means a string or a char, because we don't parse a }
         { type declaration                                      }
         ignore_equal:=false;

         { we didn't parse a object or class declaration }
         { and no function header                        }
         testcurobject:=0;

         { a long time, this was forgotten }
         aktprocsym:=nil;

         current_module:=nil;
         compiled_module:=nil;

         loaded_units.init;

         usedunits.init;

         { global switches }
         aktglobalswitches:=initglobalswitches;

         { scanner }
         c:=#0;
         pattern:='';
         orgpattern:='';
         current_scanner:=nil;

         { memory sizes }
         if heapsize=0 then
          heapsize:=target_info.heapsize;
         if maxheapsize=0 then
          maxheapsize:=target_info.maxheapsize;
         if stacksize=0 then
          stacksize:=target_info.stacksize;

         { open assembler response }
         AsmRes.Init(outputexedir+'ppas');

         { open deffile }
         DefFile.Init(outputexedir+inputfile+target_os.defext);
      end;


    procedure doneparser;
      begin
         { unload units }
         loaded_units.done;
         usedunits.done;

         { close ppas and deffile }
         asmres.done;
         deffile.done;
      end;


    procedure default_macros;
      var
        hp : pstring_item;
      begin
      { commandline }
        hp:=pstring_item(initdefines.first);
        while assigned(hp) do
         begin
           def_macro(hp^.str^);
           hp:=pstring_item(hp^.next);
         end;
      { set macros for version checking }
        set_macro('FPC_VERSION',version_nr);
        set_macro('FPC_RELEASE',release_nr);
        set_macro('FPC_PATCH',patch_nr);
      end;


    procedure preprocess(const filename:string);
      var
        i : longint;
      begin
         new(preprocfile,init('pre'));
       { default macros }
         macros:=new(psymtable,init(macrosymtable));
         macros^.name:=stringdup('Conditionals for '+filename);
         default_macros;
       { initialize a module }
         current_module:=new(pmodule,init(filename,false));
         main_module:=current_module;
       { startup scanner, and save in current_module }
         current_scanner:=new(pscannerfile,Init(filename));
         current_module^.scanner:=current_scanner;
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
       { close }
         dispose(preprocfile,done);
      end;


    procedure compile(const filename:string;compile_system:boolean);
      var
       { scanner }
         oldidtoken,
         oldtoken       : ttoken;
         oldtokenpos    : tfileposinfo;
         oldc           : char;
         oldpattern,
         oldorgpattern  : string;
         old_block_type : tblock_type;
         oldcurrent_scanner,prev_scanner,
         scanner : pscannerfile;
       { symtable }
         oldmacros,
         oldrefsymtable,
         olddefaultsymtablestack,
         oldsymtablestack : psymtable;
         oldprocprefix    : string;
         oldaktprocsym    : pprocsym;
       { cg }
         oldnextlabelnr : longint;
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
         oldconsts     : paasmoutput;
         oldasmsymbollist : pasmsymbollist;
       { akt.. things }
         oldaktlocalswitches  : tlocalswitches;
         oldaktmoduleswitches : tmoduleswitches;
         oldaktfilepos      : tfileposinfo;
         oldaktpackenum,oldaktmaxfpuregisters : longint;
         oldaktpackrecords  : tpackrecords;
         oldaktoutputformat : tasm;
         oldaktoptprocessor : tprocessors;
         oldaktasmmode      : tasmmode;
         oldaktmodeswitches : tmodeswitches;
         old_compiled_module : pmodule;
         prev_name          : pstring;
{$ifdef USEEXCEPT}
         recoverpos    : jmp_buf;
         oldrecoverpos : pjmp_buf;
{$endif useexcept}
{$ifdef newcg}
         oldcg         : pcg;
{$endif newcg}
{$ifdef GDB}
         store_dbx : plongint;
{$endif GDB}

      begin
         inc(compile_level);
         prev_name:=stringdup(parser_current_file);
         parser_current_file:=filename;
         old_compiled_module:=compiled_module;
       { save symtable state }
         oldsymtablestack:=symtablestack;
         olddefaultsymtablestack:=defaultsymtablestack;
         oldrefsymtable:=refsymtable;
         oldmacros:=macros;
         oldprocprefix:=procprefix;
         oldaktprocsym:=aktprocsym;
       { save scanner state }
         oldc:=c;
         oldpattern:=pattern;
         oldorgpattern:=orgpattern;
         oldtoken:=token;
         oldidtoken:=idtoken;
         old_block_type:=block_type;
         oldtokenpos:=tokenpos;
         oldcurrent_scanner:=current_scanner;
       { save cg }
         oldnextlabelnr:=nextlabelnr;
         oldparse_only:=parse_only;
       { save assembler lists }
         olddatasegment:=datasegment;
         oldbsssegment:=bsssegment;
         oldcodesegment:=codesegment;
         olddebuglist:=debuglist;
         oldconsts:=consts;
         oldrttilist:=rttilist;
         oldexprasmlist:=exprasmlist;
         oldimports:=importssection;
         oldexports:=exportssection;
         oldresource:=resourcesection;
         oldresourcestringlist:=resourcestringlist;
         oldasmsymbollist:=asmsymbollist;
       { save akt... state }
         oldaktlocalswitches:=aktlocalswitches;
         oldaktmoduleswitches:=aktmoduleswitches;
         oldaktpackrecords:=aktpackrecords;
         oldaktpackenum:=aktpackenum;
         oldaktmaxfpuregisters:=aktmaxfpuregisters;
         oldaktoutputformat:=aktoutputformat;
         oldaktoptprocessor:=aktoptprocessor;
         oldaktasmmode:=aktasmmode;
         oldaktfilepos:=aktfilepos;
         oldaktmodeswitches:=aktmodeswitches;
{$ifdef newcg}
         oldcg:=cg;
{$endif newcg}
{$ifdef GDB}
         store_dbx:=dbx_counter;
         dbx_counter:=nil;
{$endif GDB}
       { show info }
         Message1(parser_i_compiling,filename);

       { reset symtable }
         symtablestack:=nil;
         defaultsymtablestack:=nil;
         systemunit:=nil;
         refsymtable:=nil;
         aktprocsym:=nil;
         procprefix:='';
         registerdef:=true;
         aktmaxfpuregisters:=-1;
         { macros }
         macros:=new(psymtable,init(macrosymtable));
         macros^.name:=stringdup('Conditionals for '+filename);
         default_macros;

       { reset the unit or create a new program }
         if assigned(current_module) then
           begin
              {current_module^.reset this is wrong !! }
               scanner:=current_module^.scanner;
               current_module^.reset;
               current_module^.scanner:=scanner;
           end
         else
          begin
            current_module:=new(pmodule,init(filename,false));
            main_module:=current_module;
          end;

         compiled_module:=current_module;
         current_module^.in_compile:=true;
       { Load current state from the init values }
         aktlocalswitches:=initlocalswitches;
         aktmoduleswitches:=initmoduleswitches;
         aktmodeswitches:=initmodeswitches;
         aktpackrecords:=initpackrecords;
         aktpackenum:=initpackenum;
         aktoutputformat:=initoutputformat;
         aktoptprocessor:=initoptprocessor;
         aktasmmode:=initasmmode;
         { we need this to make the system unit }
         if compile_system then
          aktmoduleswitches:=aktmoduleswitches+[cs_compilesystem];

       { startup scanner, and save in current_module }
         current_scanner:=new(pscannerfile,Init(filename));
         current_scanner^.readtoken;
         prev_scanner:=current_module^.scanner;
         current_module^.scanner:=current_scanner;

       { init code generator for a new module }
         codegen_newmodule;
{$ifdef newcg}
{$ifdef i386}
         cg:=new(pcg386,init);
{$endif i386}
{$ifdef alpha}
         cg:=new(pcgalpha,init);
{$endif alpha}
{$ifdef powerpc}
         cg:=new(pcgppc,init);
{$endif powerpc}
{$endif newcg}

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
                current_module^.is_unit:=true;
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

{$ifdef newcg}
         dispose(cg,done);
{$endif newcg}

       { free ppu }
         if assigned(current_module^.ppufile) then
          begin
            dispose(current_module^.ppufile,done);
            current_module^.ppufile:=nil;
          end;
       { free scanner }
         dispose(current_scanner,done);
       { restore previous scanner !! }
         current_module^.scanner:=prev_scanner;
         if assigned(prev_scanner) then
           prev_scanner^.invalid:=true;

       { free macros }
         {!!! No check for unused macros yet !!! }
         dispose(macros,done);

         if (compile_level>1) then
           begin
{$ifdef newcg}
              cg:=oldcg;
{$endif newcg}
{$ifdef GDB}
              dbx_counter:=store_dbx;
{$endif GDB}
              { restore scanner }
              c:=oldc;
              pattern:=oldpattern;
              orgpattern:=oldorgpattern;
              token:=oldtoken;
              idtoken:=oldidtoken;
              tokenpos:=oldtokenpos;
              block_type:=old_block_type;
              current_scanner:=oldcurrent_scanner;
              { restore cg }
              nextlabelnr:=oldnextlabelnr;
              parse_only:=oldparse_only;
              { restore asmlists }
              exprasmlist:=oldexprasmlist;
              datasegment:=olddatasegment;
              bsssegment:=oldbsssegment;
              codesegment:=oldcodesegment;
              consts:=oldconsts;
              debuglist:=olddebuglist;
              importssection:=oldimports;
              exportssection:=oldexports;
              resourcesection:=oldresource;
              rttilist:=oldrttilist;
              resourcestringlist:=oldresourcestringlist;

              asmsymbollist:=oldasmsymbollist;
              { restore symtable state }
              refsymtable:=oldrefsymtable;
              symtablestack:=oldsymtablestack;
              defaultsymtablestack:=olddefaultsymtablestack;
              macros:=oldmacros;
              aktprocsym:=oldaktprocsym;
              procprefix:=oldprocprefix;
              aktlocalswitches:=oldaktlocalswitches;
              aktmoduleswitches:=oldaktmoduleswitches;
              aktpackrecords:=oldaktpackrecords;
              aktpackenum:=oldaktpackenum;
              aktmaxfpuregisters:=oldaktmaxfpuregisters;
              aktoutputformat:=oldaktoutputformat;
              aktoptprocessor:=oldaktoptprocessor;
              aktasmmode:=oldaktasmmode;
              aktfilepos:=oldaktfilepos;
              aktmodeswitches:=oldaktmodeswitches;
           end;
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
            { do not create browsers on errors !! }
            if status.errorcount=0 then
              begin
{$ifdef BrowserLog}
              { Write Browser Log }
              if (cs_browser_log in aktglobalswitches) and
                 (cs_browser in aktmoduleswitches) then
                 begin
                 if browserlog.elements_to_list^.empty then
                   begin
                   Message1(parser_i_writing_browser_log,browserlog.Fname);
                   WriteBrowserLog;
                   end
                 else
                  browserlog.list_elements;
                 end;
{$endif BrowserLog}

{$ifdef BrowserCol}
              { Write Browser Collections }
              CreateBrowserCol;
{$endif}
              end;

         if current_module^.in_second_compile then
           begin
             current_module^.in_second_compile:=false;
             current_module^.in_compile:=true;
           end
         else
           current_module^.in_compile:=false;

          (* Obsolete code aktprocsym
             is disposed by the localsymtable disposal (PM)
          { Free last aktprocsym }
            if assigned(aktprocsym) and (aktprocsym^.owner=nil) then
             begin
               { init parts are not needed in units !! }
               if current_module^.is_unit then
                 aktprocsym^.definition^.forwarddef:=false;
               dispose(aktprocsym,done);
             end; *)
          end;

         dec(compile_level);
         parser_current_file:=prev_name^;
         stringdispose(prev_name);
         compiled_module:=old_compiled_module;
{$ifdef USEEXCEPT}
         if longjump_used then
           longjmp(recoverpospointer^,1);
{$endif USEEXCEPT}
      end;

end.
{
  $Log$
  Revision 1.95  2000-01-04 15:15:52  florian
    + added compiler switch $maxfpuregisters
    + fixed a small problem in secondvecn

  Revision 1.94  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.93  1999/11/24 11:41:03  pierre
   * defaultsymtablestack is now restored after parser.compile

  Revision 1.92  1999/11/18 15:34:46  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.91  1999/11/09 23:48:47  pierre
   * some DBX work, still does not work

  Revision 1.90  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.89  1999/10/22 10:39:34  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.88  1999/10/12 21:20:45  florian
    * new codegenerator compiles again

  Revision 1.87  1999/10/03 19:44:41  peter
    * removed objpasunit reference, tvarrec is now searched in systemunit
      where it already was located

  Revision 1.86  1999/10/01 08:02:45  peter
    * forward type declaration rewritten

  Revision 1.85  1999/09/16 08:02:39  pierre
   + old_compiled_module to avoid wrong file info when load PPU files

  Revision 1.84  1999/09/15 22:09:23  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.83  1999/08/31 15:51:11  pierre
   * in_second_compile cleaned up, in_compile and in_second_load added

  Revision 1.82  1999/08/26 20:24:41  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.81  1999/08/04 13:02:48  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.80  1999/08/03 17:09:37  florian
    * the alpha compiler can be compiled now

  Revision 1.79  1999/08/01 23:36:40  florian
    * some changes to compile the new code generator

  Revision 1.78  1999/07/24 16:22:18  michael
  + Improved resourcestring handling

  Revision 1.77  1999/07/23 16:05:22  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.76  1999/07/22 09:37:49  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.75  1999/06/15 13:23:48  pierre
   * don't generate browser if errors during compilation

  Revision 1.74  1999/05/27 19:44:41  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.73  1999/05/18 22:35:52  pierre
   * double dispose of aktprocsym removed

  Revision 1.72  1999/04/26 13:31:36  peter
    * release storenumber,double_checksum

  Revision 1.71  1999/03/26 00:05:33  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.70  1999/03/24 23:17:10  peter
    * fixed bugs 212,222,225,227,229,231,233

}
