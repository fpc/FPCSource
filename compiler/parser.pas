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

    procedure compile(const filename:string;compile_system:boolean);
    procedure initparser;
    procedure doneparser;

  implementation

    uses
      globtype,version,tokens,systems,
      cobjects,globals,verbose,
      symtable,files,aasm,hcodegen,
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
{$endif newcg}
      comphook,tree,scanner,pbase,pdecl,psystem,pmodules,cresstr;


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

      begin
         inc(compile_level);
         prev_name:=stringdup(parser_current_file);
         parser_current_file:=filename;
         old_compiled_module:=compiled_module;
       { save symtable state }
         oldsymtablestack:=symtablestack;
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
         oldaktoutputformat:=aktoutputformat;
         oldaktoptprocessor:=aktoptprocessor;
         oldaktasmmode:=aktasmmode;
         oldaktfilepos:=aktfilepos;
         oldaktmodeswitches:=aktmodeswitches;
{$ifdef newcg}
         oldcg:=cg;
{$endif newcg}
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
              macros:=oldmacros;
              aktprocsym:=oldaktprocsym;
              procprefix:=oldprocprefix;
              aktlocalswitches:=oldaktlocalswitches;
              aktmoduleswitches:=oldaktmoduleswitches;
              aktpackrecords:=oldaktpackrecords;
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
  Revision 1.87  1999-10-03 19:44:41  peter
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

  Revision 1.69  1999/02/25 21:02:40  peter
    * ag386bin updates
    + coff writer

  Revision 1.68  1999/02/02 16:39:41  peter
    * reset c,pattern,orgpattern also at startup

  Revision 1.67  1999/01/27 13:05:44  pierre
   * give include file name on error

  Revision 1.66  1999/01/23 23:29:35  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.65  1999/01/22 12:19:31  pierre
   + currently compiled file name added on errors

  Revision 1.64  1999/01/12 14:25:29  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.63  1998/12/11 00:03:26  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.62  1998/12/01 12:51:21  peter
    * fixed placing of ppas.sh and link.res when using -FE

  Revision 1.61  1998/11/10 10:09:11  peter
    * va_list -> array of const

  Revision 1.60  1998/10/28 18:26:14  pierre
   * removed some erros after other errors (introduced by useexcept)
   * stabs works again correctly (for how long !)

  Revision 1.59  1998/10/26 17:15:18  pierre
    + added two level of longjump to
      allow clean freeing of used memory on errors

  Revision 1.58  1998/10/16 08:50:02  peter
    * reset_gdb_info -> reset_global_def becuase it also resets rangenr !

  Revision 1.57  1998/10/08 17:17:23  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.56  1998/10/08 13:48:45  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.55  1998/10/06 17:16:53  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.54  1998/10/05 21:33:23  peter
    * fixed 161,165,166,167,168

  Revision 1.53  1998/09/30 16:43:36  peter
    * fixed unit interdependency with circular uses

  Revision 1.52  1998/09/28 16:57:22  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.51  1998/09/26 17:45:30  peter
    + idtoken and only one token table

  Revision 1.50  1998/09/24 23:49:08  peter
    + aktmodeswitches

  Revision 1.49  1998/09/23 15:39:07  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.48  1998/09/22 17:13:48  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.47  1998/09/21 09:00:18  peter
    * reset_gdb_info only when debuginfo is set

  Revision 1.46  1998/09/21 08:45:12  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.45  1998/09/18 08:01:35  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.44  1998/09/10 15:25:34  daniel
  + Added maxheapsize.
  * Corrected semi-bug in calling the assembler and the linker

  Revision 1.43  1998/09/09 15:33:06  peter
    * fixed in_global to allow directives also after interface token

  Revision 1.42  1998/09/04 08:41:59  peter
    * updated some error messages

  Revision 1.41  1998/09/01 12:53:24  peter
    + aktpackenum

  Revision 1.40  1998/09/01 07:54:19  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.39  1998/08/26 10:07:09  peter
    * dispose trees is now default for all > 0.99.5 compiles

  Revision 1.38  1998/08/18 20:52:20  peter
    * renamed in_main to in_global which is more logical

  Revision 1.37  1998/08/17 09:17:49  peter
    * static/shared linking updates

  Revision 1.36  1998/08/14 21:56:36  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.35  1998/08/12 19:22:09  peter
    * reset also the link* lists when recompiling an existing unit

  Revision 1.34  1998/08/10 23:58:56  peter
    * fixed asmlist dispose for 0.99.5

  Revision 1.33  1998/08/10 14:50:07  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.32  1998/08/10 10:18:28  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.31  1998/07/14 21:46:46  peter
    * updated messages file

  Revision 1.30  1998/07/14 14:46:49  peter
    * released NEWINPUT

  Revision 1.29  1998/07/07 11:19:59  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.28  1998/06/25 11:15:33  pierre
    * ppu files where not closed in newppu !!
      second compilation was impossible due to too many opened files
      (not visible in 'make cycle' as we remove all the ppu files)

  Revision 1.27  1998/06/17 14:10:15  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.26  1998/06/16 08:56:23  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.25  1998/06/15 15:38:07  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.24  1998/06/13 00:10:08  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.23  1998/06/08 22:59:48  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.22  1998/06/05 17:47:28  peter
    * some better uses clauses

  Revision 1.21  1998/06/04 23:51:49  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.20  1998/06/03 22:48:55  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.19  1998/05/27 19:45:04  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.18  1998/05/23 01:21:15  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.17  1998/05/20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.16  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.15  1998/05/11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.14  1998/05/06 18:36:53  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

  Revision 1.13  1998/05/06 08:38:42  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.12  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.11  1998/05/01 16:38:45  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.10  1998/05/01 07:43:56  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.9  1998/04/30 15:59:40  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.8  1998/04/29 10:33:55  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.7  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.6  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.5  1998/04/10 14:41:43  peter
    * removed some Hints
    * small speed optimization for AsmLn

  Revision 1.4  1998/04/08 16:58:03  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array
}

