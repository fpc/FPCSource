{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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

  interface

    procedure compile(const filename:string;compile_system:boolean);
    procedure initparser;

  implementation

    uses
       dos,objects,cobjects,globals,scanner,systems,symtable,tree,aasm,
       types,strings,pass_1,hcodegen,files,verbose,script,import
{$ifdef i386}
       ,i386
       ,cgi386
       ,cgai386
       ,tgeni386
       ,aopt386
{$endif i386}
{$ifdef m68k}
        ,m68k
        ,cg68k
        ,tgen68k
        ,cga68k
{$endif m68k}
       { parser units }
       ,pbase,pmodules,pdecl,
       { assembling & linking }
       assemble,
       link;

  { dummy variable for search when calling exec }
  var
     file_found : boolean;

    procedure readconstdefs;

      begin
         s32bitdef:=porddef(globaldef('longint'));
         u32bitdef:=porddef(globaldef('ulong'));
         cstringdef:=pstringdef(globaldef('string'));
{$ifdef UseLongString}
         clongstringdef:=pstringdef(globaldef('longstring'));
{$endif UseLongString}
{$ifdef UseAnsiString}
         cansistringdef:=pstringdef(globaldef('ansistring'));
{$endif UseAnsiString}
         cchardef:=porddef(globaldef('char'));
{$ifdef i386}
         c64floatdef:=pfloatdef(globaldef('s64real'));
{$endif}
{$ifdef m68k}
         c64floatdef:=pfloatdef(globaldef('s32real'));
{$endif m68k}
         s80floatdef:=pfloatdef(globaldef('s80real'));
         s32fixeddef:=pfloatdef(globaldef('cs32fixed'));
         voiddef:=porddef(globaldef('void'));
         u8bitdef:=porddef(globaldef('byte'));
         u16bitdef:=porddef(globaldef('word'));
         booldef:=porddef(globaldef('boolean'));
         voidpointerdef:=ppointerdef(globaldef('void_pointer'));
         cfiledef:=pfiledef(globaldef('file'));
      end;

    procedure initparser;

      begin
         forwardsallowed:=false;

         { ^M means a string or a char, because we don't parse a }
         { type declaration                                      }
         block_type:=bt_general;
         ignore_equal:=false;

         { we didn't parse a object or class declaration }
         { and no function header                        }
         testcurobject:=0;

         { create error defintion }
         generrordef:=new(perrordef,init);

         symtablestack:=nil;

         { a long time, this was forgotten }
         aktprocsym:=nil;

         current_module:=nil;

         loaded_units.init;

         usedunits.init;
      end;

    { moved out to save stack }
    var
       addparam : string;

    procedure compile(const filename:string;compile_system:boolean);
      var
         hp : pmodule;
         comp_unit : boolean;

         { some variables to save the compiler state }
         oldtoken : ttoken;
         oldpattern : stringid;

         oldpreprocstack : ppreprocstack;
         oldorgpattern,oldprocprefix : string;
         old_block_type : tblock_type;
         oldinputbuffer : pchar;
         oldinputpointer : longint;
         olds_point,oldparse_only : boolean;
         oldc : char;
         oldcomment_level : word;

         oldimports,oldexports,oldresource,
         oldbsssegment,olddatasegment,oldcodesegment,
         oldexprasmlist,olddebuglist,
         oldinternals,oldexternals,oldconsts : paasmoutput;


         oldnextlabelnr : longint;

         oldswitches : Tcswitches;
         oldmacros,oldrefsymtable,oldsymtablestack : psymtable;


      procedure def_macro(const s : string);

        var
          mac : pmacrosym;

        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               Message1(parser_m_macro_defined,mac^.name);
               macros^.insert(mac);
             end;
           mac^.defined:=true;
        end;

      procedure set_macro(const s : string;value : string);

        var
          mac : pmacrosym;

        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               macros^.insert(mac);
             end
           else
             begin
                if assigned(mac^.buftext) then
                  freemem(mac^.buftext,mac^.buflen);
             end;
           Message2(parser_m_macro_set_to,mac^.name,value);
           mac^.buflen:=length(value);
           getmem(mac^.buftext,mac^.buflen);
           move(value[1],mac^.buftext^,mac^.buflen);
           mac^.defined:=true;
        end;

      procedure define_macros;

        var
           hp : pstring_item;

        begin
           hp:=pstring_item(commandlinedefines.first);
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

      var
         a : PAsmFile;
         g : file;
         ftime : longint;
      label
         done;

      begin {compile}
         inc(compile_level);
         { save old state }

         { save symtable state }
         oldsymtablestack:=symtablestack;
         symtablestack:=nil;
         oldrefsymtable:=refsymtable;
         refsymtable:=nil;
         oldprocprefix:=procprefix;

         { a long time, this was only in init_parser
           but it should be reset to zero for each module }
         aktprocsym:=nil;

         { first, we assume a program }
         if not(assigned(current_module)) then
           begin
{!!!}
              current_module:=new(pmodule,init(filename,false));
              main_module:=current_module;
           end;
         { reset flags }
         current_module^.flags:=0;
         { ... and crc }
         current_module^.crc:=0;

         { save scanner state }
         oldmacros:=macros;
         oldpattern:=pattern;
         oldtoken:=token;
         oldorgpattern:=orgpattern;
         old_block_type:=block_type;
         oldpreprocstack:=preprocstack;

         oldinputbuffer:=inputbuffer;
         oldinputpointer:=inputpointer;
         olds_point:=s_point;
         oldc:=c;
         oldcomment_level:=comment_level;

         oldparse_only:=parse_only;

         { save assembler lists }
         olddatasegment:=datasegment;
         oldbsssegment:=bsssegment;
         oldcodesegment:=codesegment;
         olddebuglist:=debuglist;
         oldexternals:=externals;
         oldinternals:=internals;
         oldconsts:=consts;
         oldexprasmlist:=exprasmlist;
         oldimports:=importssection;
         oldexports:=exportssection;
         oldresource:=resourcesection;

         oldswitches:=aktswitches;
         oldnextlabelnr:=nextlabelnr;

         Message1(parser_i_compiling,filename);

         InitScanner(filename);

         aktswitches:=initswitches;

         { we need this to make the system unit }
         if compile_system then
          aktswitches:=aktswitches+[cs_compilesystem];

         aktpackrecords:=initpackrecords;

         { init code generator for a new module }
         codegen_newmodule;
         macros:=new(psymtable,init(macrosymtable));

         define_macros;

         { startup scanner }
         token:=yylex;

         { init asm writing }
         datasegment:=new(paasmoutput,init);
         codesegment:=new(paasmoutput,init);
         bsssegment:=new(paasmoutput,init);
         debuglist:=new(paasmoutput,init);
         externals:=new(paasmoutput,init);
         internals:=new(paasmoutput,init);
         consts:=new(paasmoutput,init);
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;

         { global switches are read, so further changes aren't allowed }
         current_module^.in_main:=true;

         { open assembler response }
         if (compile_level=1) then
          AsmRes.Init('ppas');

         { if the current file isn't a system unit  }
         { the the system unit will be loaded       }
         if not(cs_compilesystem in aktswitches) then
           begin
              { should be done in unit system (changing the field system_unit)
                                                                      FK
              }
              hp:=loadunit(upper(target_info.system_unit),true,true);
              systemunit:=hp^.symtable;
              readconstdefs;
              { we could try to overload caret by default }
              symtablestack:=systemunit;
              { if POWER is defined in the RTL then use it for caret overloading }
              getsym('POWER',false);
              if assigned(srsym) and (srsym^.typ=procsym) and
                 (overloaded_operators[CARET]=nil) then
                overloaded_operators[CARET]:=pprocsym(srsym);
           end
         else
           begin
              { create definitions for constants }
              registerdef:=false;
              s32bitdef:=new(porddef,init(s32bit,$80000000,$7fffffff));
              u32bitdef:=new(porddef,init(u32bit,0,$ffffffff));
              cstringdef:=new(pstringdef,init(255));
              { should we give a length to the default long and ansi string definition ?? }
{$ifdef UseLongString}
              clongstringdef:=new(pstringdef,longinit(-1));
{$endif UseLongString}
{$ifdef UseAnsiString}
              cansistringdef:=new(pstringdef,ansiinit(-1));
{$endif UseAnsiString}
              cchardef:=new(porddef,init(uchar,0,255));
{$ifdef i386}
              c64floatdef:=new(pfloatdef,init(s64real));
              s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
{$ifdef m68k}
              c64floatdef:=new(pfloatdef,init(s32real));
              if (cs_fp_emulation in aktswitches) then
               s80floatdef:=new(pfloatdef,init(s32real))
              else
               s80floatdef:=new(pfloatdef,init(s80real));
{$endif}
              s32fixeddef:=new(pfloatdef,init(f32bit));

              { some other definitions }
              voiddef:=new(porddef,init(uvoid,0,0));
              u8bitdef:=new(porddef,init(u8bit,0,255));
              u16bitdef:=new(porddef,init(u16bit,0,65535));
              booldef:=new(porddef,init(bool8bit,0,1));
              voidpointerdef:=new(ppointerdef,init(voiddef));
              cfiledef:=new(pfiledef,init(ft_untyped,nil));
              systemunit:=nil;
           end;
         registerdef:=true;

         { current return type is void }
         procinfo.retdef:=voiddef;

         { reset lexical level }
         lexlevel:=0;

         { parse source }
{***BUGFIX}
         if (token=_UNIT) or (compile_level>1) then
            begin
                {If the compile level > 1 we get a nice "unit expected" error
                 message if we are trying to use a program as unit.}
                proc_unit;
                if current_module^.compiled then
                    goto done;
                comp_unit:=true;
            end
         else
           begin
              proc_program(token=_LIBRARY);
              comp_unit:=false;
           end;

         { Why? The definition of Pascal requires that everything
           after 'end.' is ignored!
         if not(cs_tp_compatible in aktswitches) then
            consume(_EOF); }

         if errorcount=0 then
           begin
             if current_module^.uses_imports then
              importlib^.generatelib;

             a:=new(PAsmFile,Init(filename));
             a^.WriteAsmSource;
             a^.DoAssemble;
             dispose(a,Done);

             { Check linking  => we are at first level in compile }
             if (compile_level=1) then
              begin
	        if Linker.ExeName='' then
                 Linker.SetFileName(FileName);
                if (comp_unit) then
                 begin
                   Linker.Make_Library;
                 end
                else
                 begin
                   if (cs_no_linking in initswitches) then
                    externlink:=true;
                   Linker.Link;
                 end;
              end;
           end
         else
           begin
              Message1(unit_e_total_errors,tostr(errorcount));
              Message(unit_f_errors_in_unit);
           end;
         { clear memory }
{$ifdef Splitheap}
         if testsplit then
           begin
           { temp heap should be empty after that !!!}
           codegen_donemodule;
           Releasetempheap;
           end;
         {else
           codegen_donemodule;}
{$endif Splitheap}
         { restore old state }
         { if already compiled jumps directly here }
done:
         { close trees }
         if dispose_asm_lists then
           begin
              dispose(datasegment,Done);
              dispose(codesegment,Done);
              dispose(bsssegment,Done);
              dispose(debuglist,Done);
              dispose(externals,Done);
              dispose(internals,Done);
              dispose(consts,Done);
           end;

         { restore symtable state }
{$ifdef UseBrowser}
         if (compile_level>1) then
{ we want to keep the current symtablestack }
{$endif UseBrowser}
           begin
              refsymtable:=oldrefsymtable;
              symtablestack:=oldsymtablestack;
           end;

         procprefix:=oldprocprefix;

         { close the inputfiles }
{$ifndef UseBrowser}
         { but not if we want the names for the browser ! }
         current_module^.sourcefiles.done;
{$endif not UseBrowser}
         { restore scanner state }
         pattern:=oldpattern;
         token:=oldtoken;
         orgpattern:=oldorgpattern;
         block_type:=old_block_type;

         { call donescanner before restoring preprocstack, because }
         { donescanner tests for a empty preprocstack              }
         { and can also check for unused macros                    }
         donescanner(current_module^.compiled);
         dispose(macros,done);
         macros:=oldmacros;


         preprocstack:=oldpreprocstack;

         aktswitches:=oldswitches;
         inputbuffer:=oldinputbuffer;
         inputpointer:=oldinputpointer;
         s_point:=olds_point;
         c:=oldc;
         comment_level:=oldcomment_level;

         parse_only:=oldparse_only;

         { restore asmlists }
         datasegment:=olddatasegment;
         bsssegment:=oldbsssegment;
         codesegment:=oldcodesegment;
         debuglist:=olddebuglist;
         externals:=oldexternals;
         internals:=oldinternals;
         importssection:=oldimports;
         exportssection:=oldexports;
         resourcesection:=oldresource;

         nextlabelnr:=oldnextlabelnr;
         exprasmlist:=oldexprasmlist;
         consts:=oldconsts;

         nextlabelnr:=oldnextlabelnr;

         reset_gdb_info;
         if (compile_level=1) then
          begin
            if (not AsmRes.Empty) then
             begin
               Message1(exec_i_closing_script,AsmRes.Fn);
               AsmRes.WriteToDisk;
             end;
          end;
         dec(compile_level);
      end;

end.
{
  $Log$
  Revision 1.4  1998-04-08 16:58:03  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.2  1998/03/26 11:18:30  florian
    - switch -Sa removed
    - support of a:=b:=0 removed

  Revision 1.1.1.1  1998/03/25 11:18:12  root
  * Restored version

  Revision 1.60  1998/03/24 21:48:32  florian
    * just a couple of fixes applied:
         - problem with fixed16 solved
         - internalerror 10005 problem fixed
         - patch for assembler reading
         - small optimizer fix
         - mem is now supported

  Revision 1.59  1998/03/20 23:31:33  florian
    * bug0113 fixed
    * problem with interdepened units fixed ("options.pas problem")
    * two small extensions for future AMD 3D support

  Revision 1.58  1998/03/13 22:45:58  florian
    * small bug fixes applied

  Revision 1.57  1998/03/10 17:19:29  peter
    * fixed bug0108
    * better linebreak scanning (concentrated in nextchar(), it supports
      #10, #13, #10#13, #13#10

  Revision 1.56  1998/03/10 16:27:40  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.55  1998/03/10 12:54:06  peter
    * def_symbol renamed to def_macro and use it in defines_macros

  Revision 1.54  1998/03/10 01:17:22  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.53  1998/03/06 00:52:34  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.52  1998/03/02 16:00:37  peter
    * -Ch works again

  Revision 1.51  1998/03/02 13:38:44  peter
    + importlib object
    * doesn't crash on a systemunit anymore
    * updated makefile and depend

  Revision 1.49  1998/02/28 00:20:31  florian
    * more changes to get import libs for Win32 working

  Revision 1.48  1998/02/27 22:27:56  florian
    + win_targ unit
    + support of sections
    + new asmlists: sections, exports and resource

  Revision 1.47  1998/02/24 10:29:17  peter
    * -a works again

  Revision 1.46  1998/02/24 00:19:14  peter
    * makefile works again (btw. linux does like any char after a \ )
    * removed circular unit with assemble and files
    * fixed a sigsegv in pexpr
    * pmodule init unit/program is the almost the same, merged them

  Revision 1.45  1998/02/22 23:03:25  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.44  1998/02/19 00:11:04  peter
    * fixed -g to work again
    * fixed some typos with the scriptobject

  Revision 1.43  1998/02/18 13:48:12  michael
  + Implemented an OS independent AsmRes object.

  Revision 1.42  1998/02/17 21:20:54  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.41  1998/02/16 12:51:35  michael
  + Implemented linker object

  Revision 1.40  1998/02/15 21:16:25  peter
    * all assembler outputs supported by assemblerobject
    * cleanup with assembleroutputs, better .ascii generation
    * help_constructor/destructor are now added to the externals
    - generation of asmresponse is not outputformat depended

  Revision 1.39  1998/02/14 01:45:26  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.38  1998/02/13 22:26:33  peter
    * fixed a few SigSegv's
    * INIT$$ was not written for linux!
    * assembling and linking works again for linux and dos
    + assembler object, only attasmi3 supported yet
    * restore pp.pas with AddPath etc.

  Revision 1.37  1998/02/13 10:35:17  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.36  1998/02/12 17:19:12  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.35  1998/02/12 11:50:16  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.34  1998/02/02 11:47:36  pierre
    + compilation stops at unit with error

  Revision 1.33  1998/02/01 22:41:08  florian
    * clean up
    + system.assigned([class])
    + system.assigned([class of xxxx])
    * first fixes of as and is-operator

  Revision 1.32  1998/01/30 17:31:23  pierre
    * bug of cyclic symtablestack fixed

  Revision 1.31  1998/01/29 12:13:21  michael
  * fixed Typos for library making

  Revision 1.30  1998/01/28 13:48:45  michael
  + Initial implementation for making libs from within FPC. Not tested, as compiler does not run

  Revision 1.29  1998/01/25 18:45:47  peter
    + Search for as and ld at startup
    + source_info works the same as target_info
    + externlink allows only external linking

  Revision 1.28  1998/01/23 21:14:59  carl
    * RunError 105 (file not open) with -Agas switch fix

  Revision 1.27  1998/01/23 17:55:11  michael
  + Moved linking stage to it's own unit (link.pas)
    Incorporated Pierres changes, but removed -E switch
    switch for not linking is now -Cn instead of -E

  Revision 1.26  1998/01/23 17:12:15  pierre
    * added some improvements for as and ld :
      - doserror and dosexitcode treated separately
      - PATH searched if doserror=2
    + start of long and ansi string (far from complete)
      in conditionnal UseLongString and UseAnsiString
    * options.pas cleaned (some variables shifted to globals)gl

  Revision 1.25  1998/01/22 14:47:16  michael
  + Reinstated linker options as -k option. How did they dissapear ?

  Revision 1.24  1998/01/17 01:57:36  michael
  + Start of shared library support. First working version.

  Revision 1.23  1998/01/16 22:34:37  michael
  * Changed 'conversation' to 'conversion'. Waayyy too much chatting going on
    in this compiler :)

  Revision 1.22  1998/01/14 12:52:04  michael
  - Removed the 'Assembled' line and replaced 'Calling Linker/assembler...'
    with 'Assembling/linking...'. Too much verbosity when V_info is on.

  Revision 1.21  1998/01/13 16:15:56  pierre
    *  bug in interdependent units handling
       - primary unit was not in loaded_units list
       - current_module^.symtable was assigned too early
       - donescanner must not call error if the compilation
       of the unit was done at a higher level.

  Revision 1.20  1998/01/11 10:54:22  florian
    + generic library support

  Revision 1.19  1998/01/11 04:17:11  carl
  + correct heap and memory variables for m68k targets

  Revision 1.18  1998/01/08 23:56:39  florian
    * parser unit divided into several smaller units

  Revision 1.17  1998/01/08 17:10:12  florian
    * the name of the initialization part of a unit was sometimes written
      in lower case

  Revision 1.16  1998/01/07 00:16:56  michael
  Restored released version (plus fixes) as current

  Revision 1.13  1997/12/14 22:43:21  florian
    + command line switch -Xs for DOS (passes -s to the linker to strip symbols from
      executable)
    * some changes of Carl-Eric implemented

  Revision 1.12  1997/12/12 13:28:31  florian
  + version 0.99.0
  * all WASM options changed into MASM
  + -O2 for Pentium II optimizations

  Revision 1.11  1997/12/10 23:07:21  florian
  * bugs fixed: 12,38 (also m68k),39,40,41
  + warning if a system unit is without -Us compiled
  + warning if a method is virtual and private (was an error)
  * some indentions changed
  + factor does a better error recovering (omit some crashes)
  + problem with @type(x) removed (crashed the compiler)

  Revision 1.10  1997/12/09 13:50:36  carl
  * bugfix of possible alignment problems with m68k
  * bugfix of circular unit use -- could still give a stack overflow,
    so changed to fatalerror instead.
  * bugfix of nil procedural variables, fpc = @nil is illogical!
    (if was reversed!)

  Revision 1.9  1997/12/08 13:31:31  daniel
  * File was in DOS format. Translated to Unix.

  Revision 1.8  1997/12/08 10:01:08  pierre
    * nil for a procvar const was not allowed (os2_targ.pas not compilable)
    * bug in loadunit for units in implementation part already loaded
      (crashed on dos_targ.pas, thanks to Daniel who permitted me
      to find this bug out)

  Revision 1.7  1997/12/04 17:47:50  carl
   + renamed m68k units and refs to these units according to cpu rules.

  Revision 1.6  1997/12/02 15:56:13  carl
  * bugfix of postfixoperator with pd =nil
  * bugfix of motorola instructions types for exit code.

  Revision 1.5  1997/12/01 18:14:33  pierre
      * fixes a bug in nasm output due to my previous changes

  Revision 1.3  1997/11/28 18:14:40  pierre
   working version with several bug fixes

  Revision 1.2  1997/11/27 17:59:46  carl
  * made it compile under BP (line too long errors)

  Revision 1.1.1.1  1997/11/27 08:32:57  michael
  FPC Compiler CVS start


  Pre-CVS log:

  CEC   Carl-Eric Codere
  FK    Florian Klaempfl
  PM    Pierre Muller
  +     feature added
  -     removed
  *     bug fixed or changed



  History (started with version 0.9.0):
       5th november 1996:
         * adapted to 0.9.0
      25th november 1996:
         * more stuff adapted
       9th december 1996:
         + support for different inline assemblers added (FK)
      22th september:
         + support for PACKED RECORD implemented (FK)
      24th september:
         + internal support of system.seg added (FK)
         * system.ofs bug fixed (FK)
         * problem with compiler switches solved (see also pass_1.pas) (FK)
         * all aktswitch memory is now recoverd (see also scanner.pas) (FK)
      24th september 1997:
         * bug in ESI offset, pushed only if not nested, changed in cgi386.pas in v93 by (FK)
           but forgotten here (PM)
      25th september:
         + parsing of open arrays implemented (FK)
         + parsing of high and low implemented (FK)
         + open array support also for proc vars added (FK)
      1th october:
         * in typed constants is now the conversion int -> real
           automatically done (FK)
      3rd october 1997:
         + started conversion to motorola 68000 - ifdef m68k to find
           changes (this makes the code look horrible, may later separate
           in includes?) - look for all ifdef i386 and ifdef m68k to see
           changes. (CEC)
         - commented out regnames (was unused) (CEC)
         + peepholeopt put in i386 define (not yet available for motorola
            68000) (CEC)
         + i386 defines around a_att, in a_o and around a_wasm, a_nasm (CEC).
         + code for in_ord_x (PM)
      4th october 1997:
         + checking for double definitions of function/procedure
           with same parameters in interface (PM)
         + enums with jumps set the boolean has_jumps and
           disable the succ and pred use (PM)
       9th october 1997:
         * Fixed problem with 80float on the 68000 output, caused a GPF (CEC).
      13th october 1997:
         + Added support for Motorola Standard assembler. (CEC)
       15th october 1997:
         + added code for static modifier for objects variables and methods
           controlled by -St switch at command line (PM)
      17th october 1997:
        + Added support for Motorola inline assembler (CEC)
        * Bugfix with .align 4,0x90, this is NOT allowed in TASM/MASM/WASM (CEC).
        + procedure set_macro and setting of fpc_* macros (FK)
      19th october 1997:
        * Bugfix of RTS on 68000 target. PC Counter would become corrupt
          with paramsize (CEC).
      23th october 1997:
        * Small bugfixes concerning SUBI #const,address_reg (CEC).
      24th october 1997:
        * array[boolean] works now (FK)
      25th october 1997:
        + CDECL and STDCALL (FK)
        * ASSEMBLER isn't a keyword anymore (FK)
       3rd november 1997:
         + added symdif for sets (PM)
       5th november 1997:
         * changed all while token<>ATOKEN do consume(token);
           by a procedure call consume_all_untill(ATOKEN)
           to test for _EOF (PM)
         * aktobjectname was not reset to '' at the end of objectcomponenten (PM)
       14th november 1997:
         * removed bug for procvar where the type was not allways set correctly (PM)
         + integer const not in longint range converted to real constant (PM)
       25th november 1997:
         * removed bugs due to wrong current_module references in compile procedure (PM)

}


