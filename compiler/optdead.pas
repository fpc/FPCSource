{
    Copyright (c) 2008 by Jonas Maebe

    Optimization information related to dead code removal

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

unit optdead;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cclasses,
      symtype,
      wpobase;

    type

      { twpodeadcodeinfo }

      twpodeadcodeinfo = class(twpodeadcodehandler)
       private
        { hashtable of symbols which are live }
        fsymbols     : tfphashlist;

        procedure documentformat(writer: twposectionwriterintf);
       public
        constructor create; override;
        destructor destroy; override;

        class function  getwpotype: twpotype; override;
        class function  generatesinfoforwposwitches: twpoptimizerswitches; override;
        class function  performswpoforswitches: twpoptimizerswitches; override;
        class function  sectionname: shortstring; override;

        class procedure checkoptions; override;

        { information collection }
        procedure storewpofilesection(writer: twposectionwriterintf); override;

        { information providing }
        procedure loadfromwpofilesection(reader: twposectionreaderintf); override;
        function  symbolinfinalbinary(const s: shortstring): boolean;override;

      end;

      { tdeadcodeinfofromexternallinker }

      twpodeadcodeinfofromexternallinker = class(twpodeadcodeinfo)
       private

        fsymtypepos,
        fsymnamepos  : longint;
        fsymfile     : text;
        fsymfilename : tcmdstr;
        aixstrings   : TDynStringArray;
        fuseaixextractstrings : boolean;
        function parselinenm(const line: ansistring): boolean;
        function parselineobjdump(const line: ansistring): boolean;
       public
        class procedure checkoptions; override;

        { information collection }
        procedure constructfromcompilerstate; override;
        destructor destroy; override;
      end;


  implementation

  uses
    cutils,cfileutl,
    sysutils,
    globals,systems,fmodule,
    verbose;


  const
    SYMBOL_SECTION_NAME = 'live_symbols';

  { twpodeadcodeinfo }

  constructor twpodeadcodeinfo.create;
    begin
      inherited create;
      fsymbols:=tfphashlist.create;
    end;


  destructor twpodeadcodeinfo.destroy;
    begin
      fsymbols.free;
      fsymbols:=nil;
      inherited destroy;
    end;


  class function twpodeadcodeinfo.getwpotype: twpotype;
    begin
      result:=wpo_live_symbol_information;
    end;


  class function twpodeadcodeinfo.generatesinfoforwposwitches: twpoptimizerswitches;
    begin
      result:=[cs_wpo_symbol_liveness];
    end;


  class function twpodeadcodeinfo.performswpoforswitches: twpoptimizerswitches;
    begin
      result:=[cs_wpo_symbol_liveness];
    end;


  class function twpodeadcodeinfo.sectionname: shortstring;
    begin
      result:=SYMBOL_SECTION_NAME;
    end;


  class procedure twpodeadcodeinfo.checkoptions;
    begin
      { we don't have access to the symbol info if the linking
        hasn't happend
      }
      if (([cs_link_on_target,cs_link_nolink] * init_settings.globalswitches) <> []) then
        begin
          cgmessage(wpo_cannot_extract_live_symbol_info_no_link);
          exit;
        end;

      { without dead code stripping/smart linking, this doesn't make sense }
      if not(cs_link_smart in init_settings.globalswitches) then
        begin
          cgmessage(wpo_symbol_live_info_needs_smart_linking);
          exit;
        end;
    end;


  procedure twpodeadcodeinfo.documentformat(writer: twposectionwriterintf);
    begin
      writer.sectionputline('# section format:');
      writer.sectionputline('# symbol1_that_is_live');
      writer.sectionputline('# symbol2_that_is_live');
      writer.sectionputline('# ...');
      writer.sectionputline('#');
    end;


  procedure twpodeadcodeinfo.storewpofilesection(writer: twposectionwriterintf);
    var
      i: longint;
    begin
      writer.startsection(SYMBOL_SECTION_NAME);
      documentformat(writer);
      for i:=0 to fsymbols.count-1 do
        writer.sectionputline(fsymbols.nameofindex(i));
    end;


  procedure twpodeadcodeinfo.loadfromwpofilesection(reader: twposectionreaderintf);
    var
      symname: shortstring;
    begin
      while reader.sectiongetnextline(symname) do
        fsymbols.add(symname,pointer(1));
    end;


  function twpodeadcodeinfo.symbolinfinalbinary(const s: shortstring): boolean;
    begin
      result:=fsymbols.find(s)<>nil;
    end;


  { twpodeadcodeinfofromexternallinker }

{$ifdef relaxed_objdump_parsing}
const
  objdumpcheckstr='.text';
{$else}
const
  objdumpcheckstr='F .text';
{$endif}
  objdumpsearchstr=' '+objdumpcheckstr;

  class procedure twpodeadcodeinfofromexternallinker.checkoptions;
    begin
      inherited checkoptions;

      { we need symbol information }
      if (cs_link_strip in init_settings.globalswitches) then
        begin
          cgmessage(wpo_cannot_extract_live_symbol_info_strip);
          exit;
        end;
    end;


  function twpodeadcodeinfofromexternallinker.parselinenm(const line: ansistring): boolean;
    begin
      if fuseaixextractstrings then
        begin
          result:=true;
          if ExtractStrings([' ',#9],[],pchar(line),aixstrings)>=2 then
            begin
              if (length(aixstrings[1])=1) and
                 (aixstrings[1][1] in ['t','T']) and
                 (aixstrings[0][1]='.') then
                fsymbols.add(copy(aixstrings[0],2,length(aixstrings[0])),pointer(1));
            end;
          setlength(aixstrings,0);
        end
      else
        begin
          if (length(line) < fsymnamepos) then
            begin
              cgmessage1(wpo_error_reading_symbol_file,'nm');
              close(fsymfile);
              deletefile(fsymfilename);
              result:=false;
              exit;
            end;
          if (line[fsymtypepos] in ['T','t']) and
             (not(target_info.system in systems_dotted_function_names) or
              (line[fsymnamepos-1]='.')) then
            fsymbols.add(copy(line,fsymnamepos,length(line)),pointer(1));
        end;
      result:=true;
    end;


  function twpodeadcodeinfofromexternallinker.parselineobjdump(const line: ansistring): boolean;
    begin
      { there are a couple of empty lines at the end }
      if (line='') then
        begin
          result:=true;
          exit;
        end;
      if (length(line) < fsymtypepos) then
        begin
          cgmessage1(wpo_error_reading_symbol_file,'objdump');
          close(fsymfile);
          deletefile(fsymfilename);
          result:=false;
          exit;
        end;
      if (copy(line,fsymtypepos,length(objdumpcheckstr))=objdumpcheckstr) then
        fsymbols.add(copy(line,fsymnamepos,length(line)),pointer(1));
      result:=true;
    end;


  procedure twpodeadcodeinfofromexternallinker.constructfromcompilerstate;

    type
      tparselineproc = function(const line: ansistring): boolean of object;

    var
      nmfullname,
      objdumpfullname,
      symbolprogfullpath  : tcmdstr;
      line                : ansistring;
      parseline           : tparselineproc;
      exitcode            : longint;
      symbolprogfound     : boolean;
      symbolprogisnm      : boolean;


    function findutil(const utilname: string; out fullutilname, fullutilpath: tcmdstr): boolean;
      begin
        result:=false;
        fullutilname:=utilsprefix+changefileext(utilname,source_info.exeext);
        if utilsdirectory<>'' then
          result:=findfile(fullutilname,utilsdirectory,false,fullutilpath);
        if not result then
          result:=findexe(fullutilname,false,fullutilpath);
      end;


    function failiferror(error: boolean): boolean;
      begin
        result:=error;
        if not result then
          exit;
        cgmessage1(wpo_error_reading_symbol_file,symbolprogfullpath);
{$push}{$i-}
        close(fsymfile);
{$pop}
        if fileexists(fsymfilename) then
          deletefile(fsymfilename);
      end;


    function setnminfo: boolean;
      begin
        { expected format:
            0000bce0 T FPC_ABSTRACTERROR
            ...
        }
        result:=false;
        if (source_info.system in systems_aix) and
           (target_info.system in systems_aix) then
          begin
            { check for native aix nm:
              .__start             t   268435792         213
              .__start             T   268435792
            }
            if not(line[1] in ['0'..'9','a'..'f','A'..'F']) then
              begin
                fuseaixextractstrings:=true;
                setlength(aixstrings,0);
                result:=true;
                exit;
              end;
          end;
        fsymtypepos:=pos(' ',line)+1;
        fsymnamepos:=fsymtypepos+2;
        { on Linux/ppc64, there is an extra '.' at the start
          of public function names
        }
        if (target_info.system=system_powerpc64_linux) then
          inc(fsymnamepos);
        if failiferror(fsymtypepos<=0) then
          exit;
        { make sure there's room for the name }
        if failiferror(fsymnamepos>length(line)) then
          exit;
        result:=true;
      end;


    function setobjdumpinfo: boolean;
      begin
        { expected format:
            prog:     file format elf32-i386

            SYMBOL TABLE:
            08048080 l    d  .text  00000000 .text
            00000000 l    d  .stabstr       00000000 .stabstr
            00000000 l    df *ABS*  00000000 nest.pp
            08048160 l     F .text  00000068 SYSTEM_INITSYSCALLINTF
            ...
        }
        result:=false;
        while (pos(objdumpsearchstr,line)<=0) do
          begin
            if failiferror(eof(fsymfile)) then
              exit;
            readln(fsymfile,line)
          end;
        fsymtypepos:=pos(objdumpsearchstr,line)+1;
        { find begin of symbol name }
        fsymnamepos:=(pointer(strrscan(pchar(line),' '))-pointer(@line[1]))+2;
        { sanity check }
        if (fsymnamepos <= fsymtypepos+length(objdumpcheckstr)) then
          exit;
        result:=true;
      end;


    begin { twpodeadcodeinfofromexternallinker }
      objdumpfullname:='';
      fuseaixextractstrings:=false;
      { gnu-nm (e.g., on solaris) }
      symbolprogfound:=findutil('gnm',nmfullname,symbolprogfullpath);
      { regular nm }
      if not symbolprogfound then
        symbolprogfound:=findutil('nm',nmfullname,symbolprogfullpath);
      if not symbolprogfound and
         (target_info.system in systems_linux) then
        begin
          { try objdump }
          symbolprogfound:=findutil('objdump',objdumpfullname,symbolprogfullpath);
          symbolprogfullpath:=symbolprogfullpath+' -t ';
          symbolprogisnm:=false;
        end
      else
        begin
          symbolprogfullpath:=symbolprogfullpath+' -p ';
          { GNU nm shows 64 bit addresses when processing 32 bit binaries on
            a 64 bit platform, but only skips 8 spaces for the address in case
            of undefined symbols -> skip undefined symbols }
          if target_info.system in (systems_linux+systems_windows) then
            symbolprogfullpath:=symbolprogfullpath+'--defined-only ';
          symbolprogisnm:=true;
        end;
      if not symbolprogfound then
        begin
          cgmessage2(wpo_cannot_find_symbol_progs,nmfullname,objdumpfullname);
          exit;
        end;

      { upper case to have the least chance of tripping some long file name
        conversion stuff
      }
      fsymfilename:=outputexedir+'FPCWPO.SYM';
      { -p gives the same kind of output with Solaris nm as
        with GNU nm, and for GNU nm it simply means "unsorted"
      }
      exitcode:=shell(symbolprogfullpath+maybequoted(current_module.exefilename)+' > '+fsymfilename);
      if (exitcode<>0) then
        begin
          cgmessage2(wpo_error_executing_symbol_prog,symbolprogfullpath,tostr(exitcode));
          if fileexists(fsymfilename) then
            deletefile(fsymfilename);
          exit;
        end;

      assign(fsymfile,fsymfilename);
{$push}{$i-}
      reset(fsymfile);
{$pop}
      if failiferror((ioresult<>0) or eof(fsymfile)) then
        exit;
      readln(fsymfile, line);
      if (symbolprogisnm) then
        begin
          if not setnminfo then
            exit;
          parseline:=@parselinenm
        end
      else
        begin
          if not setobjdumpinfo then
            exit;
          parseline:=@parselineobjdump;
        end;
      if not parseline(line) then
        exit;
      while not eof(fsymfile) do
        begin
          readln(fsymfile,line);
          if not parseline(line) then
            exit;
        end;
      close(fsymfile);
      deletefile(fsymfilename);
    end;


  destructor twpodeadcodeinfofromexternallinker.destroy;
    begin
      inherited destroy;
    end;


end.

