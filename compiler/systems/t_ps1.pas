{
    Copyright (c) 2024 by Kirill Kranz

    This unit implements support link routines
    for the (MIPSEL) PS1 target

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
unit t_ps1;

{$i fpcdefs.inc}

interface
uses link;

type
    TLinkerPS1=class(TExternalLinker)
    private

      procedure WriteScriptFile;
      function CreatePSXEXE(name: string): boolean;

    public

      constructor Create; override;

      procedure SetDefaultInfo; override;
      function  MakeExecutable: boolean; override;
      procedure InitSysInitUnitName; override;

    end;


implementation
uses
    sysutils, classes,
    cutils, cfileutl, cclasses,
    globtype, globals, systems, verbose, comphook, cscript, fmodule,
    i_ps1;


constructor TLinkerPS1.Create;
begin
  Inherited Create;
end;


procedure TLinkerPS1.SetDefaultInfo;
begin
  info.ExeCmd[1]:= 'ld $OPT $RES';
end;


procedure TLinkerPS1.WriteScriptFile;
Var
    i : integer;
    linkres  : TLinkRes;
    s : TCmdStr;
    megastr : ansistring;
    newname : ansistring;
    HPath    : TCmdStrListItem;

begin

    { Open link.res file }
    LinkRes:= TLinkRes.Create(outputexedir + Info.ResName, true);


    LinkRes.Add('MEMORY');
    LinkRes.Add('{');
    LinkRes.Add('ram (rwx) : ORIGIN = 0x80010000, LENGTH = 2M - 64K');
    LinkRes.Add('}');
    LinkRes.Add('ENTRY(main)');


    HPath:= TCmdStrListItem(LibrarySearchPath.First);
    while assigned(HPath) do begin
      s:= HPath.Str;
      
      if s <> '' then LinkRes.Add('SEARCH_DIR("' + s + '")');
      
      HPath:= TCmdStrListItem(HPath.Next);
    end;


    LinkRes.Add('INPUT(' + ExtractFileName(ObjectFiles.GetFirst) + ')');  // have to be "si_prc.o"
    while not ObjectFiles.Empty do begin
        s:= ObjectFiles.GetFirst;
        if s <> '' then LinkRes.Add('INPUT(' + ExtractFileName(s) + ')');        
    end;

    LinkRes.Add('INPUT(libcard.a libpress.a libgpu.a libgs.a libgte.a)');
    LinkRes.Add('INPUT(libcd.a libetc.a libsn.a libsnd.a libspu.a)');
    LinkRes.Add('INPUT(libmath.a libcomb.a libcard.a libtap.a libsio.a)');
    LinkRes.Add('INPUT(libpad.a libc2.a libapi.a)');

    LinkRes.Add('SECTIONS');
    LinkRes.Add('{');
    LinkRes.Add('   .text : ALIGN(8) {');
    LinkRes.Add('         __exe_start__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __text_start__ = .;');
    LinkRes.Add('           KEEP(*(.text.n_main));');
    LinkRes.Add('           *(.text .text.*)');
    LinkRes.Add('         __text_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __text_size__ = __text_start__ - __text_end__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   .rodata : ALIGN(8) {');
    LinkRes.Add('');
    LinkRes.Add('         __rdata_start__ = .;');
    LinkRes.Add('           *(.rdata .rdata.* .rodata .rodata.*)');
    LinkRes.Add('         __rdata_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __rdata_size__ = __rdata_end__ - __rdata_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   .ctors : ALIGN(8) {');
    LinkRes.Add('');
    LinkRes.Add('         __ctors_start__ = .;');
    LinkRes.Add('           KEEP(*(.ctors))');
    LinkRes.Add('           KEEP(*(SORT(.ctors.*)))');
    LinkRes.Add('         __ctors_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __ctors_size__ = __ctors_end__ - __ctors_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   .dtors : ALIGN(8) {');
    LinkRes.Add('');
    LinkRes.Add('         __dtors_start__ = .;');
    LinkRes.Add('           KEEP(*(.dtors))');
    LinkRes.Add('           KEEP(*(SORT(.dtors.*)))');
    LinkRes.Add('         __dtors_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __dtors_size__ = __dtors_end__ - __dtors_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   .data : ALIGN(8) {');
    LinkRes.Add('');
    LinkRes.Add('         __sdata_start__ = .;');
    LinkRes.Add('           *(.sdata .sdata.*)');
    LinkRes.Add('         __sdata_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __sdata_size__ = __sdata_end__ - __sdata_start__;');
    LinkRes.Add('');
    LinkRes.Add('         __data_start__ = .;');
    LinkRes.Add('           *(.data .data.*)');
    LinkRes.Add('           SORT(CONSTRUCTORS)');
    LinkRes.Add('           . = ALIGN(2048);');
    LinkRes.Add('         __data_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __exe_end__ = .;');
    LinkRes.Add('         __data_size__ = __data_end__ - __data_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   __exe_size__ = __exe_end__ - __exe_start__;');
    LinkRes.Add('');
    LinkRes.Add('   .sbss : ALIGN(64) {');
    LinkRes.Add('');
    LinkRes.Add('         __sbss_start__ = .;');
    LinkRes.Add('           *(.sbss .sbss.*)');
    LinkRes.Add('         __sbss_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __sbss_size__ = __sbss_end__ - __sbss_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('');
    LinkRes.Add('   .bss : ALIGN(64) {');
    LinkRes.Add('');
    LinkRes.Add('         __bss_start__ = .;');
    LinkRes.Add('           *(.bss .bss.*)');
    LinkRes.Add('           *(COMMON)');
    LinkRes.Add('           . = ALIGN(64);');
    LinkRes.Add('         __bss_end__ = .;');
    LinkRes.Add('');
    LinkRes.Add('         __bss_size__ = __bss_end__ - __bss_start__;');
    LinkRes.Add('   } > ram');
    LinkRes.Add('}');

    linkres.writetodisk;
    linkres.free;

end;


function FileCopy(source, target: string; startTargetOffset: dword): boolean;
var
    fin, fout : file;
    NumRead : dWord;
    Buf : Array[1..2048] of byte;

begin

  result:= false;

  AssignFile(fin, source);
  {$I-} reset(fin, 1); {$I+}
  if ioResult <> 0 then exit;

  AssignFile(fout, target);
  if startTargetOffset = 0 then rewrite(fout, 1) else begin
    {$I-} reset(fout, 1); {$I+}
    if ioResult <> 0 then exit;
    seek(fout, startTargetOffset);
  end;

  Repeat
    BlockRead(fin,buf,Sizeof(buf),NumRead);
    if NumRead = 0 then break;
    BlockWrite(fout,Buf,NumRead);
  Until false;

  close(fin);
  close(fout);

  result:= true;

end;


procedure write0till(var f: file; k: dWord);
var 
    b : byte;

begin
  
  b:= 0;
  repeat
    BlockWrite(f, b, 1);
  until filesize(f) >= k;

end;


function TLinkerPS1.CreatePSXEXE(name: string): boolean;
const
    header = 'PS-X EXE';

var
    f : file;
    fsize : dword;
    d : dword;

begin

  result:= false;
  
  AssignFile(f, name + '.bin');
  {$I-} reset(f, 1); {$I+}
  if ioResult <> 0 then exit;

  if filesize(f) >= $200000 then begin
    Message3(link_f_executable_too_big_exceeds_X_by_Y_bytes,'PS1','2097152',tostr(filesize(f)-$200000));
    exit;
  end;
  
  close(f);

  AssignFile(f, name + '.psx-exe');
  {$I-} rewrite(f, 1); {$I+}
  if ioResult <> 0 then exit;

  BlockWrite(f, header, sizeof(header));
  write0till(f, $10);

  d:= NtoLE($80010000);
  BlockWrite(f, d, 4);
  d:= NtoLE($FFFFFFFF);
  BlockWrite(f, d, 4);
  d:= NtoLE($80010000);
  BlockWrite(f, d, 4);

  write0till(f, $30);
  write0till(f, $800);

  fsize:= filesize(f);
  close(f);

  if not FileCopy(name + '.bin', name + '.psx-exe', fsize) then exit;

  {$I-} reset(f, 1); {$I+}
  if ioResult <> 0 then exit;
  fsize:= filesize(f);
  seek(f, fsize);

  if (fsize mod $800) <> 0 then begin
      fsize:= fsize + ($800 - (fsize mod $800));
      write0till(f, fsize);
  end;

  seek(f, $1C);

  d:= fsize - $800;
  BlockWrite(f, d, 4);

  close(f);

  result:= true;

end;



function TLinkerPS1.MakeExecutable: boolean;
var
    binstr, cmdstr : TCmdStr;
    success : boolean;
begin

    WriteScriptFile;

    if cs_link_nolink in current_settings.globalswitches then begin
      result:= true;
      exit;
    end;

    { Call linker }
    SplitBinCmd(Info.ExeCmd[1], binstr, cmdstr);

    Replace(cmdstr, '$OPT', Info.ExtraOptions);
    Replace(cmdstr, '$RES', '-T ' + (maybequoted(ScriptFixFileName(outputexedir + Info.ResName))));

    if cs_link_smart in current_settings.globalswitches then cmdstr:= cmdstr + ' --gc-sections';

    success:= DoExec(FindUtil(utilsprefix + BinStr), cmdstr + ' -o ' + current_module.exefilename + '.elf', true, false);
    if not success then begin
      result:= false;
      exit;
    end;

    DeleteFile(outputexedir + Info.ResName);

    
    if not FileCopy(current_module.exefilename + '.elf', current_module.exefilename + '.bin', 0) then begin
      writeln('Cant Write ' + current_module.exefilename + '.bin File!');
      result:= false;
      exit;
    end;

    DeleteFile(current_module.exefilename + '.elf');


    success:= DoExec(FindUtil(utilsprefix + 'objcopy'), ' -O binary ' + current_module.exefilename +'.bin', true, false);
    if not success then begin
      writeln('OBJDUMP failed!');
      result:= false;
      exit;
    end;

   
    success:= CreatePSXEXE(current_module.exefilename);
    if not success then begin
      writeln('Create PSX-EXE failed!');
      result:= false;
      exit;
    end;

    DeleteFile(current_module.exefilename + '.bin');

    result:= true;
end;


procedure TLinkerPS1.InitSysInitUnitName;
begin
  sysinitunit:= 'si_prc';
end;



initialization

  RegisterLinker(ld_ps1, TLinkerPS1);
  RegisterTarget(system_mipsel_ps1_info);

end.

