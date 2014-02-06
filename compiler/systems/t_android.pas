{
    Copyright (c) 1998-2008 by Peter Vreman

    This unit implements support import,export,link routines
    for the Android target

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
unit t_android;

{$i fpcdefs.inc}

interface

  uses
    aasmdata,
    symsym,symdef,ppu,
    import,export,expunix,link;

  type
    timportlibandroid=class(timportlib)
      procedure generatelib;override;
    end;

    texportlibandroid=class(texportlibunix)
      procedure setfininame(list: TAsmList; const s: string); override;
    end;

    { tlinkerandroid }

    tlinkerandroid=class(texternallinker)
    private
      prtobj  : string[80];
      reorder : boolean;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
      function DoLink(IsSharedLib: boolean): boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      procedure InitSysInitUnitName;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
      procedure LoadPredefinedLibraryOrder; override;
    end;


implementation

  uses
    SysUtils,
    cutils,cfileutl,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,
    aasmbase,aasmtai,aasmcpu,cpubase,
    cgbase,cgobj,cgutils,ogbase,ncgutil,
    comprsrc,
    rescmn, i_android
    ;

{*****************************************************************************
                               TIMPORTLIBANDROID
*****************************************************************************}

    procedure timportlibandroid.generatelib;
      var
        i : longint;
        ImportLibrary : TImportLibrary;
      begin
        for i:=0 to current_module.ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(current_module.ImportLibraryList[i]);
            current_module.linkothersharedlibs.add(ImportLibrary.Name,link_always);
          end;
      end;


{*****************************************************************************
                               TEXPORTLIBANDROID
*****************************************************************************}

    procedure texportlibandroid.setfininame(list: TAsmList; const s: string);
      begin
        { the problem with not having a .fini section is that a finalization
          routine in regular code can get "smart" linked away -> reference it
          just like the debug info }
        new_section(list,sec_fpc,'links',0);
        list.concat(Tai_const.Createname(s,0));
        inherited setfininame(list,s);
      end;

{*****************************************************************************
                                  TLINKERANDROID
*****************************************************************************}

Constructor TLinkerAndroid.Create;
begin
  Inherited Create;
end;


procedure TLinkerAndroid.SetDefaultInfo;
{
  This will also detect which libc version will be used
}

const
{$ifdef arm}       platform_select='';{$endif} {unknown :( }
{$ifdef i386}      platform_select='';{$endif} {unknown :( }
{$ifdef mipsel}    platform_select='';{$endif} {unknown :( }

var
  s: string;
begin
  with Info do
   begin
     { Specify correct max-page-size and common-page-size to prevent big gaps between sections in resulting executable }
     s:='ld '+platform_select+'-z max-page-size=0x1000 -z common-page-size=0x1000 -z noexecstack -z now $OPT -L. -T $RES -o $EXE';
     ExeCmd[1]:=s + ' --entry=_fpc_start';
     DllCmd[1]:=s + ' -shared -soname $SONAME';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     ExtDbgCmd[1]:='objcopy --only-keep-debug $EXE $DBG';
     ExtDbgCmd[2]:='objcopy --add-gnu-debuglink=$DBG $EXE';
     ExtDbgCmd[3]:='strip --strip-unneeded $EXE';
     DynamicLinker:='/system/bin/linker';
   end;
end;


procedure TLinkerAndroid.LoadPredefinedLibraryOrder;
// put your linkorder/linkalias overrides here.
// Note: assumes only called when reordering/aliasing is used.
Begin
   if not (cs_link_no_default_lib_order in  current_settings.globalswitches) Then
        Begin
          LinkLibraryOrder.add('gcc','',15);
          LinkLibraryOrder.add('c','',100);
          LinkLibraryOrder.add('gmon','',120);
          LinkLibraryOrder.add('dl','',140);
          LinkLibraryOrder.add('pthread','',160);
         end;
End;

Procedure TLinkerAndroid.InitSysInitUnitName;
begin
  reorder := ReOrderEntries;
  if current_module.islibrary then
    prtobj:='dllprt0'
  else
    prtobj:='prt0';
end;

Function TLinkerAndroid.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  HPath        : TCmdStrListItem;
  s,s1         : TCmdStr;
begin
  result:=False;
  { Always link to libc }
  AddSharedLibrary('c');

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);
  with linkres do
    begin
      { Write path to search libraries }
      HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
      while assigned(HPath) do
       begin
         Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
         HPath:=TCmdStrListItem(HPath.Next);
       end;
      HPath:=TCmdStrListItem(LibrarySearchPath.First);
      while assigned(HPath) do
       begin
         Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
         HPath:=TCmdStrListItem(HPath.Next);
       end;

      { force local symbol resolution (i.e., inside the shared }
      { library itself) for all non-exorted symbols, otherwise }
      { several RTL symbols of FPC-compiled shared libraries   }
      { will be bound to those of a single shared library or   }
      { to the main program                                    }
      if (isdll) then
        begin
          add('VERSION');
          add('{');
          add('  {');
          if not texportlibunix(exportlib).exportedsymnames.empty then
            begin
              add('    global:');
              repeat
                add('      '+texportlibunix(exportlib).exportedsymnames.getfirst+';');
              until texportlibunix(exportlib).exportedsymnames.empty;
            end;
          add('    local:');
          add('      *;');
          add('  };');
          add('}');
        end;

      StartSection('INPUT(');
      { add objectfiles, start with prt0 always }
      if not (target_info.system in systems_internal_sysinit) and (prtobj<>'') then
        AddFileName(maybequoted(FindObjectFile(prtobj,'',false)));
      { Add libc startup object file }
      if isdll then
        s:='crtbegin_so.o'
      else
        if cs_link_staticflag in current_settings.globalswitches then
          s:='crtbegin_static.o'
        else
          s:='crtbegin_dynamic.o';
      librarysearchpath.FindFile(s,false,s1);
      AddFileName(maybequoted(s1));
      { main objectfiles }
      while not ObjectFiles.Empty do
       begin
         s:=ObjectFiles.GetFirst;
         if s<>'' then
          AddFileName(maybequoted(s));
       end;
      EndSection(')');

      { Write staticlibraries }
      if not StaticLibFiles.Empty then
       begin
         Add('GROUP(');
         While not StaticLibFiles.Empty do
          begin
            S:=StaticLibFiles.GetFirst;
            AddFileName(maybequoted(s))
          end;
         Add(')');
       end;

      // we must reorder here because the result could empty sharedlibfiles
      if reorder Then
        ExpandAndApplyOrder(SharedLibFiles);
      // after this point addition of shared libs not allowed.

      if not SharedLibFiles.Empty then
       begin
         if (SharedLibFiles.Count<>1) or reorder then
           begin
             Add('INPUT(');
             While not SharedLibFiles.Empty do
              begin
                S:=SharedLibFiles.GetFirst;
                i:=Pos(target_info.sharedlibext,S);
                if i>0 then
                  Delete(S,i,255);
                Add('-l'+s);
              end;
             Add(')');
           end;

         if (cs_link_staticflag in current_settings.globalswitches) or
            (not reorder) then
           begin
             Add('GROUP(');
             { when we have -static for the linker the we also need libgcc }
             if (cs_link_staticflag in current_settings.globalswitches) then
               begin
                 Add('-lgcc');
                 if librarysearchpath.FindFile('libgcc_eh.a',false,s1) then
                   Add('-lgcc_eh');
               end;
             { be sure that libc is the last lib }
             if not reorder then
               Add('-lc');
             Add(')');
           end;
       end;

      { objects which must be at the end }
      { Add libc finalization object file }
      Add('INPUT(');
      if isdll then
        s:='crtend_so.o'
      else
        s:='crtend_android.o';
      librarysearchpath.FindFile(s,false,s1);
      AddFileName(maybequoted(s1));
      Add(')');

      { Additions to the linker script }

      add('SECTIONS');
      add('{');
      add('  .data           :');
      add('  {');
      { extra by FPC }
      add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      add('  }');
      add('}');
      add('INSERT BEFORE .data1');

      { Write and Close response }
      writetodisk;
      Free;
    end;

  WriteResponseFile:=True;
end;

function tlinkerandroid.DoLink(IsSharedLib: boolean): boolean;
var
  i: longint;
  binstr, cmdstr: TCmdStr;
  s, opts, outname: string;
  success: boolean;
begin
  Result:=False;
  if IsSharedLib then
    outname:=current_module.sharedlibfilename
  else
    outname:=current_module.exefilename;
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking, outname);

  opts:='';
  if (cs_link_strip in current_settings.globalswitches) and
     not (cs_link_separate_dbg_file in current_settings.globalswitches) then
    opts:=opts + ' -s';
  if (cs_link_map in current_settings.globalswitches) then
    opts:=opts + ' -Map '+maybequoted(ChangeFileExt(outname,'.map'));
  if create_smartlink_sections then
    opts:=opts + ' --gc-sections';
  if (cs_link_staticflag in current_settings.globalswitches) then
    opts:=opts + ' -static'
  else
    if cshared then
      opts:=opts + ' -call_shared';
  if rlinkpath<>'' then
    opts:=opts+' --rpath-link '+rlinkpath;

  if not IsSharedLib then
    begin
      opts:=opts + ' --dynamic-linker ' + Info.DynamicLinker;
      { create dynamic symbol table? }
      if HasExports then
        opts:=opts+' -E';
    end;

  opts:=Trim(opts + ' ' + Info.ExtraOptions);

{ Write used files and libraries }
  WriteResponseFile(IsSharedLib);

{ Call linker }
  if IsSharedLib then
    s:=Info.DllCmd[1]
  else
    s:=Info.ExeCmd[1];
  SplitBinCmd(s, binstr, cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(outname));
  Replace(cmdstr,'$OPT',opts);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  if IsSharedLib then
    Replace(cmdstr,'$SONAME',ExtractFileName(outname));

  binstr:=FindUtil(utilsprefix+BinStr);
  { We should use BFD version of LD, since GOLD version does not support INSERT command in linker scripts }
  if binstr <> '' then begin
    { Checking if ld.bfd exists }
    s:=ChangeFileExt(binstr, '.bfd' + source_info.exeext);
    if FileExists(s, True) then
      binstr:=s;
  end;

  success:=DoExec(binstr,CmdStr,true,false);

  { Create external .dbg file with debuginfo }
  if success and (cs_link_separate_dbg_file in current_settings.globalswitches) then
    begin
      for i:=1 to 3 do
        begin
          SplitBinCmd(Info.ExtDbgCmd[i],binstr,cmdstr);
          Replace(cmdstr,'$EXE',maybequoted(outname));
          Replace(cmdstr,'$DBGFN',maybequoted(extractfilename(current_module.dbgfilename)));
          Replace(cmdstr,'$DBG',maybequoted(current_module.dbgfilename));
          success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,false);
          if not success then
            break;
        end;
    end;

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  Result:=success;   { otherwise a recursive call to link method }
end;

function TLinkerAndroid.MakeExecutable:boolean;
begin
  Result:=DoLink(False);
end;

Function TLinkerAndroid.MakeSharedLibrary:boolean;
begin
  Result:=DoLink(True);
end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_android,TLinkerAndroid);
{$ifdef ARM}
  RegisterImport(system_arm_android,timportlibandroid);
  RegisterExport(system_arm_android,texportlibandroid);
  RegisterTarget(system_arm_android_info);
{$endif ARM}
{$ifdef I386}
  RegisterImport(system_i386_android,timportlibandroid);
  RegisterExport(system_i386_android,texportlibandroid);
  RegisterTarget(system_i386_android_info);
{$endif I386}
{$ifdef MIPSEL}
  RegisterImport(system_mipsel_android,timportlibandroid);
  RegisterExport(system_mipsel_android,texportlibandroid);
  RegisterTarget(system_mipsel_android_info);
{$endif MIPSEL}
  RegisterRes(res_elf_info,TWinLikeResourceFile);
end.
