{
    Copyright (c) 2019 by Dmitry Boyarintsev

    This unit implements support import,export,link routines
    for the WASI target

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

unit t_wasi;

{$i fpcdefs.inc}

interface

uses
  systems,

  globtype, globals,
  aasmbase,
  cfileutl, cutils, cclasses,

  import, export, aasmdata, aasmcpu,
  fmodule, ogbase, ogwasm,

  symconst, symsym, symdef, symcpu,

  link,

  i_wasi, tgcpu;

type

  { texportlibwasi }

  texportlibwasi=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

  { timportlibwasi }
  timportlibwasi = class(timportlib)
      procedure generatelib;override;
    end;

  { tlinkerwasi }

  tlinkerwasi=class(texternallinker)
  public
    constructor Create;override;
    procedure SetDefaultInfo;override;

    procedure InitSysInitUnitName;override;

    function  MakeExecutable:boolean;override;
    function  MakeSharedLibrary:boolean;override;
  end;

  { TInternalLinkerWasi }

  TInternalLinkerWasi=class(tinternallinker)
  protected
    procedure DefaultLinkScript;override;

    function GetCodeSize(aExeOutput: TExeOutput): QWord;override;
    function GetDataSize(aExeOutput: TExeOutput): QWord;override;
    function GetBssSize(aExeOutput: TExeOutput): QWord;override;
  public
    constructor create;override;

    procedure InitSysInitUnitName;override;
  end;


implementation

uses
  SysUtils,
  verbose;

{ timportlibwasi }

  procedure timportlibwasi.generatelib;
    begin
    end;

{ tlinkerwasi }

constructor tlinkerwasi.Create;
begin
  inherited Create;
end;

procedure tlinkerwasi.SetDefaultInfo;
begin
  with Info do
    begin
      ExeCmd[1] := 'wasm-ld -m wasm32 $SONAME $GCSECTIONS $MAP -z stack-size=$STACKSIZE $OPT -o $EXE';
      DllCmd[1] := 'wasm-ld -m wasm32 $SONAME $GCSECTIONS $MAP -z stack-size=$STACKSIZE $OPT -o $EXE';
    end;
end;

procedure tlinkerwasi.InitSysInitUnitName;
begin
  if current_module.islibrary then
    sysinitunit:='si_dll'
  else
    sysinitunit:='si_prc';
end;

function tlinkerwasi.MakeExecutable:boolean;
var
  GCSectionsStr  : ansistring;
  binstr, cmdstr : Tcmdstr;
  InitStr,
  FiniStr,
  SoNameStr      : string[80];
  mapstr,ltostr  : TCmdStr;
  success        : Boolean;

  tmp : TCmdStrListItem;
  tempFileName : ansistring;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Create some replacements }
  mapstr:='';
  if (cs_link_map in current_settings.globalswitches) then
    mapstr:='-Map '+maybequoted(ChangeFileExt(current_module.exefilename,'.map'));
  if (cs_link_smart in current_settings.globalswitches) and
     create_smartlink_sections then
   GCSectionsStr:='--gc-sections'
  else
    GCSectionsStr:='';

  SoNameStr:='';
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename));

  tmp := TCmdStrListItem(ObjectFiles.First);
  while Assigned(tmp) do begin
    cmdstr := tmp.Str+ ' ' + cmdstr;
    tmp := TCmdStrListItem(tmp.Next);
  end;

//  if HasExports then
//    cmdstr := cmdstr + ' --export-dynamic'; //' --export-dynamic';

  cmdstr := cmdstr + ' --no-entry';

  if ts_wasm_threads in current_settings.targetswitches then
    begin
      cmdstr := cmdstr + ' --import-memory --shared-memory --initial-memory=16777216 --max-memory=16777216 --global-base=1024';
    end;

  if (cs_link_strip in current_settings.globalswitches) then
   begin
     { only remove non global symbols and debugging info for a library }
     cmdstr := cmdstr + ' --strip-all';
   end;

  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  //Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  //Replace(cmdstr,'$INIT',InitStr);
  //Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$STACKSIZE',tostr(stacksize));
  Replace(cmdstr,'$SONAME',SoNameStr);
  Replace(cmdstr,'$MAP',mapstr);
  //Replace(cmdstr,'$LTO',ltostr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);

  MakeExecutable:=success;
end;

function tlinkerwasi.MakeSharedLibrary: boolean;
var
  GCSectionsStr  : ansistring;
  binstr, cmdstr : Tcmdstr;
  InitStr,
  FiniStr,
  SoNameStr      : string[80];
  mapstr,ltostr  : TCmdStr;
  success        : Boolean;

  tmp : TCmdStrListItem;
  tempFileName : ansistring;
begin
  Result:=false;
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.sharedlibfilename);

  { Create some replacements }
  mapstr:='';
  if (cs_link_map in current_settings.globalswitches) then
    mapstr:='-Map '+maybequoted(ChangeFileExt(current_module.sharedlibfilename,'.map'));
  if (cs_link_smart in current_settings.globalswitches) and
     create_smartlink_sections then
   GCSectionsStr:='--gc-sections'
  else
    GCSectionsStr:='';

  SoNameStr:='';
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename));

  tmp := TCmdStrListItem(ObjectFiles.First);
  while Assigned(tmp) do begin
    cmdstr := tmp.Str+ ' ' + cmdstr;
    tmp := TCmdStrListItem(tmp.Next);
  end;

//  if HasExports then
//    cmdstr := cmdstr + ' --export-dynamic'; //' --export-dynamic';

  cmdstr := cmdstr + ' --no-entry';

  if (cs_link_strip in current_settings.globalswitches) then
   begin
     { only remove non global symbols and debugging info for a library }
     cmdstr := cmdstr + ' --strip-all';
   end;

  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  //Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  //Replace(cmdstr,'$INIT',InitStr);
  //Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$STACKSIZE',tostr(stacksize));
  Replace(cmdstr,'$SONAME',SoNameStr);
  Replace(cmdstr,'$MAP',mapstr);
  //Replace(cmdstr,'$LTO',ltostr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);

  MakeSharedLibrary:=success;
end;

{ texportlibwasi }

procedure texportlibwasi.preparelib(const s: string);
begin
  //nothing to happen. wasm files are modules
end;

procedure texportlibwasi.exportprocedure(hp: texported_item);
var
  nm : TSymStr;
  pd: tcpuprocdef;
begin
  pd:=tcpuprocdef(tprocsym(hp.sym).ProcdefList[0]);
  if eo_promising_first in hp.options then
    pd.add_promising_export(hp.name^,false)
  else if eo_promising_last in hp.options then
    pd.add_promising_export(hp.name^,true)
  else
    begin
      nm := pd.mangledname;
      current_asmdata.asmlists[al_exports].Concat(tai_export_name.create(hp.name^, nm, ie_Func));
    end;
end;

procedure texportlibwasi.exportvar(hp: texported_item);
begin
  //inherited exportvar(hp);
end;

procedure texportlibwasi.generatelib;
begin
  //inherited generatelib;
end;

{ TInternalLinkerWasi }

procedure TInternalLinkerWasi.DefaultLinkScript;
var
  s: TCmdStr;
begin
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
      LinkScript.Concat('READOBJECT ' + maybequoted(s));
  end;

  LinkScript.Concat('EXESECTION .text');
  LinkScript.Concat('  OBJSECTION .text.*');
  LinkScript.Concat('ENDEXESECTION');

  LinkScript.Concat('EXESECTION .data');
  LinkScript.Concat('  OBJSECTION .rodata.*');
  LinkScript.Concat('  OBJSECTION .data.*');
  LinkScript.Concat('  OBJSECTION .bss');
  LinkScript.Concat('ENDEXESECTION');

end;

function TInternalLinkerWasi.GetCodeSize(aExeOutput: TExeOutput): QWord;
begin
  {TODO}
  Result:=0;
end;

function TInternalLinkerWasi.GetDataSize(aExeOutput: TExeOutput): QWord;
begin
  {TODO}
  Result:=0;
end;

function TInternalLinkerWasi.GetBssSize(aExeOutput: TExeOutput): QWord;
begin
  {TODO}
  Result:=0;
end;

constructor TInternalLinkerWasi.create;
begin
  inherited create;
  CExeOutput:=TWasmExeOutput;
  CObjInput:=TWasmObjInput;
end;

procedure TInternalLinkerWasi.InitSysInitUnitName;
begin
  if current_module.islibrary then
    sysinitunit:='si_dll'
  else
    sysinitunit:='si_prc';
end;

initialization
  RegisterTarget(system_wasm32_wasi_info);
  RegisterImport(system_wasm32_wasi, timportlibwasi);
  RegisterExport(system_wasm32_wasi, texportlibwasi);
  RegisterLinker(ld_int_wasi,TInternalLinkerWasi);
  RegisterLinker(ld_wasi, tlinkerwasi);

end.
