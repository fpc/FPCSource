unit t_wasm;

{$i fpcdefs.inc}

interface

uses
  systems,

  globtype, globals,
  aasmbase,
  cfileutl, cutils, cclasses,

  import, export, aasmdata, aasmcpu,
  fmodule, ogbase,

  symsym, symdef,

  link,

  i_wasm, tgcpu;

type

  { texportlibwasm }

  texportlibwasm=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

  { timportlibwasm }
  timportlibwasm = class(timportlib)
      procedure generatelib;override;
    end;

  { tlinkerjvm }

  { tlinkerwasm }

  tlinkerwasm=class(texternallinker)
  public
    constructor Create;override;
    procedure SetDefaultInfo;override;

    //function  MakeExecutable:boolean;override;
    function  MakeSharedLibrary:boolean;override;
  end;


implementation

{ timportlibwasm }

  procedure timportlibwasm.generatelib;
    begin
    end;

{ tlinkerwasm }

constructor tlinkerwasm.Create;
begin
  inherited Create;
end;

procedure tlinkerwasm.SetDefaultInfo;
begin
  Info.DllCmd[1] := 'wasm-ld $SONAME $GCSECTIONS -o $EXE';
  //Info.DllCmd[2] := 'wasmtool --exportrename $INPUT $EXE';
end;

function tlinkerwasm.MakeSharedLibrary: boolean;
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
  //Result := true;
  //Result:=inherited MakeSharedLibrary;
  if (cs_link_smart in current_settings.globalswitches) and
     create_smartlink_sections then
   GCSectionsStr:='--gc-sections'
  else
    GCSectionsStr:='';

  SoNameStr:='';
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename));

  tmp := TCmdStrListItem(ObjectFiles.First);
  while Assigned(tmp) do begin
    cmdstr := tmp.Str+ ' ' + cmdstr;
    tmp := TCmdStrListItem(tmp.Next);
  end;

  if HasExports then
    cmdstr := cmdstr + ' --export-dynamic'; //' --export-dynamic';

  cmdstr := cmdstr + ' --no-entry --allow-undefined';

  if (cs_link_strip in current_settings.globalswitches) then
   begin
     { only remove non global symbols and debugging info for a library }
     cmdstr := cmdstr + ' --strip-all';
   end;

  //Replace(cmdstr,'$OPT',Info.ExtraOptions);
  //Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  //Replace(cmdstr,'$INIT',InitStr);
  //Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$SONAME',SoNameStr);
  //Replace(cmdstr,'$MAP',mapstr);
  //Replace(cmdstr,'$LTO',ltostr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  writeln(utilsprefix+binstr,' ',cmdstr);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);

  //SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
  //Replace(cmdstr,'$INPUT',current_module.objfilename );
  //Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename));
  //DoExec(FindUtil(utilsprefix+binstr),cmdstr,false,false);

  MakeSharedLibrary:=success;
end;

{ texportlibwasm }

procedure texportlibwasm.preparelib(const s: string);
begin
  //nothing to happen. wasm files are modules
end;

procedure texportlibwasm.exportprocedure(hp: texported_item);
var
  nm : TSymStr;
begin
  nm := tprocdef(tprocsym(hp.sym).ProcdefList[0]).mangledname;
  current_asmdata.asmlists[al_exports].Concat(tai_impexp.create(hp.name^, nm, ie_Func));
end;

procedure texportlibwasm.exportvar(hp: texported_item);
begin
  //inherited exportvar(hp);
end;

procedure texportlibwasm.generatelib;
begin
  //inherited generatelib;
end;

initialization
  RegisterTarget(system_wasm_info);
  RegisterImport(system_wasm_wasm32, timportlibwasm);
  RegisterExport(system_wasm_wasm32, texportlibwasm);
  RegisterLinker(ld_wasm, tlinkerwasm);

end.
