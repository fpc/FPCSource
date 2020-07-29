unit t_wasm;

{$i fpcdefs.inc}

interface

uses
  systems,

  globtype,

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

  tlinkerwasm=class(texternallinker)
    constructor Create;override;
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

function tlinkerwasm.MakeSharedLibrary: boolean;
begin
  Result := true;
  //Result:=inherited MakeSharedLibrary;
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
