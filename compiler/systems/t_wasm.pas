unit t_wasm;

interface

uses
  systems,

  export, aasmdata, aasmcpu,

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

  { tlinkerjvm }

  tlinkerwasm=class(texternallinker)
    constructor Create;override;
    //function  MakeExecutable:boolean;override;
    function  MakeSharedLibrary:boolean;override;
  end;


implementation

{ tlinkerwasm }

constructor tlinkerwasm.Create;
begin
  inherited Create;
end;

function tlinkerwasm.MakeSharedLibrary: boolean;
begin
  writeln('tlinkerwasm.MakeSharedLibrary');
  Result := true;
  //Result:=inherited MakeSharedLibrary;
end;

{ texportlibwasm }

procedure texportlibwasm.preparelib(const s: string);
begin
  writeln('preparelib: ', s);
  //nothing to happen. wasm files are modules
  //inherited preparelib(s);
end;

procedure texportlibwasm.exportprocedure(hp: texported_item);
begin
  current_asmdata.asmlists[al_exports].Concat( tai_impexp.create(hp.name^, hp.sym.RealName, ie_Func));
end;

procedure texportlibwasm.exportvar(hp: texported_item);
begin
  writeln('exportvar: ', PtrUInt(hp));
  //inherited exportvar(hp);
end;

procedure texportlibwasm.generatelib;
begin
  writeln('gen lib');
  //inherited generatelib;
end;

initialization
  RegisterTarget(system_wasm_info);

  RegisterExport(system_wasm_wasm32, texportlibwasm);
  RegisterLinker(ld_wasm, tlinkerwasm);

end.
