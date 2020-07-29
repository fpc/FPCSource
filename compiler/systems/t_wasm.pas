unit t_wasm;

interface

uses
  systems, i_wasm, tgcpu;

implementation

initialization
  RegisterTarget(system_wasm_info);

end.
