unit system;

interface

type
  integer = longint;
  hresult = integer; 
  ttypekind = integer;
  filerec = integer;
  textrec = integer;
  pbyte = ^byte;

procedure fpc_lib_exit; compilerproc;

function test_rtl_function(a, b: integer): integer;

implementation

procedure fpc_lib_exit; compilerproc;
begin
end;

function test_rtl_function(a, b: integer): integer;
begin
  test_rtl_function := 0;
end;

end.
