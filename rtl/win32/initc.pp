unit initc;

interface

{ 0.99.12 had a bug that initialization/finalization only worked for
  objfpc,delphi mode }
{$ifdef VER0_99_12}
  {$mode objfpc}
{$endif}

 {$LINKLIB cygwin}

{ this unit is just ment to run
  startup code to get C code to work correctly PM }


implementation

procedure cygwin_crt0(p : pointer);cdecl;external;

{
procedure do_global_dtors;cdecl;external;
 this does not work because
 do_global_dtors is a static C function PM
 it is inserted into the atexit chain,
 but how do we call this from FPC ???
 it seems to be done in exit function
 but that one ends with _exit that is system dependent !! }

{ avoid loading of cygwin _exit code
  so that exit returns }
procedure _exit(status : longint);cdecl;
begin
end;

procedure C_exit(status : longint);popstack;external name '_exit';

initialization
cygwin_crt0(nil);

finalization
{ should we pass exit code ?
  its apparently only used by _exit so it doesn't matter PM }
C_exit(0);

end.
