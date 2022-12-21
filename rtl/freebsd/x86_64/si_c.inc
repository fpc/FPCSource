
{$define CRT_IRELOC_RELA}

procedure init_tls; cdecl; external name 'init_tls';
function  cmain(nrarg:longint;pp:PPAnsiChar;env:PPAnsiChar):longint; cdecl; external name 'main';

{$ifdef gcrt}
 procedure cmcleanup; cdecl; external name '_mcleanup';
 procedure monstratup(p,p2:pointer); cdecl; external name 'monstartup';

var 
  eprol:longint; external name 'eprol';
  etext:longint; external name 'etext';
{$endif}

{$i ignore_init.inc}
procedure start(ap:PPAnsiChar;cleanup:TCleanupProc);

var argc: longint;
    argv: PPAnsiChar;
    env : PPAnsiChar;
    s   : PAnsiChar;
begin
  argc:=pptrint(ap)^;
  argv:=PPAnsiChar(ap[1]);
  env:=	PPAnsiChar(ap[2+argc]);
  environ:=env;
  handle_argv(argc,argv,env);
  if assigned(PAnsiChar(@_dynamic)) then // I suspect this is a trick to find
				    // out runtime if we are shared
				    // linking, so the same code can be used
				    // for static and shared linking
    atexit(cleanup)
  else
    begin
       process_irelocs();
       init_tls;
    end;

  {$ifdef GCRT}
    atexit(@cmcleanup);
  {$endif}
  atexit(@_fini);
  {$ifdef GCRT}
    monstartup(@eprol,@etext);
   asm
    eprol:
   end;
  {$endif} 
  handle_static_init(argc, argv, env);
  libc_exit(cmain(argc,argv,env)); // doesn't return
end;

begin
end.

