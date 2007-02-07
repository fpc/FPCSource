unit dl;

interface

const
 {$ifdef BSD}   // dlopen is in libc on FreeBSD.
  LibDL = 'c';
 {$else}
  LibDL = 'dl';
{$endif}

{$if defined(linux) and defined(cpuarm)}
{ arm-linux seems to require this }
{$linklib c}
{$endif}

  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;
  RTLD_GLOBAL       = $100;
  RTLD_NEXT         = pointer(-1);
  {$ifdef LINUX}
  RTLD_DEFAULT      = nil;
  {$endif}
  {$ifdef BSD}
  RTLD_DEFAULT      = pointer(-2);
  RTLD_MODEMASK     = RTLD_BINDING_MASK;
  {$endif}

type
  Pdl_info = ^dl_info;
  dl_info =
  record
    dli_fname      : Pchar;
    dli_fbase      : pointer;
    dli_sname      : Pchar;
    dli_saddr      : pointer;
  end;

function dlopen(Name : PChar; Flags : longint) : Pointer; cdecl; external libdl;
function dlsym(Lib : Pointer; Name : Pchar) : Pointer; cdecl; external Libdl;
function dlclose(Lib : Pointer) : Longint; cdecl; external libdl;
function dlerror() : Pchar; cdecl; external libdl;
{ overloaded for compatibility with hmodule }
function dlsym(Lib : PtrInt; Name : Pchar) : Pointer; cdecl; external Libdl;
function dlclose(Lib : PtrInt) : Longint; cdecl; external libdl;
function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl; external;

implementation

end.
