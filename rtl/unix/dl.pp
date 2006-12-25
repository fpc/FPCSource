Unit dl;

Interface

Const
 {$ifdef BSD}   // dlopen is in libc on FreeBSD.
  LibDL = 'c';
 {$else}
  LibDL = 'dl';
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

Function dlopen(Name : PChar; Flags : longint) : Pointer; cdecl; external libdl;
FUnction dlsym(Lib : Pointer; Name : Pchar) : Pointer; cdecl; external Libdl;
Function dlclose(Lib : Pointer) : Longint; cdecl; external libdl;
Function dlerror() : Pchar; cdecl; external libdl;
{ overloaded for compatibility with hmodule }
FUnction dlsym(Lib : PtrInt; Name : Pchar) : Pointer; cdecl; external Libdl;
Function dlclose(Lib : PtrInt) : Longint; cdecl; external libdl;
function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl; external; platform;

implementation

end.
