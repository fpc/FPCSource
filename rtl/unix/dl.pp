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
  {$ifdef BSD}
  RTLD_MODEMASK     = RTLD_BINDING_MASK;
  {$endif}

Function dlopen(Name : PChar; Flags : longint) : Pointer; cdecl; external libdl;
FUnction dlsym(Lib : Pointer; Name : Pchar) : Pointer; cdecl; external Libdl;
Function dlclose(Lib : Pointer) : Longint; cdecl; external libdl;
Function dlerror() : Pchar; cdecl; external libdl;
{ overloaded for compatibility with hmodule }
FUnction dlsym(Lib : PtrInt; Name : Pchar) : Pointer; cdecl; external Libdl;
Function dlclose(Lib : PtrInt) : Longint; cdecl; external libdl;

implementation

end.
