Unit dl;

Interface

Const
  LibDL = 'dl';

  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;

Function dlopen(Name : PChar; Flags : longint) : Pointer; cdecl; external libdl;
FUnction dlsym(Lib : Pointer; Name : Pchar) : Pointer; cdecl; external Libdl;
Function dlclose(Lib : Pointer) : Longint; cdecl; external libdl;

implementation

end.
