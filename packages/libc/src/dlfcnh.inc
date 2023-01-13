
Const
  RTLD_NEXT = Pointer(-1);
  RTLD_DEFAULT = nil;

function dlopen(__file:PAnsiChar; __mode:longint):pointer;cdecl;external dllib name 'dlopen';
function dlclose(__handle:pointer):longint;cdecl;external dllib name 'dlclose';
function dlsym(__handle:pointer; __name:PAnsiChar):pointer;cdecl;external dllib name 'dlsym';
function dlvsym(__handle:pointer; __name:PAnsiChar; __version:PAnsiChar):pointer;cdecl;external dllib name 'dlvsym';
function dlerror:PAnsiChar;cdecl;external clib name 'dlerror';

type

   PDl_info = ^Dl_info;
   Dl_info = record
        dli_fname : PAnsiChar;
        dli_fbase : pointer;
        dli_sname : PAnsiChar;
        dli_saddr : pointer;
     end;

function dladdr(__address:pointer; __info:PDl_info):longint;cdecl;external dllib name 'dladdr';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TDLInfo = Dl_info;
  PDLInfo = ^TDLInfo;

function dladdr(__address:pointer; var __info: Dl_info):longint;cdecl;external dllib name 'dladdr';

