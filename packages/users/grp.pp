unit grp;
interface

{
  Automatically converted by H2Pas 0.99.15 from grp.h
  The following command line parameters were used:
    -D
    -l
    c
    -p
    -s
    -u
    grp
    -v
    grp.h
}

  const
    External_library='c'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

{$PACKRECORDS C}


type
  PFILE = Pointer;
  __gid_t = Longint;
  P__gid_t = ^__gid_t;
  size_t = longint;

  PGroup = ^group;
  group = record
    gr_name : Pchar;
    gr_passwd : Pchar;
    gr_gid : __gid_t;
    gr_mem : ^Pchar;
  end;
  TGroup = Group;
  PPGROUP = ^PGroup;

procedure setgrent;cdecl;external External_library name 'setgrent';
procedure endgrent;cdecl;external External_library name 'endgrent';
function getgrent:Pgroup;cdecl;external External_library name 'getgrent';
function fgetgrent(__stream:PFILE):Pgroup;cdecl;external External_library name 'fgetgrent';
function getgrgid(__gid:__gid_t):Pgroup;cdecl;external External_library name 'getgrgid';
function getgrnam(__name:Pchar):Pgroup;cdecl;external External_library name 'getgrnam';
function getgrgid_r(__gid:__gid_t; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external External_library name 'getgrgid_r';
function getgrnam_r(__name:Pchar; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external External_library name 'getgrnam_r';
function fgetgrent_r(__stream:PFILE; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external External_library name 'fgetgrent_r';
function setgroups(__n:size_t; __groups:P__gid_t):longint;cdecl;external External_library name 'setgroups';
function getgrouplist(__user:Pchar; __group:__gid_t; __groups:P__gid_t; __ngroups:Plongint):longint;cdecl;external External_library name 'getgrouplist';
function initgroups(__user:Pchar; __group:__gid_t):longint;cdecl;external External_library name 'initgroups';


implementation

end.
