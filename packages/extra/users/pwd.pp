unit pwd;
interface

{
  Automatically converted by H2Pas 0.99.15 from pwd.H
  The following command line parameters were used:
    -D
    -l
    c
    -p
    -s
    -u
    pwd
    -v
    pwd.H
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
  PFile = Pointer;
  __uid_t = longint;
  __gid_t = longint;

  Ppasswd = ^passwd;
  passwd = record
    pw_name : Pchar;
    pw_passwd : Pchar;
    pw_uid : __uid_t;
    pw_gid : __gid_t;
    pw_gecos : Pchar;
    pw_dir : Pchar;
    pw_shell : Pchar;
  end;
  TPasswordRecord = passwd;
  PPasswordRecord = ^TPasswordRecord;

procedure setpwent;cdecl; external External_library name 'setpwent';
procedure endpwent;cdecl; external External_library name 'endpwent';
function getpwent:Ppasswd;cdecl;external External_library name 'getpwent';
function fgetpwent(__stream:PFILE):Ppasswd;cdecl;external External_library name 'fgetpwent';
function putpwent(__p:Ppasswd; __f:PFILE):longint;cdecl;external External_library name 'putpwent';
function getpwuid(__uid:__uid_t):Ppasswd;cdecl;external External_library name 'getpwuid';
function getpwnam(__name:Pchar):Ppasswd;cdecl;external External_library name 'getpwnam';
function getpw(__uid:__uid_t; __buffer:Pchar):longint;cdecl;external External_library name 'getpw';


implementation


end.
