unit shadow;
interface

{
  Automatically converted by H2Pas 0.99.15 from shadow.h
  The following command line parameters were used:
    -D
    -l
    c
    -p
    -s
    -u
    shadow
    -v
    shadow.h
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

   Pspwd = ^spwd;
   spwd = record
        sp_namp : pchar;
        sp_pwdp : pchar;
        sp_lstchg : longint;
        sp_min : longint;
        sp_max : longint;
        sp_warn : longint;
        sp_inact : longint;
        sp_expire : longint;
        sp_flag : longint;
     end;
  TPasswordFileEntry = spwd;
  PPasswordFileEntry = ^TPasswordFileEntry;

procedure setspent;cdecl;external External_library name 'setspent';
procedure endspent;cdecl;external External_library name 'endspent';
function getspent:Pspwd;cdecl;external External_library name 'getspent';
function getspnam(__name:Pchar):Pspwd;cdecl;external External_library name 'getspnam';
function sgetspent(__string:Pchar):Pspwd;cdecl;external External_library name 'sgetspent';
function fgetspent(__stream:PFILE):Pspwd;cdecl;external External_library name 'fgetspent';
function putspent(__p:Pspwd; __stream:PFILE):longint;cdecl;external External_library name 'putspent';
function lckpwdf:longint;cdecl;external External_library name 'lckpwdf';
function ulckpwdf:longint;cdecl;external External_library name 'ulckpwdf';


implementation


end.
