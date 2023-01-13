
const
   SHADOW = _PATH_SHADOW;

type
   Pspwd = ^spwd;
   spwd = record
        sp_namp : PAnsiChar;
        sp_pwdp : PAnsiChar;
        sp_lstchg : longint;
        sp_min : longint;
        sp_max : longint;
        sp_warn : longint;
        sp_inact : longint;
        sp_expire : longint;
        sp_flag : dword;
     end;
   PPspwd = ^Pspwd;

procedure setspent;cdecl;external clib name 'setspent';
procedure endspent;cdecl;external clib name 'endspent';
function getspent:Pspwd;cdecl;external clib name 'getspent';
function getspnam(__name:PAnsiChar):Pspwd;cdecl;external clib name 'getspnam';
function sgetspent(__string:PAnsiChar):Pspwd;cdecl;external clib name 'sgetspent';
function fgetspent(__stream:PFILE):Pspwd;cdecl;external clib name 'fgetspent';
function putspent(__p:Pspwd; __stream:PFILE):longint;cdecl;external clib name 'putspent';
function getspent_r(__result_buf:Pspwd; __buffer:PAnsiChar; __buflen:size_t; __result:PPspwd):longint;cdecl;external clib name 'getspent_r';
function getspnam_r(__name:PAnsiChar; __result_buf:Pspwd; __buffer:PAnsiChar; __buflen:size_t; __result:PPspwd):longint;cdecl;external clib name 'getspnam_r';
function sgetspent_r(__string:PAnsiChar; __result_buf:Pspwd; __buffer:PAnsiChar; __buflen:size_t; __result:PPspwd):longint;cdecl;external clib name 'sgetspent_r';
function fgetspent_r(__stream:PFILE; __result_buf:Pspwd; __buffer:PAnsiChar; __buflen:size_t; __result:PPspwd):longint;cdecl;external clib name 'fgetspent_r';
function lckpwdf:longint;cdecl;external clib name 'lckpwdf';
function ulckpwdf:longint;cdecl;external clib name 'ulckpwdf';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPasswordFileEntry = spwd;
  PPasswordFileEntry = ^TPasswordFileEntry;

function putspent(const __p:spwd; __stream:PFILE):longint;cdecl;external clib name 'putspent';
function getspent_r(var __result_buf: spwd; __buffer:PAnsiChar; __buflen:size_t; var __result: Pspwd):longint;cdecl;external clib name 'getspent_r';
function getspnam_r(__name:PAnsiChar; var __result_buf:spwd; __buffer:PAnsiChar; __buflen:size_t; var __result:Pspwd):longint;cdecl;external clib name 'getspnam_r';
function sgetspent_r(__string:PAnsiChar; var __result_buf:spwd; __buffer:PAnsiChar; __buflen:size_t; var __result:Pspwd):longint;cdecl;external clib name 'sgetspent_r';
function fgetspent_r(__stream:PFILE; var __result_buf:spwd; __buffer:PAnsiChar; __buflen:size_t; var __result:Pspwd):longint;cdecl;external clib name 'fgetspent_r';
