
type
 Paliasent = ^aliasent;
 aliasent = record
   alias_name : PAnsiChar;
   alias_members_len : size_t;
   alias_members : ^PAnsiChar;
   alias_local : longint;
 end;
 PPaliasent = ^Paliasent;

procedure setaliasent;cdecl;external clib name 'setaliasent';
procedure endaliasent;cdecl;external clib name 'endaliasent';
function getaliasent:Paliasent;cdecl;external clib name 'getaliasent';
function getaliasent_r(__result_buf:Paliasent; __buffer:PAnsiChar; __buflen:size_t; __result:PPaliasent):longint;cdecl;external clib name 'getaliasent_r';
function getaliasbyname(__name:PAnsiChar):Paliasent;cdecl;external clib name 'getaliasbyname';
function getaliasbyname_r(__name:PAnsiChar; __result_buf:Paliasent; __buffer:PAnsiChar; __buflen:size_t; __result:PPaliasent):longint;cdecl;external clib name 'getaliasbyname_r';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TAliasEntry = aliasent;
  PAliasEntry = ^TAliasEntry;

function getaliasent_r(var __result_buf:aliasent; __buffer:PAnsiChar; __buflen:size_t; var __result:Paliasent):longint;cdecl;external clib name 'getaliasent_r';
function getaliasbyname_r(__name:PAnsiChar; var __result_buf:aliasent; __buffer:PAnsiChar; __buflen:size_t; var __result:Paliasent):longint;cdecl;external clib name 'getaliasbyname_r';
