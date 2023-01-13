
function envz_entry(__envz:PAnsiChar; __envz_len:size_t; __name:PAnsiChar):PAnsiChar;cdecl;external clib name 'envz_entry';
function envz_get(__envz:PAnsiChar; __envz_len:size_t; __name:PAnsiChar):PAnsiChar;cdecl;external clib name 'envz_get';
function envz_add(__envz:PPAnsiChar; __envz_len:Psize_t; __name:PAnsiChar; __value:PAnsiChar):error_t;cdecl;external clib name 'envz_add';
function envz_merge(__envz:PPAnsiChar; __envz_len:Psize_t; __envz2:PAnsiChar; __envz2_len:size_t; __override:longint):error_t;cdecl;external clib name 'envz_merge';
procedure envz_remove(__envz:PPAnsiChar; __envz_len:Psize_t; __name:PAnsiChar);cdecl;external clib name 'envz_remove';
procedure envz_strip(__envz:PPAnsiChar; __envz_len:Psize_t);cdecl;external clib name 'envz_strip';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function envz_add(var __envz:PAnsiChar; var __envz_len: size_t; __name:PAnsiChar; __value:PAnsiChar):error_t;cdecl;external clib name 'envz_add';
function envz_merge(var __envz: PAnsiChar; var __envz_len: size_t; __envz2:PAnsiChar; __envz2_len:size_t; __override:longint):error_t;cdecl;external clib name 'envz_merge';
procedure envz_remove(var __envz:PAnsiChar; var __envz_len: size_t; __name:PAnsiChar);cdecl;external clib name 'envz_remove';
procedure envz_strip(var __envz:PAnsiChar; var __envz_len: size_t);cdecl;external clib name 'envz_strip';
