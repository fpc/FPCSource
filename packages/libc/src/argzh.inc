

type

   Perror_t = ^error_t;

function argz_create(__argv:PPAnsiChar; __argz:PPAnsiChar; __len:Psize_t):error_t;cdecl;external clib name 'argz_create';
function argz_create_sep(__string:PAnsiChar; __sep:longint; __argz:PPAnsiChar; __len:Psize_t):error_t;cdecl;external clib name 'argz_create_sep';
function argz_count(__argz:PAnsiChar; __len:size_t):size_t;cdecl;external clib name 'argz_count';
procedure argz_extract(__argz:PAnsiChar; __len:size_t; __argv:PPAnsiChar);cdecl;external clib name 'argz_extract';
procedure argz_stringify(__argz:PAnsiChar; __len:size_t; __sep:longint);cdecl;external clib name 'argz_stringify';
function argz_append(__argz:PPAnsiChar; __argz_len:Psize_t; __buf:PAnsiChar; __buf_len:size_t):error_t;cdecl;external clib name 'argz_append';
function argz_add(__argz:PPAnsiChar; __argz_len:Psize_t; __str:PAnsiChar):error_t;cdecl;external clib name 'argz_add';
function argz_add_sep(__argz:PPAnsiChar; __argz_len:Psize_t; __string:PAnsiChar; __delim:longint):error_t;cdecl;external clib name 'argz_add_sep';
procedure argz_delete(__argz:PPAnsiChar; __argz_len:Psize_t; __entry:PAnsiChar);cdecl;external clib name 'argz_delete';
function argz_insert(__argz:PPAnsiChar; __argz_len:Psize_t; __before:PAnsiChar; __entry:PAnsiChar):error_t;cdecl;external clib name 'argz_insert';
function argz_replace(__argz:PPAnsiChar; __argz_len:Psize_t; __str:PAnsiChar; __with:PAnsiChar; __replace_count:Pdword):error_t;cdecl;external clib name 'argz_replace';
function argz_next(__argz:PAnsiChar; __argz_len:size_t; __entry:PAnsiChar):PAnsiChar;cdecl;external clib name 'argz_next';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function argz_create(__argv:PPAnsiChar; var __argz: PAnsiChar; var __len: size_t):error_t;cdecl;external clib name 'argz_create';
function argz_create_sep(__string:PAnsiChar; __sep:longint; var __argz: PAnsiChar; var __len: size_t):error_t;cdecl;external clib name 'argz_create_sep';
function argz_append(var __argz: PAnsiChar; var __argz_len: size_t; __buf:PAnsiChar; __buf_len:size_t):error_t;cdecl;external clib name 'argz_append';
function argz_add(var __argz: PAnsiChar; var __argz_len: size_t; __str:PAnsiChar):error_t;cdecl;external clib name 'argz_add';
function argz_add_sep(var __argz: PAnsiChar; var __argz_len: size_t; __string:PAnsiChar; __delim:longint):error_t;cdecl;external clib name 'argz_add_sep';
procedure argz_delete(var __argz: PAnsiChar; var __argz_len: size_t; __entry:PAnsiChar);cdecl;external clib name 'argz_delete';
function argz_insert(var __argz: PAnsiChar; var __argz_len: size_t; __before:PAnsiChar; __entry:PAnsiChar):error_t;cdecl;external clib name 'argz_insert';
function argz_replace(var __argz: PAnsiChar; var __argz_len: size_t; __str:PAnsiChar; __with:PAnsiChar; __replace_count:Pdword):error_t;cdecl;external clib name 'argz_replace';
