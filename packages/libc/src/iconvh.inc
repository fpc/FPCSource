
type

   Piconv_t = ^iconv_t;
   iconv_t = pointer;

function iconv_open(__tocode:PAnsiChar; __fromcode:PAnsiChar):iconv_t;cdecl;external clib name 'iconv_open';
function iconv(__cd:iconv_t; __inbuf:PPAnsiChar; __inbytesleft:Psize_t; __outbuf:PPAnsiChar; __outbytesleft:Psize_t):size_t;cdecl;external clib name 'iconv';
function iconv_close(__cd:iconv_t):longint;cdecl;external clib name 'iconv_close';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

function iconv(__cd:iconv_t; var __inbuf:PAnsiChar; var __inbytesleft:size_t; var __outbuf:PAnsiChar; var __outbytesleft: size_t):size_t;cdecl;external clib name 'iconv';
