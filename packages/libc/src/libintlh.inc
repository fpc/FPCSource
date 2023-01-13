

function gettext(__msgid:PAnsiChar):PAnsiChar;cdecl;external clib name 'gettext';
function dgettext(__domainname:PAnsiChar; __msgid:PAnsiChar):PAnsiChar;cdecl;external clib name 'dgettext';
function __dgettext(__domainname:PAnsiChar; __msgid:PAnsiChar):PAnsiChar;cdecl;external clib name '__dgettext';
function dcgettext(__domainname:PAnsiChar; __msgid:PAnsiChar; __category:longint):PAnsiChar;cdecl;external clib name 'dcgettext';
function __dcgettext(__domainname:PAnsiChar; __msgid:PAnsiChar; __category:longint):PAnsiChar;cdecl;external clib name '__dcgettext';
function ngettext(__msgid1:PAnsiChar; __msgid2:PAnsiChar; __n:dword):PAnsiChar;cdecl;external clib name 'ngettext';
function dngettext(__domainname:PAnsiChar; __msgid1:PAnsiChar; __msgid2:PAnsiChar; __n:dword):PAnsiChar;cdecl;external clib name 'dngettext';
function dcngettext(__domainname:PAnsiChar; __msgid1:PAnsiChar; __msgid2:PAnsiChar; __n:dword; __category:longint):PAnsiChar;cdecl;external clib name 'dcngettext';
function textdomain(__domainname:PAnsiChar):PAnsiChar;cdecl;external clib name 'textdomain';
function bindtextdomain(__domainname:PAnsiChar; __dirname:PAnsiChar):PAnsiChar;cdecl;external clib name 'bindtextdomain';
function bind_textdomain_codeset(__domainname:PAnsiChar; __codeset:PAnsiChar):PAnsiChar;cdecl;external clib name 'bind_textdomain_codeset';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
