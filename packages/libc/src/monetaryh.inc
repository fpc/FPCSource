

function strfmon(__s:PAnsiChar; __maxsize:size_t; __format:PAnsiChar; args:array of const):ssize_t;cdecl;external clib name 'strfmon';
function strfmon(__s:PAnsiChar; __maxsize:size_t; __format:PAnsiChar):ssize_t;cdecl;varargs;external clib name 'strfmon';
function __strfmon_l(__s:PAnsiChar; __maxsize:size_t; loc:__locale_t; __format:PAnsiChar; args:array of const):ssize_t;cdecl;external clib name '__strfmon_l';
function __strfmon_l(__s:PAnsiChar; __maxsize:size_t; loc:__locale_t; __format:PAnsiChar):ssize_t;cdecl;varargs;external clib name '__strfmon_l';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
