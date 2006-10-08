

function strfmon(__s:Pchar; __maxsize:size_t; __format:Pchar):ssize_t;cdecl;varargs;external clib name 'strfmon';
function __strfmon_l(__s:Pchar; __maxsize:size_t; loc:__locale_t; __format:Pchar):ssize_t;cdecl;varargs;external clib name '__strfmon_l';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
