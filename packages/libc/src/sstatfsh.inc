

function statfs(__file:PAnsiChar; __buf:Pstatfs):longint;cdecl;external clib name 'statfs';
function statfs64(__file:PAnsiChar; __buf:Pstatfs64):longint;cdecl;external clib name 'statfs64';
function fstatfs(__fildes:longint; __buf:Pstatfs):longint;cdecl;external clib name 'fstatfs';
function fstatfs64(__fildes:longint; __buf:Pstatfs64):longint;cdecl;external clib name 'fstatfs64';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function statfs(__file:PAnsiChar; var __buf:_statfs):longint;cdecl;external clib name 'statfs';
function statfs64(__file:PAnsiChar; var __buf:_statfs64):longint;cdecl;external clib name 'statfs64';
function fstatfs(__fildes:longint; var __buf:_statfs):longint;cdecl;external clib name 'fstatfs';
function fstatfs64(__fildes:longint; var __buf:_statfs64):longint;cdecl;external clib name 'fstatfs64';
