
function ioctl(__fd:longint; __request:dword):longint;cdecl;varargs;external clib name 'ioctl';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function ioctl(__fd:longint; __request:dword; args:pointer):longint;cdecl;external clib name 'ioctl';
