
procedure error(status: longint; errnum: longint; format: PAnsiChar; args: array of const); cdecl; external clib name 'error';
procedure error_at_line(status: longint; errnum: longint; fname: PAnsiChar; lineno: Cardinal; format: PAnsiChar; args : array of const); cdecl; external clib name 'error_at_line';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
