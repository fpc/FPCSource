
procedure warn(__format: PAnsiChar; args: array of const); cdecl; external clib name 'warn';
procedure vwarn(__format: PAnsiChar; arg: Pointer); cdecl;external clib name 'vwarn';
procedure warnx(__format: PAnsiChar; args: array of const); cdecl; external clib name 'warnx';
procedure vwarnx(__format: PAnsiChar; arg: pointer); cdecl; external clib name 'vwarnx';
procedure err(__status: Integer; __format: PAnsiChar; args : array of const); cdecl; external clib name 'err';
procedure verr(__status: Integer; __format: PAnsiChar; Arg: Pointer); cdecl;  external clib name 'verr';
procedure errx(__status: Integer; __format: PAnsiChar); cdecl;  external clib name 'errx';
procedure verrx(__status: Integer; __format: PAnsiChar; Arg: Pointer); cdecl; external clib name 'verrx';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
