{$PACKRECORDS C}

  magic_set = record
  end;

  magic_t = ^magic_set;

const
  LIB_NAME = 'magic';
  MAGIC_MIME_TYPE = $000010;

function magic_open(flags: cint): magic_t; cdecl; external LIB_NAME name 'magic_open';
procedure magic_close(cookie: magic_t); cdecl; external LIB_NAME name 'magic_close';
function magic_load(cookie: magic_t; filename: Pcchar): cint; cdecl; external LIB_NAME name 'magic_load';
function magic_buffer(cookie: magic_t; buffer: Pointer; length: size_t): Pcchar; cdecl; external LIB_NAME name 'magic_buffer';
