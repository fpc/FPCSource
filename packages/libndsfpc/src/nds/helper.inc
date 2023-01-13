{$ifdef NDS_INTERFACE}
type
  time_t = cint;
  ptime_t = ^time_t;

  Ptm = ^tm;
  tm = record
    tm_sec: cint;
    tm_min: cint;
    tm_hour: cint;
    tm_mday: cint;
    tm_mon: cint;
    tm_year: cint;
    tm_wday: cint;
    tm_yday: cint;
    tm_isdst: cint;
  end;


(* Some libc functions *)
//function printf(format: PAnsiChar; args: array of const): longint; cdecl; external;
function printf(format: PAnsiChar): longint; cdecl; varargs; external;
//function sprintf(s: PAnsiChar; format: PAnsiChar; args: array of const): longint; cdecl; external;
function sprintf(s: PAnsiChar; format: PAnsiChar): longint; varargs; cdecl; external;
//function iprintf(format: PAnsiChar; args: array of const): longint; cdecl; external;
function iprintf(format: PAnsiChar): longint; varargs; cdecl; external;
//function scanf(format: PAnsiChar; args: array of const): longint; cdecl; external;
function scanf(format: PAnsiChar): longint; cdecl; varargs; external;
//function sscanf(s: PAnsiChar; format: PAnsiChar; args: array of const): longint; cdecl; external;
function sscanf(s: PAnsiChar; format: PAnsiChar): longint; cdecl; varargs; external;
function strcmp(s1: PAnsiChar; s2: PAnsiChar): longint; cdecl; external;

function malloc(size: integer): pointer; cdecl; external;
function realloc(ptr: pointer; size: integer): pointer; cdecl; external;
procedure free(ptr: pointer); cdecl; external;
function memcpy(dest: pointer; src: pointer; n: integer): pointer; cdecl; external;

function gmtime(timer: ptime_t): ptm; cdecl; external;
function time(timer: ptime_t): time_t; cdecl; external;

type
  TSort = function (const a, b: pointer): integer;
procedure qsort(__base: pointer; __nmemb: integer; __size: integer; __compar: TSort); cdecl; external;

function __errno: pcint; cdecl; export;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function __errno: pcint;  cdecl; export;
begin

end;
{$endif NDS_IMPLEMENTATION}

