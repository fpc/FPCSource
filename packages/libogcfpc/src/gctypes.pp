unit gctypes;

interface

uses
  ctypes;
  
const
  LITTLE_ENDIAN = 3412;
  BIG_ENDIAN    = 1234;
  BYTE_ORDER    = BIG_ENDIAN;


type
  f32 = cfloat;
  f64 = cdouble;
   ppcchar = ^pcchar;
   __argv = record
     argvMagic: cint;      // argv magic number, set to 0x5f617267 ('_arg') if valid
     commandLine: pcchar;  // base address of command line, set of null terminated strings
     length: cint;         // total length of command line
     argc: integer;
     argv: ppcchar;
     endARGV: ppcchar;        // internal use, host ip for dslink
   end;
   Targv = __argv;
   Pargv = ^Targv;

  timespec = record
    tv_sec: Longint;
    tv_nsec: Longint;
  end;
  ptimespec = ^timespec;
  
var
  __system_argv: pargv; cvar; external;

const
  ARGV_MAGIC = $5f617267;


implementation

end.