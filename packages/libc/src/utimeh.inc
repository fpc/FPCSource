
type
   Putimbuf = ^utimbuf;
   utimbuf = record
     actime : __time_t;
     modtime : __time_t;
   end;


function utime(__file:PAnsiChar; __file_times:Putimbuf):longint;cdecl;external clib name 'utime';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  TUTimeBuffer = utimbuf;
  PUTimeBuffer = ^TUTimeBuffer;

function utime(__file:PAnsiChar; const __file_times: utimbuf):longint;cdecl;external clib name 'utime';
