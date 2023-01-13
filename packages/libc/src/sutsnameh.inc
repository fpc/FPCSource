
const
   _UTSNAME_NODENAME_LENGTH = _UTSNAME_LENGTH;

type
   Putsname = ^utsname;
   utsname = record
        sysname : array[0..(_UTSNAME_LENGTH)-1] of AnsiChar;
        nodename : array[0..(_UTSNAME_NODENAME_LENGTH)-1] of AnsiChar;
        release : array[0..(_UTSNAME_LENGTH)-1] of AnsiChar;
        version : array[0..(_UTSNAME_LENGTH)-1] of AnsiChar;
        machine : array[0..(_UTSNAME_LENGTH)-1] of AnsiChar;
        domainname : array[0..(_UTSNAME_DOMAIN_LENGTH)-1] of AnsiChar;
        __domainname : array[0..(_UTSNAME_DOMAIN_LENGTH)-1] of AnsiChar;
     end;

const
   SYS_NMLN = _UTSNAME_LENGTH;

function uname(__name:Putsname):longint;cdecl;external clib name 'uname';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TUTSName = utsname;

function uname(var __name: utsname):longint;cdecl;external clib name 'uname';

