
const O_RDONLY=0;
const O_WRONLY=1;
const O_RDWR=2;
const O_CREAT = $200;
const O_TRUNC = $400;
const O_APPEND = $800;
{const O_TEXT = $4000;
const O_BINARY = $8000;}


function sys_open (a:cardinal;name:pchar;access:longint;b:longint;c:longint):longint; cdecl; external name 'sys_open';
function sys_close (handle:longint):longint; cdecl; external name 'sys_close';
function sys_read (handle:longint;buffer:pointer;len:longint;var a:longint):longint; cdecl; external name 'sys_read';
function sys_write (handle:longint;buffer:pointer;len:longint;var a:longint):longint; cdecl; external name 'sys_write';
function sys_lseek (handle:longint;pos:int64;whence:longint): int64; cdecl; external name 'sys_lseek';


