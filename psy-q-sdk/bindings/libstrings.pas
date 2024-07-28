// string functions pseudo definition header 
// $PSLibId: Run-time Library Release 4.6$
{$MODE OBJFPC}
unit libstrings;
interface
const 
	LMAX = 256;

function strcat(destination:pchar; const source: pchar): pchar; stdcall; external;
function strncat(destination:pchar; const source, num: longint): pchar; stdcall; external;
function strcmp(str1, str2: pchar): longint; stdcall; external;
function strncmp(str1, str2: pchar; num: longint): longint; stdcall; external;
function strcpy(destination, source: pchar): pchar; stdcall; external;
function strncpy(destination, source: pchar; num: longint): pchar; stdcall; external;
function strlen(p: pchar): longint; stdcall; external;
function index(const p: pchar; num: longint): pchar; stdcall; external;
function rindex(const p: pchar; num: longint): pchar; stdcall; external;

function strchr(const str: pchar; character: longint): pchar; stdcall; external;
function strrchr(const str: pchar; character: longint): pchar; stdcall; external;
function strpbrk(const str1, str2: pchar): pchar; stdcall; external;
function strspn(const str1, str2: pchar): longint; stdcall; external;
function strcspn(const str1, str2: pchar): longint; stdcall; external;
function strtok(str1, str2: pchar): pchar; stdcall; external;
function strstr(const str1, str2: pchar): pchar; stdcall; external;

function strdup(p: pchar): pchar;

implementation
uses libstd;

function strdup(p: pchar): pchar;
begin
	result:= strcpy(malloc(strlen(p) + 1), p); 
end;

begin
end.