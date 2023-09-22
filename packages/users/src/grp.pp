{$IFNDEF FPC_DOTTEDUNITS}
unit grp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.InitC,UnixApi.Types,UnixApi.Base,System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  initc,unixtype,baseunix,ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
      External_library= clib;  {Setup as you need}

     _PATH_GROUP = '/etc/group';     

Type

     PGroup = ^TGroup;
     PPGroup = ^PGroup;
     TGroup = record
          gr_name   : PAnsiChar;                { group name  }
          gr_passwd : PAnsiChar;		     { group password  }	
          gr_gid    : gid_t;		     { group id  }
          gr_mem    : PPAnsiChar;		     { group members  }
       end;


procedure fpendgrent; cdecl;external External_library name 'endgrent';
function  fpgetgrent:pgroup; cdecl;external External_library name 'getgrent';
function  fpgetgrgid (id:gid_t):pgroup; cdecl;external External_library name 'getgrgid';
function  fpgetgrnam (name:PAnsiChar):pgroup; cdecl;external External_library name 'getgrnam';
{$ifdef BSD}
function  fpgroup_from_gid (gid:gid_t; nogrup:cint):PAnsiChar; cdecl;external External_library name 'group_from_gid';
{$endif}

function  fpsetgrent:cint;cdecl;external External_library name 'setgrent';

function  fpgetgrgid_r (id:gid_t; grp:Pgroup; buffer:PAnsiChar; buffersize:size_t; grresult:PPgroup):cint;cdecl;external External_library name 'getgrgid_r';

function  fpgetgrnam_r (nam:PAnsiChar; grp:Pgroup; buffer:PAnsiChar; buffersize:size_t; grresult:PPgroup):cint;cdecl;external External_library name 'getgrnam_r';
{$ifndef Darwin}
function  fpgetgrent_r (grp:Pgroup; buffer:PAnsiChar; buffersize:size_t; grresult:PPgroup):cint;cdecl;external External_library name 'getgrent_r';
{$endif}

function  fpsetgroupent (stayopen:cint):cint;cdecl;external External_library name 'setgroupent';

{$ifdef Darwin}
procedure fpsetgrfile(name:PAnsiChar); cdecl; external external_library name 'setgrfile';
{$endif}

// FreeBSD has these, Linux too if USE_BSD is defined. Darwin too.
// Darwin uses ints instead of gid's though, except for setgroups.
function  fpsetgroups(n:size_t;groups:pgid):cint; cdecl; external External_Library name 'setgroups';
function  fpgetgrouplist(user:PAnsiChar;group:tgid;groups:pgid;ngroups:pcint):cint; cdecl; external External_Library name 'getgrouplist';
function  fpinitgroups(user:PAnsiChar;group:tgid):cint;cdecl; external External_Library name 'initgroups';

implementation

end.
