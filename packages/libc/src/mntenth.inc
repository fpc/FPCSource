

const
   MNTTAB = _PATH_MNTTAB;
   MOUNTED = _PATH_MOUNTED;
   MNTTYPE_IGNORE = 'ignore';
   MNTTYPE_NFS = 'nfs';
   MNTTYPE_SWAP = 'swap';
   MNTOPT_DEFAULTS = 'defaults';
   MNTOPT_RO = 'ro';
   MNTOPT_RW = 'rw';
   MNTOPT_SUID = 'suid';
   MNTOPT_NOSUID = 'nosuid';
   MNTOPT_NOAUTO = 'noauto';

type
   Pmntent = ^mntent;
   mntent = record
        mnt_fsname : PAnsiChar;
        mnt_dir : PAnsiChar;
        mnt_type : PAnsiChar;
        mnt_opts : PAnsiChar;
        mnt_freq : longint;
        mnt_passno : longint;
     end;


function setmntent(__file:PAnsiChar; __mode:PAnsiChar):PFILE;cdecl;external clib name 'setmntent';
function getmntent(__stream:PFILE):Pmntent;cdecl;external clib name 'getmntent';
function getmntent_r(__stream:PFILE; __result:Pmntent; __buffer:PAnsiChar; __bufsize:longint):Pmntent;cdecl;external clib name 'getmntent_r';

function addmntent(__stream:PFILE; __mnt:Pmntent):longint;cdecl;external clib name 'addmntent';
function endmntent(__stream:PFILE):longint;cdecl;external clib name 'endmntent';
function hasmntopt(__mnt:Pmntent; __opt:PAnsiChar):PAnsiChar;cdecl;external clib name 'hasmntopt';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TMountEntry = mntent;
  PMountEntry = ^TMountEntry;

function getmntent_r(__stream:PFILE; var __result: mntent; __buffer:PAnsiChar; __bufsize:longint):Pmntent;cdecl;external clib name 'getmntent_r';
function addmntent(__stream:PFILE; const __mnt: mntent):longint;cdecl;external clib name 'addmntent';
function hasmntopt(const __mnt: mntent; __opt:PAnsiChar):PAnsiChar;cdecl;external clib name 'hasmntopt';

