{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}


const
  ISFS_MAXPATH = IPC_MAXPATH_LEN;
  ISFS_OPEN_READ = $01;
  ISFS_OPEN_WRITE = $02;
  ISFS_OPEN_RW = ( ISFS_OPEN_READ or ISFS_OPEN_WRITE );
  ISFS_OK = 0;
  ISFS_ENOMEM = - 22;
  ISFS_EINVAL = - 101;
type
  _fstats = record
    file_length : cuint32;
    file_pos : cuint32;
  end;
  fstats = _fstats;


  isfscallback = function(result: cint32; usrdata: pointer): cint32; cdecl;


function ISFS_Initialize: cint32; cdecl; external;

function ISFS_Deinitialize: cint32; cdecl; external;


function ISFS_Open(filepath: pcchar; mode: cuint8): cint32; cdecl; external;

function ISFS_OpenAsync(filepath: pcchar; mode: cuint8; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_Close(fd: cint32): cint32; cdecl; external;

function ISFS_CloseAsync(fd: cint32; cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_Delete(filepath: pcchar): cint32; cdecl; external;

function ISFS_DeleteAsync(filepath: pcchar; cb: isfscallback; usrdata: pointer)
  : cint32; cdecl; external;

function ISFS_ReadDir(filepath, name_list: pcchar; num: pcuint32): cint32; cdecl; external;

function ISFS_ReadDirAsync(filepath, name_list: pcchar; num: pcuint32;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_CreateFile(filepath: pcchar;
  attributes, owner_perm, group_perm, other_perm: cuint8): cint32; cdecl; external;

function ISFS_CreateFileAsync(filepath: pcchar;
  attributes, owner_perm, group_perm, other_perm: cuint8; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_Write(fd: cint32; buffer: pointer; length: cuint32): cint32; cdecl; external;

function ISFS_WriteAsync(fd: cint32; buffer: pointer; length: cuint32;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_Read(fd: cint32; buffer: pointer; length: cuint32): cint32; cdecl; external;

function ISFS_ReadAsync(fd: cint32; buffer: pointer; length: cuint32;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_Seek(fd, where, whence: cint32): cint32; cdecl; external;

function ISFS_SeekAsync(fd, where, whence: cint32; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_CreateDir(filepath: pcchar;
  attributes, owner_perm, group_perm, other_perm: cuint8): cint32; cdecl; external;

function ISFS_CreateDirAsync(filepath: pcchar;
  attributes, owner_perm, group_perm, other_perm: cuint8; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_GetStats(stats: pointer): cint32; cdecl; external;

function ISFS_GetStatsAsync(stats: pointer; cb: isfscallback; usrdata: pointer)
  : cint32; cdecl; external;

type
  Pfstats = ^fstats;
function ISFS_GetFileStats(fd: cint32; status: Pfstats): cint32; cdecl; external;

function ISFS_GetFileStatsAsync(fd: cint32; status: Pfstats; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_GetAttr(filepath: pcchar; ownerID: pcuint32; groupID: pcuint16;
  attributes, ownerperm, groupperm, otherperm: pcuint8): cint32; cdecl; external;

function ISFS_GetAttrAsync(filepath: pcchar; ownerID: pcuint32;
  groupID: pcuint16; attributes, ownerperm, groupperm, otherperm: pcuint8;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_Rename(filepathOld, filepathNew: pcchar): cint32; cdecl; external;

function ISFS_RenameAsync(filepathOld, filepathNew: pcchar; cb: isfscallback;
  usrdata: pointer): cint32; cdecl; external;

function ISFS_SetAttr(filepath: pcchar; ownerID: cuint32; groupID: cuint16;
  attributes, ownerperm, groupperm, otherperm: cuint8): cint32; cdecl; external;

function ISFS_SetAttrAsync(filepath: pcchar; ownerID: cuint32;
  groupID: cuint16; attributes, ownerperm, groupperm, otherperm: cuint8;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;

function ISFS_GetUsage(filepath: pcchar; usage1, usage2: pcuint32): cint32; cdecl; external;

function ISFS_GetUsageAsync(filepath: pcchar; usage1, usage2: pcuint32;
  cb: isfscallback; usrdata: pointer): cint32; cdecl; external;


{$endif HW_RVL}
{$ENDIF OGC_INTERFACE}


