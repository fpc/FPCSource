{ ---------------------------------------------------------------------
    Macros from sys/stat.h
  ---------------------------------------------------------------------}

function __S_ISTYPE(mode,mask : __mode_t) : boolean;
begin
  __S_ISTYPE:=(mode and __S_IFMT) = mask;
end;

function S_ISDIR(mode : __mode_t) : boolean;
begin
   S_ISDIR:=__S_ISTYPE(mode,__S_IFDIR);
end;

function S_ISCHR(mode : __mode_t) : boolean;
begin
   S_ISCHR:=__S_ISTYPE(mode,__S_IFCHR);
end;

function S_ISBLK(mode : __mode_t) : boolean;
begin
   S_ISBLK:=__S_ISTYPE(mode,__S_IFBLK);
end;

function S_ISREG(mode : __mode_t) : boolean;
begin
   S_ISREG:=__S_ISTYPE(mode,__S_IFREG);
end;

function S_ISFIFO(mode : __mode_t) : boolean;
begin
   S_ISFIFO:=__S_ISTYPE(mode,__S_IFIFO);
end;

function S_ISLNK(mode : __mode_t) : boolean;
begin
   S_ISLNK:=__S_ISTYPE(mode,__S_IFLNK);
end;


function S_ISSOCK(mode : __mode_t) : boolean;
begin
   S_ISSOCK:=__S_ISTYPE(mode,__S_IFSOCK);
end;
