{ %TARGET=darwin }

{$mode objfpc}{$H+}
{$packrecords c}

uses
  Classes, SysUtils, UnixType;


{$IF DEFINED(DARWIN)}
function getfsstat(buf: pstatfs; bufsize: cint; flags: cint): cint; cdecl; external 'c' name 'getfsstat';
{$ELSE}
function getfsstat(struct_statfs: PStatFS; const buffsize: int64; const int_flags: integer): integer;
{$ENDIF}

const
  MAX_FS = 128;
  MNT_WAIT = 1; // synchronously wait for I/O to complete
  MNT_NOWAIT = 2; // start all I/O, but do not wait for it
  MNT_LAZY = 3; // push data not written by filesystem syncer
  MNT_SUSPEND = 4; // suspend file system after sync

procedure AssertTrue(const s : string;b : boolean);
begin
  if not(b) then
    begin
      writeln(s);
      halt(1);
    end;
end;

var
  fs: tstatfs;
  fsList: array[0..MAX_FS] of tstatfs;
  count: integer;
  i: integer;
begin
  count := getfsstat( @fsList, SizeOf(fsList), MNT_WAIT );
  for i := 0 to count - 1 do
  begin
    fs := fsList[i];
    AssertTrue( 'tstatfs.fstypename error' , fs.fstypename[0] <> char(0) );
    AssertTrue( 'tstatfs.mountpoint error' , fs.mountpoint[0] <> char(0) );
    AssertTrue( 'tstatfs.mntfromname error' , fs.mntfromname[0] <> char(0) );
    WriteLn( IntToStr(i) + ':' + fs.fstypename + ''#9'' + fs.mountpoint + ''#9'' + fs.mntfromname );
  end;
end.
