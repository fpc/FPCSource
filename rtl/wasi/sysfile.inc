{
    This file is part of the Free Pascal run time library.

    Main OS dependant body of the system unit, loosely modelled
    after POSIX.  *BSD version (Linux version is near identical)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure Do_Close(Handle:thandle);
begin
  DebugWriteLn('Do_Close');
end;

procedure Do_Erase(p: pchar; pchangeable: boolean);
begin
  DebugWriteLn('Do_Erase');
end;

procedure do_truncate (handle:thandle;fpos:int64);
begin
  DebugWriteLn('do_truncate');
end;

procedure Do_Rename(p1,p2:pchar; p1changeable, p2changeable: boolean);
begin
  DebugWriteLn('Do_Rename');
end;

function Do_Write(Handle:thandle;Addr:Pointer;Len:Longint):longint;
var
  our_iov: __wasi_ciovec_t;
  our_nwritten: longint;
begin
  our_iov.buf := Addr;
  our_iov.buf_len := Len;
  fd_write(Handle, @our_iov, 1, @our_nwritten);
  Do_Write:=our_nwritten;
end;

function Do_Read(Handle:thandle;Addr:Pointer;Len:Longint):Longint;
var
  our_iov: __wasi_iovec_t;
  our_nread: __wasi_size_t;
begin
  our_iov.buf:=Addr;
  our_iov.buf_len:=Len;
  fd_read(Handle,@our_iov,1,@our_nread);
  Do_Read:=our_nread;
end;

function Do_FilePos(Handle: thandle):Int64;
begin
  DebugWriteLn('Do_FilePos');
end;

procedure Do_Seek(Handle:thandle;Pos:Int64);
begin
  DebugWriteLn('Do_Seek');
end;

function Do_Seekend(Handle:thandle):Int64;
begin
  DebugWriteLn('Do_Seekend');
end;

function Do_FileSize(Handle:thandle):Int64;
begin
  DebugWriteLn('Do_FileSize');
end;

procedure Do_Open(var f; p: pchar; flags: longint; pchangeable: boolean);
begin
  DebugWriteLn('Do_Open');
end;
