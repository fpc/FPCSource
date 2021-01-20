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
end;

procedure Do_Erase(p: pchar; pchangeable: boolean);
begin
end;

procedure do_truncate (handle:thandle;fpos:int64);
begin
end;

procedure Do_Rename(p1,p2:pchar; p1changeable, p2changeable: boolean);
begin
end;

function Do_Write(Handle:thandle;Addr:Pointer;Len:Longint):longint;
begin
end;

function Do_Read(Handle:thandle;Addr:Pointer;Len:Longint):Longint;
begin
end;

function Do_FilePos(Handle: thandle):Int64;
begin
end;

procedure Do_Seek(Handle:thandle;Pos:Int64);
begin
end;

function Do_Seekend(Handle:thandle):Int64;
begin
end;

function Do_FileSize(Handle:thandle):Int64;
begin
end;

procedure Do_Open(var f; p: pchar; flags: longint; pchangeable: boolean);
begin
end;
