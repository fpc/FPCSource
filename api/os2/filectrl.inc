{
  System independent filecontrol interface for os2

  $Id$
}

{$IFDEF PPC_Virtual}
  uses
    OS2Base;
{$ENDIF}
{$IFDEF PPC_Speed}
  uses
    BseDOS;
{$ENDIF}
{$IFDEF PPC_FPC}
  uses
    DosCalls;
{$ENDIF}

{ not converted to the new error handling scheme (ie: calling
  ErrorHandler, instead of dumping an error code to ErrorCode) }

{$IFDEF BIT_16}

{ There's a unit for DOSCALLS for BPOS2 available as well, should be using
  these functions from there... }

function DosClose (Handle: Word): Word; far;
  external 'DOSCALLS' Index 59;                    { Dos close function }

function DosOpen (FileName: PChar; var Handle: Word;
  var ActionTaken: Word; FileSize: LongInt;
  FileAttr: Word; OpenFlag, OpenMode: Word;
  Reserved: Pointer): Word; far;
  external 'DOSCALLS' index 70;                    { Dos open function }

function DosDelete(FileName: PChar; Reserved: Longint): Word; far;
  external 'DOSCALLS' index 60;

function DosRead(Handle: Word; var BufferArea;
  BufferLength: Word; var BytesRead : Word): Word; far;
  external 'DOSCALLS' index 137;                   { Dos read procedure }

function DosWrite(Handle: Word; var BufferArea;
  BufferLength: Word; var BytesRead : Word): Word; far;
  external 'DOSCALLS' index 138;                   { Dos write procedure }

function DosSetFilePtr (Handle: Word; ulOffset: LongInt;
  MoveType: Word; var NewPointer: LongInt): LongInt; far;
  external 'DOSCALLS' index 58;                    { Dos write procedure }

{$ENDIF}

function OpenFile(FName: PChar; Flags: Longint): TFileHandle;
var
  Handle, ActionTaken: CPUWord;
begin
  ErrorCode := DosOpen(FName, Handle, ActionTaken, 0, $20, 1, Flags);
  if ErrorCode <> 0 then
    OpenFile := -1
   else begin
    OpenFile := Handle;
    ErrorCode := 0;
  end;
end;

function CreateFile(FName: PChar): TFileHandle;
begin
  ErrorCode := DosOpen(FName, Handle, ActionTaken, 0, $20, $12, 1);
  if ErrorCode <> 0 then
    OpenFile := -1
   else begin
    OpenFile := Handle;
    ErrorCode := 0;
  end;
end;

procedure CloseFile(Handle: TFileHandle);
begin
  ErrorCode := DosClose(Handle);
end;

procedure DeleteFile(FName: PChar);
begin
  ErrorCode := DosDelete(FName, 0);
end;

function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  BuffRead: CPUWord;
begin
  ErrorCode := DosRead(Handle, Buff, Count, BuffRead);
  if ErrorCode <> 0 then
    ReadFile := 0
   else begin
    ReadFile := BuffRead;
  end;
end;

function WriteFile(Handle: TFileHandle; var Buff; Count: Word): Word;
var
  BuffWritten: Word;
begin
  ErrorCode := DosWrite(Handle, Buff, Count, BuffWritten);
  if ErrorCode <> 0 then
    WriteFile := 0
   else begin
    WriteFile := BuffRead;
  end;
end;

function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt;
var
  NewPos: Longint;
begin
  ErrorCode := DosSetFilePtr(Handle, Pos, SeekType, NewPos);
  if ErrorCode <> 0 then
    SeekFile := -1
   else begin
    SeekFile := NewPos;
  end;
end;

procedure FlushFile(Handle: TFileHandle);
begin
  { not implemented yet }
end;

procedure TruncateFile(Handle: TFileHandle);
begin
  { not implemented yet }
end;

function EndOfFile(Handle: TFileHandle): Boolean;
begin
  EndOfFile := FilePos(Handle) >= FileSize(Handle);
end;

function FilePos(Handle: TFileHandle): TFileInt;
begin
  FilePos := SeekFile(Handle, 0, skCur);
end;

function FileSize(Handle: TFileHandle): TFileInt;
var
  L: Longint;
begin
  L := FilePos(Handle);
  FileSize := SeekFile(Handle, 0, skEnd);
  SeekFile(Handle, L, skBeg);
end;

{$ENDIF}

{
  $Log$
  Revision 1.2  2000-07-09 07:56:09  hajny
    * proper units added

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:32  peter
    * moved some dirs

  Revision 1.1  1998/10/26 11:31:49  peter
    + inital include files

}