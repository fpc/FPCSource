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
  external 'DOSCALLS' index 58;                    { Dos seek procedure }

function DosGetFilePtr (Handle: Word; var CurPos: longint): longint;
begin
  DosGetFilePtr := DosSetFilePtr (Handle, 0, skCur, CurPos);
end;

function DosResetBuffer (Handle: Word): longint; far;
  external 'DOSCALLS' index 56;                    { Dos flush procedure }

function DosSetFileSize (Handle: Word; Size: longint): longint; far;
  external 'DOSCALLS' index 68;                    { Dos write procedure }

{$ENDIF}

function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle;
var
  Handle, ActionTaken: CPUWord;
begin
  ErrorCode := DosOpen(FName, Handle, ActionTaken, 0, $20, 1, Flags, nil);
  if ErrorCode <> 0 then
    OpenFileStr := -1
   else begin
    OpenFileStr := Handle;
    ErrorCode := 0;
  end;
end;

function CreateFileStr(FName: PChar): TFileHandle;
var
  Handle, ActionTaken: CPUWord;
begin
  ErrorCode := DosOpen(FName, Handle, ActionTaken, 0, $20, $12, 1, nil);
  if ErrorCode <> 0 then
    CreateFileStr := -1
   else begin
    CreateFileStr := Handle;
    ErrorCode := 0;
  end;
end;

procedure CloseFile(Handle: TFileHandle);
begin
  ErrorCode := DosClose(Handle);
end;

procedure DeleteFileStr(FName: PChar);
begin
  ErrorCode := DosDelete(FName);
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

function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  BuffWritten: CPUWord;
begin
  ErrorCode := DosWrite(Handle, Buff, Count, BuffWritten);
  if ErrorCode <> 0 then
    WriteFile := 0
   else begin
    WriteFile := BuffWritten;
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
  ErrorCode := DosResetBuffer (Handle);
end;

procedure TruncateFile(Handle: TFileHandle);
var
  CurPos: longint;
begin
  ErrorCode := DosGetFilePtr(Handle, CurPos);
  if ErrorCode = 0 then ErrorCode := DosSetFileSize (Handle, CurPos);
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


{
  $Log$
  Revision 1.3  2000-12-17 20:39:46  hajny
    * OS/2 implementation corrected and finished

  Revision 1.2  2000/07/13 11:32:25  michael
  + removed logs
 
}
