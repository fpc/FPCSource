{ Source provided for Free Pascal Bug Report 1658 }
{ Submitted by "Luis Castedo" on  2001-10-28 }
{ e-mail: castedo@elai.upm.es }
program Buggy;

uses

  Objects, Strings;

type

  TMyStream = object(TDosStream)
    m_fAutoDelete: Boolean;
    destructor Done; virtual;
  end;
  PMyStream = ^TMyStream;

destructor TMyStream.Done;
var
  strFName: String;
  F       : File;
begin
  strFName := StrPas(FName);
  inherited Done;
  if not m_fAutoDelete then
    Exit;
  Assign(F, strFName);
  Erase(F);
end;

// Global vars
var
  pTempStream: PMyStream;
  EntryMem,ExitMem : Cardinal;
// Main routine
begin
  EntryMem:=MemAvail;
  pTempStream := nil;
  pTempStream := New(PMyStream, Init('TEMP00.TMP', stCreate));
  if not Assigned(pTempStream) then
    Halt(1);
  pTempStream^.m_fAutoDelete := False;
  Dispose(pTempStream, Done);
  pTempStream := nil;
  ExitMem:=MemAvail;
  If ExitMem<EntryMem then
    begin
      Writeln('Memory lost');
      Halt(1);
    end;
end.
