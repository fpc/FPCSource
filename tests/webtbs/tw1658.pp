{ Source provided for Free Pascal Bug Report 1658 }
{ Submitted by "Luis Castedo" on  2001-10-28 }
{ e-mail: castedo@elai.upm.es }
program Buggy;

uses
  erroru,
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
  mem : sizeint;
  f : file;
begin
  DoMem(mem);
  pTempStream := nil;
  pTempStream := New(PMyStream, Init('tw1658.tmp', stCreate));
  if not Assigned(pTempStream) then
    Halt(1);
  pTempStream^.m_fAutoDelete := False;
  Dispose(pTempStream, Done);
  pTempStream := nil;
  if DoMem(mem)<>0 then
    begin
      Writeln('Memory lost');
      Halt(1);
    end;
  Assign(f,'tw1658.tmp');
  Erase(f);
end.
