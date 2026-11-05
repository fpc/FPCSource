{ Tests that memory stream writes past High(PtrInt) raise EStreamError. }
program tmemstreamoverflow;

{$mode objfpc}{$H+}
{$apptype console}

uses
  SysUtils, Classes;

// Write 100 bytes at 10 bytes below High(PtrInt) to aStream; aExit is the exit code on failure
procedure CheckOverflow(aStream: TMemoryStream; aExit: Integer);

var
  lBuf: array[0..99] of Byte;
  lRaised: Boolean;

begin
  FillChar(lBuf, SizeOf(lBuf), 1);
  aStream.Position:=High(PtrInt)-10;
  lRaised:=False;
  try
    aStream.Write(lBuf, SizeOf(lBuf));
  except
    on EStreamError do
      lRaised:=True;
  end;
  if not lRaised then
    begin
    Writeln(aStream.ClassName, ': write past High(PtrInt) did not raise EStreamError');
    Halt(aExit);
    end;
  if aStream.Size<>0 then
    begin
    Writeln(aStream.ClassName, ': failed write changed the size to ', aStream.Size);
    Halt(aExit+1);
    end;
end;


// Write 100 bytes at aStream's start; aExit is the exit code on failure
procedure CheckNormalWrite(aStream: TMemoryStream; aExit: Integer);

var
  lBuf: array[0..99] of Byte;

begin
  FillChar(lBuf, SizeOf(lBuf), 2);
  aStream.Position:=0;
  if (aStream.Write(lBuf, SizeOf(lBuf))<>SizeOf(lBuf)) or (aStream.Size<>SizeOf(lBuf)) then
    begin
    Writeln(aStream.ClassName, ': a normal write after the failed one does not work');
    Halt(aExit);
    end;
end;


var
  lStream: TMemoryStream;

begin
  lStream:=TMemoryStream.Create;
  try
    CheckOverflow(lStream, 1);
    CheckNormalWrite(lStream, 3);
  finally
    lStream.Free;
  end;
  lStream:=TBytesStream.Create;
  try
    CheckOverflow(lStream, 4);
    CheckNormalWrite(lStream, 6);
  finally
    lStream.Free;
  end;
  lStream:=TStringStream.Create('');
  try
    CheckOverflow(lStream, 7);
    CheckNormalWrite(lStream, 9);
  finally
    lStream.Free;
  end;
  Writeln('ok');
end.
