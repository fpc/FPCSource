{ Tests that SetSize resizes a memory stream whichever overload a call binds.
  Inside a descendant TStream's protected Longint form is visible too, and on
  64-bit CPUs a call with a Longint argument binds it; it does nothing unless
  the stream overrides it.  From outside only the public form is visible, and
  an argument of another integer type still compiles. }
program tmemstreamsetsize;

{$mode objfpc}{$H+}
{$apptype console}

uses
  SysUtils, Classes;

type
  { a descendant that redeclares no SetSize, resizing itself from its own methods }
  TPlainMemoryStream = class(TMemoryStream)
  public
    procedure ResizeByConstant;
    procedure ResizeByLongint(aSize: Longint);
    procedure ResizeByInt64(aSize: Int64);
  end;

  { the same, below TBytesStream and TStringStream }
  TPlainBytesStream = class(TBytesStream)
  public
    procedure ResizeByLongint(aSize: Longint);
  end;

  TPlainStringStream = class(TStringStream)
  public
    procedure ResizeByLongint(aSize: Longint);
  end;

procedure TPlainMemoryStream.ResizeByConstant;
begin
  SetSize(5);
end;

procedure TPlainMemoryStream.ResizeByLongint(aSize: Longint);
begin
  SetSize(aSize);
end;

procedure TPlainMemoryStream.ResizeByInt64(aSize: Int64);
begin
  SetSize(aSize);
end;

procedure TPlainBytesStream.ResizeByLongint(aSize: Longint);
begin
  SetSize(aSize);
end;

procedure TPlainStringStream.ResizeByLongint(aSize: Longint);
begin
  SetSize(aSize);
end;

var
  gExit: Integer = 0;

procedure Check(aStream: TStream; const aWhat: string; aExpected: Int64);
begin
  Inc(gExit);
  if aStream.Size<>aExpected then
    begin
    Writeln(aStream.ClassName, ': ', aWhat, ': size ', aStream.Size, ', expected ', aExpected);
    Halt(gExit);
    end;
end;

var
  lPlain: TPlainMemoryStream;
  lBytes: TPlainBytesStream;
  lString: TPlainStringStream;
  lMemory: TMemoryStream;
  lWord: Longword;
  lInt64: Int64;
begin
  lPlain:=TPlainMemoryStream.Create;
  try
    lPlain.ResizeByConstant;
    Check(lPlain, 'SetSize(5) inside a descendant', 5);
    lPlain.ResizeByLongint(9);
    Check(lPlain, 'SetSize(Longint) inside a descendant', 9);
    lPlain.ResizeByInt64(12);
    Check(lPlain, 'SetSize(Int64) inside a descendant', 12);
    lPlain.ResizeByLongint(0);
    Check(lPlain, 'SetSize(Longint 0) inside a descendant', 0);
  finally
    lPlain.Free;
  end;
  lBytes:=TPlainBytesStream.Create;
  try
    lBytes.ResizeByLongint(7);
    Check(lBytes, 'SetSize(Longint) inside a TBytesStream descendant', 7);
  finally
    lBytes.Free;
  end;
  lString:=TPlainStringStream.Create('');
  try
    lString.ResizeByLongint(6);
    Check(lString, 'SetSize(Longint) inside a TStringStream descendant', 6);
  finally
    lString.Free;
  end;

  { from outside: the public form, with arguments of several integer types }
  lMemory:=TMemoryStream.Create;
  try
    lMemory.SetSize(3);
    Check(lMemory, 'SetSize(3)', 3);
    lWord:=4;
    lMemory.SetSize(lWord*lWord);
    Check(lMemory, 'SetSize(Longword)', 16);
    lInt64:=20;
    lMemory.SetSize(lInt64);
    Check(lMemory, 'SetSize(Int64)', 20);
    lMemory.Size:=8;
    Check(lMemory, 'Size:=8', 8);
    TStream(lMemory).Size:=11;
    Check(lMemory, 'Size:=11 through TStream', 11);
  finally
    lMemory.Free;
  end;
  Writeln('ok');
end.
