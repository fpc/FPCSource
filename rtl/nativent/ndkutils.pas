{
    FPC Utility Function for Native NT applications

    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit NDKUtils;

{.$H+}

interface

uses
  NDK;

procedure ShortStrToNTStr(aStr: ShortString; var aNTStr: TNtUnicodeString);
//procedure AnsiStrToNTStr(const aStr: String; var aNTStr: TNtUnicodeString);

implementation

procedure ShortStrToNTStr(aStr: ShortString; var aNTStr: TNtUnicodeString);
var
  buf: Pointer;
  i: Integer;
begin
  FillChar(aNTStr, SizeOf(TNtUnicodeString), 0);
  aNTStr.Length := Length(aStr) * 2;
  aNTStr.buffer := GetMem(aNTStr.Length);
  buf := aNTStr.buffer;
  for i := 1 to Length(aStr) do begin
    PWord(buf)^ := Word(aStr[i]);
    buf := Pointer(PtrUInt(buf) + SizeOf(Word));
  end;
  aNTStr.MaximumLength := aNTStr.Length;
end;

procedure InitializeObjectAttributes(var aObjectAttr: TObjectAttributes; aName: PNtUnicodeString; aAttributes: ULONG; aRootDir: THandle; aSecurity: Pointer);
begin
  with aObjectAttr do begin
    Length := SizeOf(TObjectAttributes);
    RootDirectory := aRootDir;
    Attributes := aAttributes;
    ObjectName := aName;
    SecurityDescriptor := aSecurity;
    SecurityQualityOfService := Nil;
  end;
end;

end.

