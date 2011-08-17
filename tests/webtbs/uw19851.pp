{ %norun }
{ Exit code:
  =  0 - pass
  =  1 - incorrect input for test (first three characters are not abc)
  >= 2 - fail (but can be caused by incorrect test input too)
}

{$mode objfpc}{$H+}

unit uw19851;

interface

uses
  Classes;

function Test(AStream: TStream): Integer;

implementation

function Test(AStream: TStream): Integer;
var
  AString: string;
begin
  try
    Result := 0;
    SetLength(AString, 3);

    // Test reading first 3 characters
    AStream.ReadBuffer(AString[1], Length(AString));
    if AString <> 'abc' then
      Exit(1);
    if AStream.Position <> 3 then
      Exit(2);

    // Test 32-bit seek from current
    if AStream.Seek(Longint(3), soFromCurrent) <> 6 then
      Exit(3);

    // Read & make sure position is correct
    AStream.ReadBuffer(AString[1], Length(AString));
    if AString <> 'ghi' then
      Exit(4);
    if AStream.Position <> AStream.Seek(Longint(0), soFromCurrent) then
      Exit(5);
    if AStream.Position <> AStream.Seek(Int64(0), soCurrent) then
      Exit(6);
    if AStream.Position <> 9 then
      Exit(7);

    // Test 64-bit seek from current
    if AStream.Seek(Int64(3), soCurrent) <> 12 then
      Exit(8);

    // Read & make sure position is correct
    AStream.ReadBuffer(AString[1], Length(AString));
    if AString <> 'mno' then
      Exit(9);
    if AStream.Position <> 15 then
      Exit(10);
    if AStream.Seek(Longint(0), soFromCurrent) <> 15 then
      Exit(11);
    if AStream.Seek(Int64(15), soBeginning) <> 15 then
      Exit(12);
    if AStream.Seek(Longint(15), soFromBeginning) <> 15 then
      Exit(13);
    if AStream.Seek(Int64(0), soCurrent) <> 15 then
      Exit(14);

    // Test 32-bit seek from beginning
    if AStream.Seek(Longint(18), soFromBeginning) <> 18 then
      Exit(15);

    // Read & make sure position is correct
    AStream.ReadBuffer(AString[1], Length(AString));
    if AString <> 'stu' then
      Exit(16);
    if AStream.Position <> 21 then
      Exit(17);

    // Test 64-bit seek from beginning
    if AStream.Seek(Int64(24), soBeginning) <> 24 then
      Exit(18);
    if AStream.Position <> 24 then
      Exit(19);
    if AStream.Seek(Longint(0), soFromCurrent) <> 24 then
      Exit(20);

    // Read & make sure position is correct
    AStream.ReadBuffer(AString[1], Length(AString));
    if AString <> 'yz1' then
      Exit(21);
    if AStream.Position <> 27 then
      Exit(22);
  finally
    {on E: EStreamError do
	begin
      Result := 23;
	  WriteLn(E.Message);
	end;}
  end;
end;

end.

