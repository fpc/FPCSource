{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPPalette implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPPalette }

constructor TFPPalette.create (ACount : integer);
begin
  inherited create;
  if aCount > 0 then
    getmem (FData, sizeof(TFPColor)*ACount)
  else
    FData := nil;
  FCapacity := ACount;
  SetCount (0);
end;

destructor TFPPalette.destroy;
begin
  if FCapacity > 0 then
    freemem (FData);
  inherited;
end;

procedure TFPPalette.Build (Img : TFPCustomImage);
var x,y : integer;
begin
  if (Img.Palette <> self) then
    begin
    Count := 0;
    for x := 0 to img.width-1 do
      for y := 0 to img.height-1 do
        IndexOf(img[x,y]);
    end;
end;

procedure TFPPalette.Merge (pal : TFPPalette);
var r : integer;
begin
  for r := 0 to pal.count-1 do
    IndexOf (pal[r]);
end;

procedure TFPPalette.CheckIndex (index:integer);
begin
  if (index >= FCount) or (index < 0) then
    FPImgError (StrInvalidIndex,[ErrorText[StrPalette],index]);
end;

function TFPPalette.Add (const Value:TFPColor) : integer;
begin
  result := FCount;
  inc (FCount);
  if FCount > FCapacity then
    EnlargeData;
  FData^[result] := Value;
end;

procedure TFPPalette.SetColor (index:integer; const Value:TFPColor);
begin
  if index = FCount then
    Add (Value)
  else
    begin
    CheckIndex (index);
    FData^[index] := Value;
    end;
end;

function TFPPalette.GetColor (index:integer) : TFPColor;
begin
  CheckIndex (index);
  result := FData^[index];
end;

function TFPPalette.GetCount : integer;
begin
  result := FCount;
end;

procedure TFPPalette.EnlargeData;
var old : integer;
    NewData : PFPColorArray;
begin
  old := FCapacity;
  if FCapacity <= 16 then
    FCapacity := 32
  else if FCapacity <= 128 then
    FCapacity := 256
  else
    // MG: changed to exponential growth
    inc (FCapacity, FCapacity);
  GetMem (NewData, sizeof(TFPColor)*FCapacity);
  if old > 0 then
    begin
    move (FData^[0], NewData^[0], sizeof(TFPColor)*FCount);
    FreeMem (FData);
    end;
  FData := NewData;
end;

procedure TFPPalette.SetCount (Value:integer);
var NewData : PFPColorArray;
    O : integer;
begin
  if Value <> FCount then
    begin
    if Value > FCapacity then
      begin
      O := FCapacity;
      FCapacity := Value + 8;
      if FCapacity > 0 then
        GetMem (NewData, sizeof(TFPColor)*FCapacity)
      else
        FData := nil;
      move (FData^, NewData^, sizeof(TFPColor)*FCount);
      if O > 0 then
        FreeMem (FData);
      FData := NewData;
      end;
    for o := FCount to Value-1 do
      FData^[o] := colBlack;
    FCount := Value;
    end;
end;

function TFPPalette.IndexOf (const AColor:TFPColor) : integer;
begin
  result := FCount;
  repeat
    dec (result);
  until (result < 0) or (FData^[result]=AColor);
  if result < 0 then
    result := Add (AColor);
end;

procedure TFPPalette.Clear;
begin
  SetCount (0);
end;
