{ TFPCustomImage }

constructor TFPCustomImage.create (AWidth,AHeight:integer);
begin
  inherited create;
  FExtra := TStringList.Create;
  FWidth := 0;
  FHeight := 0;
  FPalette := nil;
  SetSize (AWidth,AHeight);
end;

destructor TFPCustomImage.destroy;
begin
  FExtra.Free;
  if assigned (FPalette) then
    FPalette.Free;
  inherited;
end;

procedure TFPCustomImage.LoadFromStream (Str:TStream; Handler:TFPCustomImagereader);
begin
  Handler.ImageRead (Str, self);
end;

procedure TFPCustomImage.LoadFromFile (filename:String; Handler:TFPCustomImageReader);
var str : TStream;
begin
  if FileExists (filename) then
    try
      str := TFileStream.Create (filename, fmOpenRead);
      LoadFromStream (str, handler);
    finally
      str.Free;
    end
  else
    FPImgError (StrNoFile, [filename]);
end;

procedure TFPCustomImage.SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
begin
  Handler.ImageWrite (Str, Self);
end;

procedure TFPCustomImage.SaveToFile (filename:String; Handler:TFPCustomImageWriter);
var str : TStream;
begin
  try
    str := TFileStream.Create (filename, fmCreate);
    SaveToStream (str, handler);
  finally
    str.Free;
  end
end;

procedure TFPCustomImage.SetHeight (Value : integer);
begin
  if Value <> FHeight then
    SetSize (FWidth, Value);
end;

procedure TFPCustomImage.SetWidth (Value : integer);
begin
  if Value <> FWidth then
    SetSize (Value, FHeight);
end;

procedure TFPCustomImage.SetSize (AWidth, AHeight : integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TFPCustomImage.SetExtraValue (index:integer; AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    FExtra[index] := copy(s, 1, p) + AValue
  else
    FPImgError (StrInvalidIndex,[ErrorText[StrImageExtra],index]);
end;

function TFPCustomImage.GetExtraValue (index:integer) : string;
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    result := copy(s, p+1, maxint)
  else
    result := '';
end;

procedure TFPCustomImage.SetExtraKey (index:integer; AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos('=',s);
  if p > 0 then
    s := AValue + copy(s,p,maxint)
  else
    s := AValue;
  FExtra[index] := s;
end;

function TFPCustomImage.GetExtraKey (index:integer) : string;
begin
  result := FExtra.Names[index];
end;

procedure TFPCustomImage.SetExtra (key:String; AValue:string);
begin
  FExtra.values[key] := AValue;
end;

function TFPCustomImage.GetExtra (key:String) : string;
begin
  result := FExtra.values[key];
end;

function  TFPCustomImage.ExtraCount : integer;
begin
  result := FExtra.count;
end;

procedure TFPCustomImage.RemoveExtra (key:string);
var p : integer;
begin
  p := FExtra.indexOfName(key);
  if p >= 0 then
    FExtra.Delete (p);
end;

procedure TFPCustomImage.SetPixel (x,y:integer; Value:integer);
begin
  CheckPaletteIndex (Value);
  CheckIndex (x,y);
  SetInternalPixel (x,y,Value);
end;

function TFPCustomImage.GetPixel (x,y:integer) : integer;
begin
  CheckIndex (x,y);
  result := GetInternalPixel(x,y);
end;

procedure TFPCustomImage.SetColor (x,y:integer; Value:TFPColor);
begin
  CheckIndex (x,y);
  SetInternalColor (x,y,Value);
end;

function TFPCustomImage.GetColor (x,y:integer) : TFPColor;
begin
  CheckIndex (x,y);
  result := GetInternalColor(x,y);
end;

procedure TFPCustomImage.SetInternalColor (x,y:integer; Value:TFPColor);
var i : integer;
begin
  i := FPalette.IndexOf (Value);
  SetInternalPixel (x,y,i);
end;

function TFPCustomImage.GetInternalColor (x,y:integer) : TFPColor;
begin
  result := FPalette.Color[GetInternalPixel(x,y)];
end;

function TFPCustomImage.GetUsePalette : boolean;
begin
  result := assigned(FPalette);
end;

procedure TFPCustomImage.SetUsePalette (Value : boolean);
begin
  if Value <> assigned(FPalette) then
    if Value then
      FPalette := TFPPalette.Create (0)
    else
      begin
      FPalette.Free;
      FPalette := nil;
      end;
end;

procedure TFPCustomImage.CheckPaletteIndex (PalIndex:integer);
begin
  if UsePalette then
    begin
    if (PalIndex < -1) or (PalIndex >= FPalette.Count) then
      FPImgError (StrInvalidIndex,[ErrorText[StrPalette],PalIndex]);
    end
  else
    FPImgError (StrNoPaletteAvailable);
end;

procedure TFPCustomImage.CheckIndex (x,y:integer);
begin
  if (x < 0) or (x >= FWidth) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= FHeight) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageY],y]);
end;


{ TFPMemoryImage }

constructor TFPMemoryImage.Create (AWidth,AHeight:integer);
begin
  inherited create (AWidth,AHeight);
  UsePalette := True;
end;

destructor TFPMemoryImage.Destroy;
begin
  FreeMem (FData);
  inherited Destroy;
end;

function TFPMemoryImage.GetInternalPixel (x,y:integer) : integer;
begin
  result := FData^[y*FWidth+x];
end;

procedure TFPMemoryImage.SetInternalPixel (x,y:integer; Value:integer);
begin
  FData^[y*FWidth+x] := Value;
end;

function Lowest (a,b : integer) : integer;
begin
  if a <= b then
    result := a
  else
    result := b;
end;

procedure TFPMemoryImage.SetSize (AWidth, AHeight : integer);
var w, h, r, old : integer;
    NewData : PFPIntegerArray;
begin
  if (AWidth <> Width) and (AHeight <> Height) then
    begin
    old := Height * Width;
    r := SizeOf(integer)*AWidth*AHeight;
    if r = 0 then
      NewData := nil
    else
      begin
      GetMem (NewData, r);
      Fillchar (Newdata^[0], r, $FF);
      end;
    if (old <> 0) and assigned(FData) then
      begin
      if r <> 0 then
        begin
        w := Lowest(Width, AWidth);
        h := Lowest(Height, AHeight);
        for r := 0 to h do
          move (FData^[r*Width], NewData^[r*AWidth], w);
        end;
      FreeMem (FData);
      end;
    FData := NewData;
    inherited;
    end;
end;


