{ TFPCustomPen }

procedure TFPCustomPen.SetMode (AValue : TFPPenMode);
begin
  FMode := AValue;
end;

procedure TFPCustomPen.SetWidth (AValue : byte);
begin
  if AValue < 1 then
    FWidth := 1
  else
    FWidth := AValue;
end;

procedure TFPCustomPen.SetStyle (AValue : TFPPenStyle);
begin
  FStyle := AValue;
end;

procedure TFPCustomPen.SetPattern (AValue : longword);
begin
  FPattern := AValue;
end;

procedure TFPCustomPen.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomPen do
    begin
    self.Style := Style;
    self.Width := Width;
    self.Mode := Mode;
    self.pattern := pattern;
    end;
  inherited;
end;

function TFPCustomPen.CopyPen : TFPCustomPen;
begin
  result := TFPCustomPen(self.ClassType.Create);
  result.DoCopyProps (self);
end;


