{ TFPCustomBrush }

procedure TFPCustomBrush.SetStyle (AValue : TFPBrushStyle);
begin
  FStyle := AValue
end;

procedure TFPCustomBrush.SetImage (AValue : TFPCustomImage);
begin
  FImage := AValue;
end;

procedure TFPCustomBrush.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomBrush do
    begin
    self.Style := Style;
    self.Image := Image;
    end;
  inherited;
end;

function TFPCustomBrush.CopyBrush : TFPCustomBrush;
begin
  result := TFPCustomBrush(self.ClassType.Create);
  result.DoCopyProps (self);
end;



