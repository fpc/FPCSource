{ TFPCanvasHelper }

constructor TFPCanvasHelper.Create;
begin
  inherited create;
  FCanvas := nil;
  FFixedCanvas := false;
  FAllocated := false;
end;

destructor TFPCanvasHelper.destroy;
begin
  if Allocated then
    DeAllocateResources;
  inherited;
end;

procedure TFPCanvasHelper.SetFixedCanvas (AValue : boolean);
begin
  FFixedCanvas := AValue;
end;

procedure TFPCanvasHelper.NotifyCanvas;
begin
  FCanvas.CheckHelper (self);
end;

procedure TFPCanvasHelper.CheckAllocated (ValueNeeded:boolean);
begin
  if (Allocated <> ValueNeeded) then
    Raise TFPFontException.CreateFmt (ErrAllocation, [EFont, ErrAlloc[ValueNeeded]]);
end;

procedure TFPCanvasHelper.SetColor (AValue:TFPColor);
begin
  FColor := AValue;
end;

procedure TFPCanvasHelper.SetFlags (index:integer; AValue:boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl index)
  else
    FFlags := FFlags and not (1 shl index);
end;

function TFPCanvasHelper.GetFlags (index:integer) : boolean;
begin
  result := (FFlags and (1 shl index)) <> 0;
end;

function TFPCanvasHelper.GetAllocated : boolean;
begin
  if FFixedCanvas then
    result := assigned(FCanvas)
  else
    result := FAllocated;
end;

procedure TFPCanvasHelper.AllocateResources (ACanvas : TFPCustomCanvas);
begin
  if FFixedCanvas and FAllocated then
    DeallocateResources;
  try
    FCanvas := ACanvas;
    DoAllocateResources;
    FAllocated := True;
  except
    FCanvas := nil;
    FAllocated := False;
  end;
end;

procedure TFPCanvasHelper.DeallocateResources;
begin
  if FAllocated then
    try
      DoDeallocateResources;
    finally
      FAllocated := false;
      NotifyCanvas;
      FCanvas := nil;
    end;
end;

procedure TFPCanvasHelper.DoCopyProps (From:TFPCanvasHelper);
begin
  FCanvas := nil;
  color := from.color;
end;

procedure TFPCanvasHelper.DoAllocateResources;
begin
end;

procedure TFPCanvasHelper.DoDeallocateResources;
begin
end;


