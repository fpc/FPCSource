var
  v : 0..5;
  sMin, sMax : 0..5;  // if top of range is less than 32, get compiler Panic
begin
  if v in [sMin..sMax] then ;
end.
