
function NSMakeRange (loc: NSUInteger; len: NSUInteger): NSRange;
begin
 result.location := loc;
 result.length := len;
end;

function NSMaxRange (range: NSRange): NSUInteger;
begin
 result := range.location + range.length;
end;

function NSLocationInRange (loc: NSUInteger; range: NSRange): boolean;
begin
 if (loc <= range.location + range.length) and (loc >= range.location) then
  result := true
 else
  result := false;
end;

function NSEqualRanges (range1, range2: NSRange): boolean;
begin
 if (range1.location = range2.location) and (range1.length = range2.length) then
  result := true
 else
  result := false;
end;

function NSMakePoint (x: CGFloat; y: CGFloat): NSPoint;
begin
  result.y := y;
  result.x := x;
end;

function NSMakeSize(w: CGFloat; h: CGFloat): NSSize;
begin
  result.width := w;
  result.height := h;
end;

function NSMakeRect(x, y: CGFloat; w, h: CGFloat): NSRect;
begin
  result.origin.x := x;
  result.origin.y := y;
  result.size.width := w;
  result.size.height := h;
end;

function NSMaxX (aRect: NSRect): CGFloat;
begin
  result := aRect.origin.x + aRect.size.width;
end;

function NSMaxY (aRect: NSRect): CGFloat;
begin
  result := aRect.origin.y + aRect.size.height;
end;

function NSMidX (aRect: NSRect): CGFloat;
begin
  result := (aRect.origin.x + aRect.size.width) * 0.5 ;
end;

function NSMidY (aRect: NSRect): CGFloat;
begin
  result := (aRect.origin.y + aRect.size.height) * 0.5 ;
end;

function NSMinX (aRect: NSRect): CGFloat;
begin
  result := aRect.origin.x;
end;

function NSMinY (aRect: NSRect): CGFloat;
begin
  result := aRect.origin.y;
end;

function NSWidth (aRect: NSRect): CGFloat;
begin
  result := aRect.size.width;
end;

function NSHeight (aRect: NSRect): CGFloat;
begin
  result := aRect.size.height;
end;

function NSRectFromCGRect (aRect: CGRect): NSRect;
begin
  result.origin.x := aRect.origin.x;
  result.origin.y := aRect.origin.y;
  result.size.width := aRect.size.width;
  result.size.height := aRect.size.height;
end;

function NSRectToCGRect (aRect: NSRect): CGRect;
begin
  result.origin.x := aRect.origin.x;
  result.origin.y := aRect.origin.y;
  result.size.width := aRect.size.width;
  result.size.height := aRect.size.height;
end;

function NSPointFromCGPoint (aPoint: CGPoint): NSPoint;
begin
  result.y := aPoint.y;
  result.x := aPoint.x;
end;

function NSPointToCGPoint (aPoint: NSPoint): CGPoint;
begin
  result.y := aPoint.y;
  result.x := aPoint.x;
end;

function NSSizeFromCGSize(aSize: CGSize): NSSize;
begin
  result.width := aSize.width;
  result.height := aSize.height;
end;

function NSSizeToCGSize(aSize: NSSize): CGSize;
begin
  result.width := aSize.width;
  result.height := aSize.height;
end;

function NSSTR (inString: PChar): NSString;
begin
  Result := NSString(CFSTR(inString));
end;
