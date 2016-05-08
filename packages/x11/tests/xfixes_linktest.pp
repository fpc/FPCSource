{ this program just links all externals, declared in the xfixes unit }
program xfixes_linktest;
uses
  xfixes;
begin
  halt(0);
  XFixesQueryExtension(nil, nil, nil);
  XFixesQueryVersion(nil, nil, nil);
  XFixesVersion;
  XFixesSelectSelectionInput(nil, 0, 0, 0);
  XFixesSelectCursorInput(nil, 0, 0);
  XFixesGetCursorImage(nil);
  XFixesCreateRegion(nil, nil, 0);
  XFixesCreateRegionFromBitmap(nil, 0);
  XFixesCreateRegionFromWindow(nil, 0, 0);
  XFixesCreateRegionFromGC(nil, nil);
  XFixesCreateRegionFromPicture(nil, 0);
  XFixesDestroyRegion(nil, 0);
  XFixesSetRegion(nil, 0, nil, 0);
  XFixesCopyRegion(nil, 0, 0);
  XFixesUnionRegion(nil, 0, 0, 0);
  XFixesIntersectRegion(nil, 0, 0, 0);
  XFixesSubtractRegion(nil, 0, 0, 0);
  XFixesInvertRegion(nil, 0, nil, 0);
  XFixesTranslateRegion(nil, 0, 0, 0);
  XFixesRegionExtents(nil, 0, 0);
  XFixesFetchRegion(nil, 0, nil);
  XFixesFetchRegionAndBounds(nil, 0, nil, nil);
  XFixesSetGCClipRegion(nil, nil, 0, 0, 0);
  XFixesSetWindowShapeRegion(nil, 0, 0, 0, 0, 0);
  XFixesSetPictureClipRegion(nil, 0, 0, 0, 0);
  XFixesSetCursorName(nil, 0, nil);
  XFixesGetCursorName(nil, 0, nil);
  XFixesChangeCursor(nil, 0, 0);
  XFixesChangeCursorByName(nil, 0, nil);
  XFixesExpandRegion(nil, 0, 0, 0, 0, 0, 0);
  XFixesHideCursor(nil, 0);
  XFixesShowCursor(nil, 0);
  XFixesCreatePointerBarrier(nil, 0, 0, 0, 0, 0, 0, 0, nil);
  XFixesDestroyPointerBarrier(nil, 0);
end.
