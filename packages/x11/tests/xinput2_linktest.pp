{ this program just links all externals, declared in the xinput2 unit }
program xinput2_linktest;
uses
  xinput2;
begin
  halt(0);
  XIQueryPointer(nil, 0, 0, nil, nil, nil, nil, nil, nil, nil, nil, nil);
  XIWarpPointer(nil, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  XIDefineCursor(nil, 0, 0, 0);
  XIUndefineCursor(nil, 0, 0);
  XIChangeHierarchy(nil, nil, 0);
  XISetClientPointer(nil, 0, 0);
  XIGetClientPointer(nil, 0, nil);
  XISelectEvents(nil, 0, nil, 0);
  XIGetSelectedEvents(nil, 0, nil);
  XIQueryVersion(nil, nil, nil);
  XIQueryDevice(nil, 0, nil);
  XISetFocus(nil, 0, 0, 0);
  XIGetFocus(nil, 0, nil);
  XIGrabDevice(nil, 0, 0, 0, 0, 0, 0, 0, nil);
  XIUngrabDevice(nil, 0, 0);
  XIAllowEvents(nil, 0, 0, 0);
  XIAllowTouchEvents(nil, 0, 0, 0, 0);
  XIGrabButton(nil, 0, 0, 0, 0, 0, 0, 0, nil, 0, nil);
  XIGrabKeycode(nil, 0, 0, 0, 0, 0, 0, nil, 0, nil);
  XIGrabEnter(nil, 0, 0, 0, 0, 0, 0, nil, 0, nil);
  XIGrabFocusIn(nil, 0, 0, 0, 0, 0, nil, 0, nil);
  XIGrabTouchBegin(nil, 0, 0, 0, nil, 0, nil);
  XIUngrabButton(nil, 0, 0, 0, 0, nil);
  XIUngrabKeycode(nil, 0, 0, 0, 0, nil);
  XIUngrabEnter(nil, 0, 0, 0, nil);
  XIUngrabFocusIn(nil, 0, 0, 0, nil);
  XIUngrabTouchBegin(nil, 0, 0, 0, nil);
  XIListProperties(nil, 0, nil);
  XIChangeProperty(nil, 0, 0, 0, 0, 0, nil, 0);
  XIDeleteProperty(nil, 0, 0);
  XIGetProperty(nil, 0, 0, 0, 0, 0, 0, nil, nil, nil, nil, nil);
  XIBarrierReleasePointers(nil, nil, 0);
  XIBarrierReleasePointer(nil, 0, 0, 0);
  XIFreeDeviceInfo(nil);
end.
