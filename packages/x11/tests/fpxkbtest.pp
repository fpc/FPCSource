program fpxkbtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Unix, x, xlib, XKB, xkblib
  { you can add units after this };

var
  LXDisplay: PDisplay;
  pDesc: PXkbDescPtr;
  pKeySyms: PKeySym;
  iKeycodeLow, iKeycodeHigh, iKeySymsPerKeycode: cint;
  btKeycode: TKeyCode;
  btGroup, btGroups, btLevel, btLevels, btMap, btMask: Byte;
  wXkbKeySym: TKeySym;
  pXkbKeyType: PXkbKeyTypePtr;
  pMap: PXkbKTMapEntryPtr;
begin

  LXDisplay := XOpenDisplay(nil);

  XDisplayKeycodes(LXDisplay, @iKeycodeLow, @iKeycodeHigh);
  pKeySyms := XGetKeyboardMapping(LXDisplay, iKeycodeLow, iKeycodeHigh - iKeycodeLow + 1,  @iKeySymsPerKeycode);
  XFree(pKeySyms);

  pDesc := XkbGetMap(LXDisplay, XkbAllClientInfoMask, XkbUseCoreKbd);

  try
    for btKeycode := iKeycodeLow to iKeycodeHigh do
    begin

      btGroups := XkbKeyNumGroups(pDesc, btKeycode);

      if (btKeycode = 17) then
        WriteLn('Keycode=', btKeycode, ', Groups=', btGroups);

      for btGroup := 0 to btGroups-1 do
      begin

        pXkbKeyType := XkbKeyKeyType(pDesc, btKeycode, btGroup);
        btLevels := pXkbKeyType^.num_levels;

        if (btKeycode = 17) then
          WriteLn('Keycode=', btKeycode, ', Group=', btGroup, ', Levels=', btLevels);

        for btLevel:=0 to btLevels-1 do
        begin

          wXkbKeySym := XkbKeycodeToKeysym(LXDisplay, btKeycode, btGroup, btLevel);


          if (btKeycode = 17) and (wXkbKeySym <> 0) then
          begin

            btMask := 0;
            for btMap:=0 to pXkbKeyType^.map_count-1 do
            begin
              // crashing due to active being Boolean
              //try
                pMap := PXkbKTMapEntryPtr(@(pXkbKeyType^.map[btMap]));
                if {(pXkbKeyType^.map[btMap].active) and} (pMap^.level = btLevel) then
                begin
                  btMask := pMap^.mods.mask;
                  Break;
                end;
              //except
              //end;
            end;

            WriteLn('Keycode=', btKeycode, ', KeySym=', IntToHex(wXkbKeySym, 4), ', Group=', btGroup, ', Level=', btLevel, ', Mask=', btMask);

          end;

        end;

      end;

    end;

  finally
    XkbFreeKeyboard(pDesc, 0, True);
  end;

end.
