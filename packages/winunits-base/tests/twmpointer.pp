{ WmPointer record layouts, constants, wParam helpers and user32 binding }
program twmpointer;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, WmPointer;

type
  TExport = record
    Name: string;
    Bound: Pointer;
  end;

var
  lErrors: Integer;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Returns the distance in bytes from aBase to aField }
function OffsetOf(aBase, aField: Pointer): PtrUInt;

begin
  Result:=PByte(aField)-PByte(aBase);
end;


{ Checks that aActual equals aExpected for the layout item aName }
procedure CheckLayout(const aName: string; aActual, aExpected: PtrUInt);

begin
  Check(aActual=aExpected, aName+' is '+IntToStr(aExpected)+', got '+IntToStr(aActual));
end;


{ Checks record sizes and field offsets against the C compiler for this target }
procedure TestLayouts;

var
  lInfo: POINTER_INFO;
  lTouch: POINTER_TOUCH_INFO;
  lPen: POINTER_PEN_INFO;
  lType: POINTER_TYPE_INFO;
  lUsage: USAGE_PROPERTIES;
  lValue: INPUT_INJECTION_VALUE;
  lEval: TOUCH_HIT_TESTING_PROXIMITY_EVALUATION;
  lHit: TOUCH_HIT_TESTING_INPUT;
  lDevice: POINTER_DEVICE_INFO;
  lProp: POINTER_DEVICE_PROPERTY;
  lTransform: INPUT_TRANSFORM;

begin
{$ifdef CPU64}
  CheckLayout('SizeOf(POINTER_INFO)', SizeOf(POINTER_INFO), 96);
  CheckLayout('POINTER_INFO.sourceDevice', OffsetOf(@lInfo, @lInfo.sourceDevice), 16);
  CheckLayout('POINTER_INFO.hwndTarget', OffsetOf(@lInfo, @lInfo.hwndTarget), 24);
  CheckLayout('POINTER_INFO.ptPixelLocation', OffsetOf(@lInfo, @lInfo.ptPixelLocation), 32);
  CheckLayout('POINTER_INFO.dwTime', OffsetOf(@lInfo, @lInfo.dwTime), 64);
  CheckLayout('POINTER_INFO.PerformanceCount', OffsetOf(@lInfo, @lInfo.PerformanceCount), 80);
  CheckLayout('POINTER_INFO.ButtonChangeType', OffsetOf(@lInfo, @lInfo.ButtonChangeType), 88);
  CheckLayout('SizeOf(POINTER_TOUCH_INFO)', SizeOf(POINTER_TOUCH_INFO), 144);
  CheckLayout('POINTER_TOUCH_INFO.touchFlags', OffsetOf(@lTouch, @lTouch.touchFlags), 96);
  CheckLayout('POINTER_TOUCH_INFO.rcContact', OffsetOf(@lTouch, @lTouch.rcContact), 104);
  CheckLayout('POINTER_TOUCH_INFO.pressure', OffsetOf(@lTouch, @lTouch.pressure), 140);
  CheckLayout('SizeOf(POINTER_PEN_INFO)', SizeOf(POINTER_PEN_INFO), 120);
  CheckLayout('POINTER_PEN_INFO.penFlags', OffsetOf(@lPen, @lPen.penFlags), 96);
  CheckLayout('POINTER_PEN_INFO.tiltY', OffsetOf(@lPen, @lPen.tiltY), 116);
  CheckLayout('SizeOf(POINTER_TYPE_INFO)', SizeOf(POINTER_TYPE_INFO), 152);
  CheckLayout('SizeOf(POINTER_DEVICE_INFO)', SizeOf(POINTER_DEVICE_INFO), 1080);
  CheckLayout('POINTER_DEVICE_INFO.device', OffsetOf(@lDevice, @lDevice.device), 8);
  CheckLayout('POINTER_DEVICE_INFO.pointerDeviceType', OffsetOf(@lDevice, @lDevice.pointerDeviceType), 16);
  CheckLayout('POINTER_DEVICE_INFO.monitor', OffsetOf(@lDevice, @lDevice.monitor), 24);
  CheckLayout('POINTER_DEVICE_INFO.startingCursorId', OffsetOf(@lDevice, @lDevice.startingCursorId), 32);
  CheckLayout('POINTER_DEVICE_INFO.maxActiveContacts', OffsetOf(@lDevice, @lDevice.maxActiveContacts), 36);
  CheckLayout('POINTER_DEVICE_INFO.productString', OffsetOf(@lDevice, @lDevice.productString), 38);
{$else}
  CheckLayout('SizeOf(POINTER_INFO)', SizeOf(POINTER_INFO), 88);
  CheckLayout('POINTER_INFO.sourceDevice', OffsetOf(@lInfo, @lInfo.sourceDevice), 16);
  CheckLayout('POINTER_INFO.hwndTarget', OffsetOf(@lInfo, @lInfo.hwndTarget), 20);
  CheckLayout('POINTER_INFO.ptPixelLocation', OffsetOf(@lInfo, @lInfo.ptPixelLocation), 24);
  CheckLayout('POINTER_INFO.dwTime', OffsetOf(@lInfo, @lInfo.dwTime), 56);
  CheckLayout('POINTER_INFO.PerformanceCount', OffsetOf(@lInfo, @lInfo.PerformanceCount), 72);
  CheckLayout('POINTER_INFO.ButtonChangeType', OffsetOf(@lInfo, @lInfo.ButtonChangeType), 80);
  CheckLayout('SizeOf(POINTER_TOUCH_INFO)', SizeOf(POINTER_TOUCH_INFO), 136);
  CheckLayout('POINTER_TOUCH_INFO.touchFlags', OffsetOf(@lTouch, @lTouch.touchFlags), 88);
  CheckLayout('POINTER_TOUCH_INFO.rcContact', OffsetOf(@lTouch, @lTouch.rcContact), 96);
  CheckLayout('POINTER_TOUCH_INFO.pressure', OffsetOf(@lTouch, @lTouch.pressure), 132);
  CheckLayout('SizeOf(POINTER_PEN_INFO)', SizeOf(POINTER_PEN_INFO), 112);
  CheckLayout('POINTER_PEN_INFO.penFlags', OffsetOf(@lPen, @lPen.penFlags), 88);
  CheckLayout('POINTER_PEN_INFO.tiltY', OffsetOf(@lPen, @lPen.tiltY), 108);
  CheckLayout('SizeOf(POINTER_TYPE_INFO)', SizeOf(POINTER_TYPE_INFO), 144);
  CheckLayout('SizeOf(POINTER_DEVICE_INFO)', SizeOf(POINTER_DEVICE_INFO), 1064);
  CheckLayout('POINTER_DEVICE_INFO.device', OffsetOf(@lDevice, @lDevice.device), 4);
  CheckLayout('POINTER_DEVICE_INFO.pointerDeviceType', OffsetOf(@lDevice, @lDevice.pointerDeviceType), 8);
  CheckLayout('POINTER_DEVICE_INFO.monitor', OffsetOf(@lDevice, @lDevice.monitor), 12);
  CheckLayout('POINTER_DEVICE_INFO.startingCursorId', OffsetOf(@lDevice, @lDevice.startingCursorId), 16);
  CheckLayout('POINTER_DEVICE_INFO.maxActiveContacts', OffsetOf(@lDevice, @lDevice.maxActiveContacts), 20);
  CheckLayout('POINTER_DEVICE_INFO.productString', OffsetOf(@lDevice, @lDevice.productString), 22);
{$endif}
  CheckLayout('POINTER_TYPE_INFO.touchInfo', OffsetOf(@lType, @lType.touchInfo), 8);
  CheckLayout('POINTER_TYPE_INFO.penInfo', OffsetOf(@lType, @lType.penInfo), 8);
  CheckLayout('SizeOf(USAGE_PROPERTIES)', SizeOf(USAGE_PROPERTIES), 32);
  CheckLayout('USAGE_PROPERTIES.logicalMinimum', OffsetOf(@lUsage, @lUsage.logicalMinimum), 8);
  CheckLayout('USAGE_PROPERTIES.count', OffsetOf(@lUsage, @lUsage.count), 20);
  CheckLayout('USAGE_PROPERTIES.physicalMinimum', OffsetOf(@lUsage, @lUsage.physicalMinimum), 24);
  CheckLayout('SizeOf(INPUT_INJECTION_VALUE)', SizeOf(INPUT_INJECTION_VALUE), 12);
  CheckLayout('INPUT_INJECTION_VALUE.index', OffsetOf(@lValue, @lValue.index), 8);
  CheckLayout('SizeOf(TOUCH_HIT_TESTING_PROXIMITY_EVALUATION)', SizeOf(TOUCH_HIT_TESTING_PROXIMITY_EVALUATION), 12);
  CheckLayout('TOUCH_HIT_TESTING_PROXIMITY_EVALUATION.adjustedPoint', OffsetOf(@lEval, @lEval.adjustedPoint), 4);
  CheckLayout('SizeOf(TOUCH_HIT_TESTING_INPUT)', SizeOf(TOUCH_HIT_TESTING_INPUT), 48);
  CheckLayout('TOUCH_HIT_TESTING_INPUT.orientation', OffsetOf(@lHit, @lHit.orientation), 44);
  CheckLayout('SizeOf(POINTER_DEVICE_PROPERTY)', SizeOf(POINTER_DEVICE_PROPERTY), 28);
  CheckLayout('POINTER_DEVICE_PROPERTY.usageId', OffsetOf(@lProp, @lProp.usageId), 26);
  CheckLayout('SizeOf(POINTER_DEVICE_CURSOR_INFO)', SizeOf(POINTER_DEVICE_CURSOR_INFO), 8);
  CheckLayout('SizeOf(INPUT_TRANSFORM)', SizeOf(INPUT_TRANSFORM), 64);
  CheckLayout('INPUT_TRANSFORM._44', OffsetOf(@lTransform, @lTransform._44), 60);
  CheckLayout('INPUT_TRANSFORM.m[3,3]', OffsetOf(@lTransform, @lTransform.m[3, 3]), 60);
  CheckLayout('INPUT_TRANSFORM.m[1,2]', OffsetOf(@lTransform, @lTransform.m[1, 2]), OffsetOf(@lTransform, @lTransform._23));
end;


{ Checks a sample of the constants against winuser.h }
procedure TestConstants;

begin
  CheckLayout('WM_POINTERDEVICECHANGE', WM_POINTERDEVICECHANGE, $0238);
  CheckLayout('WM_NCPOINTERUPDATE', WM_NCPOINTERUPDATE, $0241);
  CheckLayout('WM_POINTERUPDATE', WM_POINTERUPDATE, $0245);
  CheckLayout('WM_POINTERDOWN', WM_POINTERDOWN, $0246);
  CheckLayout('WM_POINTERUP', WM_POINTERUP, $0247);
  CheckLayout('WM_POINTERWHEEL', WM_POINTERWHEEL, $024E);
  CheckLayout('WM_POINTERROUTEDRELEASED', WM_POINTERROUTEDRELEASED, $0253);
  CheckLayout('QS_POINTER', QS_POINTER, $1000);
  CheckLayout('POINTER_FLAG_HASTRANSFORM', POINTER_FLAG_HASTRANSFORM, $00400000);
  CheckLayout('PT_TOUCHPAD', PT_TOUCHPAD, 5);
  CheckLayout('POINTER_CHANGE_FIFTHBUTTON_UP', POINTER_CHANGE_FIFTHBUTTON_UP, 10);
  CheckLayout('FEEDBACK_GESTURE_PRESSANDTAP', FEEDBACK_GESTURE_PRESSANDTAP, 11);
  CheckLayout('PA_NOACTIVATE', PA_NOACTIVATE, 3);
  CheckLayout('PDC_MODE_ASPECTRATIOPRESERVED', PDC_MODE_ASPECTRATIOPRESERVED, $800);
end;


{ Checks the wParam helpers of pointer messages }
procedure TestWParamHelpers;

var
  lWParam: WPARAM;

begin
  lWParam:=WPARAM((POINTER_MESSAGE_FLAG_NEW or POINTER_MESSAGE_FLAG_PRIMARY or POINTER_MESSAGE_FLAG_FIRSTBUTTON) shl 16) or 1234;
  Check(GET_POINTERID_WPARAM(lWParam)=1234, 'GET_POINTERID_WPARAM returns the low word');
  Check(IS_POINTER_NEW_WPARAM(lWParam), 'IS_POINTER_NEW_WPARAM detects the new flag');
  Check(IS_POINTER_PRIMARY_WPARAM(lWParam), 'IS_POINTER_PRIMARY_WPARAM detects the primary flag');
  Check(IS_POINTER_FIRSTBUTTON_WPARAM(lWParam), 'IS_POINTER_FIRSTBUTTON_WPARAM detects the first button flag');
  Check(not IS_POINTER_INCONTACT_WPARAM(lWParam), 'IS_POINTER_INCONTACT_WPARAM is false without the flag');
  Check(not IS_POINTER_CANCELED_WPARAM(lWParam), 'IS_POINTER_CANCELED_WPARAM is false without the flag');
  Check(not HAS_POINTER_CONFIDENCE_WPARAM(lWParam), 'HAS_POINTER_CONFIDENCE_WPARAM is false without the flag');
  Check(IS_POINTER_FLAG_SET_WPARAM(lWParam, POINTER_MESSAGE_FLAG_NEW or POINTER_MESSAGE_FLAG_PRIMARY), 'IS_POINTER_FLAG_SET_WPARAM accepts a combination of set flags');
  Check(not IS_POINTER_FLAG_SET_WPARAM(lWParam, POINTER_MESSAGE_FLAG_NEW or POINTER_MESSAGE_FLAG_INRANGE), 'IS_POINTER_FLAG_SET_WPARAM rejects a combination with a missing flag');
  Check(not IS_POINTER_NEW_WPARAM(1234), 'Pointer identifier bits are not flags');
end;


{ Checks that each function is bound exactly when user32 exports it, and calls the safe ones }
procedure TestBinding;

var
  lExports: array of TExport;
  lUser32: HMODULE;
  lType: POINTER_INPUT_TYPE;
  lCount, i: Integer;

  { Adds an export name and the bound address of its variable }
  procedure Add(const aName: string; aBound: Pointer);

  begin
    SetLength(lExports, Length(lExports)+1);
    lExports[High(lExports)].Name:=aName;
    lExports[High(lExports)].Bound:=aBound;
  end;

begin
  lExports:=nil;
  Add('InitializeTouchInjection', Pointer(InitializeTouchInjection));
  Add('InjectTouchInput', Pointer(InjectTouchInput));
  Add('GetPointerType', Pointer(GetPointerType));
  Add('GetPointerCursorId', Pointer(GetPointerCursorId));
  Add('GetPointerInfo', Pointer(GetPointerInfo));
  Add('GetPointerInfoHistory', Pointer(GetPointerInfoHistory));
  Add('GetPointerFrameInfo', Pointer(GetPointerFrameInfo));
  Add('GetPointerFrameInfoHistory', Pointer(GetPointerFrameInfoHistory));
  Add('GetPointerTouchInfo', Pointer(GetPointerTouchInfo));
  Add('GetPointerTouchInfoHistory', Pointer(GetPointerTouchInfoHistory));
  Add('GetPointerFrameTouchInfo', Pointer(GetPointerFrameTouchInfo));
  Add('GetPointerFrameTouchInfoHistory', Pointer(GetPointerFrameTouchInfoHistory));
  Add('GetPointerPenInfo', Pointer(GetPointerPenInfo));
  Add('GetPointerPenInfoHistory', Pointer(GetPointerPenInfoHistory));
  Add('GetPointerFramePenInfo', Pointer(GetPointerFramePenInfo));
  Add('GetPointerFramePenInfoHistory', Pointer(GetPointerFramePenInfoHistory));
  Add('SkipPointerFrameMessages', Pointer(SkipPointerFrameMessages));
  Add('RegisterPointerInputTarget', Pointer(RegisterPointerInputTarget));
  Add('RegisterPointerInputTargetEx', Pointer(RegisterPointerInputTargetEx));
  Add('UnregisterPointerInputTarget', Pointer(UnregisterPointerInputTarget));
  Add('UnregisterPointerInputTargetEx', Pointer(UnregisterPointerInputTargetEx));
  Add('EnableMouseInPointer', Pointer(EnableMouseInPointer));
  Add('IsMouseInPointerEnabled', Pointer(IsMouseInPointerEnabled));
  Add('EnableMouseInPointerForThread', Pointer(EnableMouseInPointerForThread));
  Add('RegisterTouchHitTestingWindow', Pointer(RegisterTouchHitTestingWindow));
  Add('EvaluateProximityToRect', Pointer(EvaluateProximityToRect));
  Add('EvaluateProximityToPolygon', Pointer(EvaluateProximityToPolygon));
  Add('PackTouchHitTestingProximityEvaluation', Pointer(PackTouchHitTestingProximityEvaluation));
  Add('GetWindowFeedbackSetting', Pointer(GetWindowFeedbackSetting));
  Add('SetWindowFeedbackSetting', Pointer(SetWindowFeedbackSetting));
  Add('CreateSyntheticPointerDevice', Pointer(CreateSyntheticPointerDevice));
  Add('InjectSyntheticPointerInput', Pointer(InjectSyntheticPointerInput));
  Add('DestroySyntheticPointerDevice', Pointer(DestroySyntheticPointerDevice));
  Add('GetPointerDevices', Pointer(GetPointerDevices));
  Add('GetPointerDevice', Pointer(GetPointerDevice));
  Add('GetPointerDeviceProperties', Pointer(GetPointerDeviceProperties));
  Add('RegisterPointerDeviceNotifications', Pointer(RegisterPointerDeviceNotifications));
  Add('GetPointerDeviceRects', Pointer(GetPointerDeviceRects));
  Add('GetPointerDeviceCursors', Pointer(GetPointerDeviceCursors));
  Add('GetRawPointerDeviceData', Pointer(GetRawPointerDeviceData));
  Add('GetPointerInputTransform', Pointer(GetPointerInputTransform));
  lUser32:=GetModuleHandle('user32.dll');
  Check(lUser32<>0, 'user32.dll is loaded');
  lCount:=0;
  for i:=0 to High(lExports) do
    begin
      Check(lExports[i].Bound=GetProcAddress(lUser32, PAnsiChar(AnsiString(lExports[i].Name))),
        lExports[i].Name+' is bound to the user32 export');
      if Assigned(lExports[i].Bound) then
        Inc(lCount);
    end;
  writeln(lCount, ' of ', Length(lExports), ' functions exported by user32');
  Check(PointerApiAvailable=Assigned(GetPointerInfo), 'PointerApiAvailable reports whether GetPointerInfo is bound');
  if Assigned(GetPointerType) then
    begin
      lType:=0;
      if GetPointerType($FFFF, @lType) then
        Check((lType>=PT_POINTER) and (lType<=PT_TOUCHPAD), 'GetPointerType returns a valid input type, got '+IntToStr(lType));
    end;
  if Assigned(IsMouseInPointerEnabled) then
    Check(not IsMouseInPointerEnabled(), 'Mouse in pointer mode is off by default');
end;


begin
  lErrors:=0;
  TestLayouts;
  TestConstants;
  TestWParamHelpers;
  TestBinding;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
