{
  Pointer input API of user32 (Windows 8 and later), translated from winuser.h.
  This file is part of the Free Pascal run time library. Copyright (c) 2026 by the Free Pascal development team.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit WmPointer;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$packrecords C}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
  Windows;
{$ENDIF FPC_DOTTEDUNITS}

const
  WM_POINTERDEVICECHANGE = $0238;
  WM_POINTERDEVICEINRANGE = $0239;
  WM_POINTERDEVICEOUTOFRANGE = $023A;
  WM_NCPOINTERUPDATE = $0241;
  WM_NCPOINTERDOWN = $0242;
  WM_NCPOINTERUP = $0243;
  WM_POINTERUPDATE = $0245;
  WM_POINTERDOWN = $0246;
  WM_POINTERUP = $0247;
  WM_POINTERENTER = $0249;
  WM_POINTERLEAVE = $024A;
  WM_POINTERACTIVATE = $024B;
  WM_POINTERCAPTURECHANGED = $024C;
  WM_TOUCHHITTESTING = $024D;
  WM_POINTERWHEEL = $024E;
  WM_POINTERHWHEEL = $024F;
  DM_POINTERHITTEST = $0250;
  WM_POINTERROUTEDTO = $0251;
  WM_POINTERROUTEDAWAY = $0252;
  WM_POINTERROUTEDRELEASED = $0253;

  QS_TOUCH = $0800;
  QS_POINTER = $1000;

  POINTER_FLAG_NONE = $00000000;
  POINTER_FLAG_NEW = $00000001;
  POINTER_FLAG_INRANGE = $00000002;
  POINTER_FLAG_INCONTACT = $00000004;
  POINTER_FLAG_FIRSTBUTTON = $00000010;
  POINTER_FLAG_SECONDBUTTON = $00000020;
  POINTER_FLAG_THIRDBUTTON = $00000040;
  POINTER_FLAG_FOURTHBUTTON = $00000080;
  POINTER_FLAG_FIFTHBUTTON = $00000100;
  POINTER_FLAG_PRIMARY = $00002000;
  POINTER_FLAG_CONFIDENCE = $00004000;
  POINTER_FLAG_CANCELED = $00008000;
  POINTER_FLAG_DOWN = $00010000;
  POINTER_FLAG_UPDATE = $00020000;
  POINTER_FLAG_UP = $00040000;
  POINTER_FLAG_WHEEL = $00080000;
  POINTER_FLAG_HWHEEL = $00100000;
  POINTER_FLAG_CAPTURECHANGED = $00200000;
  POINTER_FLAG_HASTRANSFORM = $00400000;

  POINTER_MOD_SHIFT = $0004;
  POINTER_MOD_CTRL = $0008;

  TOUCH_FLAG_NONE = $00000000;

  TOUCH_MASK_NONE = $00000000;
  TOUCH_MASK_CONTACTAREA = $00000001;
  TOUCH_MASK_ORIENTATION = $00000002;
  TOUCH_MASK_PRESSURE = $00000004;

  PEN_FLAG_NONE = $00000000;
  PEN_FLAG_BARREL = $00000001;
  PEN_FLAG_INVERTED = $00000002;
  PEN_FLAG_ERASER = $00000004;

  PEN_MASK_NONE = $00000000;
  PEN_MASK_PRESSURE = $00000001;
  PEN_MASK_ROTATION = $00000002;
  PEN_MASK_TILT_X = $00000004;
  PEN_MASK_TILT_Y = $00000008;

  POINTER_MESSAGE_FLAG_NEW = $00000001;
  POINTER_MESSAGE_FLAG_INRANGE = $00000002;
  POINTER_MESSAGE_FLAG_INCONTACT = $00000004;
  POINTER_MESSAGE_FLAG_FIRSTBUTTON = $00000010;
  POINTER_MESSAGE_FLAG_SECONDBUTTON = $00000020;
  POINTER_MESSAGE_FLAG_THIRDBUTTON = $00000040;
  POINTER_MESSAGE_FLAG_FOURTHBUTTON = $00000080;
  POINTER_MESSAGE_FLAG_FIFTHBUTTON = $00000100;
  POINTER_MESSAGE_FLAG_PRIMARY = $00002000;
  POINTER_MESSAGE_FLAG_CONFIDENCE = $00004000;
  POINTER_MESSAGE_FLAG_CANCELED = $00008000;

  PA_ACTIVATE = MA_ACTIVATE;
  PA_NOACTIVATE = MA_NOACTIVATE;

  MAX_TOUCH_COUNT = 256;

  TOUCH_FEEDBACK_DEFAULT = $1;
  TOUCH_FEEDBACK_INDIRECT = $2;
  TOUCH_FEEDBACK_NONE = $3;

  TOUCH_HIT_TESTING_DEFAULT = $0;
  TOUCH_HIT_TESTING_CLIENT = $1;
  TOUCH_HIT_TESTING_NONE = $2;

  TOUCH_HIT_TESTING_PROXIMITY_CLOSEST = $0;
  TOUCH_HIT_TESTING_PROXIMITY_FARTHEST = $FFF;

  GWFS_INCLUDE_ANCESTORS = $00000001;

  POINTER_DEVICE_PRODUCT_STRING_MAX = 520;

  PDC_ARRIVAL = $001;
  PDC_REMOVAL = $002;
  PDC_ORIENTATION_0 = $004;
  PDC_ORIENTATION_90 = $008;
  PDC_ORIENTATION_180 = $010;
  PDC_ORIENTATION_270 = $020;
  PDC_MODE_DEFAULT = $040;
  PDC_MODE_CENTERED = $080;
  PDC_MAPPING_CHANGE = $100;
  PDC_RESOLUTION = $200;
  PDC_ORIGIN = $400;
  PDC_MODE_ASPECTRATIOPRESERVED = $800;

type
  POINTER_INPUT_TYPE = DWORD;
  PPOINTER_INPUT_TYPE = ^POINTER_INPUT_TYPE;
  POINTER_FLAGS = UInt32;
  TOUCH_FLAGS = UInt32;
  TOUCH_MASK = UInt32;
  PEN_FLAGS = UInt32;
  PEN_MASK = UInt32;

const
  PT_POINTER = $00000001;
  PT_TOUCH = $00000002;
  PT_PEN = $00000003;
  PT_MOUSE = $00000004;
  PT_TOUCHPAD = $00000005;

type
  FEEDBACK_TYPE = DWORD;

const
  FEEDBACK_TOUCH_CONTACTVISUALIZATION = 1;
  FEEDBACK_PEN_BARRELVISUALIZATION = 2;
  FEEDBACK_PEN_TAP = 3;
  FEEDBACK_PEN_DOUBLETAP = 4;
  FEEDBACK_PEN_PRESSANDHOLD = 5;
  FEEDBACK_PEN_RIGHTTAP = 6;
  FEEDBACK_TOUCH_TAP = 7;
  FEEDBACK_TOUCH_DOUBLETAP = 8;
  FEEDBACK_TOUCH_PRESSANDHOLD = 9;
  FEEDBACK_TOUCH_RIGHTTAP = 10;
  FEEDBACK_GESTURE_PRESSANDTAP = 11;
  FEEDBACK_MAX = $FFFFFFFF;

type
  POINTER_BUTTON_CHANGE_TYPE = Integer;

const
  POINTER_CHANGE_NONE = 0;
  POINTER_CHANGE_FIRSTBUTTON_DOWN = 1;
  POINTER_CHANGE_FIRSTBUTTON_UP = 2;
  POINTER_CHANGE_SECONDBUTTON_DOWN = 3;
  POINTER_CHANGE_SECONDBUTTON_UP = 4;
  POINTER_CHANGE_THIRDBUTTON_DOWN = 5;
  POINTER_CHANGE_THIRDBUTTON_UP = 6;
  POINTER_CHANGE_FOURTHBUTTON_DOWN = 7;
  POINTER_CHANGE_FOURTHBUTTON_UP = 8;
  POINTER_CHANGE_FIFTHBUTTON_DOWN = 9;
  POINTER_CHANGE_FIFTHBUTTON_UP = 10;

type
  POINTER_FEEDBACK_MODE = Integer;

const
  POINTER_FEEDBACK_DEFAULT = 1;
  POINTER_FEEDBACK_INDIRECT = 2;
  POINTER_FEEDBACK_NONE = 3;

type
  POINTER_DEVICE_TYPE = DWORD;

const
  POINTER_DEVICE_TYPE_INTEGRATED_PEN = $00000001;
  POINTER_DEVICE_TYPE_EXTERNAL_PEN = $00000002;
  POINTER_DEVICE_TYPE_TOUCH = $00000003;
  POINTER_DEVICE_TYPE_TOUCH_PAD = $00000004;
  POINTER_DEVICE_TYPE_MAX = $FFFFFFFF;

type
  POINTER_DEVICE_CURSOR_TYPE = DWORD;

const
  POINTER_DEVICE_CURSOR_TYPE_UNKNOWN = $00000000;
  POINTER_DEVICE_CURSOR_TYPE_TIP = $00000001;
  POINTER_DEVICE_CURSOR_TYPE_ERASER = $00000002;
  POINTER_DEVICE_CURSOR_TYPE_MAX = $FFFFFFFF;

type
  HSYNTHETICPOINTERDEVICE = THandle;

  tagPOINTER_INFO = record
    pointerType: POINTER_INPUT_TYPE;
    pointerId: UInt32;
    frameId: UInt32;
    pointerFlags: POINTER_FLAGS;
    sourceDevice: THandle;
    hwndTarget: HWND;
    ptPixelLocation: TPoint;
    ptHimetricLocation: TPoint;
    ptPixelLocationRaw: TPoint;
    ptHimetricLocationRaw: TPoint;
    dwTime: DWORD;
    historyCount: UInt32;
    InputData: Int32;
    dwKeyStates: DWORD;
    PerformanceCount: UInt64;
    ButtonChangeType: POINTER_BUTTON_CHANGE_TYPE;
  end;
  POINTER_INFO = tagPOINTER_INFO;
  TPointerInfo = tagPOINTER_INFO;
  PPointerInfo = ^tagPOINTER_INFO;

  tagPOINTER_TOUCH_INFO = record
    pointerInfo: POINTER_INFO;
    touchFlags: TOUCH_FLAGS;
    touchMask: TOUCH_MASK;
    rcContact: TRect;
    rcContactRaw: TRect;
    orientation: UInt32;
    pressure: UInt32;
  end;
  POINTER_TOUCH_INFO = tagPOINTER_TOUCH_INFO;
  TPointerTouchInfo = tagPOINTER_TOUCH_INFO;
  PPointerTouchInfo = ^tagPOINTER_TOUCH_INFO;

  tagPOINTER_PEN_INFO = record
    pointerInfo: POINTER_INFO;
    penFlags: PEN_FLAGS;
    penMask: PEN_MASK;
    pressure: UInt32;
    rotation: UInt32;
    tiltX: Int32;
    tiltY: Int32;
  end;
  POINTER_PEN_INFO = tagPOINTER_PEN_INFO;
  TPointerPenInfo = tagPOINTER_PEN_INFO;
  PPointerPenInfo = ^tagPOINTER_PEN_INFO;

  tagUSAGE_PROPERTIES = record
    level: USHORT;
    page: USHORT;
    usage: USHORT;
    logicalMinimum: Int32;
    logicalMaximum: Int32;
    &unit: USHORT;
    exponent: USHORT;
    count: Byte;
    physicalMinimum: Int32;
    physicalMaximum: Int32;
  end;
  USAGE_PROPERTIES = tagUSAGE_PROPERTIES;
  PUSAGE_PROPERTIES = ^tagUSAGE_PROPERTIES;
  TUsageProperties = tagUSAGE_PROPERTIES;
  PUsageProperties = ^tagUSAGE_PROPERTIES;

  tagPOINTER_TYPE_INFO = record
    &type: POINTER_INPUT_TYPE;
    case Integer of
      0: (touchInfo: POINTER_TOUCH_INFO);
      1: (penInfo: POINTER_PEN_INFO);
  end;
  POINTER_TYPE_INFO = tagPOINTER_TYPE_INFO;
  PPOINTER_TYPE_INFO = ^tagPOINTER_TYPE_INFO;
  TPointerTypeInfo = tagPOINTER_TYPE_INFO;
  PPointerTypeInfo = ^tagPOINTER_TYPE_INFO;

  tagINPUT_INJECTION_VALUE = record
    page: USHORT;
    usage: USHORT;
    value: Int32;
    index: USHORT;
  end;
  INPUT_INJECTION_VALUE = tagINPUT_INJECTION_VALUE;
  PINPUT_INJECTION_VALUE = ^tagINPUT_INJECTION_VALUE;
  TInputInjectionValue = tagINPUT_INJECTION_VALUE;
  PInputInjectionValue = ^tagINPUT_INJECTION_VALUE;

  tagTOUCH_HIT_TESTING_PROXIMITY_EVALUATION = record
    score: UInt16;
    adjustedPoint: TPoint;
  end;
  TOUCH_HIT_TESTING_PROXIMITY_EVALUATION = tagTOUCH_HIT_TESTING_PROXIMITY_EVALUATION;
  PTOUCH_HIT_TESTING_PROXIMITY_EVALUATION = ^tagTOUCH_HIT_TESTING_PROXIMITY_EVALUATION;
  TTouchHitTestingProximityEvaluation = tagTOUCH_HIT_TESTING_PROXIMITY_EVALUATION;
  PTouchHitTestingProximityEvaluation = ^tagTOUCH_HIT_TESTING_PROXIMITY_EVALUATION;

  tagTOUCH_HIT_TESTING_INPUT = record
    pointerId: UInt32;
    point: TPoint;
    boundingBox: TRect;
    nonOccludedBoundingBox: TRect;
    orientation: UInt32;
  end;
  TOUCH_HIT_TESTING_INPUT = tagTOUCH_HIT_TESTING_INPUT;
  PTOUCH_HIT_TESTING_INPUT = ^tagTOUCH_HIT_TESTING_INPUT;
  TTouchHitTestingInput = tagTOUCH_HIT_TESTING_INPUT;
  PTouchHitTestingInput = ^tagTOUCH_HIT_TESTING_INPUT;

  tagPOINTER_DEVICE_INFO = record
    displayOrientation: DWORD;
    device: THandle;
    pointerDeviceType: POINTER_DEVICE_TYPE;
    monitor: HMONITOR;
    startingCursorId: ULONG;
    maxActiveContacts: USHORT;
    productString: array[0..POINTER_DEVICE_PRODUCT_STRING_MAX-1] of WCHAR;
  end;
  POINTER_DEVICE_INFO = tagPOINTER_DEVICE_INFO;
  TPointerDeviceInfo = tagPOINTER_DEVICE_INFO;
  PPointerDeviceInfo = ^tagPOINTER_DEVICE_INFO;

  tagPOINTER_DEVICE_PROPERTY = record
    logicalMin: Int32;
    logicalMax: Int32;
    physicalMin: Int32;
    physicalMax: Int32;
    &unit: UInt32;
    unitExponent: UInt32;
    usagePageId: USHORT;
    usageId: USHORT;
  end;
  POINTER_DEVICE_PROPERTY = tagPOINTER_DEVICE_PROPERTY;
  TPointerDeviceProperty = tagPOINTER_DEVICE_PROPERTY;
  PPointerDeviceProperty = ^tagPOINTER_DEVICE_PROPERTY;

  tagPOINTER_DEVICE_CURSOR_INFO = record
    cursorId: UInt32;
    cursor: POINTER_DEVICE_CURSOR_TYPE;
  end;
  POINTER_DEVICE_CURSOR_INFO = tagPOINTER_DEVICE_CURSOR_INFO;
  TPointerDeviceCursorInfo = tagPOINTER_DEVICE_CURSOR_INFO;
  PPointerDeviceCursorInfo = ^tagPOINTER_DEVICE_CURSOR_INFO;

  tagINPUT_TRANSFORM = record
    case Integer of
      0: (_11, _12, _13, _14,
          _21, _22, _23, _24,
          _31, _32, _33, _34,
          _41, _42, _43, _44: Single);
      1: (m: array[0..3, 0..3] of Single);
  end;
  INPUT_TRANSFORM = tagINPUT_TRANSFORM;
  TInputTransform = tagINPUT_TRANSFORM;
  PInputTransform = ^tagINPUT_TRANSFORM;

var
  // Prepares touch injection for up to maxCount contacts
  InitializeTouchInjection: function(maxCount: UInt32; dwMode: DWORD): WINBOOL; stdcall;
  // Injects count touch contacts
  InjectTouchInput: function(count: UInt32; contacts: PPointerTouchInfo): WINBOOL; stdcall;
  // Returns the input type of a pointer
  GetPointerType: function(pointerId: UInt32; pointerType: PPOINTER_INPUT_TYPE): WINBOOL; stdcall;
  // Returns the cursor identifier of a pointer
  GetPointerCursorId: function(pointerId: UInt32; cursorId: PUInt32): WINBOOL; stdcall;
  // Returns the information of a pointer for the current message
  GetPointerInfo: function(pointerId: UInt32; pointerInfo: PPointerInfo): WINBOOL; stdcall;
  // Returns the coalesced history of a pointer
  GetPointerInfoHistory: function(pointerId: UInt32; entriesCount: PUInt32; pointerInfo: PPointerInfo): WINBOOL; stdcall;
  // Returns the information of all pointers in the frame of a pointer
  GetPointerFrameInfo: function(pointerId: UInt32; pointerCount: PUInt32; pointerInfo: PPointerInfo): WINBOOL; stdcall;
  // Returns the frame history of all pointers in the frame of a pointer
  GetPointerFrameInfoHistory: function(pointerId: UInt32; entriesCount, pointerCount: PUInt32; pointerInfo: PPointerInfo): WINBOOL; stdcall;
  // Returns the touch information of a pointer
  GetPointerTouchInfo: function(pointerId: UInt32; touchInfo: PPointerTouchInfo): WINBOOL; stdcall;
  // Returns the coalesced touch history of a pointer
  GetPointerTouchInfoHistory: function(pointerId: UInt32; entriesCount: PUInt32; touchInfo: PPointerTouchInfo): WINBOOL; stdcall;
  // Returns the touch information of all pointers in the frame of a pointer
  GetPointerFrameTouchInfo: function(pointerId: UInt32; pointerCount: PUInt32; touchInfo: PPointerTouchInfo): WINBOOL; stdcall;
  // Returns the touch frame history of all pointers in the frame of a pointer
  GetPointerFrameTouchInfoHistory: function(pointerId: UInt32; entriesCount, pointerCount: PUInt32; touchInfo: PPointerTouchInfo): WINBOOL; stdcall;
  // Returns the pen information of a pointer
  GetPointerPenInfo: function(pointerId: UInt32; penInfo: PPointerPenInfo): WINBOOL; stdcall;
  // Returns the coalesced pen history of a pointer
  GetPointerPenInfoHistory: function(pointerId: UInt32; entriesCount: PUInt32; penInfo: PPointerPenInfo): WINBOOL; stdcall;
  // Returns the pen information of all pointers in the frame of a pointer
  GetPointerFramePenInfo: function(pointerId: UInt32; pointerCount: PUInt32; penInfo: PPointerPenInfo): WINBOOL; stdcall;
  // Returns the pen frame history of all pointers in the frame of a pointer
  GetPointerFramePenInfoHistory: function(pointerId: UInt32; entriesCount, pointerCount: PUInt32; penInfo: PPointerPenInfo): WINBOOL; stdcall;
  // Removes the remaining pointer messages of the frame of a pointer
  SkipPointerFrameMessages: function(pointerId: UInt32): WINBOOL; stdcall;
  // Routes all input of a pointer type to a window
  RegisterPointerInputTarget: function(hwnd: HWND; pointerType: POINTER_INPUT_TYPE): WINBOOL; stdcall;
  // Routes or copies all input of a pointer type to a window
  RegisterPointerInputTargetEx: function(hwnd: HWND; pointerType: POINTER_INPUT_TYPE; fObserve: WINBOOL): WINBOOL; stdcall;
  // Stops routing input of a pointer type to a window
  UnregisterPointerInputTarget: function(hwnd: HWND; pointerType: POINTER_INPUT_TYPE): WINBOOL; stdcall;
  // Stops routing or copying input of a pointer type to a window
  UnregisterPointerInputTargetEx: function(hwnd: HWND; pointerType: POINTER_INPUT_TYPE): WINBOOL; stdcall;
  // Makes the mouse send pointer messages for the process
  EnableMouseInPointer: function(fEnable: WINBOOL): WINBOOL; stdcall;
  // Returns whether the mouse sends pointer messages
  IsMouseInPointerEnabled: function: WINBOOL; stdcall;
  // Makes the mouse send pointer messages for the calling thread
  EnableMouseInPointerForThread: function: WINBOOL; stdcall;
  // Registers a window for WM_TOUCHHITTESTING
  RegisterTouchHitTestingWindow: function(hwnd: HWND; value: ULONG): WINBOOL; stdcall;
  // Scores a touch contact against a rectangle
  EvaluateProximityToRect: function(controlBoundingBox: PRect; pHitTestingInput: PTouchHitTestingInput;
    pProximityEval: PTouchHitTestingProximityEvaluation): WINBOOL; stdcall;
  // Scores a touch contact against a polygon of numVertices points
  EvaluateProximityToPolygon: function(numVertices: UInt32; controlPolygon: PPoint; pHitTestingInput: PTouchHitTestingInput;
    pProximityEval: PTouchHitTestingProximityEvaluation): WINBOOL; stdcall;
  // Returns the WM_TOUCHHITTESTING result for a proximity evaluation
  PackTouchHitTestingProximityEvaluation: function(pHitTestingInput: PTouchHitTestingInput;
    pProximityEval: PTouchHitTestingProximityEvaluation): LRESULT; stdcall;
  // Returns a feedback setting of a window
  GetWindowFeedbackSetting: function(hwnd: HWND; feedback: FEEDBACK_TYPE; dwFlags: DWORD; pSize: PUInt32; config: Pointer): WINBOOL; stdcall;
  // Changes a feedback setting of a window
  SetWindowFeedbackSetting: function(hwnd: HWND; feedback: FEEDBACK_TYPE; dwFlags: DWORD; size: UInt32; configuration: Pointer): WINBOOL; stdcall;
  // Creates a device for injecting pen or touch input
  CreateSyntheticPointerDevice: function(pointerType: POINTER_INPUT_TYPE; maxCount: ULONG; mode: POINTER_FEEDBACK_MODE): HSYNTHETICPOINTERDEVICE; stdcall;
  // Injects count pointers through a synthetic pointer device
  InjectSyntheticPointerInput: function(device: HSYNTHETICPOINTERDEVICE; pointerInfo: PPointerTypeInfo; count: UInt32): WINBOOL; stdcall;
  // Destroys a synthetic pointer device
  DestroySyntheticPointerDevice: procedure(device: HSYNTHETICPOINTERDEVICE); stdcall;
  // Returns the pointer devices attached to the system
  GetPointerDevices: function(deviceCount: PUInt32; pointerDevices: PPointerDeviceInfo): WINBOOL; stdcall;
  // Returns the information of a pointer device
  GetPointerDevice: function(device: THandle; pointerDevice: PPointerDeviceInfo): WINBOOL; stdcall;
  // Returns the properties of a pointer device
  GetPointerDeviceProperties: function(device: THandle; propertyCount: PUInt32; pointerProperties: PPointerDeviceProperty): WINBOOL; stdcall;
  // Registers a window for WM_POINTERDEVICECHANGE
  RegisterPointerDeviceNotifications: function(window: HWND; notifyRange: WINBOOL): WINBOOL; stdcall;
  // Returns the device and display rectangles of a pointer device
  GetPointerDeviceRects: function(device: THandle; pointerDeviceRect, displayRect: PRect): WINBOOL; stdcall;
  // Returns the cursors of a pointer device
  GetPointerDeviceCursors: function(device: THandle; cursorCount: PUInt32; deviceCursors: PPointerDeviceCursorInfo): WINBOOL; stdcall;
  // Returns raw property values of a pointer
  GetRawPointerDeviceData: function(pointerId, historyCount, propertiesCount: UInt32; pProperties: PPointerDeviceProperty;
    pValues: PLONG): WINBOOL; stdcall;
  // Returns the input transforms of a pointer
  GetPointerInputTransform: function(pointerId, historyCount: UInt32; inputTransform: PInputTransform): WINBOOL; stdcall;

// Returns whether user32 exports the pointer functions (Windows 8 and later)
function PointerApiAvailable: Boolean;

// Returns the pointer identifier of a pointer message
function GET_POINTERID_WPARAM(aWParam: WPARAM): Word; inline;
// Returns whether all bits of aFlag are set in the flags of a pointer message
function IS_POINTER_FLAG_SET_WPARAM(aWParam: WPARAM; aFlag: DWORD): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_NEW
function IS_POINTER_NEW_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_INRANGE
function IS_POINTER_INRANGE_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_INCONTACT
function IS_POINTER_INCONTACT_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_FIRSTBUTTON
function IS_POINTER_FIRSTBUTTON_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_SECONDBUTTON
function IS_POINTER_SECONDBUTTON_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_THIRDBUTTON
function IS_POINTER_THIRDBUTTON_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_FOURTHBUTTON
function IS_POINTER_FOURTHBUTTON_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_FIFTHBUTTON
function IS_POINTER_FIFTHBUTTON_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_PRIMARY
function IS_POINTER_PRIMARY_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_CONFIDENCE
function HAS_POINTER_CONFIDENCE_WPARAM(aWParam: WPARAM): Boolean; inline;
// Returns whether a pointer message has POINTER_MESSAGE_FLAG_CANCELED
function IS_POINTER_CANCELED_WPARAM(aWParam: WPARAM): Boolean; inline;

implementation

function PointerApiAvailable: Boolean;

begin
  Result:=Assigned(GetPointerInfo);
end;


function GET_POINTERID_WPARAM(aWParam: WPARAM): Word;

begin
  Result:=LOWORD(aWParam);
end;


function IS_POINTER_FLAG_SET_WPARAM(aWParam: WPARAM; aFlag: DWORD): Boolean;

begin
  Result:=(DWORD(HIWORD(aWParam)) and aFlag)=aFlag;
end;


function IS_POINTER_NEW_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_NEW);
end;


function IS_POINTER_INRANGE_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_INRANGE);
end;


function IS_POINTER_INCONTACT_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_INCONTACT);
end;


function IS_POINTER_FIRSTBUTTON_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_FIRSTBUTTON);
end;


function IS_POINTER_SECONDBUTTON_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_SECONDBUTTON);
end;


function IS_POINTER_THIRDBUTTON_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_THIRDBUTTON);
end;


function IS_POINTER_FOURTHBUTTON_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_FOURTHBUTTON);
end;


function IS_POINTER_FIFTHBUTTON_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_FIFTHBUTTON);
end;


function IS_POINTER_PRIMARY_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_PRIMARY);
end;


function HAS_POINTER_CONFIDENCE_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_CONFIDENCE);
end;


function IS_POINTER_CANCELED_WPARAM(aWParam: WPARAM): Boolean;

begin
  Result:=IS_POINTER_FLAG_SET_WPARAM(aWParam, POINTER_MESSAGE_FLAG_CANCELED);
end;


{ Binds the procedural variables to the user32 exports; missing exports stay nil }
procedure LoadPointerApi;

var
  lUser32: HMODULE;

begin
  lUser32:=GetModuleHandle('user32.dll');
  if lUser32=0 then
    exit;
  Pointer(InitializeTouchInjection):=GetProcAddress(lUser32, 'InitializeTouchInjection');
  Pointer(InjectTouchInput):=GetProcAddress(lUser32, 'InjectTouchInput');
  Pointer(GetPointerType):=GetProcAddress(lUser32, 'GetPointerType');
  Pointer(GetPointerCursorId):=GetProcAddress(lUser32, 'GetPointerCursorId');
  Pointer(GetPointerInfo):=GetProcAddress(lUser32, 'GetPointerInfo');
  Pointer(GetPointerInfoHistory):=GetProcAddress(lUser32, 'GetPointerInfoHistory');
  Pointer(GetPointerFrameInfo):=GetProcAddress(lUser32, 'GetPointerFrameInfo');
  Pointer(GetPointerFrameInfoHistory):=GetProcAddress(lUser32, 'GetPointerFrameInfoHistory');
  Pointer(GetPointerTouchInfo):=GetProcAddress(lUser32, 'GetPointerTouchInfo');
  Pointer(GetPointerTouchInfoHistory):=GetProcAddress(lUser32, 'GetPointerTouchInfoHistory');
  Pointer(GetPointerFrameTouchInfo):=GetProcAddress(lUser32, 'GetPointerFrameTouchInfo');
  Pointer(GetPointerFrameTouchInfoHistory):=GetProcAddress(lUser32, 'GetPointerFrameTouchInfoHistory');
  Pointer(GetPointerPenInfo):=GetProcAddress(lUser32, 'GetPointerPenInfo');
  Pointer(GetPointerPenInfoHistory):=GetProcAddress(lUser32, 'GetPointerPenInfoHistory');
  Pointer(GetPointerFramePenInfo):=GetProcAddress(lUser32, 'GetPointerFramePenInfo');
  Pointer(GetPointerFramePenInfoHistory):=GetProcAddress(lUser32, 'GetPointerFramePenInfoHistory');
  Pointer(SkipPointerFrameMessages):=GetProcAddress(lUser32, 'SkipPointerFrameMessages');
  Pointer(RegisterPointerInputTarget):=GetProcAddress(lUser32, 'RegisterPointerInputTarget');
  Pointer(RegisterPointerInputTargetEx):=GetProcAddress(lUser32, 'RegisterPointerInputTargetEx');
  Pointer(UnregisterPointerInputTarget):=GetProcAddress(lUser32, 'UnregisterPointerInputTarget');
  Pointer(UnregisterPointerInputTargetEx):=GetProcAddress(lUser32, 'UnregisterPointerInputTargetEx');
  Pointer(EnableMouseInPointer):=GetProcAddress(lUser32, 'EnableMouseInPointer');
  Pointer(IsMouseInPointerEnabled):=GetProcAddress(lUser32, 'IsMouseInPointerEnabled');
  Pointer(EnableMouseInPointerForThread):=GetProcAddress(lUser32, 'EnableMouseInPointerForThread');
  Pointer(RegisterTouchHitTestingWindow):=GetProcAddress(lUser32, 'RegisterTouchHitTestingWindow');
  Pointer(EvaluateProximityToRect):=GetProcAddress(lUser32, 'EvaluateProximityToRect');
  Pointer(EvaluateProximityToPolygon):=GetProcAddress(lUser32, 'EvaluateProximityToPolygon');
  Pointer(PackTouchHitTestingProximityEvaluation):=GetProcAddress(lUser32, 'PackTouchHitTestingProximityEvaluation');
  Pointer(GetWindowFeedbackSetting):=GetProcAddress(lUser32, 'GetWindowFeedbackSetting');
  Pointer(SetWindowFeedbackSetting):=GetProcAddress(lUser32, 'SetWindowFeedbackSetting');
  Pointer(CreateSyntheticPointerDevice):=GetProcAddress(lUser32, 'CreateSyntheticPointerDevice');
  Pointer(InjectSyntheticPointerInput):=GetProcAddress(lUser32, 'InjectSyntheticPointerInput');
  Pointer(DestroySyntheticPointerDevice):=GetProcAddress(lUser32, 'DestroySyntheticPointerDevice');
  Pointer(GetPointerDevices):=GetProcAddress(lUser32, 'GetPointerDevices');
  Pointer(GetPointerDevice):=GetProcAddress(lUser32, 'GetPointerDevice');
  Pointer(GetPointerDeviceProperties):=GetProcAddress(lUser32, 'GetPointerDeviceProperties');
  Pointer(RegisterPointerDeviceNotifications):=GetProcAddress(lUser32, 'RegisterPointerDeviceNotifications');
  Pointer(GetPointerDeviceRects):=GetProcAddress(lUser32, 'GetPointerDeviceRects');
  Pointer(GetPointerDeviceCursors):=GetProcAddress(lUser32, 'GetPointerDeviceCursors');
  Pointer(GetRawPointerDeviceData):=GetProcAddress(lUser32, 'GetRawPointerDeviceData');
  Pointer(GetPointerInputTransform):=GetProcAddress(lUser32, 'GetPointerInputTransform');
end;


initialization
  LoadPointerApi;
end.
