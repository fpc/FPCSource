{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team

    Microsoft Kinect SDK import.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Original C++ Microsoft headers are (c) Microsoft:
  NuiSensor.h, NuiApi.h, NuiSkeleton.h and NuiImageCamera.h
}

{$MODE objfpc}
{$H+}
{$PACKRECORDS C}

unit libkinect10;

interface

uses sysutils, windows;

const
  LibKinect = 'kinect10.dll';

Const
  NUI_INITIALIZE_FLAG_USES_AUDIO                  = $10000000;
  NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX = $00000001;
  NUI_INITIALIZE_FLAG_USES_COLOR                  = $00000002;
  NUI_INITIALIZE_FLAG_USES_SKELETON               = $00000008;
  NUI_INITIALIZE_FLAG_USES_DEPTH                  = $00000020;
  NUI_INITIALIZE_FLAG_USES_HIGH_QUALITY_COLOR     = $00000040;
  NUI_INITIALIZE_DEFAULT_HARDWARE_THREAD          = $FFFFFFFF;

  ERROR_DEVICE_NOT_CONNECTED  = 1167;
  E_NUI_DEVICE_NOT_CONNECTED  = ERROR_DEVICE_NOT_CONNECTED;
  E_NUI_DEVICE_NOT_READY      = ERROR_NOT_READY;
  E_NUI_ALREADY_INITIALIZED   = ERROR_ALREADY_INITIALIZED;
  E_NUI_NO_MORE_ITEMS         = ERROR_NO_MORE_ITEMS;

  FACILITY_NUI                       = $301;
  S_NUI_INITIALIZING                 = $03010001;
  E_NUI_FRAME_NO_DATA                = $83010001;
  E_NUI_STREAM_NOT_ENABLED           = $83010002;
  E_NUI_IMAGE_STREAM_IN_USE          = $83010003;
  E_NUI_FRAME_LIMIT_EXCEEDED         = $83010004;
  E_NUI_FEATURE_NOT_INITIALIZED      = $83010005;
  E_NUI_NOTGENUINE                   = $83010006;
  E_NUI_INSUFFICIENTBANDWIDTH        = $83010007;
  E_NUI_NOTSUPPORTED                 = $83010008;
  E_NUI_DEVICE_IN_USE                = $83010009;
  E_NUI_DATABASE_NOT_FOUND           = $8301000D;
  E_NUI_DATABASE_VERSION_MISMATCH    = $8301000E;
  E_NUI_HARDWARE_FEATURE_UNAVAILABLE = $8301000F;
  E_NUI_NOTCONNECTED                 = $83010014;
  E_NUI_NOTREADY                     = $83010015;
  E_NUI_SKELETAL_ENGINE_BUSY         = $830100AA;
  E_NUI_NOTPOWERED                   = $8301027F;
  E_NUI_BADINDEX                     = $83010585;
  E_NUI_BADIINDEX                    = E_NUI_BADINDEX;


  MICARRAY_ADAPTIVE_BEAM = $1100;
  MAX_DEV_STR_LEN        = 512;
  NUI_SKELETON_COUNT     = 6;

  NUI_SKELETON_POSITION_HIP_CENTER       = 0;
  NUI_SKELETON_POSITION_SPINE            = NUI_SKELETON_POSITION_HIP_CENTER+1;
  NUI_SKELETON_POSITION_SHOULDER_CENTER  = NUI_SKELETON_POSITION_SPINE+1;
  NUI_SKELETON_POSITION_HEAD             = NUI_SKELETON_POSITION_SHOULDER_CENTER+1;
  NUI_SKELETON_POSITION_SHOULDER_LEFT    = NUI_SKELETON_POSITION_HEAD+1;
  NUI_SKELETON_POSITION_ELBOW_LEFT       = NUI_SKELETON_POSITION_SHOULDER_LEFT+1;
  NUI_SKELETON_POSITION_WRIST_LEFT       = NUI_SKELETON_POSITION_ELBOW_LEFT+1;
  NUI_SKELETON_POSITION_HAND_LEFT        = NUI_SKELETON_POSITION_WRIST_LEFT+1;
  NUI_SKELETON_POSITION_SHOULDER_RIGHT   = NUI_SKELETON_POSITION_HAND_LEFT+1;
  NUI_SKELETON_POSITION_ELBOW_RIGHT      = NUI_SKELETON_POSITION_SHOULDER_RIGHT+1;
  NUI_SKELETON_POSITION_WRIST_RIGHT      = NUI_SKELETON_POSITION_ELBOW_RIGHT+1;
  NUI_SKELETON_POSITION_HAND_RIGHT       = NUI_SKELETON_POSITION_WRIST_RIGHT+1;
  NUI_SKELETON_POSITION_HIP_LEFT         = NUI_SKELETON_POSITION_HAND_RIGHT+1;
  NUI_SKELETON_POSITION_KNEE_LEFT        = NUI_SKELETON_POSITION_HIP_LEFT+1;
  NUI_SKELETON_POSITION_ANKLE_LEFT       = NUI_SKELETON_POSITION_KNEE_LEFT+1;
  NUI_SKELETON_POSITION_FOOT_LEFT        = NUI_SKELETON_POSITION_ANKLE_LEFT+1;
  NUI_SKELETON_POSITION_HIP_RIGHT        = NUI_SKELETON_POSITION_FOOT_LEFT+1;
  NUI_SKELETON_POSITION_KNEE_RIGHT       = NUI_SKELETON_POSITION_HIP_RIGHT+1;
  NUI_SKELETON_POSITION_ANKLE_RIGHT      = NUI_SKELETON_POSITION_KNEE_RIGHT+1;
  NUI_SKELETON_POSITION_FOOT_RIGHT       = NUI_SKELETON_POSITION_ANKLE_RIGHT+1;
  NUI_SKELETON_POSITION_COUNT            = NUI_SKELETON_POSITION_FOOT_RIGHT+1;

  NUI_IMAGE_RESOLUTION_INVALID   = -1;
  NUI_IMAGE_RESOLUTION_80x60     = 0;
  NUI_IMAGE_RESOLUTION_320x240   = NUI_IMAGE_RESOLUTION_80x60+1;
  NUI_IMAGE_RESOLUTION_640x480   = NUI_IMAGE_RESOLUTION_320x240+1;
  NUI_IMAGE_RESOLUTION_1280x960  = NUI_IMAGE_RESOLUTION_640x480+1;

  NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX = 0;
  NUI_IMAGE_TYPE_COLOR                  = NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX+1;
  NUI_IMAGE_TYPE_COLOR_YUV              = NUI_IMAGE_TYPE_COLOR+1;
  NUI_IMAGE_TYPE_COLOR_RAW_YUV          = NUI_IMAGE_TYPE_COLOR_YUV+1;
  NUI_IMAGE_TYPE_DEPTH                  = NUI_IMAGE_TYPE_COLOR_RAW_YUV+1;

  NUI_SKELETON_POSITION_NOT_TRACKED = 0;
  NUI_SKELETON_POSITION_INFERRED    = NUI_SKELETON_POSITION_NOT_TRACKED+1;
  NUI_SKELETON_POSITION_TRACKED     = NUI_SKELETON_POSITION_INFERRED+1;

  NUI_SKELETON_NOT_TRACKED   = 0;
  NUI_SKELETON_POSITION_ONLY = NUI_SKELETON_NOT_TRACKED+1;
  NUI_SKELETON_TRACKED       = NUI_SKELETON_POSITION_ONLY+1;

  NUI_IMAGE_PLAYER_INDEX_SHIFT      = 3;
  NUI_IMAGE_PLAYER_INDEX_MASK       = (1 shl NUI_IMAGE_PLAYER_INDEX_SHIFT)-1;
  NUI_IMAGE_DEPTH_MAXIMUM           = (4000 shl NUI_IMAGE_PLAYER_INDEX_SHIFT) or NUI_IMAGE_PLAYER_INDEX_MASK;
  NUI_IMAGE_DEPTH_MINIMUM           = 800 shl NUI_IMAGE_PLAYER_INDEX_SHIFT;
  NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE = (3000 shl NUI_IMAGE_PLAYER_INDEX_SHIFT) or NUI_IMAGE_PLAYER_INDEX_MASK;
  NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE = 400 shl NUI_IMAGE_PLAYER_INDEX_SHIFT;
  NUI_IMAGE_DEPTH_NO_VALUE          = 0;
  NUI_IMAGE_DEPTH_TOO_FAR_VALUE     = $0fff shl NUI_IMAGE_PLAYER_INDEX_SHIFT;
  NUI_DEPTH_DEPTH_UNKNOWN_VALUE     = $1fff shl NUI_IMAGE_PLAYER_INDEX_SHIFT;

  NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS         = 285.63;
  NUI_CAMERA_DEPTH_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS = 3.501e-3;
  NUI_CAMERA_DEPTH_NOMINAL_DIAGONAL_FOV                   = 70.0;
  NUI_CAMERA_DEPTH_NOMINAL_HORIZONTAL_FOV                 = 58.5;
  NUI_CAMERA_DEPTH_NOMINAL_VERTICAL_FOV                   = 45.6;

  NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS         = 531.15;
  NUI_CAMERA_COLOR_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS = 1.83e-3;
  NUI_CAMERA_COLOR_NOMINAL_DIAGONAL_FOV                   = 73.9;
  NUI_CAMERA_COLOR_NOMINAL_HORIZONTAL_FOV                 = 62.0;
  NUI_CAMERA_COLOR_NOMINAL_VERTICAL_FOV                   = 48.6;

  NUI_IMAGE_FRAME_FLAG_NONE              = $00000000;
  NUI_IMAGE_FRAME_FLAG_VIEW_AREA_UNKNOWN = $00000001;

  NUI_IMAGE_STREAM_FLAG_SUPPRESS_NO_FRAME_DATA          = $00010000;
  NUI_IMAGE_STREAM_FLAG_ENABLE_NEAR_MODE                = $00020000;
  NUI_IMAGE_STREAM_FLAG_DISTINCT_OVERFLOW_DEPTH_VALUES  = $00040000;
  NUI_IMAGE_STREAM_FRAME_LIMIT_MAXIMUM                  = 4;
  NUI_CAMERA_ELEVATION_MAXIMUM                          = 27;
  NUI_CAMERA_ELEVATION_MINIMUM                          = -27;

  FLT_EPSILON                                           = 1.192092896e-07;

  NUI_SKELETON_MAX_TRACKED_COUNT                          = 2;
  NUI_SKELETON_INVALID_TRACKING_ID                        = 0;
  NUI_SKELETON_QUALITY_CLIPPED_RIGHT                      = $00000001;
  NUI_SKELETON_QUALITY_CLIPPED_LEFT                       = $00000002;
  NUI_SKELETON_QUALITY_CLIPPED_TOP                        = $00000004;
  NUI_SKELETON_QUALITY_CLIPPED_BOTTOM                     = $00000008;
  NUI_SKELETON_FRAME_FLAG_SEATED_SUPPORT_ENABLED          = $00000008;
  NUI_SKELETON_TRACKING_FLAG_SUPPRESS_NO_FRAME_DATA       = $00000001;
  NUI_SKELETON_TRACKING_FLAG_TITLE_SETS_TRACKED_SKELETONS = $00000002;
  NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT        = $00000004;
  NUI_SKELETON_TRACKING_FLAG_ENABLE_IN_NEAR_RANGE         = $00000008;

  NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 = NUI_CAMERA_DEPTH_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS;
  NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 = NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS;

type
  Vector4 = record
    x,y,z,w : Single;
  end;
  PVector4 = ^Vector4;
  TVector4 = Vector4;

  Matrix4 = record
    M11, M12, M13, M14,
    M21, M22, M23, M24,
    M31, M32, M33, M34,
    M41, M42, M43, M44 : Single;
  end;
  PMatrix4 = ^Matrix4;
  TMatrix4 = Matrix4;

  NUI_SKELETON_POSITION_INDEX = Type Integer;
  PNUI_SKELETON_POSITION_INDEX = ^NUI_SKELETON_POSITION_INDEX;
  TNUI_SKELETON_POSITION_INDEX = NUI_SKELETON_POSITION_INDEX;

  NUI_IMAGE_TYPE = Type Integer;
  PNUI_IMAGE_TYPE = ^NUI_IMAGE_TYPE;
  TNUI_IMAGE_TYPE = NUI_IMAGE_TYPE;

  NUI_IMAGE_RESOLUTION = Type integer;
  PNUI_IMAGE_RESOLUTION = ^NUI_IMAGE_RESOLUTION;
  TNUI_IMAGE_RESOLUTION = NUI_IMAGE_RESOLUTION;

  NUI_IMAGE_VIEW_AREA = record
    eDigitalZoom : integer;
    lCenterX : Longint;
    lCenterY : Longint;
  end;
  PNUI_IMAGE_VIEW_AREA = ^NUI_IMAGE_VIEW_AREA;
  TNUI_IMAGE_VIEW_AREA = NUI_IMAGE_VIEW_AREA;

  NUI_TRANSFORM_SMOOTH_PARAMETERS = record
    fSmoothing,
    fCorrection,
    fPrediction,
    fJitterRadius,
    fMaxDeviationRadius : single;
  end;
  PNUI_TRANSFORM_SMOOTH_PARAMETERS = ^NUI_TRANSFORM_SMOOTH_PARAMETERS;
  TNUI_TRANSFORM_SMOOTH_PARAMETERS = NUI_TRANSFORM_SMOOTH_PARAMETERS;

  NUI_SURFACE_DESC = record
    Width : cardinal;
    Height : cardinal;
  end;
  PNUI_SURFACE_DESC = ^NUI_SURFACE_DESC;
  TNUI_SURFACE_DESC = NUI_SURFACE_DESC;

  NUI_SKELETON_POSITION_TRACKING_STATE = Type integer;
  PNUI_SKELETON_POSITION_TRACKING_STATE = ^NUI_SKELETON_POSITION_TRACKING_STATE;
  TNUI_SKELETON_POSITION_TRACKING_STATE = NUI_SKELETON_POSITION_TRACKING_STATE;

  NUI_SKELETON_TRACKING_STATE = Type integer;
  PNUI_SKELETON_TRACKING_STATE = ^NUI_SKELETON_TRACKING_STATE;
  TNUI_SKELETON_TRACKING_STATE = NUI_SKELETON_TRACKING_STATE;

  NUI_SKELETON_DATA = record
    eTrackingState : NUI_SKELETON_TRACKING_STATE;
    dwTrackingID,
    dwEnrollmentIndex,
    dwUserIndex : DWORD;
    Position : Vector4;
    SkeletonPositions : array[0..19] of Vector4;
    eSkeletonPositionTrackingState : array[0..19] of NUI_SKELETON_POSITION_TRACKING_STATE;
    dwQualityFlags : DWORD;
  end;
  PNUI_SKELETON_DATA = ^NUI_SKELETON_DATA;
  TNUI_SKELETON_DATA = NUI_SKELETON_DATA;

  NUI_SKELETON_FRAME = record
    liTimeStamp : int64;
    dwFrameNumber,
    dwFlags : DWORD;
    vFloorClipPlane,
    vNormalToGravity : Vector4;
    SkeletonData : array[0..NUI_SKELETON_COUNT-1] of NUI_SKELETON_DATA;
  end;
  PNUI_SKELETON_FRAME = ^NUI_SKELETON_FRAME;
  TNUI_SKELETON_FRAME = NUI_SKELETON_FRAME;

  NUI_SKELETON_BONE_ROTATION = record
    rotationMatrix : Matrix4;
    rotationQuaternion : Vector4;
  end;
  PNUI_SKELETON_BONE_ROTATION = ^NUI_SKELETON_BONE_ROTATION;
  TNUI_SKELETON_BONE_ROTATION = NUI_SKELETON_BONE_ROTATION;

  NUI_SKELETON_BONE_ORIENTATION = record
    endJoint : NUI_SKELETON_POSITION_INDEX;
    startJoint : NUI_SKELETON_POSITION_INDEX;
    hierarchicalRotation : NUI_SKELETON_BONE_ROTATION;
    absoluteRotation : NUI_SKELETON_BONE_ROTATION;
  end;
  PNUI_SKELETON_BONE_ORIENTATION = ^NUI_SKELETON_BONE_ORIENTATION;
  TNUI_SKELETON_BONE_ORIENTATION = NUI_SKELETON_BONE_ORIENTATION;

  NUI_LOCKED_RECT = record
    Pitch : integer;
    size : integer;
    pBits : pointer;
  end;
  PNUI_LOCKED_RECT = ^NUI_LOCKED_RECT;
  TNUI_LOCKED_RECT = NUI_LOCKED_RECT;

  INuiAudioBeam = interface(IUnknown)['{8c3cebfa-a35d-497e-bc9a-e9752a8155e0}']
    Function GetBeam(out angle : double) : HRESULT; stdcall;
    Function SetBeam(angle : double) : HRESULT; stdcall;
    Function GetPosition(out angle, confidence : double) : HRESULT; stdcall;
  end;

  INuiFrameTexture = interface(IUnknown)['{13ea17f5-ff2e-4670-9ee5-1297a6e880d1}']
    Function BufferLen : integer; stdcall;
    Function Pitch : integer; stdcall;
    Function LockRect(Level : UINT; pLockedRect : PNUI_LOCKED_RECT; pRect : PRECT; Flags : DWORD ) : HRESULT; stdcall;
    Function GetLevelDesc(Level : UINT; out desc : NUI_SURFACE_DESC) : HRESULT; stdcall;
    Function UnlockRect(Level: UINT) : HRESULT; stdcall;
  end;

  NUI_IMAGE_FRAME = record
    liTimeStamp : int64;
    dwFrameNumber : DWORD;
    eImageType : NUI_IMAGE_TYPE;
    eResolution : NUI_IMAGE_RESOLUTION;
    pFrameTexture : INuiFrameTexture;
    dwFrameFlags : DWORD;
    ViewArea : NUI_IMAGE_VIEW_AREA;
  end;
  PNUI_IMAGE_FRAME = ^NUI_IMAGE_FRAME;
  TNUI_IMAGE_FRAME = NUI_IMAGE_FRAME;

  NUI_MICROPHONE_ARRAY_DEVICE = record
    szDeviceName : array[0..MAX_DEV_STR_LEN - 1] of WideChar;
    szDeviceID : array[0..MAX_DEV_STR_LEN - 1] of WideChar;
    iDeviceIndex : integer;
  end;
  PNUI_MICROPHONE_ARRAY_DEVICE = ^NUI_MICROPHONE_ARRAY_DEVICE;
  TNUI_MICROPHONE_ARRAY_DEVICE = NUI_MICROPHONE_ARRAY_DEVICE;

  TNuiGetMicrophoneArrayDevices = Function(param1: PNUI_MICROPHONE_ARRAY_DEVICE; param2 : integer; out param3: integer) : HRESULT; stdcall;

  NUI_SPEAKER_DEVICE = record
    szDeviceName : array[0..MAX_DEV_STR_LEN - 1] of widechar;
    iDeviceIndex : integer;
    fDefault : boolean;
  end;
  PNUI_SPEAKER_DEVICE = ^NUI_SPEAKER_DEVICE;
  TNUI_SPEAKER_DEVICE = NUI_SPEAKER_DEVICE;

  INuiSensor = interface(IUnknown)['{1f5e088c-a8c7-41d3-9957-209677a13e85}']
    Function NuiInitialize(dwFlags : DWORD) : HRESULT; stdcall;
    Procedure NuiShutdown; stdcall;
    Function NuiSetFrameEndEvent(hEvent : THandle; dwFrameEventFlag : DWORD) : HRESULT; stdcall;
    Function NuiImageStreamOpen(eImageType : NUI_IMAGE_TYPE; eResolution : NUI_IMAGE_RESOLUTION; dwImageFrameFlags : DWORD; dwFrameLimit : DWORD; hNextFrameEvent : THandle; out phStreamHandle : THandle) : HRESULT; stdcall;
    Function NuiImageStreamSetImageFrameFlags(hStream : THandle; dwImageFrameFlags : DWORD) : HRESULT; stdcall;
    Function NuiImageStreamGetImageFrameFlags(hStream : THandle; pdwImageFrameFlags : PDWORD) : HRESULT; stdcall;
    Function NuiImageStreamGetNextFrame(hStream : THandle; dwMillisecondsToWait : DWORD; pImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;
    Function NuiImageStreamReleaseFrame(hStream : THandle; pImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;
    Function NuiImageGetColorPixelCoordinatesFromDepthPixel(eColorResolution : NUI_IMAGE_RESOLUTION; const pcViewArea : PNUI_IMAGE_VIEW_AREA; lDepthX : longint;lDepthY : longint; usDepthValue : word; plColorX : PLongint; plColorY : PLongint) : HRESULT; stdcall;
    Function NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution(eColorResolution : NUI_IMAGE_RESOLUTION;eDepthResolution : NUI_IMAGE_RESOLUTION; const pcViewArea : PNUI_IMAGE_VIEW_AREA; lDepthX : longint; lDepthY : longint; usDepthValue : word; plColorX : PLongint; plColorY : PLongint) : HRESULT; stdcall;
    Function NuiImageGetColorPixelCoordinateFrameFromDepthPixelFrameAtResolution(eColorResolution : NUI_IMAGE_RESOLUTION; eDepthResolution : NUI_IMAGE_RESOLUTION; cDepthValues : DWORD; pDepthValues : PWORD; cColorCoordinates : DWORD; pColorCoordinates : PLongint) : HRESULT; stdcall;
    Function NuiCameraElevationSetAngle(lAngleDegrees : longint) : HRESULT; stdcall;
    Function NuiCameraElevationGetAngle(plAngleDegrees : PLongint) : HRESULT; stdcall;
    Function NuiSkeletonTrackingEnable(hNextFrameEvent : THandle; dwFlags : DWORD ) : HRESULT; stdcall;
    Function NuiSkeletonTrackingDisable : HRESULT; stdcall;
    Function NuiSkeletonSetTrackedSkeletons(TrackingIDs : PDWORD) : HRESULT; stdcall;
    Function NuiSkeletonGetNextFrame(dwMillisecondsToWait : DWORD;pSkeletonFrame : PNUI_SKELETON_FRAME) : HRESULT; stdcall;
    Function NuiTransformSmooth(pSkeletonFrame : PNUI_SKELETON_FRAME; const pSmoothingParams : PNUI_TRANSFORM_SMOOTH_PARAMETERS) : HRESULT; stdcall;
    Function NuiGetAudioSource(out ppDmo : INuiAudioBeam) : HRESULT; stdcall;
    Function NuiInstanceIndex : integer; stdcall;
    Function NuiDeviceConnectionId : PWideString; stdcall;
    Function NuiUniqueId : PWideString; stdcall;
    Function NuiAudioArrayId : PWideString; stdcall;
    Function NuiStatus : HRESULT; stdcall;
    Function NuiInitializationFlags : DWORD; stdcall;
  end;

  IID_INuiSensor = INuiSensor;
  IID_INuiFrameTexture = INuiFrameTexture;
  IID_INuiAudioBeam = INuiAudioBeam;

  TNuiGetSensorCount = Function (out count : integer) : HRESULT; stdcall;
  TNuiCreateSensorByIndex = Function (index : integer; out ppNuiSensor : INuiSensor) : HRESULT; stdcall;
  TNuiCreateSensorById = Function (const strInstanceId : PWideString; out ppNuiSensor : INuiSensor) : HRESULT; stdcall;
  TNuiGetAudioSource = Function (out ppDmo : INuiAudioBeam) : HRESULT; stdcall;
  TNuiStatusProc = Procedure (hrStatus : HRESULT; const instanceName : PWideString; const uniqueDeviceName : PWideString; pUserData : pointer); stdcall;
  TNuiSetDeviceStatusCallback = Procedure (callback : TNuiStatusProc; pUserData : pointer); stdcall;
  TNuiGetSpeakerDevices = Function (pDeviceInfo : PNUI_SPEAKER_DEVICE; size : integer; out piDeviceCount : integer) : HRESULT; stdcall;

  Procedure NuiImageResolutionToSize(res : NUI_IMAGE_RESOLUTION;out refWidth, refHeight : DWORD);


  Function NuiDepthPixelToDepth(packedPixel: WORD) : WORD;
  Function NuiDepthPixelToPlayerIndex(packedPixel : WORD) : WORD;

Type
  NUI_IMAGE_DIGITALZOOM = (NUI_IMAGE_DIGITAL_ZOOM_1X = 0);
  TNUI_IMAGE_DIGITALZOOM = NUI_IMAGE_DIGITALZOOM;

  TNuiImageStreamSetImageFrameFlags = Function (hStream : THANDLE; dwImageFrameFlags : DWORD) : HRESULT; stdcall;
  TNuiImageStreamGetImageFrameFlags = Function (hStream : THANDLE; pdwImageFrameFlags : PDWORD) : HRESULT; stdcall;
  TNuiSetFrameEndEvent = Function (hEvent : THANDLE; dwFrameEventFlag : DWORD) : HRESULT; stdcall;
  TNuiImageStreamOpen = Function (eImageType : NUI_IMAGE_TYPE; eResolution : NUI_IMAGE_RESOLUTION; dwImageFrameFlags : DWORD; dwFrameLimit : DWORD; hNextFrameEvent : THANDLE; out phStreamHandle : THANDLE) : HRESULT; stdcall;
  TNuiImageStreamGetNextFrame = Function (hStream : THANDLE; dwMillisecondsToWait : DWORD;out ppcImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;
  TNuiImageStreamReleaseFrame = Function (hStream : THANDLE; CONST pImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;
  TNuiImageGetColorPixelCoordinatesFromDepthPixel = Function (eColorResolution : NUI_IMAGE_RESOLUTION;CONST pcViewArea : PNUI_IMAGE_VIEW_AREA;lDepthX : LONGINT;lDepthY : LONGINT; usDepthValue : WORD; out plColorX : LONGINT; out plColorY : LONGINT) : HRESULT; stdcall;
  TNuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution = Function (eColorResolution : NUI_IMAGE_RESOLUTION;eDepthResolution : NUI_IMAGE_RESOLUTION;CONST pcViewArea : PNUI_IMAGE_VIEW_AREA;lDepthX : LONGINT; lDepthY : LONGINT; usDepthValue : WORD; out plColorX : LONGINT; out plColorY : LONGINT) : HRESULT; stdcall;
  TNuiCameraElevationGetAngle = Function (out plAngleDegrees : LONGINT) : HRESULT; stdcall;
  TNuiCameraElevationSetAngle = Function (lAngleDegrees : LONGINT) : HRESULT; stdcall;

  TTrackingIDsArray = array[0..NUI_SKELETON_MAX_TRACKED_COUNT-1] of DWORD;
  
  TNuiSkeletonTrackingEnable = Function (hNextFrameEvent : THandle;dwFlags : DWORD) : HRESULT; stdcall;
  TNuiSkeletonTrackingDisable = Function : HRESULT; stdcall;
  TNuiSkeletonGetNextFrame = Function (dwMillisecondsToWait : DWORD; pSkeletonFrame : PNUI_SKELETON_FRAME) : HRESULT; stdcall;
  TNuiSkeletonSetTrackedSkeletons = Function (TrackingIDs : TTrackingIDsArray) : HRESULT; stdcall;
  TNuiTransformSmooth = Function (pSkeletonFrame : PNUI_SKELETON_FRAME;CONST pSmoothingParams : PNUI_TRANSFORM_SMOOTH_PARAMETERS) : HRESULT; stdcall;
  TNuiSkeletonCalculateBoneOrientations = Function (const pSkeletonData : PNUI_SKELETON_DATA; pBoneOrientations : PNUI_SKELETON_BONE_ORIENTATION) : Integer; stdcall;

// Converted macros

Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out lDepthX : Integer;out lDepthY : Integer; out usDepthValue : word; eResolution : NUI_IMAGE_RESOLUTION); inline;overload;
Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out lDepthX : Integer;out lDepthY : Integer; out usDepthValue : word); inline;overload;
Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out fDepthX : single;out fDepthY : single; eResolution : NUI_IMAGE_RESOLUTION); inline;overload;
Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out fDepthX : single;out fDepthY : single); inline;overload;
Function NuiTransformDepthImageToSkeleton(lDepthX : Integer;lDepthY : Integer; usDepthValue : word; eResolution : NUI_IMAGE_RESOLUTION) : TVector4; inline;overload;
Function NuiTransformDepthImageToSkeleton(lDepthX, lDepthY : Integer; usDepthValue : word) : TVector4; inline;overload;
Function HasSkeletalEngine(pNuiSensor : INuiSensor) : boolean;

type
  TNuiInitialize = Function (dwFlags : DWORD) : HRESULT; stdcall;
  TNuiShutdown = Procedure; stdcall;

var
  NuiInitialize : TNuiInitialize = nil;
  NuiShutdown : TNuiShutdown = nil;
  NuiGetSensorCount : TNuiGetSensorCount = nil;
  NuiCreateSensorByIndex : TNuiCreateSensorByIndex = nil;
  NuiCreateSensorById : TNuiCreateSensorById = nil;
  NuiGetAudioSource : TNuiGetAudioSource = nil;
  NuiSetDeviceStatusCallback : TNuiSetDeviceStatusCallback = nil;
  NuiSkeletonTrackingEnable : TNuiSkeletonTrackingEnable = nil;
  NuiSkeletonTrackingDisable : TNuiSkeletonTrackingDisable = nil;
  NuiSkeletonGetNextFrame : TNuiSkeletonGetNextFrame = nil;
  NuiSkeletonSetTrackedSkeletons : TNuiSkeletonSetTrackedSkeletons = nil;
  NuiTransformSmooth : TNuiTransformSmooth = nil;
  NuiSkeletonCalculateBoneOrientations : TNuiSkeletonCalculateBoneOrientations = nil;
  NuiImageStreamSetImageFrameFlags : TNuiImageStreamSetImageFrameFlags = nil;
  NuiImageStreamGetImageFrameFlags : TNuiImageStreamGetImageFrameFlags = nil;
  NuiSetFrameEndEvent : TNuiSetFrameEndEvent = nil;
  NuiImageStreamOpen : TNuiImageStreamOpen = nil;
  NuiImageStreamGetNextFrame : TNuiImageStreamGetNextFrame = nil;
  NuiImageStreamReleaseFrame : TNuiImageStreamReleaseFrame = nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixel : TNuiImageGetColorPixelCoordinatesFromDepthPixel = nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution : TNuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution = nil;
  NuiCameraElevationGetAngle : TNuiCameraElevationGetAngle = nil;
  NuiCameraElevationSetAngle : TNuiCameraElevationSetAngle = nil;

Function LoadNuiLibrary(Const Filename : string = LibKinect) : Integer;
Procedure UnloadNuiLibrary;

implementation

uses dynlibs;

var
  LoadedLib : String;
  HLib : TLibHandle = NilHandle;
  ARefCount : Integer = 0;

Function LoadNuiLibrary(Const Filename : string = LibKinect) : Integer;
begin
  If (ARefCount>0) then
    begin
    If (LoadedLib<>FileName) then
      Raise Exception.CreateFmt('NUI Library "%s" is already loaded as "%s"',[FileName,LoadedLib]);
    Inc(ArefCount);
    Exit;  
    end;
  HLib:=LoadLibrary(Filename);
  if HLib=NilHandle then 
    Raise Exception.CreateFmt('Failed to load NUI Library "%s"',[FileName]);
  Pointer(NuiCameraElevationGetAngle):=GetProcAddress(HLib, 'NuiCameraElevationGetAngle');
  Pointer(NuiCameraElevationSetAngle):=GetProcAddress(HLib, 'NuiCameraElevationSetAngle');
  Pointer(NuiCreateSensorById):=GetProcAddress(HLib, 'NuiCreateSensorById');
  Pointer(NuiCreateSensorByIndex):=GetProcAddress(HLib,'NuiCreateSensorByIndex');
  Pointer(NuiGetAudioSource):=GetProcAddress(HLib, 'NuiGetAudioSource');
  Pointer(NuiGetSensorCount):=GetProcAddress(HLib,'NuiGetSensorCount');
  Pointer(NuiImageGetColorPixelCoordinatesFromDepthPixel):=GetProcAddress(HLib, 'NuiImageGetColorPixelCoordinatesFromDepthPixel');
  Pointer(NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution):=GetProcAddress(HLib, 'NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution');
  Pointer(NuiImageStreamGetImageFrameFlags):=GetProcAddress(HLib, 'NuiImageStreamGetImageFrameFlags');
  Pointer(NuiImageStreamGetNextFrame):=GetProcAddress(HLib, 'NuiImageStreamGetNextFrame');
  Pointer(NuiImageStreamOpen):=GetProcAddress(HLib, 'NuiImageStreamOpen');
  Pointer(NuiImageStreamReleaseFrame):=GetProcAddress(HLib, 'NuiImageStreamReleaseFrame');
  Pointer(NuiImageStreamSetImageFrameFlags):=GetProcAddress(HLib, 'NuiImageStreamSetImageFrameFlags');
  Pointer(NuiInitialize):=GetProcAddress(HLib,'NuiInitialize');
  Pointer(NuiSetDeviceStatusCallback):=GetProcAddress(HLib, 'NuiSetDeviceStatusCallback');
  Pointer(NuiSetFrameEndEvent):=GetProcAddress(HLib, 'NuiSetFrameEndEvent');
  Pointer(NuiShutdown):=GetProcAddress(HLib,'NuiShutdown');
  Pointer(NuiSkeletonCalculateBoneOrientations):=GetProcAddress(HLib, 'NuiSkeletonCalculateBoneOrientations');
  Pointer(NuiSkeletonGetNextFrame):=GetProcAddress(HLib, 'NuiSkeletonGetNextFrame');
  Pointer(NuiSkeletonSetTrackedSkeletons):=GetProcAddress(HLib, 'NuiSkeletonSetTrackedSkeletons');
  Pointer(NuiSkeletonTrackingDisable):=GetProcAddress(HLib, 'NuiSkeletonTrackingDisable');
  Pointer(NuiSkeletonTrackingEnable):=GetProcAddress(HLib, 'NuiSkeletonTrackingEnable');
  Pointer(NuiTransformSmooth):=GetProcAddress(HLib, 'NuiTransformSmooth');
  ARefCount:=1;
end;

Procedure UnloadNuiLibrary;
begin
  Dec(ARefcount);
  If ARefCount>0 then 
    Exit;
  NuiCameraElevationGetAngle:=nil;
  NuiCameraElevationSetAngle:=nil;
  NuiCreateSensorById:=nil;
  NuiCreateSensorByIndex:=nil;
  NuiGetAudioSource:=nil;
  NuiGetSensorCount:=nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixel:=nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution:=nil;
  NuiImageStreamGetImageFrameFlags:=nil;
  NuiImageStreamGetNextFrame:=nil;
  NuiImageStreamOpen:=nil;
  NuiImageStreamReleaseFrame:=nil;
  NuiImageStreamSetImageFrameFlags:=nil;
  NuiInitialize:=nil;
  NuiSetDeviceStatusCallback:=nil;
  NuiSetFrameEndEvent:=nil;
  NuiShutdown:=nil;
  NuiSkeletonCalculateBoneOrientations:=nil;
  NuiSkeletonGetNextFrame:=nil;
  NuiSkeletonSetTrackedSkeletons:=nil;
  NuiSkeletonTrackingDisable:=nil;
  NuiSkeletonTrackingEnable:=nil;
  NuiTransformSmooth:=nil;
  FreeLibrary(HLib);
  HLib:=NilHandle;
end;

Procedure NuiImageResolutionToSize(res : NUI_IMAGE_RESOLUTION; out refWidth, refHeight : DWORD);
begin
  case res of
  NUI_IMAGE_RESOLUTION_80x60: 
    begin
    refWidth := 80;
    refHeight := 60;
    end;
  NUI_IMAGE_RESOLUTION_320x240: 
    begin
    refWidth := 320;
    refHeight := 240;
    end;
  NUI_IMAGE_RESOLUTION_640x480: 
    begin
    refWidth := 640;
    refHeight := 480;
    end;
  NUI_IMAGE_RESOLUTION_1280x960: 
    begin
    refWidth := 1280;
    refHeight := 960;
    end;
  else
    refWidth := 0;
    refHeight := 0;
  end;
end;

Function NuiDepthPixelToDepth(packedPixel: WORD) : WORD;
begin
  Result:=packedPixel shr NUI_IMAGE_PLAYER_INDEX_SHIFT;
end;

Function NuiDepthPixelToPlayerIndex(packedPixel : WORD) : WORD;
begin
  Result:=packedPixel and NUI_IMAGE_PLAYER_INDEX_MASK;
end;

Function HasSkeletalEngine(pNuiSensor : INuiSensor) : boolean;

begin
  if Not assigned(pNuiSensor) then 
    Result:=False
  else
    Result := ((pNuiSensor.NuiInitializationFlags() and NUI_INITIALIZE_FLAG_USES_SKELETON)<>0) or
              ((pNuiSensor.NuiInitializationFlags() and NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX)<>0);
end;

Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out lDepthX : Integer;out lDepthY : Integer;out usDepthValue : word; eResolution : NUI_IMAGE_RESOLUTION);inline;

var
  w, h : DWORD;
  
begin
  if (vPoint.z>FLT_EPSILON) then 
    begin
    NuiImageResolutionToSize(eResolution,w,h);
    lDepthX:=round(w/2+vPoint.x*(w/320.0)*NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240/vPoint.z+0.5);
    lDepthY:=round(h/2-vPoint.y*(h/240.0)*NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240/vPoint.z+0.5);
    usDepthValue := round(vPoint.z *1000) shl 3;
    end 
  else 
    begin
    lDepthX := 0;
    lDepthY := 0;
    usDepthValue := 0;
    end;
end;

Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out lDepthX : Integer; out lDepthY : Integer;out usDepthValue : word); inline;
begin
  NuiTransformSkeletonToDepthImage(vPoint,lDepthX,lDepthY,usDepthValue,NUI_IMAGE_RESOLUTION_320x240);
end;

Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out fDepthX : single;out fDepthY : single; eResolution : NUI_IMAGE_RESOLUTION); inline;

var
  w, h : DWORD;
  
begin
  if (vPoint.z>FLT_EPSILON) then
    begin
    NuiImageResolutionToSize(eResolution,w,h);
    fDepthX:=w/2+vPoint.x*(w/320.0)*NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240/vPoint.z;
    fDepthY:=h/2-vPoint.y*(h/240.0)*NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240/vPoint.z;
    end 
  else 
    begin
    fDepthX:=0.0;
    fDepthY:=0.0;
    end;
end;

Procedure NuiTransformSkeletonToDepthImage(vPoint : TVector4;out fDepthX : single;out fDepthY : single);
begin
  NuiTransformSkeletonToDepthImage(vPoint, fDepthX, fDepthY,NUI_IMAGE_RESOLUTION_320x240);
end;

Function NuiTransformDepthImageToSkeleton(lDepthX : Integer;lDepthY : Integer; usDepthValue : word;eResolution : NUI_IMAGE_RESOLUTION) : TVector4;  inline;

var
  w,h : DWORD;
  Z : single;
  
begin
  NuiImageResolutionToSize(eResolution,w,h);
  Z:=(usDepthValue shr 3)/1000.0;
  result.Z:=Z;
  result.X:=(lDepthX-w/2.0)*(320.0/w)*NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240*Z;
  result.Y:=-(lDepthY-h/2.0)*(240.0/h)*NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240*Z;
  result.W:=1.0;
end;

Function NuiTransformDepthImageToSkeleton(lDepthX,lDepthY : Integer;usDepthValue : word) : TVector4;  inline;
begin
  result:=NuiTransformDepthImageToSkeleton(lDepthX,lDepthY,usDepthValue,NUI_IMAGE_RESOLUTION_320x240);
end;

end.
