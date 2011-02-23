{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}

const
  CONF_EBADFILE = - $6001;
  CONF_ENOENT = - $6002;
  CONF_ETOOBIG = - $6003;
  CONF_ENOTINIT = - $6004;
  CONF_ENOTIMPL = - $6005;
  CONF_EBADVALUE = - $6006;
  CONF_ENOMEM = - $6007;
  CONF_ERR_OK = 0;

  CONF_BIGARRAY = 1;
  CONF_SMALLARRAY = 2;
  CONF_BYTE = 3;
  CONF_SHORT = 4;
  CONF_LONG = 5;
  CONF_BOOL = 7;

  CONF_VIDEO_NTSC = 0;
  CONF_VIDEO_PAL = 1;
  CONF_VIDEO_MPAL = 2;

  CONF_REGION_JP = 0;
  CONF_REGION_US = 1;
  CONF_REGION_EU = 2;
  CONF_REGION_KR = 4;
  CONF_REGION_CN = 5;

  CONF_AREA_JPN = 0;
  CONF_AREA_USA = 1;
  CONF_AREA_EUR = 2;
  CONF_AREA_AUS = 3;
  CONF_AREA_BRA = 4;
  CONF_AREA_TWN = 5;
  CONF_AREA_ROC = 6;
  CONF_AREA_KOR = 7;
  CONF_AREA_HKG = 8;
  CONF_AREA_ASI = 9;
  CONF_AREA_LTN = 10;
  CONF_AREA_SAF = 11;
  CONF_AREA_CHN = 12;

  CONF_SHUTDOWN_STANDBY = 0;
  CONF_SHUTDOWN_IDLE = 1;

  CONF_LED_OFF = 0;
  CONF_LED_DIM = 1;
  CONF_LED_BRIGHT = 2;

  CONF_SOUND_MONO = 0;
  CONF_SOUND_STEREO = 1;
  CONF_SOUND_SURROUND = 2;

  CONF_LANG_JAPANESE = 0;
  CONF_LANG_ENGLISH = 1;
  CONF_LANG_GERMAN = 2;
  CONF_LANG_FRENCH = 3;
  CONF_LANG_SPANISH = 4;
  CONF_LANG_ITALIAN = 5;
  CONF_LANG_DUTCH = 6;
  CONF_LANG_SIMP_CHINESE = 7;
  CONF_LANG_TRAD_CHINESE = 8;
  CONF_LANG_KOREAN = 9;

  CONF_ASPECT_4_3 = 0;
  CONF_ASPECT_16_9 = 1;

  CONF_SENSORBAR_BOTTOM = 0;
  CONF_SENSORBAR_TOP = 1;

  CONF_PAD_MAX_REGISTERED = 10;

  CONF_PAD_MAX_ACTIVE = 4;



type
  _conf_pad_device = packed record
    bdaddr : array [0..5] of cuint8;
    name_ : array [0..63] of cchar;
  end;
  conf_pad_device = _conf_pad_device;
  pconf_pad_device = ^conf_pad_device;


  _conf_pads = packed record
    num_registered : cuint8;
    registered : array [0..CONF_PAD_MAX_REGISTERED-1] of conf_pad_device;
    active : array [0..CONF_PAD_MAX_ACTIVE-1] of conf_pad_device;
    balance_board : conf_pad_device;
    unknown : conf_pad_device;
  end;
  conf_pads = _conf_pads;
  pconf_pads = ^conf_pads;


function CONF_Init: cint32; cdecl; external;

function CONF_GetLength(name_: pcchar): cint32; cdecl; external;

function CONF_GetType(name_: pcchar): cint32; cdecl; external;

function CONF_Get(name_: pcchar; buffer: pointer; length: cuint32): cint32; cdecl; external;

function CONF_GetShutdownMode: cint32; cdecl; external;

function CONF_GetIdleLedMode: cint32; cdecl; external;

function CONF_GetProgressiveScan: cint32; cdecl; external;

function CONF_GetEuRGB60: cint32; cdecl; external;

function CONF_GetIRSensitivity: cint32; cdecl; external;

function CONF_GetSensorBarPosition: cint32; cdecl; external;

function CONF_GetPadSpeakerVolume: cint32; cdecl; external;

function CONF_GetPadMotorMode: cint32; cdecl; external;

function CONF_GetSoundMode: cint32; cdecl; external;

function CONF_GetLanguage: cint32; cdecl; external;

function CONF_GetCounterBias(bias: pcuint32): cint32; cdecl; external;

function CONF_GetScreenSaverMode: cint32; cdecl; external;

function CONF_GetDisplayOffsetH(offset: pcint8): cint32; cdecl; external;

function CONF_GetPadDevices(pads: Pconf_pads): cint32; cdecl; external;

function CONF_GetNickName(nickname: pcuint8): cint32; cdecl; external;

function CONF_GetAspectRatio: cint32; cdecl; external;

function CONF_GetEULA: cint32; cdecl; external;

function CONF_GetParentalPassword(password: pcint8): cint32; cdecl; external;

function CONF_GetParentalAnswer(answer: pcint8): cint32; cdecl; external;

function CONF_GetWiiConnect24: cint32; cdecl; external;

function CONF_GetRegion: cint32; cdecl; external;

function CONF_GetArea: cint32; cdecl; external;

function CONF_GetVideo: cint32; cdecl; external;


{$endif HV_RVL}

{$ENDIF}


