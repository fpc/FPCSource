{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Karoly Balogh for Genesi S.a.r.l.

    ahi.library interface unit for MorphOS/PowerPC

    Based on work of Nils Sjoholm member of the Amiga RTL
    development team.

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$PACKRECORDS 2}
unit AHI;

interface

uses exec, utility;

var
  AHIBase: PLibrary;

  type

     PFixed = ^Fixed;
     Fixed = LONGINT;
  { A fixed-point value, 16 bits
    to the left of the point and
    16 bits to the right  }

  type

     Psposition = ^sposition;
     sposition = Fixed;

  { AHIAudioCtrl  }
  { Lots of private data follows!  }
     PAHIAudioCtrl = ^tAHIAudioCtrl;
     tAHIAudioCtrl = record
          ahiac_UserData : Pointer;
       end;

  { AHISoundMessage  }
     PAHISoundMessage = ^tAHISoundMessage;
     tAHISoundMessage = record
          ahism_Channel : Word;
       end;

  { AHIRecordMessage  }
     PAHIRecordMessage = ^tAHIRecordMessage;
     tAHIRecordMessage = record
          ahirm_Type : Cardinal;     { Format of buffer (object)  }
          ahirm_Buffer : Pointer;    { Pointer to the sample array  }
          ahirm_Length : Cardinal;   { Number of sample frames in buffer  }
       end;

  { AHISampleInfo  }
     PAHISampleInfo = ^tAHISampleInfo;
     tAHISampleInfo = record
          ahisi_Type : Cardinal;      { Format of samples  }
          ahisi_Address : Pointer;    { Address to array of samples  }
          ahisi_Length : Cardinal;    { Number of samples in array  }
       end;

  { AHIAudioModeRequester  }
     PAHIAudioModeRequester = ^tAHIAudioModeRequester;
     tAHIAudioModeRequester = record
          ahiam_AudioID : Cardinal;      { Selected audio mode  }
          ahiam_MixFreq : Cardinal;      { Selected mixing/sampling frequency  }
          ahiam_LeftEdge : WORD;      { Coordinates of requester on exit  }
          ahiam_TopEdge : WORD;
          ahiam_Width : WORD;
          ahiam_Height : WORD;
          ahiam_InfoOpened : Boolean;    { Info window opened on exit?  }
          ahiam_InfoLeftEdge : WORD;  { Last coordinates of Info window  }
          ahiam_InfoTopEdge : WORD;
          ahiam_InfoWidth : WORD;
          ahiam_InfoHeight : WORD;
          ahiam_UserData : Pointer;      { You can store your own data here  }
          { Lots of private data follows!  }
       end;

  { AHIEffMasterVolume  }
     PAHIEffMasterVolume = ^tAHIEffMasterVolume;
     tAHIEffMasterVolume = record
          ahie_Effect : Cardinal;     { Set to AHIET_MASTERVOLUME  }
          ahiemv_Volume : Fixed;   { See autodocs for range!  }
       end;

  { AHIEffOutputBuffer  }
     PAHIEffOutputBuffer = ^tAHIEffOutputBuffer;
     tAHIEffOutputBuffer = record
          ahie_Effect : Cardinal;     { Set to AHIET_OUTPUTBUFFER  }
          ahieob_Func : PHook;
          { These fields are filled by AHI  }
          ahieob_Type : Cardinal;     { Format of buffer  }
          ahieob_Buffer : Pointer;    { Pointer to the sample array  }
          ahieob_Length : Cardinal;   { Number of sample frames in buffer  }
       end;

  { AHIEffDSPMask (V4)  }
     PAHIEffDSPMask = ^tAHIEffDSPMask;
     tAHIEffDSPMask = record
          ahie_Effect : Cardinal;       { Set to AHIET_DSPMASK  }
          ahiedm_Channels : Word;   { Number of elements in array  }
          ahiedm_Mask : array[0..0] of Byte;   { Here follows the array  }
       end;


  const
     AHIEDM_WET = 0;
     AHIEDM_DRY = 1;
  { AHIEffDSPEcho (V4)  }
  type
     PAHIDSPEcho = ^tAHIDSPEcho;
     tAHIDSPEcho = record
          ahie_Effect : Cardinal;     { Set to AHIET_DSPECHO  }
          ahiede_Delay : Cardinal;    { In samples  }
          ahiede_Feedback : Fixed;
          ahiede_Mix : Fixed;
          ahiede_Cross : Fixed;
       end;

  { AHIEffChannelInfo (V4)  }
     PAHIEffChannelInfo = ^tAHIEffChannelInfo;
     tAHIEffChannelInfo = record
          ahie_Effect : Cardinal;    { Set to AHIET_CHANNELINFO  }
          ahieci_Func : PHook;
          ahieci_Channels : Word;
          ahieci_Pad : Word;
          { The rest is filled by AHI  }
          ahieci_Offset : array[0..0] of Cardinal;   { The array follows  }
       end;

  {   TAGS  }

  const
     AHI_TagBase = TAG_USER;
     AHI_TagBaseR = AHI_TagBase or $8000;
  { AHI_AllocAudioA tags  }
  { Desired audio mode  }
     AHIA_AudioID = AHI_TagBase + 1;
  { Suggested mixing frequency  }
     AHIA_MixFreq = AHI_TagBase + 2;
  { Suggested number of channels  }
     AHIA_Channels = AHI_TagBase + 3;
  { Number of sounds to use  }
     AHIA_Sounds = AHI_TagBase + 4;
  { End-of-Sound Hook  }
     AHIA_SoundFunc = AHI_TagBase + 5;
  { Player Hook  }
     AHIA_PlayerFunc = AHI_TagBase + 6;
  { Frequency for player Hook (Fixed) }
     AHIA_PlayerFreq = AHI_TagBase + 7;
  { Minimum Frequency for player Hook  }
     AHIA_MinPlayerFreq = AHI_TagBase + 8;
  { Maximum Frequency for player Hook  }
     AHIA_MaxPlayerFreq = AHI_TagBase + 9;
  { Sample recording Hook  }
     AHIA_RecordFunc = AHI_TagBase + 10;
  { What to put in ahiac_UserData  }
     AHIA_UserData = AHI_TagBase + 11;
  { AHI_PlayA tags (V4)  }
  { All command tags should be...  }
     AHIP_BeginChannel = AHI_TagBase + 40;
  { ... enclosed by these tags.  }
     AHIP_EndChannel = AHI_TagBase + 41;
     AHIP_Freq = AHI_TagBase + 50;
     AHIP_Vol = AHI_TagBase + 51;
     AHIP_Pan = AHI_TagBase + 52;
     AHIP_Sound = AHI_TagBase + 53;
     AHIP_Offset = AHI_TagBase + 54;
     AHIP_Length = AHI_TagBase + 55;
     AHIP_LoopFreq = AHI_TagBase + 60;
     AHIP_LoopVol = AHI_TagBase + 61;
     AHIP_LoopPan = AHI_TagBase + 62;
     AHIP_LoopSound = AHI_TagBase + 63;
     AHIP_LoopOffset = AHI_TagBase + 64;
     AHIP_LoopLength = AHI_TagBase + 65;
  { AHI_ControlAudioA tags  }
  { Booleanean  }
     AHIC_Play = AHI_TagBase + 80;
  { Booleanean  }
     AHIC_Record = AHI_TagBase + 81;
     AHIC_MonitorVolume = AHI_TagBase + 82;
  { ti_Data is pointer to Fixed (LONG)  }
     AHIC_MonitorVolume_Query = AHI_TagBase + 83;
  { ti_Data is pointer to Cardinal  }
     AHIC_MixFreq_Query = AHI_TagBase + 84;
  { --- New for V2, they will be ignored by V1 ---  }
     AHIC_InputGain = AHI_TagBase + 85;
  { ti_Data is pointer to Fixed (LONG)  }
     AHIC_InputGain_Query = AHI_TagBase + 86;
     AHIC_OutputVolume = AHI_TagBase + 87;
  { ti_Data is pointer to Fixed (LONG)  }
     AHIC_OutputVolume_Query = AHI_TagBase + 88;
     AHIC_Input = AHI_TagBase + 89;
  { ti_Data is pointer to Cardinal  }
     AHIC_Input_Query = AHI_TagBase + 90;
     AHIC_Output = AHI_TagBase + 91;
  { ti_Data is pointer to Cardinal  }
     AHIC_Output_Query = AHI_TagBase + 92;
  { AHI_GetAudioAttrsA tags  }
     AHIDB_AudioID = AHI_TagBase + 100;
  { Pointer to name of driver  }
     AHIDB_Driver = AHI_TagBaseR + 101;
  { Private!  }
     AHIDB_Flags = AHI_TagBase + 102;
  { Booleanean  }
     AHIDB_Volume = AHI_TagBase + 103;
  { Booleanean  }
     AHIDB_Panning = AHI_TagBase + 104;
  { Booleanean  }
     AHIDB_Stereo = AHI_TagBase + 105;
  { Booleanean  }
     AHIDB_HiFi = AHI_TagBase + 106;
  { Booleanean  }
     AHIDB_PingPong = AHI_TagBase + 107;
  { Private!  }
     AHIDB_MultTable = AHI_TagBase + 108;
  { Pointer to name of this mode  }
     AHIDB_Name = AHI_TagBaseR + 109;
  { Output bits  }
     AHIDB_Bits = AHI_TagBase + 110;
  { Max supported channels  }
     AHIDB_MaxChannels = AHI_TagBase + 111;
  { Min mixing freq. supported  }
     AHIDB_MinMixFreq = AHI_TagBase + 112;
  { Max mixing freq. supported  }
     AHIDB_MaxMixFreq = AHI_TagBase + 113;
  { Booleanean  }
     AHIDB_Record = AHI_TagBase + 114;
     AHIDB_Frequencies = AHI_TagBase + 115;
  { ti_Data is frequency index  }
     AHIDB_FrequencyArg = AHI_TagBase + 116;
     AHIDB_Frequency = AHI_TagBase + 117;
  { Pointer to driver author name  }
     AHIDB_Author = AHI_TagBase + 118;
  { Pointer to driver copyright notice  }
     AHIDB_Copyright = AHI_TagBase + 119;
  { Pointer to driver version string  }
     AHIDB_Version = AHI_TagBase + 120;
  { Pointer to driver annotation text  }
     AHIDB_Annotation = AHI_TagBase + 121;
  { Specifies the string buffer size  }
     AHIDB_BufferLen = AHI_TagBase + 122;
  { ti_Data is frequency!  }
     AHIDB_IndexArg = AHI_TagBase + 123;
     AHIDB_Index = AHI_TagBase + 124;
  { Booleanean  }
     AHIDB_Realtime = AHI_TagBase + 125;
  { It's sample  frames   }
     AHIDB_MaxPlaySamples = AHI_TagBase + 126;
  { It's sample  frames   }
     AHIDB_MaxRecordSamples = AHI_TagBase + 127;
  { Booleanean  }
     AHIDB_FullDuplex = AHI_TagBase + 129;
  { --- New for V2, they will be ignored by V1 ---  }
     AHIDB_MinMonitorVolume = AHI_TagBase + 130;
     AHIDB_MaxMonitorVolume = AHI_TagBase + 131;
     AHIDB_MinInputGain = AHI_TagBase + 132;
     AHIDB_MaxInputGain = AHI_TagBase + 133;
     AHIDB_MinOutputVolume = AHI_TagBase + 134;
     AHIDB_MaxOutputVolume = AHI_TagBase + 135;
     AHIDB_Inputs = AHI_TagBase + 136;
  { ti_Data is input index  }
     AHIDB_InputArg = AHI_TagBase + 137;
     AHIDB_Input = AHI_TagBase + 138;
     AHIDB_Outputs = AHI_TagBase + 139;
  { ti_Data is input index  }
     AHIDB_OutputArg = AHI_TagBase + 140;
     AHIDB_Output = AHI_TagBase + 141;
  { --- New for V4, they will be ignored by V2 and earlier ---  }
  { Private!  }
     AHIDB_Data = AHI_TagBaseR + 142;
  { AHI_BestAudioIDA tags  }
  { --- New for V4, they will be ignored by V2 and earlier ---  }
     AHIB_Dizzy = AHI_TagBase + 190;
  { AHI_AudioRequestA tags  }
  { Window control  }
  { Parent window  }
     AHIR_Window = AHI_TagBase + 200;
  { Screen to open on if no window  }
     AHIR_Screen = AHI_TagBase + 201;
  { Name of public screen  }
     AHIR_PubScreenName = AHI_TagBase + 202;
  { Allocate private IDCMP?  }
     AHIR_PrivateIDCMP = AHI_TagBase + 203;
  { Function to handle IntuiMessages  }
     AHIR_IntuiMsgFunc = AHI_TagBase + 204;
  { Block input in AHIR_Window?  }
     AHIR_SleepWindow = AHI_TagBase + 205;
  { What to put in ahiam_UserData  }
     AHIR_UserData = AHI_TagBase + 206;
  { Text display  }
  { Text font to use for gadget text  }
     AHIR_TextAttr = AHI_TagBase + 220;
  { Locale to use for text  }
     AHIR_Locale = AHI_TagBase + 221;
  { Title of requester  }
     AHIR_TitleText = AHI_TagBase + 222;
  { Positive gadget text  }
     AHIR_PositiveText = AHI_TagBase + 223;
  { Negative gadget text  }
     AHIR_NegativeText = AHI_TagBase + 224;
  { Initial settings  }
  { Initial requester coordinates  }
     AHIR_InitialLeftEdge = AHI_TagBase + 240;
     AHIR_InitialTopEdge = AHI_TagBase + 241;
  { Initial requester dimensions  }
     AHIR_InitialWidth = AHI_TagBase + 242;
     AHIR_InitialHeight = AHI_TagBase + 243;
  { Initial audio mode id  }
     AHIR_InitialAudioID = AHI_TagBase + 244;
  { Initial mixing/sampling frequency  }
     AHIR_InitialMixFreq = AHI_TagBase + 245;
  { Info window initially opened?  }
     AHIR_InitialInfoOpened = AHI_TagBase + 246;
  { Initial Info window coords.  }
     AHIR_InitialInfoLeftEdge = AHI_TagBase + 247;
     AHIR_InitialInfoTopEdge = AHI_TagBase + 248;
  { Not used!  }
     AHIR_InitialInfoWidth = AHI_TagBase + 249;
  { Not used!  }
     AHIR_InitialInfoHeight = AHI_TagBase + 250;
  { Options  }
  { Allow selection of mixing frequency?  }
     AHIR_DoMixFreq = AHI_TagBase + 260;
  { Allow selection of default mode? (V4)  }
     AHIR_DoDefaultMode = AHI_TagBase + 261;
  { Filtering  }
  { Pointer to filter taglist  }
     AHIR_FilterTags = AHI_TagBase + 270;
  { Function to filter mode id's  }
     AHIR_FilterFunc = AHI_TagBase + 271;
  {   DEFS  }
     AHINAME : PChar = 'ahi.device';
  { Invalid Audio ID  }
     AHI_INVALID_ID =  not (0);
  { Only for AHI_AllocAudioA()!  }
     AHI_DEFAULT_ID = $00000000;
  { Special sample render Audio ID  }
     AHI_LOOPBACK_ID = $00000001;
  { Only for AHI_AllocAudioA()!  }
     AHI_DEFAULT_FREQ = 0;
  { Special frequency for AHI_SetFreq()  }
     AHI_MIXFREQ =  not (0);
  { Turns a channel off  }
     AHI_NOSOUND = $ffff;
  { Set#? Flags  }
     AHISF_IMM = 1 shl 0;
     AHISB_IMM = 0;
  { Effect Types  }
  { OR with effect to disable  }
     AHIET_CANCEL = 1 shl 31;
     AHIET_MASTERVOLUME = 1;
     AHIET_OUTPUTBUFFER = 2;
  { --- New for V4 ---  }
     AHIET_DSPMASK = 3;
     AHIET_DSPECHO = 4;
     AHIET_CHANNELINFO = 5;
  { Sound Types  }
  { Private  }
     AHIST_NOTYPE =  not (0);
  { 8 or 16 bit sample  }
     AHIST_SAMPLE = 0;
  { Dynamic sample  }
     AHIST_DYNAMICSAMPLE = 1;
  { The input from your sampler  }
     AHIST_INPUT = 1 shl 29;
  { Private  }
     AHIST_BW = 1 shl 30;
  { Sample types  }
  { Note that only AHIST_M8S, AHIST_S8S, AHIST_M16S and AHIST_S16S
     are supported by AHI_LoadSound().  }
  { Mono, 8 bit signed (BYTE)  }
     AHIST_M8S = 0;
  { Mono, 16 bit signed (WORD)  }
     AHIST_M16S = 1;
  { Stereo, 8 bit signed (2×BYTE)  }
     AHIST_S8S = 2;
  { Stereo, 16 bit signed (2×WORD)  }
     AHIST_S16S = 3;
  { Mono, 32 bit signed (LONG)  }
     AHIST_M32S = 8;
  { Stereo, 32 bit signed (2×LONG)  }
     AHIST_S32S = 10;
  { OBSOLETE!  }
     AHIST_M8U = 4;
  { Error codes  }
  { No error  }
     AHIE_OK = 0;
  { Out of memory  }
     AHIE_NOMEM = 1;
  { Unknown sound type  }
     AHIE_BADSOUNDTYPE = 2;
  { Unknown/unsupported sample type  }
     AHIE_BADSAMPLETYPE = 3;
  { User-triggered abortion  }
     AHIE_ABORTED = 4;
  { Error, but unknown  }
     AHIE_UNKNOWN = 5;
  { CMD_WRITE/CMD_READ failure  }
     AHIE_HALFDUPLEX = 6;
  { DEVICE INTERFACE DEFINITIONS FOLLOWS                                     }
  { Device units  }
     AHI_DEFAULT_UNIT = 0;
     AHI_NO_UNIT = 255;
  { The preference file  }
     ID_AHIU = $41484955;
     ID_AHIG = $41484947;


  type
     PAHIUnitPrefs = ^tAHIUnitPrefs;
     tAHIUnitPrefs = record
          ahiup_Unit : Byte;
          ahiup_Pad : Byte;
          ahiup_Channels : Word;
          ahiup_AudioMode : Cardinal;
          ahiup_Frequency : Cardinal;
          ahiup_MonitorVolume : Fixed;
          ahiup_InputGain : Fixed;
          ahiup_OutputVolume : Fixed;
          ahiup_Input : Cardinal;
          ahiup_Output : Cardinal;
       end;



     PAHIGlobalPrefs = ^tAHIGlobalPrefs;
     tAHIGlobalPrefs = record
          ahigp_DebugLevel : Word;       { Range: 0-3 (for None, Low,}
          ahigp_DisableSurround : Boolean;   { High and All)  }
          ahigp_DisableEcho : Boolean;
          ahigp_FastEcho : Boolean;
          ahigp_MaxCPU : Fixed;
          ahigp_ClipMasterVolume : Boolean;
       end;

  { Debug levels  }

  const
     AHI_DEBUG_NONE = 0;
     AHI_DEBUG_LOW = 1;
     AHI_DEBUG_HIGH = 2;
     AHI_DEBUG_ALL = 3;

  { AHIRequest  }
  type
     PAHIRequest = ^tAHIRequest;
     tAHIRequest = record
          ahir_Std : tIOStdReq;                  { Standard IO request  }
          ahir_Version : Word;                  { Needed version  }
          { --- New for V4, they will be ignored by V2 and earlier ---  }
          ahir_Pad1 : Word;
          ahir_Private : array[0..1] of Cardinal;   { Hands off!  }
          ahir_Type : Cardinal;                     { Sample format  }
          ahir_Frequency : Cardinal;                { Sample/Record frequency  }
          ahir_Volume : Fixed;                   { Sample volume  }
          ahir_Position : Fixed;                 { Stereo position  }
          ahir_Link : PAHIRequest;               { For double buffering  }
       end;

  { Flags for OpenDevice()  }

  const
     AHIDF_NOMODESCAN = 1 shl 0;
     AHIDB_NOMODESCAN = 0;


function AHI_AllocAudioA(tagList : pTagItem location 'a1') : pAHIAudioCtrl;
SysCall AHIBase 042;

procedure AHI_FreeAudio(AudioCtrl : pAHIAudioCtrl location 'a2');
SysCall AHIBase 048;

procedure AHI_KillAudio;
SysCall AHIBase 054;

function AHI_ControlAudioA(AudioCtrl : pAHIAudioCtrl location 'a2'; tagList : pTagItem location 'a1') : Cardinal;
SysCall AHIBase 060;

procedure AHI_SetVol(Channel : Word location 'd0'; Volume : LongInt location 'd1'; Pan : LongInt location 'd2'; AudioCtrl : pAHIAudioCtrl location 'a2'; Flags : Cardinal location 'd3');
SysCall AHIBase 066;

procedure AHI_SetFreq(Channel : Word location 'd0'; Freq : Cardinal location 'd1'; AudioCtrl : pAHIAudioCtrl location 'a2'; Flags : Cardinal location 'd2');
SysCall AHIBase 072;

procedure AHI_SetSound(Channel : Word location 'd0'; Sound : Word location 'd1'; Offset : Cardinal location 'd2'; Length : LongInt location 'd3'; AudioCtrl : pAHIAudioCtrl location 'a2'; Flags : Cardinal location 'd4');
SysCall AHIBase 078;

function AHI_SetEffect(Effect : Pointer location 'a0'; AudioCtrl : pAHIAudioCtrl location 'a2') : Cardinal;
SysCall AHIBase 084;

function AHI_LoadSound(Sound : Word location 'd0'; SType : Cardinal location 'd1'; Info : Pointer location 'a0'; AudioCtrl : pAHIAudioCtrl location 'a2') : Cardinal;
SysCall AHIBase 090;

procedure AHI_UnloadSound(Sound : Word location 'd0'; Audioctrl : pAHIAudioCtrl location 'a2');
SysCall AHIBase 096;

function AHI_NextAudioID(Last_ID : Cardinal location 'd0') : Cardinal;
SysCall AHIBase 102;

function AHI_GetAudioAttrsA(ID : Cardinal location 'd0'; Audioctrl : pAHIAudioCtrl location 'a2'; tagList : pTagItem location 'a1') : BOOLEAN;
SysCall AHIBase 108;

function AHI_BestAudioIDA(tagList : pTagItem location 'a1') : Cardinal;
SysCall AHIBase 114;

function AHI_AllocAudioRequestA(tagList : pTagItem location 'a0') : pAHIAudioModeRequester;
SysCall AHIBase 120;

function AHI_AudioRequestA(Requester : pAHIAudioModeRequester location 'a0'; tagList : pTagItem location 'a1') : BOOLEAN;
SysCall AHIBase 126;

procedure AHI_FreeAudioRequest(Requester : pAHIAudioModeRequester location 'a0');
SysCall AHIBase 132;

procedure AHI_PlayA(Audioctrl : pAHIAudioCtrl location 'a2'; tagList : pTagItem location 'a1');
SysCall AHIBase 138;

function AHI_SampleFrameSize(SampleType : Cardinal location 'd0') : Cardinal;
SysCall AHIBase 144;

function AHI_AddAudioMode(a0arg : pTagItem location 'a0') : Cardinal;
SysCall AHIBase 150;

function AHI_RemoveAudioMode(d0arg : Cardinal location 'd0') : Cardinal;
SysCall AHIBase 156;

function AHI_LoadModeFile(a0arg : PChar location 'a0') : Cardinal;
SysCall AHIBase 162;


{
 Functions and procedures with tags
}
function AHI_AllocAudio(tagList : array of DWord): pAHIAudioCtrl;
function AHI_ControlAudio(AudioCtrl : pAHIAudioCtrl; tagList : array Of DWord) : Cardinal;
function AHI_GetAudioAttrs(ID : CARDINAL; Audioctrl : pAHIAudioCtrl; taglist : array of DWord) : Boolean;
function AHI_BestAudioID(taglist : array of DWord) : Cardinal;
function AHI_AllocAudioRequest(taglist : array of DWord) : pAHIAudioModeRequester;
function AHI_AudioRequest(Requester : pAHIAudioModeRequester; taglist : array of DWord) : Boolean;
procedure AHI_Play(Audioctrl : pAHIAudioCtrl; taglist : array of DWord);

function InitAHILibrary : boolean;


implementation


function AHI_AllocAudio(tagList : array of DWord): pAHIAudioCtrl;
begin
    AHI_AllocAudio:=AHI_AllocAudioA(@taglist);
end;

function AHI_AllocAudioRequest(taglist : array of DWord) : pAHIAudioModeRequester;
begin
    AHI_AllocAudioRequest:=AHI_AllocAudioRequestA(@taglist);
end;

function AHI_AudioRequest(Requester : pAHIAudioModeRequester; taglist : array of DWord) : Boolean;
begin
    AHI_AudioRequest:=AHI_AudioRequestA(Requester,@taglist);
end;

function AHI_BestAudioID(taglist : array of DWord) : longword;
begin
    AHI_BestAudioID:=AHI_BestAudioIDA(@taglist);
end;

function AHI_ControlAudio(AudioCtrl : pAHIAudioCtrl; taglist : array of DWord) : longword;
begin
    AHI_ControlAudio:=AHI_ControlAudioA(AudioCtrl,@taglist);
end;

function AHI_GetAudioAttrs(ID : longword; Audioctrl : pAHIAudioCtrl; taglist : array of DWord) : Boolean;
begin
    AHI_GetAudioAttrs:=AHI_GetAudioAttrsA(ID,Audioctrl,@taglist);
end;

procedure AHI_Play(Audioctrl : pAHIAudioCtrl; taglist : array of DWord);
begin
    AHI_PlayA(Audioctrl,@taglist);
end;


const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

var
  ahi_exit : Pointer;

procedure CloseAHILibrary;
begin
  ExitProc := ahi_exit;
  if AHIBase <> nil then begin
    CloseLibrary(PLibrary(AHIBase));
    AHIBase := nil;
  end;
end;

function InitAHILibrary : boolean;
begin
  AHIBase := nil;
  AHIBase := OpenLibrary(AHINAME,LIBVERSION);
  if AHIBase <> nil then begin
    ahi_exit := ExitProc;
    ExitProc := @CloseAhiLibrary;
    InitAhiLibrary:=True;
  end else begin
    InitAhiLibrary:=False;
  end;
end;

end. (* UNIT AHI *)

