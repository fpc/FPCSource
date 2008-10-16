// -----------------------------------------------------------------------------
//
//  msacm.h     Audio Compression Manager Public Header File
//
// -----------------------------------------------------------------------------

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit msacm;

{$CALLING cdecl}
{$PACKRECORDS 1} // #pragma pack(1)         /* Assume byte packing throughout */

interface

uses Windows, mmreg;

//
//  there are four types of 'handles' used by the ACM. the first three
//  are unique types that define specific objects:
//
//  HACMDRIVERID: used to _identify_ an ACM driver. this identifier can be
//  used to _open_ the driver for querying details, etc about the driver.
//
//  HACMDRIVER: used to manage a driver (codec, filter, etc). this handle
//  is much like a handle to other media drivers--you use it to send
//  messages to the converter, query for capabilities, etc.
//
//  HACMSTREAM: used to manage a 'stream' (conversion channel) with the
//  ACM. you use a stream handle to convert data from one format/type
//  to another--much like dealing with a file handle.
//
//
//  the fourth handle type is a generic type used on ACM functions that
//  can accept two or more of the above handle types (for example the
//  acmMetrics and acmDriverID functions).
//
//  HACMOBJ: used to identify ACM objects. this handle is used on functions
//  that can accept two or more ACM handle types.
//
type
     HACMDRIVERID = HANDLE;
     PHACMDRIVERID = ^HACMDRIVERID;
     LPHACMDRIVERID = ^HACMDRIVERID;

     HACMDRIVER = HANDLE;
     PHACMDRIVER = ^HACMDRIVER;
     LPHACMDRIVER = ^HACMDRIVER;

     HACMSTREAM = HANDLE;
     PHACMSTREAM = ^HACMSTREAM;
     LPHACMSTREAM = ^HACMSTREAM;

     HACMOBJ = HANDLE;
     PHACMOBJ = ^HACMOBJ;
     LPHACMOBJ = ^HACMOBJ;

     
// - Module: msacmdrv.h
// -----------------------------------------------------------------------------
//
//  Audio Compression Manager Public Header File for Drivers
//
// -----------------------------------------------------------------------------

const
// Driver messages
      DRV_LOAD                = $0001;
      DRV_ENABLE              = $0002;
      DRV_OPEN                = $0003;
      DRV_CLOSE               = $0004;
      DRV_DISABLE             = $0005;
      DRV_FREE                = $0006;
      DRV_CONFIGURE           = $0007;
      DRV_QUERYCONFIGURE      = $0008;
      DRV_INSTALL             = $0009;
      DRV_REMOVE              = $000A;
      DRV_EXITSESSION         = $000B;
      DRV_POWER               = $000F;
      DRV_RESERVED            = $0800;
      DRV_USER                = $4000;

type
     DRVCONFIGINFOEX = record
       dwDCISize:DWORD;
       lpszDCISectionName:LPCWSTR;
       lpszDCIAliasName:LPCWSTR;
       dnDevNode:DWORD;
     end;
     PDRVCONFIGINFOEX = ^DRVCONFIGINFOEX;
     NPDRVCONFIGINFOEX = ^DRVCONFIGINFOEX;
     LPDRVCONFIGINFOEX = ^DRVCONFIGINFOEX;

// LPARAM of DRV_CONFIGURE message
type
     tagDRVCONFIGINFO  = record
       dwDCISize:DWORD;
       lpszDCISectionName:LPCWSTR;
       lpszDCIAliasName:LPCWSTR;
     end;
     DRVCONFIGINFO = tagDRVCONFIGINFO;
     PDRVCONFIGINFO = ^tagDRVCONFIGINFO;
     NPDRVCONFIGINFO = ^tagDRVCONFIGINFO;
     LPDRVCONFIGINFO = ^tagDRVCONFIGINFO;

// Supported return values for DRV_CONFIGURE message
const
      DRVCNF_CANCEL           = $0000;
      DRVCNF_OK               = $0001;
      DRVCNF_RESTART          = $0002;

// return values from DriverProc() function
const
      DRV_CANCEL             = DRVCNF_CANCEL;
      DRV_OK                 = DRVCNF_OK;
      DRV_RESTART            = DRVCNF_RESTART;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  ACM Driver Version:
//
//  the version is a 32 bit number that is broken into three parts as
//  follows:
//
//      bits 24 - 31:   8 bit _major_ version number
//      bits 16 - 23:   8 bit _minor_ version number
//      bits  0 - 15:   16 bit build number
//
//  this is then displayed as follows:
//
//      bMajor = (BYTE)(dwVersion >> 24)
//      bMinor = (BYTE)(dwVersion >> 16) &
//      wBuild = LOWORD(dwVersion)
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function MAKE_ACM_VERSION(mjr:byte; mnr:byte; bld:word):DWORD;

{$DEFINE ACMDRVOPENDESC_SECTIONNAME_CHARS}

type
     tACMDRVOPENDESC = record
       cbStruct:DWORD;      // sizeof(ACMDRVOPENDESC)
       fccType:FOURCC;      // 'audc'
       fccComp:FOURCC;        // sub-type (not used--must be 0)
       dwVersion:DWORD;      // current version of ACM opening you
       dwFlags:DWORD;        //
       dwError:DWORD;        // result from DRV_OPEN request
       pszSectionName:LPCTSTR; // see DRVCONFIGINFO.lpszDCISectionName
       pszAliasName:LPCTSTR;   // see DRVCONFIGINFO.lpszDCIAliasName
       dnDevNode:DWORD;      // devnode id for pnp drivers.
     end;
     ACMDRVOPENDESC = tACMDRVOPENDESC;
     PACMDRVOPENDESC = ^tACMDRVOPENDESC;
     LPACMDRVOPENDESC = ^tACMDRVOPENDESC;

type
     tACMDRVSTREAMINSTANCE = record
       cbStruct:DWORD;
       pwfxSrc:LPWAVEFORMATEX;
       pwfxDst:LPWAVEFORMATEX;
       pwfltr:LPWAVEFILTER;
       dwCallback:DWORD;
       dwInstance:DWORD;
       fdwOpen:DWORD;
       fdwDriver:DWORD;
       dwDriver:DWORD;
       has:HACMSTREAM;
     end;
     ACMDRVSTREAMINSTANCE = tACMDRVSTREAMINSTANCE;
     PACMDRVSTREAMINSTANCE = ^tACMDRVSTREAMINSTANCE;
     LPACMDRVSTREAMINSTANCE = ^tACMDRVSTREAMINSTANCE;

//
//  NOTE! this structure must match the ACMSTREAMHEADER in msacm.h but
//  defines more information for the driver writing convenience
//
type
     LPACMDRVSTREAMHEADER = ^tACMDRVSTREAMHEADER;
     tACMDRVSTREAMHEADER = record
       cbStruct:DWORD;
       fdwStatus:DWORD;
       dwUser:DWORD;
       pbSrc:LPBYTE;
       cbSrcLength:DWORD;
       cbSrcLengthUsed:DWORD;
       dwSrcUser:DWORD;
       pbDst:LPBYTE;
       cbDstLength:DWORD;
       cbDstLengthUsed:DWORD;
       dwDstUser:DWORD;
       fdwConvert:DWORD;     // flags passed from convert func
       padshNext:LPACMDRVSTREAMHEADER;      // for async driver queueing
       fdwDriver:DWORD;      // driver instance flags
       dwDriver:DWORD;       // driver instance data

      //
      //  all remaining fields are used by the ACM for bookkeeping purposes.
      //  an ACM driver should never use these fields (though than can be
      //  helpful for debugging)--note that the meaning of these fields
      //  may change, so do NOT rely on them in shipping code.
      //
       fdwPrepared:DWORD;
       dwPrepared:DWORD;
       pbPreparedSrc:LPBYTE;
       cbPreparedSrcLength:DWORD;
       pbPreparedDst:LPBYTE;
       cbPreparedDstLength:DWORD;
     end;
     ACMDRVSTREAMHEADER = tACMDRVSTREAMHEADER;
     PACMDRVSTREAMHEADER = ^tACMDRVSTREAMHEADER;

//
//  structure for ACMDM_STREAM_SIZE message
//
type
     tACMDRVSTREAMSIZE = record
       cbStruct:DWORD;
       fdwSize:DWORD;
       cbSrcLength:DWORD;
       cbDstLength:DWORD;
     end;
     ACMDRVSTREAMSIZE = tACMDRVSTREAMSIZE;
     PACMDRVSTREAMSIZE = ^tACMDRVSTREAMSIZE;
     LPACMDRVSTREAMSIZE = ^tACMDRVSTREAMSIZE;

//
//  structure containing the information for the ACMDM_FORMAT_SUGGEST message
//
type
     tACMDRVFORMATSUGGEST = record
       cbStruct:DWORD; // sizeof(ACMDRVFORMATSUGGEST)
       fdwSuggest:DWORD;         // Suggest flags
       pwfxSrc:LPWAVEFORMATEX;            // Source Format
       cbwfxSrc:DWORD;           // Source Size
       pwfxDst:LPWAVEFORMATEX;            // Dest format
       cbwfxDst:DWORD;           // Dest Size
     end;
     ACMDRVFORMATSUGGEST = tACMDRVFORMATSUGGEST;
     PACMDRVFORMATSUGGEST = ^tACMDRVFORMATSUGGEST;
     LPACMDRVFORMATSUGGEST = ^tACMDRVFORMATSUGGEST;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  ACM Driver Messages
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMDM_USER                  = DRV_USER + $0000;
      ACMDM_RESERVED_LOW          = DRV_USER + $2000;
      ACMDM_RESERVED_HIGH         = DRV_USER + $2FFF;

      ACMDM_BASE                  = ACMDM_RESERVED_LOW;

      ACMDM_DRIVER_ABOUT          = ACMDM_BASE + 11;

const
      ACMDM_DRIVER_NOTIFY             = ACMDM_BASE + 1;
      ACMDM_DRIVER_DETAILS            = ACMDM_BASE + 10;

      ACMDM_HARDWARE_WAVE_CAPS_INPUT  = ACMDM_BASE + 20;
      ACMDM_HARDWARE_WAVE_CAPS_OUTPUT = ACMDM_BASE + 21;

      ACMDM_FORMATTAG_DETAILS         = ACMDM_BASE + 25;
      ACMDM_FORMAT_DETAILS            = ACMDM_BASE + 26;
      ACMDM_FORMAT_SUGGEST            = ACMDM_BASE + 27;

      ACMDM_FILTERTAG_DETAILS         = ACMDM_BASE + 50;
      ACMDM_FILTER_DETAILS            = ACMDM_BASE + 51;

      ACMDM_STREAM_OPEN               = ACMDM_BASE + 76;
      ACMDM_STREAM_CLOSE              = ACMDM_BASE + 77;
      ACMDM_STREAM_SIZE               = ACMDM_BASE + 78;
      ACMDM_STREAM_CONVERT            = ACMDM_BASE + 79;
      ACMDM_STREAM_RESET              = ACMDM_BASE + 80;
      ACMDM_STREAM_PREPARE            = ACMDM_BASE + 81;
      ACMDM_STREAM_UNPREPARE          = ACMDM_BASE + 82;
      ACMDM_STREAM_UPDATE             = ACMDM_BASE + 83;

// - End of module: msacmdrv.h


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//
//
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      DRV_MAPPER_PREFERRED_INPUT_GET  = DRV_USER + 0;
      DRV_MAPPER_PREFERRED_OUTPUT_GET = DRV_USER + 2;
      DRVM_MAPPER_STATUS              = $2000;

      WIDM_MAPPER_STATUS              = DRVM_MAPPER_STATUS + 0;
      WAVEIN_MAPPER_STATUS_DEVICE     = 0;
      WAVEIN_MAPPER_STATUS_MAPPED     = 1;
      WAVEIN_MAPPER_STATUS_FORMAT     = 2;

      WODM_MAPPER_STATUS              = DRVM_MAPPER_STATUS + 0;
      WAVEOUT_MAPPER_STATUS_DEVICE    = 0;
      WAVEOUT_MAPPER_STATUS_MAPPED    = 1;
      WAVEOUT_MAPPER_STATUS_FORMAT    = 2;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  ACM Error Codes
//
//  Note that these error codes are specific errors that apply to the ACM
//  directly--general errors are defined as MMSYSERR_*.
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     MMRESULT = UINT;

const
      ACMERR_BASE         = 512;
      ACMERR_NOTPOSSIBLE  = ACMERR_BASE + 0;
      ACMERR_BUSY         = ACMERR_BASE + 1;
      ACMERR_UNPREPARED   = ACMERR_BASE + 2;
      ACMERR_CANCELED     = ACMERR_BASE + 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  ACM Window Messages
//
//  These window messages are sent by the ACM or ACM drivers to notify
//  applications of events.
//
//  Note that these window message numbers will also be defined in
//  mmsystem.
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      MM_ACM_OPEN         = $03D4; // MM_STREAM_OPEN  // conversion callback messages
      MM_ACM_CLOSE        = $03D5; // MM_STREAM_CLOSE
      MM_ACM_DONE         = $03D6; // MM_STREAM_DONE

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmGetVersion()
//
//  the ACM version is a 32 bit number that is broken into three parts as 
//  follows:
//
//      bits 24 - 31:   8 bit _major_ version number
//      bits 16 - 23:   8 bit _minor_ version number
//      bits  0 - 15:   16 bit build number
//
//  this is then displayed as follows:
//
//      bMajor = (BYTE)(dwVersion >> 24)
//      bMinor = (BYTE)(dwVersion >> 16) & 
//      wBuild = LOWORD(dwVersion)
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmGetVersion:DWORD; external KernelDLL name 'acmGetVersion'; // index 2A5

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmMetrics()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmMetrics(hao:HACMOBJ; uMetric:UINT; pMetric:LPVOID):MMRESULT; external KernelDLL name 'acmMetrics'; // index 2A6

const
      ACM_METRIC_COUNT_DRIVERS            = 1;
      ACM_METRIC_COUNT_CODECS             = 2;
      ACM_METRIC_COUNT_CONVERTERS         = 3;
      ACM_METRIC_COUNT_FILTERS            = 4;
      ACM_METRIC_COUNT_DISABLED           = 5;
      ACM_METRIC_COUNT_HARDWARE           = 6;
      ACM_METRIC_COUNT_LOCAL_DRIVERS      = 20;
      ACM_METRIC_COUNT_LOCAL_CODECS       = 21;
      ACM_METRIC_COUNT_LOCAL_CONVERTERS   = 22;
      ACM_METRIC_COUNT_LOCAL_FILTERS      = 23;
      ACM_METRIC_COUNT_LOCAL_DISABLED     = 24;
      ACM_METRIC_HARDWARE_WAVE_INPUT      = 30;
      ACM_METRIC_HARDWARE_WAVE_OUTPUT     = 31;
      ACM_METRIC_MAX_SIZE_FORMAT          = 50;
      ACM_METRIC_MAX_SIZE_FILTER          = 51;
      ACM_METRIC_DRIVER_SUPPORT           = 100;
      ACM_METRIC_DRIVER_PRIORITY          = 101;


//--------------------------------------------------------------------------;
//
//  ACM Drivers
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverEnum()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     ACMDRIVERENUMCB = function(hadid:HACMDRIVERID; dwInstance:DWORD; fdwSupport:DWORD):BOOL; cdecl;


function acmDriverEnum(fnCallback:ACMDRIVERENUMCB; dwInstance:DWORD; fdwEnum:DWORD):MMRESULT; external KernelDLL name 'acmDriverEnum'; // index 28E

const
      ACM_DRIVERENUMF_NOLOCAL     = $40000000;
      ACM_DRIVERENUMF_DISABLED    = $80000000;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverID()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverID(hao:HACMOBJ; phadid:LPHACMDRIVERID; fdwDriverID:DWORD):MMRESULT; external KernelDLL name 'acmDriverID'; // index 28F

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverAdd()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverAdd(phadid:LPHACMDRIVERID;
                      hinstModule:HINST;
                      lParam:LPARAM;
                      dwPriority:DWORD;
                      fdwAdd:DWORD):MMRESULT; external KernelDLL name 'acmDriverAdd'; // index 28B

const
      ACM_DRIVERADDF_NOTIFYHWND   = $00000004;  // lParam is notify hwnd


//
//  prototype for ACM driver procedures that are installed as _functions_
//  or _notifications_ instead of as a standalone installable driver.
//
type
     ACMDRIVERPROC = function(dwID:DWORD; hDrvID:HACMDRIVERID; uMsg:UINT; lParam1:LPARAM; lParam2:LPARAM):LRESULT; cdecl;
     LPACMDRIVERPROC = ^ACMDRIVERPROC;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverRemove()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverRemove(hadid:HACMDRIVERID; fdwRemove:DWORD):MMRESULT; external KernelDLL name 'acmDriverRemove'; // index 293

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverOpen()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverOpen(phad:LPHACMDRIVER; hadid:HACMDRIVERID; fdwOpen:DWORD):MMRESULT; external KernelDLL name 'acmDriverOpen'; // index 291

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverClose()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverClose(had:HACMDRIVER; fdwClose:DWORD):MMRESULT; external KernelDLL name 'acmDriverClose'; // index 28C

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverMessage()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverMessage(had:HACMDRIVER; uMsg:UINT; lParam1:LPARAM; lParam2:LPARAM):LRESULT; external KernelDLL name 'acmDriverMessage'; // index 290


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverPriority
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmDriverPriority(hadid:HACMDRIVERID; dwPriority:DWORD; fdwPriority:DWORD):MMRESULT; external KernelDLL name 'acmDriverPriority'; // index 292

const
      ACM_DRIVERPRIORITYF_ENABLE      = $00000001;
      ACM_DRIVERPRIORITYF_DISABLE     = $00000002;
      ACM_DRIVERPRIORITYF_ABLEMASK    = $00000003;
      ACM_DRIVERPRIORITYF_BEGIN       = $00010000;
      ACM_DRIVERPRIORITYF_END         = $00020000;
      ACM_DRIVERPRIORITYF_DEFERMASK   = $00030000;

      
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmDriverDetails()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

//
//  ACMDRIVERDETAILS
//
//  the ACMDRIVERDETAILS structure is used to get various capabilities from
//  an ACM driver (codec, converter, filter).
//
const
      ACMDRIVERDETAILS_SHORTNAME_CHARS    = 32;
      ACMDRIVERDETAILS_LONGNAME_CHARS     = 128;
      ACMDRIVERDETAILS_COPYRIGHT_CHARS    = 80;
      ACMDRIVERDETAILS_LICENSING_CHARS    = 128;
      ACMDRIVERDETAILS_FEATURES_CHARS     = 512;

type
     tACMDRIVERDETAILS = record
       cbStruct:DWORD;           // number of valid bytes in structure
       fccType:FOURCC;            // compressor type 'audc'
       fccComp:FOURCC;            // sub-type (not used; reserved)

       wMid:word;               // manufacturer id
       wPid:word;               // product id

       vdwACM:DWORD;             // version of the ACM *compiled* for
       vdwDriver:DWORD;          // version of the driver

       fdwSupport:DWORD;         // misc. support flags
       cFormatTags:DWORD;        // total unique format tags supported
       cFilterTags:DWORD;        // total unique filter tags supported

       _hicon:HICON;              // handle to custom icon

       szShortName:array[0..ACMDRIVERDETAILS_SHORTNAME_CHARS-1] of WCHAR;
       szLongName:array[0..ACMDRIVERDETAILS_LONGNAME_CHARS-1] of WCHAR;
       szCopyright:array[0..ACMDRIVERDETAILS_COPYRIGHT_CHARS-1] of WCHAR;
       szLicensing:array[0..ACMDRIVERDETAILS_LICENSING_CHARS-1] of WCHAR;
       szFeatures:array[0..ACMDRIVERDETAILS_FEATURES_CHARS-1] of WCHAR;
     end;
     _ACMDRIVERDETAILS = tACMDRIVERDETAILS;
     PACMDRIVERDETAILS = ^tACMDRIVERDETAILS;
     LPACMDRIVERDETAILS = ^tACMDRIVERDETAILS;


//
//  ACMDRIVERDETAILS.fccType
//
//  ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC: the FOURCC used in the fccType
//  field of the ACMDRIVERDETAILS structure to specify that this is an ACM
//  codec designed for audio.
//
//
//  ACMDRIVERDETAILS.fccComp
//
//  ACMDRIVERDETAILS_FCCCOMP_UNDEFINED: the FOURCC used in the fccComp
//  field of the ACMDRIVERDETAILS structure. this is currently an unused
//  field.
//
const
      ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC = FOURCC(byte(AnsiChar('a')) or
                                                   (byte(AnsiChar('u')) shl 8) or
                                                   (byte(AnsiChar('d')) shl 16) or
                                                   (byte(AnsiChar('c')) shl 24)
                                                  );

      ACMDRIVERDETAILS_FCCCOMP_UNDEFINED  = FOURCC(byte(AnsiChar(#0)) or
                                                   (byte(AnsiChar(#0)) shl 8) or
                                                   (byte(AnsiChar(#0)) shl 16) or
                                                   (byte(AnsiChar(#0)) shl 24) 
                                                  );

//
//  the following flags are used to specify the type of conversion(s) that
//  the converter/codec/filter supports. these are placed in the fdwSupport
//  field of the ACMDRIVERDETAILS structure. note that a converter can
//  support one or more of these flags in any combination.
//
//  ACMDRIVERDETAILS_SUPPORTF_CODEC: this flag is set if the driver supports
//  conversions from one format tag to another format tag. for example, if a
//  converter compresses WAVE_FORMAT_PCM to WAVE_FORMAT_ADPCM, then this bit
//  should be set.
//
//  ACMDRIVERDETAILS_SUPPORTF_CONVERTER: this flags is set if the driver
//  supports conversions on the same format tag. as an example, the PCM
//  converter that is built into the ACM sets this bit (and only this bit)
//  because it converts only PCM formats (bits, sample rate).
//
//  ACMDRIVERDETAILS_SUPPORTF_FILTER: this flag is set if the driver supports
//  transformations on a single format. for example, a converter that changed
//  the 'volume' of PCM data would set this bit. 'echo' and 'reverb' are
//  also filter types.
//
//  ACMDRIVERDETAILS_SUPPORTF_HARDWARE: this flag is set if the driver supports
//  hardware input and/or output through a waveform device.
//
//  ACMDRIVERDETAILS_SUPPORTF_ASYNC: this flag is set if the driver supports
//  async conversions.
//
//
//  ACMDRIVERDETAILS_SUPPORTF_LOCAL: this flag is set _by the ACM_ if a
//  driver has been installed local to the current task. this flag is also
//  set in the fdwSupport argument to the enumeration callback function
//  for drivers.
//
//  ACMDRIVERDETAILS_SUPPORTF_DISABLED: this flag is set _by the ACM_ if a
//  driver has been disabled. this flag is also passed set in the fdwSupport
//  argument to the enumeration callback function for drivers.
//
const
      ACMDRIVERDETAILS_SUPPORTF_CODEC     = $00000001;
      ACMDRIVERDETAILS_SUPPORTF_CONVERTER = $00000002;
      ACMDRIVERDETAILS_SUPPORTF_FILTER    = $00000004;
      ACMDRIVERDETAILS_SUPPORTF_HARDWARE  = $00000008;
      ACMDRIVERDETAILS_SUPPORTF_ASYNC     = $00000010;
      ACMDRIVERDETAILS_SUPPORTF_LOCAL     = $40000000;
      ACMDRIVERDETAILS_SUPPORTF_DISABLED  = $80000000;

function acmDriverDetails(hadid:HACMDRIVERID;
                          padd:LPACMDRIVERDETAILS;
                          fdwDetails:DWORD):MMRESULT; external KernelDLL name 'acmDriverDetails'; // index 28D

                          

//--------------------------------------------------------------------------;
//
//  ACM Format Tags
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatTagDetails()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;

type
     tACMFORMATTAGDETAILSW = record
       cbStruct:DWORD;
       dwFormatTagIndex:DWORD;
       dwFormatTag:DWORD;
       cbFormatSize:DWORD;
       fdwSupport:DWORD;
       cStandardFormats:DWORD;
       szFormatTag:array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of WCHAR;
     end;
     tACMFORMATTAGDETAILS = tACMFORMATTAGDETAILSW;
     _ACMFORMATTAGDETAILS = tACMFORMATTAGDETAILSW;
     PACMFORMATTAGDETAILS = ^tACMFORMATTAGDETAILSW;
     LPACMFORMATTAGDETAILS = ^tACMFORMATTAGDETAILSW;

function acmFormatTagDetails(had:HACMDRIVER;
                             paftd:LPACMFORMATTAGDETAILS;
                             fdwDetails:DWORD):MMRESULT; external KernelDLL name 'acmFormatTagDetails'; // index 29B

const
      ACM_FORMATTAGDETAILSF_INDEX         = $00000000;
      ACM_FORMATTAGDETAILSF_FORMATTAG     = $00000001;
      ACM_FORMATTAGDETAILSF_LARGESTSIZE   = $00000002;
      ACM_FORMATTAGDETAILSF_QUERYMASK     = $0000000F;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatTagEnum()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     ACMFORMATTAGENUMCB = function(hadid:HACMDRIVERID;
                                   paftd:LPACMFORMATTAGDETAILS;
                                   dwInstance:DWORD;
                                   fdwSupport:DWORD):BOOL; cdecl;

function acmFormatTagEnum(had:HACMDRIVER;
                          paftd:LPACMFORMATTAGDETAILS;
                          fnCallback:ACMFORMATTAGENUMCB;
                          dwInstance:DWORD;
                          fdwEnum:DWORD):MMRESULT; external KernelDLL name 'acmFormatTagEnum'; // index 29C


//--------------------------------------------------------------------------;
//
//  ACM Formats
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatDetails()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMFORMATDETAILS_FORMAT_CHARS   = 128;

type
     tACMFORMATDETAILS = record
       cbStruct:DWORD;
       dwFormatIndex:DWORD;
       dwFormatTag:DWORD;
       fdwSupport:DWORD;
       pwfx:LPWAVEFORMATEX;
       cbwfx:DWORD;
       szFormat:array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of WCHAR;
     end;
     _ACMFORMATDETAILS = tACMFORMATDETAILS;
     PACMFORMATDETAILS = ^tACMFORMATDETAILS;
     LPACMFORMATDETAILS = ^tACMFORMATDETAILS;

function acmFormatDetails(had:HACMDRIVER; pafd:LPACMFORMATDETAILS; fdwDetails:DWORD):MMRESULT; external KernelDLL name 'acmFormatDetails'; // index 298

const
      ACM_FORMATDETAILSF_INDEX        = $00000000;
      ACM_FORMATDETAILSF_FORMAT       = $00000001;
      ACM_FORMATDETAILSF_QUERYMASK    = $0000000F;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatEnum()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     ACMFORMATENUMCB = function(hadid:HACMDRIVERID;
                                pafd:LPACMFORMATDETAILS;
                                dwInstance:DWORD;
                                fdwSupport:DWORD):BOOL; cdecl;

function acmFormatEnum(had:HACMDRIVER;
                       pafd:LPACMFORMATDETAILS;
                       fnCallback:ACMFORMATENUMCB;
                       dwInstance:DWORD;
                       fdwEnum:DWORD):MMRESULT; external KernelDLL name 'acmFormatEnum'; // index 299

const
      ACM_FORMATENUMF_WFORMATTAG       = $00010000;
      ACM_FORMATENUMF_NCHANNELS        = $00020000;
      ACM_FORMATENUMF_NSAMPLESPERSEC   = $00040000;
      ACM_FORMATENUMF_WBITSPERSAMPLE   = $00080000;
      ACM_FORMATENUMF_CONVERT          = $00100000;
      ACM_FORMATENUMF_SUGGEST          = $00200000;
      ACM_FORMATENUMF_HARDWARE         = $00400000;
      ACM_FORMATENUMF_INPUT            = $00800000;
      ACM_FORMATENUMF_OUTPUT           = $01000000;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatSuggest()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmFormatSuggest(had:HACMDRIVER;
                          pwfxSrc:LPWAVEFORMATEX;
                          pwfxDst:LPWAVEFORMATEX;
                          cbwfxDst:DWORD;
                          fdwSuggest:DWORD):MMRESULT; external KernelDLL name 'acmFormatSuggest'; // index 29A

const
      ACM_FORMATSUGGESTF_WFORMATTAG       = $00010000;
      ACM_FORMATSUGGESTF_NCHANNELS        = $00020000;
      ACM_FORMATSUGGESTF_NSAMPLESPERSEC   = $00040000;
      ACM_FORMATSUGGESTF_WBITSPERSAMPLE   = $00080000;

      ACM_FORMATSUGGESTF_TYPEMASK         = $00FF0000;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFormatChoose()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMHELPMSGSTRING        = 'acmchoose_help';
      ACMHELPMSGCONTEXTMENU   = 'acmchoose_contextmenu';
      ACMHELPMSGCONTEXTHELP   = 'acmchoose_contexthelp';

//
//  MM_ACM_FORMATCHOOSE is sent to hook callbacks by the Format Chooser
//  Dialog...
//
const
      MM_ACM_FORMATCHOOSE             = $8000;

       FORMATCHOOSE_MESSAGE           = 0;
       FORMATCHOOSE_FORMATTAG_VERIFY  = FORMATCHOOSE_MESSAGE + 0;
       FORMATCHOOSE_FORMAT_VERIFY     = FORMATCHOOSE_MESSAGE + 1;
       FORMATCHOOSE_CUSTOM_VERIFY     = FORMATCHOOSE_MESSAGE + 2;

type
     ACMFORMATCHOOSEHOOKPROC = function(hwnd:HWND;
                                        uMsg:UINT;
                                        wParam:WPARAM;
                                        lParam:LPARAM):UINT; cdecl;
type
     tACMFORMATCHOOSE = record
       cbStruct:DWORD;           // sizeof(ACMFORMATCHOOSE)
       fdwStyle:DWORD;           // chooser style flags

       hwndOwner:HWND;          // caller's window handle

       pwfx:LPWAVEFORMATEX;               // ptr to wfx buf to receive choice
       cbwfx:DWORD;              // size of mem buf for pwfx
       pszTitle:LPCWSTR;           // dialog box title bar

       szFormatTag:array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of WCHAR;
       szFormat:array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of WCHAR;

       pszName:LPWSTR;            // custom name selection
       cchName:DWORD;            // size in chars of mem buf for pszName

       fdwEnum:DWORD;            // format enumeration restrictions
       pwfxEnum:LPWAVEFORMATEX;  // format describing restrictions

       hInstance:HINST;          // app instance containing dlg template
       pszTemplateName:LPCWSTR;    // custom template name
       lCustData:LPARAM;          // data passed to hook fn.
       pfnHook:ACMFORMATCHOOSEHOOKPROC;    // ptr to hook function
     end;
     _ACMFORMATCHOOSE = tACMFORMATCHOOSE;
     PACMFORMATCHOOSE = ^tACMFORMATCHOOSE;
     LPACMFORMATCHOOSE = ^tACMFORMATCHOOSE;

//
//  ACMFORMATCHOOSE.fdwStyle
//
const
      ACMFORMATCHOOSE_STYLEF_SHOWHELP              = $00000004;
      ACMFORMATCHOOSE_STYLEF_ENABLEHOOK            = $00000008;
      ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE        = $00000010;
      ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE  = $00000020;
      ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT       = $00000040;
      ACMFORMATCHOOSE_STYLEF_CONTEXTHELP           = $00000080;

function acmFormatChoose(pafmtc:LPACMFORMATCHOOSE):MMRESULT; external KernelDLL name 'acmFormatChoose'; // index 2A7


//--------------------------------------------------------------------------;
//
//  ACM Filter Tags
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFilterTagDetails()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMFILTERTAGDETAILS_FILTERTAG_CHARS = 48;

type
     tACMFILTERTAGDETAILS = record
       cbStruct:DWORD;
       dwFilterTagIndex:DWORD;
       dwFilterTag:DWORD;
       cbFilterSize:DWORD;
       fdwSupport:DWORD;
       cStandardFilters:DWORD;
       szFilterTag:array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of WCHAR;
     end;
     _ACMFILTERTAGDETAILS = tACMFILTERTAGDETAILS;
     PACMFILTERTAGDETAILS = ^tACMFILTERTAGDETAILS;
     LPACMFILTERTAGDETAILS = ^tACMFILTERTAGDETAILS;

function acmFilterTagDetails(had:HACMDRIVER;
                             paftd:LPACMFILTERTAGDETAILS;
                             fdwDetails:DWORD):MMRESULT; external KernelDLL name 'acmFilterTagDetails'; // index 296 

const
      ACM_FILTERTAGDETAILSF_INDEX         = $00000000;
      ACM_FILTERTAGDETAILSF_FILTERTAG     = $00000001;
      ACM_FILTERTAGDETAILSF_LARGESTSIZE   = $00000002;
      ACM_FILTERTAGDETAILSF_QUERYMASK     = $0000000F;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFilterTagEnum()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     ACMFILTERTAGENUMCB = function(hadid:HACMDRIVERID;
                                   paftd:LPACMFILTERTAGDETAILS;
                                   dwInstance:DWORD;
                                   fdwSupport:DWORD):BOOL; cdecl;

function acmFilterTagEnum(had:HACMDRIVER;
                          paftd:LPACMFILTERTAGDETAILS;
                          fnCallback:ACMFILTERTAGENUMCB;
                          dwInstance:DWORD;
                          fdwEnum:DWORD):MMRESULT; external KernelDLL name 'acmFilterTagEnum'; // index 297 


//--------------------------------------------------------------------------;
//
//  ACM Filters
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFilterDetails()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
const
      ACMFILTERDETAILS_FILTER_CHARS   = 128;

type
     tACMFILTERDETAILS = record
       cbStruct:DWORD;
       dwFilterIndex:DWORD;
       dwFilterTag:DWORD;
       fdwSupport:DWORD;
       pwfltr:LPWAVEFILTER;
       cbwfltr:DWORD;
       szFilter:array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of WCHAR;
     end;
     _ACMFILTERDETAILS = tACMFILTERDETAILS;
     PACMFILTERDETAILS = ^tACMFILTERDETAILS;
     LPACMFILTERDETAILS = ^tACMFILTERDETAILS;

function acmFilterDetails(had:HACMDRIVER;
                          pafd:LPACMFILTERDETAILS;
                          fdwDetails:DWORD):MMRESULT; external KernelDLL name 'acmFilterDetails'; // index 294

const
      ACM_FILTERDETAILSF_INDEX        = $00000000;
      ACM_FILTERDETAILSF_FILTER       = $00000001;
      ACM_FILTERDETAILSF_QUERYMASK    = $0000000F;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFilterEnum()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     ACMFILTERENUMCB = function(hadid:HACMDRIVERID;
                                pafd:LPACMFILTERDETAILS;      
                                dwInstance:DWORD;
                                fdwSupport:DWORD):BOOL; cdecl;

function acmFilterEnum(had:HACMDRIVER;
                       pafd:LPACMFILTERDETAILS;
                       fnCallback:ACMFILTERENUMCB;
                       dwInstance:DWORD;
                       fdwEnum:DWORD):MMRESULT; external KernelDLL name 'acmFilterEnum'; // index 295

const
      ACM_FILTERENUMF_DWFILTERTAG         = $00010000;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmFilterChoose()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

//
//  MM_ACM_FILTERCHOOSE is sent to hook callbacks by the Filter Chooser
//  Dialog...
//
const
      MM_ACM_FILTERCHOOSE             = $8000;

      FILTERCHOOSE_MESSAGE            = 0;
      FILTERCHOOSE_FILTERTAG_VERIFY   = FILTERCHOOSE_MESSAGE + 0;
      FILTERCHOOSE_FILTER_VERIFY      = FILTERCHOOSE_MESSAGE + 1;
      FILTERCHOOSE_CUSTOM_VERIFY      = FILTERCHOOSE_MESSAGE + 2;

type
     ACMFILTERCHOOSEHOOKPROC = function(hwnd:HWND;
                                        uMsg:UINT;                    
                                        wParam:WPARAM;
                                        lParam:LPARAM):UINT; cdecl;

//
//  ACMFILTERCHOOSE
//
type
     tACMFILTERCHOOSE = record
       cbStruct:DWORD;           // sizeof(ACMFILTERCHOOSE)
       fdwStyle:DWORD;           // chooser style flags

       hwndOwner:HWND;          // caller's window handle

       pwfltr:LPWAVEFILTER;             // ptr to wfltr buf to receive choice
       cbwfltr:DWORD;            // size of mem buf for pwfltr

       pszTitle:LPCWSTR;

       szFilterTag:array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of WCHAR;
       szFilter:array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of WCHAR;
       pszName:LPWSTR;            // custom name selection
       cchName:DWORD;            // size in chars of mem buf for pszName

       fdwEnum:DWORD;            // filter enumeration restrictions
       pwfltrEnum:LPWAVEFILTER;         // filter describing restrictions

       hInstance:HINST;          // app instance containing dlg template
       pszTemplateName:LPCWSTR;    // custom template name
       lCustData:LPARAM;          // data passed to hook fn.
       pfnHook:ACMFILTERCHOOSEHOOKPROC;    // ptr to hook function
     end;
     _ACMFILTERCHOOSE = tACMFILTERCHOOSE;
     PACMFILTERCHOOSE = ^tACMFILTERCHOOSE;
     LPACMFILTERCHOOSE = ^tACMFILTERCHOOSE;

//
//  ACMFILTERCHOOSE.fdwStyle
//
const
      ACMFILTERCHOOSE_STYLEF_SHOWHELP              = $00000004;
      ACMFILTERCHOOSE_STYLEF_ENABLEHOOK            = $00000008;
      ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATE        = $00000010;
      ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATEHANDLE  = $00000020;
      ACMFILTERCHOOSE_STYLEF_INITTOFILTERSTRUCT    = $00000040;
      ACMFILTERCHOOSE_STYLEF_CONTEXTHELP           = $00000080;

function acmFilterChoose(pafltrc:LPACMFILTERCHOOSE):MMRESULT; external KernelDLL name 'acmFilterChoose'; // index 2A8

//--------------------------------------------------------------------------;
//
//  ACM Stream API's
//
//--------------------------------------------------------------------------;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamOpen()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
type
     tACMSTREAMHEADER = record
       cbStruct:DWORD;               // sizeof(ACMSTREAMHEADER)
       fdwStatus:DWORD;              // ACMSTREAMHEADER_STATUSF_*
       dwUser:DWORD;                 // user instance data for hdr
       pbSrc:LPBYTE;
       cbSrcLength:DWORD;
       cbSrcLengthUsed:DWORD;
       dwSrcUser:DWORD;              // user instance data for src
       pbDst:LPBYTE;
       cbDstLength:DWORD;
       cbDstLengthUsed:DWORD;
       dwDstUser:DWORD;              // user instance data for dst
       dwReservedDriver:array[0..9] of DWORD;   // driver reserved work space
     end;
     ACMSTREAMHEADER = tACMSTREAMHEADER;
     PACMSTREAMHEADER = ^tACMSTREAMHEADER;
     LPACMSTREAMHEADER = ^tACMSTREAMHEADER;

//
//  ACMSTREAMHEADER.fdwStatus
//
//  ACMSTREAMHEADER_STATUSF_DONE: done bit for async conversions.
//
const
      ACMSTREAMHEADER_STATUSF_DONE        = $00010000;
      ACMSTREAMHEADER_STATUSF_PREPARED    = $00020000;
      ACMSTREAMHEADER_STATUSF_INQUEUE     = $00100000;

function acmStreamOpen(phas:LPHACMSTREAM;       // pointer to stream handle
                       had:HACMDRIVER;        // optional driver handle
                       pwfxSrc:LPWAVEFORMATEX;    // source format to convert
                       pwfxDst:LPWAVEFORMATEX;    // required destination format
                       pwfltr:LPWAVEFILTER;     // optional filter
                       dwCallback:DWORD; // callback
                       dwInstance:DWORD; // callback instance data
                       fdwOpen:DWORD     // ACM_STREAMOPENF_* and CALLBACK_*
                      ):MMRESULT; external KernelDLL name 'acmStreamOpen'; // index 2A0

const
      ACM_STREAMOPENF_QUERY           = $00000001;
      ACM_STREAMOPENF_ASYNC           = $00000002;
      ACM_STREAMOPENF_NONREALTIME     = $00000004;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamClose()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamClose(has:HACMSTREAM;
                        fdwClose:DWORD):MMRESULT; external KernelDLL name 'acmStreamClose'; // index 29D

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamSize()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamSize(has:HACMSTREAM;
                       cbInput:DWORD;
                       pdwOutputBytes:LPDWORD;
                       fdwSize:DWORD):MMRESULT; external KernelDLL name 'acmStreamSize'; // index 2A3

const
      ACM_STREAMSIZEF_SOURCE          = $00000000;
      ACM_STREAMSIZEF_DESTINATION     = $00000001;
      ACM_STREAMSIZEF_QUERYMASK       = $0000000F;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamReset()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamReset(has:HACMSTREAM;
                        fdwReset:DWORD):MMRESULT; external KernelDLL name 'acmStreamReset'; // index 2A2

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamMessage()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamMessage(has:HACMSTREAM;
                          uMsg:UINT;
                          lParam1:LPARAM;
                          lParam2:LPARAM):MMRESULT; external KernelDLL name 'acmStreamMessage'; // index 29F

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamConvert()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamConvert(has:HACMSTREAM;
                          pash:LPACMSTREAMHEADER;
                          fdwConvert:DWORD):MMRESULT; external KernelDLL name 'acmStreamConvert'; // index 29E

const                          
      ACM_STREAMCONVERTF_BLOCKALIGN   = $00000004;
      ACM_STREAMCONVERTF_START        = $00000010;
      ACM_STREAMCONVERTF_END          = $00000020;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamPrepareHeader()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamPrepareHeader(has:HACMSTREAM;
                                pash:LPACMSTREAMHEADER;
                                fdwPrepare:DWORD):MMRESULT; external KernelDLL name 'acmStreamPrepareHeader'; // index 2A1

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  acmStreamUnprepareHeader()
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
function acmStreamUnprepareHeader(has:HACMSTREAM;
                                  pash:LPACMSTREAMHEADER;
                                  fdwUnprepare:DWORD):MMRESULT; external KernelDLL name 'acmStreamUnprepareHeader'; // index 2A4

{$PACKRECORDS DEFAULT} // #pragma pack()          /* Revert to default packing */



// Module: msacmdlg.h

// -----------------------------------------------------------------------------
//
//  Audio Compression Manager Common Dialogs Identifiers
//
// -----------------------------------------------------------------------------
const
      DLG_ACMFORMATCHOOSE_ID              = 70;
      IDD_ACMFORMATCHOOSE_CMB_FORMATTAG   = 101;
      IDD_ACMFORMATCHOOSE_CMB_FORMAT      = 102;

      DLG_ACMFILTERCHOOSE_ID              = 71;
      IDD_ACMFILTERCHOOSE_CMB_FILTERTAG   = 101;
      IDD_ACMFILTERCHOOSE_CMB_FILTER      = 102;

// End of module msacmdlg.h

implementation

// - Module: msacmdrv.h
// -----------------------------------------------------------------------------
//
//  Audio Compression Manager Public Header File for Drivers
//
// -----------------------------------------------------------------------------
function MAKE_ACM_VERSION(mjr:byte; mnr:byte; bld:word):DWORD; inline;
begin
  MAKE_ACM_VERSION:=(DWORD(mjr) shl 24) or (DWORD(mnr) shl 16) or bld;
end;
// - End of module: msacmdrv.h

end.