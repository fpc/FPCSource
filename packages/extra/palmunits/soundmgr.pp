{$MACRO ON}
{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SoundMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Sound Manager
 *
 * History:
 *    4/11/95  VMK - Created by Vitaly Kruglikov
 *
 *****************************************************************************)

unit soundmgr;

interface

uses palmos, coretraps, errorbase, preferences;

(************************************************************
 * Sound Manager constants
 *
 *************************************************************)

const
// Sound Manager max and default volume levels
  sndMaxAmp         = 64;
//sndVolumeMask     = $ff;
  sndDefaultAmp     = sndMaxAmp;

  sndMidiNameLength = 32; // MIDI track name length *including* NULL terminator

(************************************************************
 * Sound Manager data structures
 *
 *************************************************************)

//
// Command numbers for SndCommandType's cmd field
//
type
  SndCmdIDType = Enum;

const
  sndCmdFreqDurationAmp = 1;                // play a sound, blocking for the entire duration (except for zero amplitude)
                                            // param1 = frequency in Hz
                                            // param2 = duration in milliseconds
                                            // param3 = amplitude (0 - sndMaxAmp); if 0, will return immediately

    // Commands added in PilotOS v3.0
    // ***IMPORTANT***
    // Please note that SndDoCmd() in PilotOS before v3.0 will Fatal Error on unknown
    // commands (anything other than sndCmdFreqDurationAmp).  For this reason,
    // applications wishing to take advantage of these new commands while staying
    // compatible with the earlier version of the OS, _must_ avoid using these commands
    // when running on OS versions less thatn v3.0 (see sysFtrNumROMVersion in SystemMgr.h).
    // Beginning with v3.0, SndDoCmd has been fixed to return sndErrBadParam when an
    // unknown command is passed.

  sndCmdNoteOn = Succ(sndCmdFreqDurationAmp); // start a sound given its MIDI key index, max duration and velocity;
                                            // the call will not wait for the sound to complete, returning imeediately;
                                            // any other sound play request made before this one completes will interrupt it.
                                            // param1 = MIDI key index (0-127)
                                            // param2 = maximum duration in milliseconds
                                            // param3 = velocity (0 - 127) (will be interpolated as amplitude)

  sndCmdFrqOn = Succ(sndCmdNoteOn);         // start a sound given its frequency in Hz, max duration and amplitude;
                                            // the call will not wait for the sound to complete, returning imeediately;
                                            // any other sound play request made before this one completes will interrupt it.
                                            // param1 = frequency in Hz
                                            // param2 = maximum duration in milliseconds
                                            // param3 = amplitude (0 - sndMaxAmp)

  sndCmdQuiet = Succ(sndCmdFrqOn);          // stop current sound
                                            // param1 = 0
                                            // param2 = 0
                                            // param3 = 0

//
// SndCommandType: used by SndDoCmd()
//

type
  SndCommandType = record
    cmd: SndCmdIDType;             // command id
    reserved: UInt8;
    param1: Int32;                 // first parameter
    param2: UInt16;                // second parameter
    param3: UInt16;                // third parameter
  end;

  SndCommandPtr = ^SndCommandType;

//
// Beep numbers used by SndSysBeep()
//

type
  SndSysBeepType = Enum;

const
  sndInfo = 1;
  sndWarning = Succ(sndInfo);
  sndError = Succ(sndWarning);
  sndStartUp = Succ(sndError);
  sndAlarm = Succ(sndStartUp);
  sndConfirmation = Succ(sndAlarm);
  sndClick = Succ(sndConfirmation);

(************************************************************
 * Standard MIDI File (SMF) support structures
 *************************************************************)

// Structure of records in the MIDI sound database:
//
// Each MIDI record consists of a record header followed immediately by the
// Standard MIDI File (SMF) data stream.  Only SMF format #0 is presently supported.
// The first byte of the record header is the byte offset from the beginning of the record
// to the SMF data stream.  The name of the record follows the byte offset
// field.  sndMidiNameLength is the limit on name size (including NULL).

const
  sndMidiRecSignature = Rsc('PMrc');

type
  SndMidiRecHdrType = record
    signature: UInt32;             // set to sndMidiRecSignature
    bDataOffset: UInt8;            // offset from the beginning of the record
                                   // to the Standard Midi File data stream
    reserved: UInt8;               // set to zero
  end;

  SndMidiRecType = record
    hdr: SndMidiRecHdrType;            // offset from the beginning of the record
                                       // to the Standard Midi File data stream
    name: array [0..2-1] of Char;      // Track name: 1 or more chars including NULL terminator.
                                       // If a track has no name, the NULL character must still
                                       // be provided.
                                       // Set to 2 to pad the structure out to a word boundary.
  end;

// Midi records found by SndCreateMidiList.
  SndMidiListItemType = record
    name: array [0..sndMidiNameLength-1] of Char; // including NULL terminator
    uniqueRecID: UInt32;
    dbID: LocalID;
    cardNo: UInt16;
  end;

// Commands for SndPlaySmf
  SndSmfCmdEnum = Enum;

const
  sndSmfCmdPlay = 1;                       // play the selection
  sndSmfCmdDuration = Succ(sndSmfCmdPlay); // get the duration in milliseconds of the entire track

type
  SndComplFuncType = procedure(chanP: Pointer; dwUserData: UInt32);
  SndComplFuncPtr = SndComplFuncType;

// Return true to continue, false to abort
  SndBlockingFuncType = function(chanP: Pointer; dwUserData: UInt32; sysTicksAvailable: Int32): Boolean;
  SndBlockingFuncPtr = SndBlockingFuncType;

type
  SndCallbackInfoType = record
    funcP: MemPtr;          // pointer to the callback function (NULL = no function)
    dwUserData: UInt32;     // value to be passed in the dwUserData parameter of the callback function
  end;

  SndSmfCallbacksType = record
    completion: SndCallbackInfoType;     // completion callback function (see SndComplFuncType)
    blocking: SndCallbackInfoType;       // blocking hook callback function (see SndBlockingFuncType)
    reserved: SndCallbackInfoType;       // RESERVED -- SET ALL FIELDS TO ZERO BEFORE PASSING
  end;
  SndSmfCallbacksPtr = ^SndSmfCallbacksType;

const
  sndSmfPlayAllMilliSec = $FFFFFFFF;

type
  SndSmfOptionsType = record
    // dwStartMilliSec and dwEndMilliSec are used as inputs to the function for sndSmfCmdPlay and as
    // outputs for sndSmfCmdDuration
    dwStartMilliSec: UInt32;                // 0 = "start from the beginning"
    dwEndMilliSec: UInt32;                  // sndSmfPlayAllMilliSec = "play the entire track";
                                            // the default is "play entire track" if this structure
                                            // is not passed in

    // The amplitude and interruptible fields are used only for sndSmfCmdPlay
    amplitude: UInt16;                      // relative volume: 0 - sndMaxAmp, inclusively;  the default is
                                            // sndMaxAmp if this structure is not passed in; if 0, the play will
                                            // be skipped and the call will return immediately

    interruptible: Boolean;                 // if true, sound play will be interrupted if
                                            // user interacts with the controls (digitizer, buttons, etc.);
                                            // if false, the paly will not be interrupted; the default behavior
                                            // is "interruptible" if this structure is not passed in

    reserved1: UInt8;
    reserved: UInt32;                       // RESERVED! -- MUST SET TO ZERO BEFORE PASSING
  end;
  SndSmfOptionsPtr = ^SndSmfOptionsType;

  SndSmfChanRangeType = record
    bFirstChan: UInt8;                         // first MIDI channel (0-15 decimal)
    bLastChan: UInt8;                          // last MIDI channel (0-15 decimal)
  end;
  SndSmfChanRangePtr = ^SndSmfChanRangeType;

(************************************************************
 * Sound Manager result codes
 * (sndErrorClass is defined in SystemMgr.h)
 *************************************************************)

const
  sndErrBadParam    = sndErrorClass or 1;
  sndErrBadChannel  = sndErrorClass or 2;
  sndErrMemory      = sndErrorClass or 3;
  sndErrOpen        = sndErrorClass or 4;
  sndErrQFull       = sndErrorClass or 5;
  sndErrQEmpty      = sndErrorClass or 6; // internal
  sndErrFormat      = sndErrorClass or 7; // unsupported data format
  sndErrBadStream   = sndErrorClass or 8; // invalid data stream
  sndErrInterrupted = sndErrorClass or 9; // play was interrupted

(********************************************************************
 * Sound Manager Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

//-------------------------------------------------------------------
// Initialization
//-------------------------------------------------------------------

// Initializes the Sound Manager.  Should only be called by
// Pilot initialization code.
function SndInit: Err; syscall sysTrapSndInit;

// Frees the Sound Manager.
//procedure SndFree; syscall sysTrapSndFree;

//-------------------------------------------------------------------
// API
//-------------------------------------------------------------------

// Sets default sound volume levels
//
// Any parameter may be passed as NULL
procedure SndSetDefaultVolume(var alarmAmpP, sysAmpP, defAmpP: UInt16); syscall sysTrapSndSetDefaultVolume;

// Gets default sound volume levels
//
// Any parameter may be passed as NULL
procedure SndGetDefaultVolume(var alarmAmpP, sysAmpP, masterAmpP: UInt16); syscall sysTrapSndGetDefaultVolume;

// Executes a sound command on the given sound channel (pass
// channelP = 0 to use the shared channel).
function SndDoCmd({SndChanPtr} channelP: Pointer; cmdP: SndCommandPtr; noWait: Boolean): Err; syscall sysTrapSndDoCmd;

// Plays one of several defined system beeps/sounds (see sndSysBeep...
// constants).
procedure SndPlaySystemSound(beepID: SndSysBeepType); syscall sysTrapSndPlaySystemSound;

// NEW FOR v3.0
// Performs an operation on a Standard MIDI File (SMF) Format #0
function SndPlaySmf(chanP: Pointer; cmd: SndSmfCmdEnum; var smfP: UInt8; selP: SndSmfOptionsPtr;
                    chanRangeP: SndSmfChanRangePtr; callbacksP: SndSmfCallbacksPtr;
                    bNoWait: Boolean): Err; syscall sysTrapSndPlaySmf;

// NEW FOR v3.0
// Creates a list of all midi records.  Useful for displaying in lists.
// For creator wildcard, pass creator=0;
function SndCreateMidiList(creator: UInt32; multipleDBs: Boolean; var wCountP: UInt16; var entHP: MemHandle): Boolean; syscall sysTrapSndCreateMidiList;

// NEW FOR v3.2
// Plays a MIDI sound which is read out of an open resource database
function SndPlaySmfResource(resType: UInt32; resID: Int16; volumeSelector: SystemPreferencesChoice): Err; syscall sysTrapSndPlaySmfResource;


// NEW FOR v4.0
// Plays MIDI sounds regardless of the how the interruptible flag is set
function SndPlaySmfIrregardless(chanP: Pointer; cmd: SndSmfCmdEnum; var smfP: UInt8;
                                var selP: SndSmfOptionsType; var chanRangeP: SndSmfChanRangeType;
                                var callbacksP: SndSmfCallbacksType; bNoWait: Boolean): Err; syscall sysTrapSndPlaySmfIrregardless;

function SndPlaySmfResourceIrregardless(resType: UInt32; resID: Int16; volumeSelector: SystemPreferencesChoice): Err; syscall sysTrapSndPlaySmfResourceIrregardless;

function SndInterruptSmfIrregardless: Err; syscall sysTrapSndInterruptSmfIrregardless;

(************************************************************
 * Sound Manager Macros
 *
 *************************************************************)

implementation

end.
