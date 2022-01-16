{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Free Pascal development team

    PalmOS specific structures and definitions to be used by the RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
type
  MemPtr = Pointer;
  MemHandle = Pointer;
  Err = Word;
  DmOpenRef = Pointer;

type
  DmResType = UInt32;
  DmResID = UInt16;

type
  SysAppInfoTag = record
    cmd: Int16;            // command code for app
    cmdPBP: MemPtr;        // cmd ParamBlock
    launchFlags: UInt16;   // launch flags

    taskID: UInt32;        // AMX task ID of task that app runs in
    codeH: MemHandle;      // code MemHandle of the main code segment
    dbP: DmOpenRef;        // Application database access MemPtr of App
    stackP: ^UInt8;        // stack chunk for the App
    globalsChunkP: ^UInt8; // globals chunk for the App

    memOwnerID: UInt16;    // owner ID for Memory Manager chunks
    dmAccessP: MemPtr;     // pointer to linked list of opened DB's
    dmLastErr: Err;        // Last error from Data Manager
    errExceptionP: MemPtr; // ErrTry,Catch exception list

    // PalmOS v3.0 fields begin here
    a5Ptr: ^UInt8;         // A5 MemPtr for this app
    stackEndP: ^UInt8;     // stack chunk end for the App (last byte)
    globalEndP: ^UInt8;    // global chunk end for the App (last byte)
    rootP: ^SysAppInfoType;// Points to the SysAppInfoType first
                           // allocated for this thread.
    extraP: MemPtr;        // unused MemPtr for the App.
  end;
  SysAppInfoType = SysAppInfoTag;
  SysAppInfoPtr = ^SysAppInfoType;


const
  sysAppLaunchCmdNormalLaunch = 0;

const
  sysAppLaunchFlagNewThread = $01;  // create a new thread for application
                                    //  - implies sysAppLaunchFlagNewStack
  sysAppLaunchFlagNewStack = $02;   // create separate stack for application
  sysAppLaunchFlagNewGlobals = $04; // create new globals world for application
                                    //  - implies new owner ID for Memory chunks
  sysAppLaunchFlagUIApp = $08;      // notifies launch routine that this is a UI app being
                                    //  launched.

const
  sysTrapSysAppStartup = $A08F;
  sysTrapSysAppExit = $A090;

function SysAppStartup(var appInfoPP: SysAppInfoPtr; var prevGlobalsP, globalsPtrP: MemPtr): UInt16; syscall sysTrapSysAppStartup;
function SysAppExit(appInfoP: SysAppInfoPtr; prevGlobalsP, globalsP: MemPtr): UInt16; syscall sysTrapSysAppExit;

const
  sysTrapSndPlaySystemSound = $A234;
  sndError = 3;

procedure SndPlaySystemSound(beepID: Word); syscall sysTrapSndPlaySystemSound;

const
  sysTrapMemChunkFree = $A012;
  sysTrapMemPtrNew = $A013;
  sysTrapMemHandleLock = $A021;
  sysTrapMemHandleUnlock = $A022;

function MemPtrNew(size: UInt32): MemPtr; syscall sysTrapMemPtrNew;
function MemPtrFree(chunkDataP: MemPtr): Err; syscall sysTrapMemChunkFree;
function MemHandleLock(h: MemHandle): MemPtr; syscall sysTrapMemHandleLock;
function MemHandleUnlock(h: MemHandle): Err; syscall sysTrapMemHandleUnlock;

const
  sysTrapDmGet1Resource = $A060;
  sysTrapDmReleaseResource = $A061;

function DmGet1Resource(type_: DmResType; resID: DmResID): MemHandle; syscall sysTrapDmGet1Resource;
function DmReleaseResource(resourceH: MemHandle): Err; syscall sysTrapDmReleaseResource;

const
  sysTrapWinDrawChars = $A220;

procedure WinDrawChars(const chars: PChar; len: SmallInt; x, y: SmallInt); syscall sysTrapWinDrawChars;

{$PACKRECORDS DEFAULT}
