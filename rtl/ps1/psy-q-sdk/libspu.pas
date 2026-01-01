// Copyright (c) 1993, 1994, 1995, 1996 Sony Computer Entertainment Inc.
// All Rights Reserved.
//
// This file is part of ``PlayStation(R)'' Programmer Tool /
// Runtime Library.
//
// R & D Division, Sony Computer Entertainment Inc.
//
// $Id: libspu.h,v 1.80 1997/06/24 11:14:04 kaol Exp $
//
// $PSLibId: Run-time Library Release 4.6$
{$MODE OBJFPC}
unit libspu;
interface

const
//  CONSTANT
    SPU_SUCCESS         = 0;
    SPU_INVALID_ARGS    = -3;
    SPU_DIAG	        = -2;
    SPU_CHECK	        = -1;
    SPU_OFF		        = 0;
    SPU_ON		        = 1;
    SPU_CLEAR	        = 2;
    SPU_RESET	        = 3;
    SPU_DONT_CARE	    = 4;
    SPU_ALL		        = 0;
    SPU_CDONLY	        = 5;
    SPU_VOICEONLY	    = 6;
    SPU_CONT	        = 7;
    SPU_BIT		        = 8;
    SPU_NULL	        = 0;

// Macros below will be obsoleted.
    SpuDiag		        = SPU_DIAG;
    SpuCheck	        = SPU_CHECK;
    SpuOff		        = SPU_OFF;
    SpuOn		        = SPU_ON;
    SpuClear	        = SPU_CLEAR;
    SpuReset	        = SPU_RESET;
    SpuDontCare	        = SPU_DONT_CARE;
    SpuALL		        = SPU_ALL;
    SpuCDOnly	        = SPU_CDONLY;
    SpuVoiceOnly	    = SPU_VOICEONLY;
    SpuCont		        = SPU_CONT;
    SpuNull		        = SPU_NULL;

    SPU_OFF_ENV_ON	    = 2;
    SPU_ON_ENV_OFF	    = 3;

// Macros below will be obsoleted.
    SpuOffEnvOn	        = SPU_OFF_ENV_ON;
    SpuOnEnvOff	        = SPU_ON_ENV_OFF;

    SPU_ERROR	        = -1;
// Macros below will be obsoleted.
    SpuError	        = SPU_ERROR;

    SPU_TRANSFER_BY_DMA = 0;
    SPU_TRANSFER_BY_IO  = 1;
// Macros below will be obsoleted.
    SpuTransferByDMA    = SPU_TRANSFER_BY_DMA;
    SpuTransferByIO     = SPU_TRANSFER_BY_IO;
    SpuTransByDMA       = SpuTransferByDMA;
    SpuTransByIO        = SpuTransferByIO;

    SPU_TRANSFER_WAIT   = 1;
    SPU_TRANSFER_PEEK   = 0;
    SPU_TRANSFER_GLANCE = SPU_TRANSFER_PEEK;


// Voice designate
    SPU_00CH            = 1 shl 0;
    SPU_01CH            = 1 shl 1;
    SPU_02CH            = 1 shl 2;
    SPU_03CH            = 1 shl 3;
    SPU_04CH            = 1 shl 4;
    SPU_05CH            = 1 shl 5;
    SPU_06CH            = 1 shl 6;
    SPU_07CH            = 1 shl 7;
    SPU_08CH            = 1 shl 8;
    SPU_09CH            = 1 shl 9;
    SPU_10CH            = 1 shl 10;
    SPU_11CH            = 1 shl 11;
    SPU_12CH            = 1 shl 12;
    SPU_13CH            = 1 shl 13;
    SPU_14CH            = 1 shl 14;
    SPU_15CH            = 1 shl 15;
    SPU_16CH            = 1 shl 16;
    SPU_17CH            = 1 shl 17;
    SPU_18CH            = 1 shl 18;
    SPU_19CH            = 1 shl 19;

    SPU_20CH            = 1 shl 20;
    SPU_21CH            = 1 shl 21;
    SPU_22CH            = 1 shl 22;
    SPU_23CH            = 1 shl 23;

    SPU_0CH             = SPU_00CH;
    SPU_1CH             = SPU_01CH;
    SPU_2CH             = SPU_02CH;
    SPU_3CH             = SPU_03CH;
    SPU_4CH             = SPU_04CH;
    SPU_5CH             = SPU_05CH;
    SPU_6CH             = SPU_06CH;
    SPU_7CH             = SPU_07CH;
    SPU_8CH             = SPU_08CH;
    SPU_9CH             = SPU_09CH;

    SPU_ALLCH           = SPU_00CH or SPU_01CH or SPU_02CH or SPU_03CH or SPU_04CH or SPU_05CH or SPU_06CH or SPU_07CH or SPU_08CH or SPU_09CH or SPU_10CH or SPU_11CH or SPU_12CH or SPU_13CH or SPU_14CH or SPU_15CH or SPU_16CH or SPU_17CH or SPU_18CH or SPU_19CH or SPU_20CH or SPU_21CH or SPU_22CH or SPU_23CH;


function SPU_KEYCH(x: longint): longint;
function SPU_VOICECH(x: longint): longint;


// for Voice setting
const
    SPU_VOICE_VOLL		 = 1 shl  0;                //  volume (left)
    SPU_VOICE_VOLR		 = 1 shl  1;                // volume (right)
    SPU_VOICE_VOLMODEL	 = 1 shl  2;                // volume mode (left)
    SPU_VOICE_VOLMODER	 = 1 shl  3;                // volume mode (right)
    SPU_VOICE_PITCH		 = 1 shl  4;                // tone (pitch setting)
    SPU_VOICE_NOTE		 = 1 shl  5;                // tone (note setting)
    SPU_VOICE_SAMPLE_NOTE= 1 shl  6;                // waveform data sample note
    SPU_VOICE_WDSA		 = 1 shl  7;                // waveform data start address
    SPU_VOICE_ADSR_AMODE = 1 shl  8;                // ADSR Attack rate mode
    SPU_VOICE_ADSR_SMODE = 1 shl  9;                // ADSR Sustain rate mode
    SPU_VOICE_ADSR_RMODE = 1 shl 10;                // ADSR Release rate mode
    SPU_VOICE_ADSR_AR	 = 1 shl 11;                // ADSR Attack rate
    SPU_VOICE_ADSR_DR	 = 1 shl 12;                // ADSR Decay rate
    SPU_VOICE_ADSR_SR    = 1 shl 13;                // ADSR Sustain rate
    SPU_VOICE_ADSR_RR	 = 1 shl 14;                // ADSR Release rate
    SPU_VOICE_ADSR_SL	 = 1 shl 15;                // ADSR Sustain level
    SPU_VOICE_LSAX		 = 1 shl 16;                // start address for loop
    SPU_VOICE_ADSR_ADSR1 = 1 shl 17;                // ADSR adsr1 for `VagAtr'
    SPU_VOICE_ADSR_ADSR2 = 1 shl 18;                // ADSR adsr2 for `VagAtr'

    SPU_VOICE_DIRECT	 = 0;
    SPU_VOICE_LINEARIncN = 1;
    SPU_VOICE_LINEARIncR = 2;
    SPU_VOICE_LINEARDecN = 3;
    SPU_VOICE_LINEARDecR = 4;
    SPU_VOICE_EXPIncN	 = 5;
    SPU_VOICE_EXPIncR	 = 6;
    SPU_VOICE_EXPDec	 = 7;
    SPU_VOICE_EXPDecN	 = SPU_VOICE_EXPDec;
    SPU_VOICE_EXPDecR	 = SPU_VOICE_EXPDec;

    SPU_DECODED_FIRSTHALF=	0;
    SPU_DECODED_SECONDHALF=	1;
    SPU_DECODE_FIRSTHALF =	SPU_DECODED_FIRSTHALF;
    SPU_DECODE_SECONDHALF= 	SPU_DECODED_SECONDHALF;


    SPU_COMMON_MVOLL	 =	1 shl  0;               // master volume (left)
    SPU_COMMON_MVOLR	 =	1 shl  1;               // master volume (right)
    SPU_COMMON_MVOLMODEL =	1 shl  2;               // master volume mode (left)
    SPU_COMMON_MVOLMODER =	1 shl  3;               // master volume mode (right)
    SPU_COMMON_RVOLL	 =	1 shl  4;               // reverb volume (left)
    SPU_COMMON_RVOLR	 =	1 shl  5;               // reverb volume (right)
    SPU_COMMON_CDVOLL	 = 	1 shl  6;               // CD input volume (left)
    SPU_COMMON_CDVOLR	 =	1 shl  7;               // CD input volume (right)
    SPU_COMMON_CDREV	 =	1 shl  8;               // CD input reverb on/off
    SPU_COMMON_CDMIX	 =	1 shl  9;               // CD input on/off
    SPU_COMMON_EXTVOLL	 =	1 shl 10;               // external digital input volume (left)
    SPU_COMMON_EXTVOLR	 =	1 shl 11;               // external digital input volume (right)
    SPU_COMMON_EXTREV	 =	1 shl 12;               // external digital input reverb on/off
    SPU_COMMON_EXTMIX	 =	1 shl 13;               // external digital input on/off

// for Reverb setting
    SPU_REV_MODE		 = 1 shl 0;                 // mode setting
    SPU_REV_DEPTHL		 = 1 shl 1;                 // reverb depth (left)
    SPU_REV_DEPTHR		 = 1 shl 2;                 // reverb depth (right)
    SPU_REV_DELAYTIME	 = 1 shl 3;                 // Delay Time  (ECHO, DELAY only)
    SPU_REV_FEEDBACK	 = 1 shl 4;                 // Feedback    (ECHO only)

    SPU_REV_MODE_CHECK	 = -1;
    SPU_REV_MODE_OFF	 = 0;
    SPU_REV_MODE_ROOM	 = 1;
    SPU_REV_MODE_STUDIO_A= 2;
    SPU_REV_MODE_STUDIO_B= 3;
    SPU_REV_MODE_STUDIO_C= 4;
    SPU_REV_MODE_HALL	 = 5;
    SPU_REV_MODE_SPACE	 = 6;
    SPU_REV_MODE_ECHO	 = 7;
    SPU_REV_MODE_DELAY	 = 8;
    SPU_REV_MODE_PIPE	 = 9;
    SPU_REV_MODE_MAX	 = 10;

    SPU_REV_MODE_CLEAR_WA= $100;


//	Event flushing
    SPU_EVENT_KEY        = 1 shl 0;
    SPU_EVENT_PITCHLFO   = 1 shl 1;
    SPU_EVENT_NOISE      = 1 shl 2;
    SPU_EVENT_REVERB     = 1 shl 3;

    SPU_EVENT_ALL        = 0;

type
    SpuVolume = packed record
        left : smallint;
        right : smallint;
    end;

    SpuVoiceAttr = packed record
        voice : dword;		                        // set voice:
                                                    //  SpuSetVoiceAttr: each voice is a bit array
                                                    //  SpuGetVoiceAttr: voice is a bit value
        mask : dword;		                        // settings attribute bit (invalid with Get)
        volume : SpuVolume;		                    // volume
        volmode : SpuVolume;	                    // volume mode
        volumex : SpuVolume;	                    // current volume (invalid with Set)
        pitch : word;		                        // tone (pitch setting)
        note : word;		                        // tone (note setting)
        sample_note : word;	                        // tone (note setting)
        envx : smallint;		                    // current envelope value (invalid with Set)
        addr : dword;		                        // waveform data start address
        loop_addr : dword;	                        // loop start address
        a_mode : longint;		                    // Attack rate mode
        s_mode : longint;		                    // Sustain rate mode
        r_mode : longint;		                    // Release rate mode
        ar : word;		                            // Attack rate
        dr : word;		                            // Decay rate
        sr : word;		                            // Sustain rate
        rr : word;		                            // Release rate
        sl : word;		                            // Sustain level
        adsr1 : word;		                        // adsr1 for `VagAtr'
        adsr2 : word;		                        // adsr2 for `VagAtr'
    end;
    PSpuVoiceAttr = ^SpuVoiceAttr;

    SpuLVoiceAttr = packed record
        voiceNum : smallint;		                // voice number
        pad : smallint;			                    // padding
    	attr : SpuVoiceAttr;		                // voice attribute
    end;
    PSpuLVoiceAttr = ^SpuLVoiceAttr;

    SpuReverbAttr = packed record
        mask : dword;	                            // settings mask
        mode : longint;	                            // reverb mode
        depth : SpuVolume;	                        // reverb depth
        delay : longint;	                        // Delay Time  (ECHO, DELAY only)
        feedback : longint;                         // Feedback    (ECHO only)
    end;
    PSpuReverbAttr = ^SpuReverbAttr;

const
    SPU_DECODEDDATA_SIZE = $200;
    SPU_DECODEDATA_SIZE  = SPU_DECODEDDATA_SIZE;

type
    SpuDecodedData = packed record
        cd_left  : array [0..SPU_DECODEDDATA_SIZE - 1] of smallint;
        cd_right : array [0..SPU_DECODEDDATA_SIZE - 1] of smallint;
        voice1   : array [0..SPU_DECODEDDATA_SIZE - 1] of smallint;
        voice3   : array [0..SPU_DECODEDDATA_SIZE - 1] of smallint;
    end;
    SpuDecodeData = SpuDecodedData;
    PSpuDecodedData = ^SpuDecodedData;

    SpuExtAttr = packed record
        volume : SpuVolume;		                    // volume
        reverb : longint;		                    // reverb on/off
        mix : longint;		                        // mixing on/off
    end;

    SpuCommonAttr = packed record
        mask : dword;	                            // settings mask
        mvol : SpuVolume;	                        // master volume
        mvolmode : SpuVolume;                       // master volume mode
        mvolx : SpuVolume;	                        // current master volume
        cd : SpuExtAttr;	                        // CD input attributes
        ext : SpuExtAttr;	                        // external digital input attributes
    end;
    PSpuCommonAttr = ^SpuCommonAttr;

    SpuIRQCallbackProc = procedure;
    SpuTransferCallbackProc = procedure;

// for SPU Malloc (used in SpuInitMalloc())
const
    SPU_MALLOC_RECSIZ   = 8;

// User specifiable global environment
type
    SpuEnv = packed record
        mask : dword;
        queueing : dword;
    end;
    PSpuEnv = ^SpuEnv;

const
    SPU_ENV_EVENT_QUEUEING	= 01 shl 0;


procedure SpuInit; stdcall; external;
procedure SpuInitHot; stdcall; external;
procedure SpuStart; stdcall; external;
procedure SpuQuit; stdcall; external;
function SpuSetMute(on_off: longint): longint; stdcall; external;
function SpuGetMute: longint; stdcall; external;
procedure SpuSetEnv(env: PSpuEnv); stdcall; external;

function SpuSetNoiseClock(n_clock: longint): longint; stdcall; external;
function SpuGetNoiseClock: longint; stdcall; external;
function SpuSetNoiseVoice(on_off: longint; voice_bit: dword): dword; stdcall; external;
function SpuGetNoiseVoice: dword; stdcall; external;

function SpuSetReverb(on_off: longint): longint; stdcall; external;
function SpuGetReverb: longint; stdcall; external;
function SpuSetReverbModeParam(attr: PSpuReverbAttr): longint; stdcall; external;
procedure SpuGetReverbModeParam(attr: PSpuReverbAttr); stdcall; external;
function SpuSetReverbDepth(attr: PSpuReverbAttr): longint; stdcall; external;
function SpuReserveReverbWorkArea(on_off: longint): longint; stdcall; external;
function SpuIsReverbWorkAreaReserved(on_off: longint): longint; stdcall; external;
function SpuSetReverbVoice(on_off: longint; voice_bit: dword): dword; stdcall; external;
function SpuGetReverbVoice: dword; stdcall; external;
function SpuClearReverbWorkArea(mode: longint): longint; stdcall; external;

function SpuWrite(addr: pointer; size: dword): dword; stdcall; external;
function SpuWrite0(size: dword): dword; stdcall; external;
function SpuRead(addr: pointer; size: dword): dword; stdcall; external;
function SpuSetTransferMode(mode: longint): longint; stdcall; external;
function SpuSetTransMode(mode: longint):longint; // NO EXTERNAL
function SpuGetTransferMode: longint; stdcall; external;
function SpuGetTransMode: longint; // NO EXTERNAL
function SpuSetTransferStartAddr(addr: dword): dword; stdcall; external;
function SpuSetTransStartAddr(addr: dword): dword; // NO EXTERNAL
function SpuGetTransferStartAddr: dword; stdcall; external;
function SpuGetTransStartAddr: dword; // NO EXTERNAL
function SpuWritePartly(addr: pointer; size: dword): dword; stdcall; external;

function SpuIsTransferCompleted(flag: longint): longint; stdcall; external;
function SpuSetTransferCallback(func: SpuTransferCallbackProc): SpuTransferCallbackProc; stdcall; external;
function SpuReadDecodedData(d_data: PSpuDecodedData; flag: longint): longint; stdcall; external;
function SpuReadDecodeData(d_data: PSpuDecodedData; flag: longint): longint; // NO EXTERNAL

function SpuSetIRQ(on_off: longint): longint; stdcall; external;
function SpuGetIRQ: longint; stdcall; external;
function SpuSetIRQAddr(addr: dword): dword; stdcall; external;
function SpuGetIRQAddr: dword; stdcall; external;
function SpuSetIRQCallback(func: SpuIRQCallbackProc): SpuIRQCallbackProc; stdcall; external;

procedure SpuSetVoiceAttr(arg: PSpuVoiceAttr); stdcall; external;
procedure SpuGetVoiceAttr(arg: PSpuVoiceAttr); stdcall; external;
procedure SpuSetKey(on_off: longint; voice_bit: dword); stdcall; external;
procedure SpuSetKeyOnWithAttr(attr: PSpuVoiceAttr); stdcall; external;
function SpuGetKeyStatus(voice_bit: dword): longint; stdcall; external;
procedure SpuGetAllKeysStatus(status: pointer); stdcall; external;
function SpuFlush(ev: dword): dword; stdcall; external;

function SpuSetPitchLFOVoice(on_off: longint; voice_bit: dword): dword; stdcall; external;
function SpuGetPitchLFOVoice: dword; stdcall; external;

procedure SpuSetCommonAttr(attr: PSpuCommonAttr); stdcall; external;
procedure SpuGetCommonAttr(attr: PSpuCommonAttr); stdcall; external;

function SpuInitMalloc(num: longint; top: pointer): longint; stdcall; external;
function SpuMalloc(size: longint): longint; stdcall; external;
function SpuMallocWithStartAddr(addr: dword; size: longint): longint; stdcall; external;
procedure SpuFree(addr: dword); stdcall; external;

function SpuRGetAllKeysStatus(min_, max_: longint; status: pointer): longint; stdcall; external;
function SpuRSetVoiceAttr(min_, max_: longint; arg: PSpuVoiceAttr): longint; stdcall; external;

procedure SpuNSetVoiceAttr(vNum: longint; arg: PSpuVoiceAttr); stdcall; external;
procedure SpuNGetVoiceAttr(vNum: longint; arg: PSpuVoiceAttr); stdcall; external;

procedure SpuLSetVoiceAttr(num: longint; argList: PSpuLVoiceAttr); stdcall; external;

procedure SpuSetVoiceVolume(vNum: longint; volL, volR: smallint); stdcall; external;
procedure SpuSetVoiceVolumeAttr(vNum: longint; volL, volR: smallint; volModeL, volModeR: smallint); stdcall; external;
procedure SpuSetVoicePitch(vNum: longint; pitch: word); stdcall; external;
procedure SpuSetVoiceNote(vNum: longint; note: word); stdcall; external;
procedure SpuSetVoiceSampleNote(vNum: longint; sampleNote: word); stdcall; external;
procedure SpuSetVoiceStartAddr(vNum: longint; startAddr: dword); stdcall; external;
procedure SpuSetVoiceLoopStartAddr(vNum: longint; lsa: dword); stdcall; external;
procedure SpuSetVoiceAR(vNum: longint; AR: word); stdcall; external;
procedure SpuSetVoiceDR(vNum: longint; DR: word); stdcall; external;
procedure SpuSetVoiceSR(vNum: longint; SR: word); stdcall; external;
procedure SpuSetVoiceRR(vNum: longint; RR: word); stdcall; external;
procedure SpuSetVoiceSL(vNum: longint; SL: word); stdcall; external;
procedure SpuSetVoiceARAttr(vNum: longint; AR: word; ARmode: longint); stdcall; external;
procedure SpuSetVoiceSRAttr(vNum: longint; SR: word; SRmode: longint); stdcall; external;
procedure SpuSetVoiceRRAttr(vNum: longint; RR: word; RRmode: longint); stdcall; external;
procedure SpuSetVoiceADSR(vNum: longint; AR, DR, SR, RR, SL: word); stdcall; external;
procedure SpuSetVoiceADSRAttr(vNum: longint; AR, DR, SR, RR, SL: word; ARmode, SRmode, RRmode: longint); stdcall; external;

procedure SpuGetVoiceVolume(vNum: longint; volL, volR: psmallint); stdcall; external;
procedure SpuGetVoiceVolumeAttr(vNum: longint; volL, volR, volModeL, volModeR: psmallint); stdcall; external;
procedure SpuGetVoiceVolumeX(vNum: longint; volXL, volXR: psmallint); stdcall; external;
procedure SpuGetVoicePitch(vNum: longint; pitch: pword); stdcall; external;
procedure SpuGetVoiceNote(vNum: longint; note:pword); stdcall; external;
procedure SpuGetVoiceSampleNote(vNum: longint; sampleNote: pword); stdcall; external;
procedure SpuGetVoiceEnvelope(vNum: longint; envx: psmallint); stdcall; external;
procedure SpuGetVoiceStartAddr(vNum: longint; startAddr: pword); stdcall; external;
procedure SpuGetVoiceLoopStartAddr(vNum: longint; loopStartAddr: pword); stdcall; external;
procedure SpuGetVoiceAR(vNum: longint; AR: pword); stdcall; external;
procedure SpuGetVoiceDR(vNum: longint; DR: pword); stdcall; external;
procedure SpuGetVoiceSR(vNum: longint; SR: pword); stdcall; external;
procedure SpuGetVoiceRR(vNum: longint; RR: pword); stdcall; external;
procedure SpuGetVoiceSL(vNum: longint; SL: pword); stdcall; external;
procedure SpuGetVoiceARAttr(vNum: longint; AR: pword; ARmode: plongint); stdcall; external;
procedure SpuGetVoiceSRAttr(vNum: longint; SR: pword; SRmode: plongint); stdcall; external;
procedure SpuGetVoiceRRAttr(vNum: longint; RR: pword; RRmode: plongint); stdcall; external;
procedure SpuGetVoiceADSR(vNum: longint; AR, DR, SR, RR, SL: pword); stdcall; external;
procedure SpuGetVoiceADSRAttr(vNum: longint; AR, DR, SR, RR, SL: pword; ARmode, SRmode, RRmode: plongint); stdcall; external;
procedure SpuGetVoiceEnvelopeAttr(vNum: longint; keyStat: plongint; envx: psmallint); stdcall; external;

procedure SpuSetCommonMasterVolume(mvol_left, mvol_right: smallint); stdcall; external;
procedure SpuSetCommonMasterVolumeAttr(mvol_left, mvol_right, mvolmode_left, mvolmode_right: smallint); stdcall; external;
procedure SpuSetCommonCDMix(cd_mix: longint); stdcall; external;
procedure SpuSetCommonCDVolume(cd_left, cd_right: smallint); stdcall; external;
procedure SpuSetCommonCDReverb(cd_reverb: longint); stdcall; external;

procedure SpuGetCommonMasterVolume(mvol_left, mvol_right: psmallint); stdcall; external;
procedure SpuGetCommonMasterVolumeX(mvolx_left, mvolx_right: psmallint); stdcall; external;
procedure SpuGetCommonMasterVolumeAttr(mvol_left, mvol_right, mvolmode_left, mvolmode_right: psmallint); stdcall; external;
procedure SpuGetCommonCDMix(cd_mix: plongint); stdcall; external;
procedure SpuGetCommonCDVolume(cd_left, cd_right: psmallint); stdcall; external;
procedure SpuGetCommonCDReverb(cd_reverb: plongint); stdcall; external;

function SpuSetReverbModeType(mode: longint): longint; stdcall; external;
procedure SpuSetReverbModeDepth(depth_left, depth_right: smallint); stdcall; external;
procedure SpuSetReverbModeDelayTime(del: longint); stdcall; external;
procedure SpuSetReverbModeFeedback(feedback: longint); stdcall; external;
procedure SpuGetReverbModeType(mode: plongint); stdcall; external;
procedure SpuGetReverbModeDepth(depth_left, depth_right: psmallint); stdcall; external;
procedure SpuGetReverbModeDelayTime(del: plongint); stdcall; external;
procedure SpuGetReverbModeFeedback(feedback: plongint); stdcall; external;
procedure SpuSetESA(revAddr: longint); stdcall; external;


const
    SPU_ST_NOT_AVAILABLE    = 0;
    SPU_ST_ACCEPT           = 1;

    SPU_ST_ERROR            = -1;
    SPU_ST_INVALID_ARGUMENT = -2;
    SPU_ST_WRONG_STATUS     = -3;

    SPU_ST_STOP             = 2;
    SPU_ST_IDLE             = 3;
    SPU_ST_PREPARE          = 4;
    SPU_ST_START            = 5;
    SPU_ST_PLAY             = 6;
    SPU_ST_TRANSFER         = 7;
    SPU_ST_FINAL            = 8;


// VAG's header size
const
    SPU_ST_VAG_HEADER_SIZE  = $30;

type
    SpuStVoiceAttr = packed record
        status : byte;		                        // stream status
        pad1 : byte;		                        // padding
        pad2 : byte;			                    // padding
        pad3 : byte;			                    // padding
        last_size : longint;		                // the size of last transferring (last_size <= (size / 2))
        buf_addr : dword;	                        // The start address of stream buffer
        data_addr : dword;	                        // The start address of SPU streaming data in main memory
    end;

    SpuStEnv = packed record
        size : longint;			                    // The size of stream buffer
        low_priority : longint;		                // transfer priority
        voice : array [0..23] of SpuStVoiceAttr;
    end;
    PSpuStEnv = ^SpuStEnv;

type
    SpuStCallbackProc = function(d: dword; l: longint): pointer;

function SpuStInit(x: longint): PSpuStEnv; stdcall; external;
function SpuStQuit: longint; stdcall; external;
function SpuStGetStatus: longint; stdcall; external;
function SpuStGetVoiceStatus: dword; stdcall; external;
function SpuStTransfer(flag: longint; voice_bit: dword): longint; stdcall; external;
function SpuStSetPreparationFinishedCallback(func: SpuStCallbackProc): SpuStCallbackProc; stdcall; external;
function SpuStSetTransferFinishedCallback(func: SpuStCallbackProc): SpuStCallbackProc; stdcall; external;
function SpuStSetStreamFinishedCallback(func: SpuStCallbackProc): SpuStCallbackProc; stdcall; external;


implementation

function SPU_KEYCH(x: longint): longint;
begin
    result:= 1 shl x;
end;

function SPU_VOICECH(x: longint): longint;
begin
    result:= SPU_KEYCH(x);
end;

function SpuSetTransMode(mode: longint): longint;
begin
    result:= SpuSetTransferMode(mode);
end;

function SpuGetTransMode: longint;
begin
    result:= SpuGetTransferMode;
end;

function SpuSetTransStartAddr(addr: dword): dword;
begin
    result:= SpuSetTransferStartAddr(addr);
end;

function SpuGetTransStartAddr: dword;
begin
    result:= SpuGetTransferStartAddr;
end;


function SpuReadDecodeData(d_data: PSpuDecodedData; flag: longint): longint;
begin
     result:= SpuReadDecodedData(d_data, flag);
end;

begin
end.