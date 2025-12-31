//
// 	Copyright (C) 1994 by Sony Computer Entertainment Inc.
//				          All Rights Reserved.
//
//	Sony Computer Entertainment Inc. Development Department
//
// Run-time Library Release 4.6$
{$MODE OBJFPC}
unit libsnd;
interface

//include <sys/types.h>

const
	SSPLAY_INFINITY  = 0;
	SS_NOTICK	 = $1000;
	SS_NOTICK0	 = 0;
	SS_TICK60	 = 1;
	SS_TICK240	 = 2;
	SS_TICK120	 = 3;
	SS_TICK50	 = 4;
	SS_TICKVSYNC	 = 5;
	SS_TICKMODE_MAX  = 6;
	SSPLAY_PAUSE     = 0;
	SSPLAY_PLAY      = 1;
	SS_SOFF          = 0;     
	SS_SON           = 1;     
	SS_MIX           = 0;   
	SS_REV           = 1;   
	SS_SERIAL_A      = 0;
	SS_SERIAL_B      = 1;    
	SS_MUTE_OFF      = 0;    
	SS_MUTE_ON       = 1;    

	SS_IMEDIATE 	    = 0;
	SS_IMMEDIATE        = 0;
	SS_WAIT_COMPLETED   = 1;

	SS_REV_TYPE_OFF        = 0;
	SS_REV_TYPE_ROOM       = 1;
	SS_REV_TYPE_STUDIO_A   = 2;
	SS_REV_TYPE_STUDIO_B   = 3;
	SS_REV_TYPE_STUDIO_C   = 4;
	SS_REV_TYPE_HALL       = 5;
	SS_REV_TYPE_SPACE      = 6;
	SS_REV_TYPE_ECHO       = 7;
	SS_REV_TYPE_DELAY      = 8;
	SS_REV_TYPE_PIPE       = 9;
	SSSKIP_TICK    	       = 0;
	SSSKIP_NOTE4   	       = 1; 
	SSSKIP_NOTE8   	       = 2; 
	SSSKIP_BAR     	       = 3;

	SS_SEQ_TABSIZ          = 176;

	SND_VOLL  	       = 1;
	SND_VOLR  	       = 2;
	SND_ADSR1 	       = 4;
	SND_ADSR2 	       = 8;
	SND_ADDR  	       = 16;
	SND_PITCH 	       = 32;

	NULL 		       = 0;


// Vag & Vab Structure 
type
	VabHdr = packed record			// VAB Bank Headdings
		form : longint;          	// always 'VABp'
		ver : longint;           	// VAB file version number
		id : longint;            	// VAB id
		fsize : dword;         		// VAB file size
		reserved0 : word;     		// system reserved
		ps : word;            		// # of the programs in this bank
		ts : word;            		// # of the tones in this bank
		vs : word;            		// # of the vags in this bank
		mvol : byte;          		// master volume for this bank
		pan : byte;           		// master panning for this bank
		attr1 : byte;         		// bank attributes1
		attr2 : byte;         		// bank attributes2
		reserved1 : dword;     		// system reserved
	end;					// 32 byte
	PVabHdr = ^VabHdr;

	ProgAtr = packed record        		// Program Headings
		tones : byte;          		// # of tones
		mvol : byte;           		// program volume
		prior : byte;          		// program priority
		mode : byte;           		// program mode
		mpan : byte;           		// program pan
		reserved0 : byte;      		// system reserved
		attr : smallint;           	// program attribute
		reserved1 : dword;      	// system reserved
		reserved2 : dword;      	// system reserved
	end;					// 16 byte
	PProgAtr = ^ProgAtr;

	VagAtr = packed record          	// VAG Tone Headings
		prior : byte;         		// tone priority
		mode : byte;          		// play mode
		vol : byte;           		// tone volume
		pan : byte;           		// tone panning
		center : byte;        		// center note
		shift : byte;         		// center note fine tune
		min : byte;           		// minimum note limit
		max : byte;           		// maximum note limit
		vibW : byte;          		// vibrate depth
		vibT : byte;          		// vibrate duration
		porW : byte;          		// portamento depth
		porT : byte;          		// portamento duration
		pbmin : byte;         		// under pitch bend max
		pbmax : byte;         		// upper pitch bend max
		reserved1 : byte;     		// system reserved
		reserved2 : byte;     		// system reserved
		adsr1 : word;         		// adsr1
		adsr2 : word;         		// adsr2
		prog : smallint;          	// parent program
		vag : smallint;           	// vag reference
		reserved : array [0..3] of smallint;   // system reserved
	end; 					// 32 byte
	PVagAtr = ^VagAtr;

// Volume Structure
	SndVolume = packed record
        	left : word;
        	right : word;
        end;
        PSndVolume = ^SndVolume;

	SndVolume2 = packed record
		left : smallint;
		right : smallint;
	end;

	SndRegisterAttr = packed record
		volume : SndVolume2;
		pitch : smallint;
		mask : smallint;
		addr : smallint;
		adsr1 : smallint;
		adsr2 : smallint;
	end;
	PSndRegisterAttr = ^SndRegisterAttr;

	SndVoiceStats = packed record
		vagId : smallint;
		vabId : smallint;
		pitch : word;
		note : smallint;
		tone : smallint;
		prog_num : smallint;
		prog_actual : smallint;
		vol : smallint;
		pan : smallint;
	end;
	PSndVoiceStats = ^SndVoiceStats;

// CallBack
 SsMarkCallbackProc = procedure(ac_no, tr_no: smallint; data: smallint);


function SsVabOpenHead(addr: pointer; vab_id: smallint): smallint; stdcall; external;
function SsVabOpenHeadSticky(addr: pointer; vab_id: smallint; sbaddr: dword): smallint; stdcall; external;
function SsVabTransBody(addr: pointer; vab_id: smallint): smallint; stdcall; external;
function SsVabTransBodyPartly(addr: pointer; bufsize: dword; vab_id: smallint): smallint; stdcall; external;
function SsVabTransfer(vh_add, vb_addr: pointer; vab_id: smallint; i_flag: smallint): smallint; stdcall; external;
function SsVabTransCompleted(immediateFlag: smallint): smallint; stdcall; external;
procedure SsVabClose(vab_id: smallint); stdcall; external;

procedure SsInit; stdcall; external;
procedure SsInitHot; stdcall; external;
procedure SsSetTableSize(table: pointer; s_max, t_max: smallint); stdcall; external;
procedure SsSetTickMode(tick_mode: longint); stdcall; external;
function SsSetTickCallback(cb: pointer): longint; stdcall; external;
procedure SsStart; stdcall; external;
procedure SsStart2; stdcall; external;
procedure SsEnd; stdcall; external;
procedure SsQuit; stdcall; external;

procedure SsSeqCalledTbyT; stdcall; external;

function SsSeqOpen(addr: pointer; vab_id: smallint): smallint; stdcall; external;
procedure SsSeqPlay(seq_access_num: smallint; play_mode: byte; l_count: smallint); stdcall; external;
procedure SsSeqPlayPtoP(access_num: smallint; seq_num: smallint; start_point, end_point: pointer; play_mode: byte; l_count: smallint); stdcall; external;
procedure SsSeqPause(seq_access_num: smallint); stdcall; external;
procedure SsSeqReplay(seq_access_num: smallint); stdcall; external;
function SsSeqSkip(access_num, seq_num: smallint; _unit: byte; count: smallint): longint; stdcall; external;
procedure SsSeqStop(seq_access_num: smallint); stdcall; external;
procedure SsSeqSetVol(seq_access_num: smallint; voll, volr: smallint); stdcall; external;
procedure SsSeqSetNext(seq_access_num1, seq_access_num2: smallint); stdcall; external;
procedure SsSeqSetCrescendo(seq_access_num: smallint; vol: smallint; v_time: longint); stdcall; external; 
procedure SsSeqSetDecrescendo(seq_access_num: smallint; vol: smallint; v_time: longint); stdcall; external;
procedure SsSeqSetAccelerando(seq_access_num: smallint; tempo: longint; v_time: longint); stdcall; external;
procedure SsSeqSetRitardando(seq_access_num: smallint; tempo: longint; v_time: longint); stdcall; external;
procedure SsSeqClose(seq_access_num: smallint); stdcall; external;

function SsSepOpen(addr: pointer; vab_id: smallint; seq_num: smallint): smallint; stdcall; external;
procedure SsSepPlay(sep_access_num: smallint; seq_num: smallint; play_mode: byte; l_count: smallint); stdcall; external;
procedure SsSepPause(sep_access_num, seq_num: smallint); stdcall; external;
procedure SsSepReplay(sep_access_num, seq_num: smallint); stdcall; external;
procedure SsSepStop(sep_access_num, seq_num: smallint); stdcall; external;
procedure SsSepSetVol(sep_access_num, seq_num: smallint; voll, volr: smallint); stdcall; external;
procedure SsSepSetCrescendo(sep_access_num, seq_num: smallint; vol: smallint; v_time: longint); stdcall; external;
procedure SsSepSetDecrescendo(sep_access_num, seq_num: smallint; vol: smallint; v_time: longint); stdcall; external;
procedure SsSepSetAccelerando(sep_access_num, seq_num: smallint; tempo: longint; v_time: longint); stdcall; external;
procedure SsSepSetRitardando(sep_access_num, seq_num: smallint; tempo: longint; v_time: longint); stdcall; external;
procedure SsSepClose(sep_access_num: smallint); stdcall; external;

function SsVoKeyOn(vab_pro, pitch: longint; volL,volR: byte): longint; stdcall; external;
function SsVoKeyOff(vab_pro, pitch: longint): longint; stdcall; external;

procedure SsSetMVol(voll, volr: smallint); stdcall; external;
procedure SsGetMVol(m_vol: PSndVolume); stdcall; external;
procedure SsSetRVol(voll, volr: smallint); stdcall; external;
procedure SsGetRVol(r_vol: PSndVolume); stdcall; external;
procedure SsSetMute(mode: byte); stdcall; external;
function SsGetMute: byte; stdcall; external;
procedure SsSetSerialAttr(s_num, attr, mode: byte); stdcall; external;
function SsGetSerialAttr(s_num, attr: byte): byte; stdcall; external;
procedure SsSetSerialVol(s_num:  byte; voll, volr: smallint); stdcall; external;
procedure SsGetSerialVol(s_num: byte; s_vol: PSndVolume); stdcall; external;
procedure SsSetNck(n_clock: smallint); stdcall; external;
function SsGetNck: smallint; stdcall; external;
procedure SsSetNoiseOn(voll, volr: smallint); stdcall; external;  
procedure SsSetNoiseOff; stdcall; external;
procedure SsSetMono; stdcall; external;
procedure SsSetStereo; stdcall; external;      
procedure SsSetTempo(access_num, seq_num: smallint; tempo: smallint); stdcall; external;
procedure SsSetLoop(access_num, seq_num: smallint; I_count: smallint); stdcall; external;
function SsIsEos(access_num, seq_num: smallint): smallint; stdcall; external;
procedure SsPlayBack(access_num, seq_num: smallint; I_count: smallint); stdcall; external;
procedure SsSetMarkCallback(access_num, seq_num: smallint; proc: SsMarkCallbackProc); stdcall; external;
function SsSetReservedVoice(voices: byte): byte; stdcall; external;

function SsUtKeyOn(vabId, prog, tone, note, fine, voll, volr: smallint): smallint; stdcall; external;
function SsUtKeyOff(voice, vabId, prog, tone, note: smallint): smallint; stdcall; external;
function SsUtKeyOnV(voice, vabId, prog, tone, fine, voll, volr: smallint): smallint; stdcall; external;
function SsUtKeyOffV(voice: smallint): smallint; stdcall; external;
function SsUtPitchBend(voice, vabId, prog, note, pbend: smallint): smallint; stdcall; external;
function SsUtChangePitch(voice, vabId, prog, old_note, old_fine, new_note, new_fine: smallint): smallint; stdcall; external;
function SsUtChangeADSR(vc, vabId, prog, old_note: smallint;  adsr1, adsr2: word): smallint; stdcall; external;
function SsUtSetVabHdr(vabId, vabhdrptr: PVabHdr): smallint; stdcall; external;
function SsUtGetVabHdr(vabId, vabhdrptr: PVabHdr): smallint; stdcall; external;
function SsUtSetProgAtr(vabId, progNum: smallint; progatrptr: PProgAtr): smallint; stdcall; external;
function SsUtGetProgAtr(vabId, progNum: smallint; progatrptr: PProgAtr): smallint; stdcall; external;
function SsUtSetVagAtr(vabId, progNum, toneNum: smallint; vagatrptr: PVagAtr): smallint; stdcall; external;
function SsUtGetVagAtr(vabId, progNum, toneNum: smallint; vagatrptr: PVagAtr): smallint; stdcall; external;
function SsUtSetDetVVol(vc: smallint; detvoll, detvolr: smallint): smallint; stdcall; external;
function SsUtGetDetVVol(vc: smallint; detvoll, detvolr: psmallint): smallint; stdcall; external;
function SsUtSetVVol(vc: smallint; detvoll, detvolr: smallint): smallint; stdcall; external;
function SsUtGetVVol(vc: smallint; detvoll, detvolr: psmallint): smallint; stdcall; external;
function SsUtAutoVol(vc: smallint; start_vol, end_vol, delta_time: smallint): smallint; stdcall; external;
function SsUtAutoPan(vc: smallint; start_pan, end_pan, delta_time: smallint): smallint; stdcall; external;
procedure SsUtReverbOn; stdcall; external;
procedure SsUtReverbOff; stdcall; external;
function SsUtSetReverbType(_type: smallint): smallint; stdcall; external;
function SsUtGetReverbType: smallint; stdcall; external;
procedure SsUtSetReverbDepth(ldepth, rdepth: smallint); stdcall; external;
procedure SsUtSetReverbFeedback(feedback: smallint); stdcall; external;
procedure SsUtSetReverbDelay(delay: smallint); stdcall; external;
procedure SsUtAllKeyOff(mode: smallint); stdcall; external;
procedure SsSetAutoKeyOffMode(mode: smallint); stdcall; external;
procedure SsUtFlush; stdcall; external;
function SsVabFakeHead(addr: pointer; vabid: smallint; sbaddr: dword): smallint; stdcall; external;
function SsVabFakeBody(vabid: smallint): smallint; stdcall; external;
function SsUtGetVBaddrInSB(vabid: smallint): dword; stdcall; external;
function SsUtGetVagAddr(vabId, vagId: smallint): longint; stdcall; external;
function SsUtGetVagAddrFromTone(vabId, progId, toneId: smallint): dword; stdcall; external;
procedure SsSetNext(ac_no1, tr_no1, ac_no2, tr_no2: smallint); stdcall; external;
procedure SsSeqGetVol(access_num, seq_num: smallint; voll, volr: psmallint); stdcall; external;
procedure SsChannelMute(acn, trn: smallint; channels: longint); stdcall; external;
function SsSeqOpenJ(addr: pointer; vab_id: smallint): smallint; stdcall; external;
function SsSepOpenJ(addr: pointer; vab_id, seq_num: smallint): smallint; stdcall; external;
function SsGetCurrentPoint(acn, trn: smallint): pointer; stdcall; external;
function SsSetCurrentPoint(acn, trn: smallint; point: pointer): longint; stdcall; external;
function SsGetChannelMute(sep_num, seq_num: smallint): longint; stdcall; external;
procedure SsSetVoiceMask(s_voice: dword); stdcall; external;
function SsGetVoiceMask: dword; stdcall; external;
procedure SsQueueRegisters(voice: longint; SRA: PSndRegisterAttr); stdcall; external;
procedure SsQueueKeyOn(voices: longint); stdcall; external;
procedure SsQueueReverb(voices, reverb: longint); stdcall; external;
function SsGetActualProgFromProg(vabId, ProgNum: smallint): smallint; stdcall; external;
procedure SsSetVoiceSettings(voice: longint; Snd_v_attr: PSndVoiceStats); stdcall; external;
function SsPitchFromNote(note, fine: smallint; center, shift: byte): word; stdcall; external;
function SsVoiceCheck(voice, vabId: longint; note: smallint): smallint; stdcall; external;
function SsBlockVoiceAllocation: byte; stdcall; external;
function SsUnBlockVoiceAllocation: byte; stdcall; external;
function SsAllocateVoices(voices, priority: byte): longint; stdcall; external;


// for function table
const
	CC_NUMBER     	= 0;
	CC_BANKCHANGE 	= 1;
	CC_DATAENTRY  	= 2;
	CC_MAINVOL    	= 3;
	CC_PANPOT     	= 4;
	CC_EXPRESSION 	= 5;
	CC_DAMPER     	= 6;
	CC_NRPN1      	= 7;
	CC_NRPN2      	= 8;
	CC_RPN1       	= 9;
	CC_RPN2       	= 10;
	CC_EXTERNAL   	= 11;
	CC_RESETALL   	= 12;

	DE_PRIORITY	= 0;	 
	DE_MODE  	= 1;
	DE_LIMITL	= 2;
	DE_LIMITH	= 3;
	DE_ADSR_AR_L	= 4; 
	DE_ADSR_AR_E	= 5;
	DE_ADSR_DR  	= 6;
	DE_ADSR_SL  	= 7; 
	DE_ADSR_SR_L  	= 8;
	DE_ADSR_SR_E 	= 9;
	DE_ADSR_RR_L	= 10; 
	DE_ADSR_RR_E	= 11;
	DE_ADSR_SR  	= 12;
	DE_VIB_TIME 	= 13; 
	DE_PORTA_DEPTH	= 14;
	DE_REV_TYPE  	= 15;
	DE_REV_DEPTH 	= 16;
	DE_ECHO_FB  	= 17;
	DE_ECHO_DELAY	= 18;
	DE_DELAY  	= 19;
{
type
	_SsFCALL = packed record
		void (*noteon) ();
		void (*programchange) ();
		void (*pitchbend) ();
		void (*metaevent) ();
		void (*control[13]) ();
		void (*ccentry[20]) ();
	end;

procedure _SsNoteOn (short, short, unsigned char, unsigned char);
procedure _SsSetProgramChange(short, short, unsigned char); 
procedure _SsGetMetaEvent(short, short, unsigned char); 
procedure _SsSetPitchBend(short, short);
procedure _SsSetControlChange(short, short, unsigned char); 
procedure _SsContBankChange(short, short); 
procedure _SsContDataEntry(short, short, unsigned char);
procedure _SsContMainVol(short, short, unsigned char);  
procedure _SsContPanpot(short, short, unsigned char);
procedure _SsContExpression(short, short, unsigned char); 
procedure _SsContDamper(short, short, unsigned char); 
procedure _SsContExternal(short, short, unsigned char); 
procedure _SsContNrpn1(short, short, unsigned char); 
procedure _SsContNrpn2(short, short, unsigned char); 
procedure _SsContRpn1(short, short, unsigned char); 
procedure _SsContRpn2(short, short, unsigned char); 
procedure _SsContResetAll(short, short);

procedure _SsSetNrpnVabAttr0(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr1(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr2(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr3(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr4(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr5(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr6(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr7(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr8(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr9(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr10(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr11(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr12(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr13(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr14(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr15(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr16(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr17(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr18(short, short, short, VagAtr, short, unsigned char);
procedure _SsSetNrpnVabAttr19(short, short, short, VagAtr, short, unsigned char);

procedure dmy_nothing1(short, short, unsigned char, unsigned char);
procedure dmy_SsNoteOn (short, short, unsigned char, unsigned char);
procedure dmy_SsSetProgramChange(short, short, unsigned char); 
procedure dmy_SsGetMetaEvent(short, short, unsigned char); 
procedure dmy_SsSetPitchBend(short, short);
procedure dmy_SsSetControlChange(short, short, unsigned char); 
procedure dmy_SsContBankChange(short, short); 
procedure dmy_SsContDataEntry(short, short, unsigned char);
procedure dmy_SsContMainVol(short, short, unsigned char);  
procedure dmy_SsContPanpot(short, short, unsigned char);
procedure dmy_SsContExpression(short, short, unsigned char); 
procedure dmy_SsContDamper(short, short, unsigned char); 
procedure dmy_SsContExternal(short, short, unsigned char); 
procedure dmy_SsContNrpn1(short, short, unsigned char); 
procedure dmy_SsContNrpn2(short, short, unsigned char); 
procedure dmy_SsContRpn1(short, short, unsigned char); 
procedure dmy_SsContRpn2(short, short, unsigned char); 
procedure dmy_SsContResetAll(short, short);
procedure dmy_SsSetNrpnVabAttr0(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr1(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr2(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr3(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr4(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr5(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr6(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr7(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr8(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr9(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr10(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr11(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr12(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr13(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr14(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr15(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr16(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr17(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr18(short, short, short, VagAtr, short, unsigned char);
procedure dmy_SsSetNrpnVabAttr19(short, short, short, VagAtr, short, unsigned char);

_SsFCALL SsFCALL;



procedure jt_SsInit;
begin
//	SsFCALL.noteon                   = (void (*)())_SsNoteOn;
//	SsFCALL.programchange            = (void (*)())_SsSetProgramChange;
//	SsFCALL.metaevent                = (void (*)())_SsGetMetaEvent;
//	SsFCALL.pitchbend                = (void (*)())_SsSetPitchBend; 
//        SsFCALL.control [CC_NUMBER]      = (void (*)())_SsSetControlChange;
//	SsFCALL.control [CC_BANKCHANGE]  = (void (*)())_SsContBankChange;
//	SsFCALL.control [CC_MAINVOL]     = (void (*)())_SsContMainVol;
//	SsFCALL.control [CC_PANPOT]      = (void (*)())_SsContPanpot;
//	SsFCALL.control [CC_EXPRESSION]  = (void (*)())_SsContExpression;
//	SsFCALL.control [CC_DAMPER]      = (void (*)())_SsContDamper;
//	SsFCALL.control [CC_NRPN1]       = (void (*)())_SsContNrpn1;
//	SsFCALL.control [CC_NRPN2]       = (void (*)())_SsContNrpn2;
//	SsFCALL.control [CC_RPN1]        = (void (*)())_SsContRpn1;
//	SsFCALL.control [CC_RPN2]        = (void (*)())_SsContRpn2;
//	SsFCALL.control [CC_EXTERNAL]    = (void (*)())_SsContExternal;
//	SsFCALL.control [CC_RESETALL]    = (void (*)())_SsContResetAll;
//	SsFCALL.control [CC_DATAENTRY]   = (void (*)())_SsContDataEntry;
//	SsFCALL.ccentry [DE_PRIORITY]	 = (void (*)())_SsSetNrpnVabAttr0;   
//	SsFCALL.ccentry [DE_MODE]        = (void (*)())_SsSetNrpnVabAttr1; 
//	SsFCALL.ccentry [DE_LIMITL]      = (void (*)())_SsSetNrpnVabAttr2; 
//	SsFCALL.ccentry [DE_LIMITH]      = (void (*)())_SsSetNrpnVabAttr3; 
//	SsFCALL.ccentry [DE_ADSR_AR_L]   = (void (*)())_SsSetNrpnVabAttr4; 
//	SsFCALL.ccentry [DE_ADSR_AR_E]   = (void (*)())_SsSetNrpnVabAttr5; 
//	SsFCALL.ccentry [DE_ADSR_DR]     = (void (*)())_SsSetNrpnVabAttr6; 
//	SsFCALL.ccentry [DE_ADSR_SL]     = (void (*)())_SsSetNrpnVabAttr7; 
//	SsFCALL.ccentry [DE_ADSR_SR_L]   = (void (*)())_SsSetNrpnVabAttr8; 
//	SsFCALL.ccentry [DE_ADSR_SR_E]   = (void (*)())_SsSetNrpnVabAttr9; 
//	SsFCALL.ccentry [DE_ADSR_RR_L]   = (void (*)())_SsSetNrpnVabAttr10; 
//	SsFCALL.ccentry [DE_ADSR_RR_E]   = (void (*)())_SsSetNrpnVabAttr11; 
//	SsFCALL.ccentry [DE_ADSR_SR]     = (void (*)())_SsSetNrpnVabAttr12; 
//	SsFCALL.ccentry [DE_VIB_TIME]    = (void (*)())_SsSetNrpnVabAttr13; 
//	SsFCALL.ccentry [DE_PORTA_DEPTH] = (void (*)())_SsSetNrpnVabAttr14; 
//	SsFCALL.ccentry [DE_REV_TYPE]    = (void (*)())_SsSetNrpnVabAttr15; 
//	SsFCALL.ccentry [DE_REV_DEPTH]   = (void (*)())_SsSetNrpnVabAttr16; 
//	SsFCALL.ccentry [DE_ECHO_FB]     = (void (*)())_SsSetNrpnVabAttr17; 
//	SsFCALL.ccentry [DE_ECHO_DELAY]  = (void (*)())_SsSetNrpnVabAttr18; 
//	SsFCALL.ccentry [DE_DELAY]       = (void (*)())_SsSetNrpnVabAttr19; 
end;
}


implementation
begin
end.