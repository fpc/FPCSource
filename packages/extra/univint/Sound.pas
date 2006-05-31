{
     File:       Sound.p
 
     Contains:   Sound Manager Interfaces.
 
     Version:    Technology: Sound Manager 3.6
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1986-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit Sound;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,Components,MixedMode,Dialogs;


{$ALIGN MAC68K}

{
                        * * *  N O T E  * * *

    This file has been updated to include Sound Manager 3.3 interfaces.

    Some of the Sound Manager 3.0 interfaces were not put into the InterfaceLib
    that originally shipped with the PowerMacs. These missing functions and the
    new 3.3 interfaces have been released in the SoundLib library for PowerPC
    developers to link with. The runtime library for these functions are
    installed by the Sound Manager. The following functions are found in SoundLib.

        GetCompressionInfo(), GetSoundPreference(), SetSoundPreference(),
        UnsignedFixedMulDiv(), SndGetInfo(), SndSetInfo(), GetSoundOutputInfo(),
        SetSoundOutputInfo(), GetCompressionName(), SoundConverterOpen(),
        SoundConverterClose(), SoundConverterGetBufferSizes(), SoundConverterBeginConversion(),
        SoundConverterConvertBuffer(), SoundConverterEndConversion(),
        AudioGetBass(), AudioGetInfo(), AudioGetMute(), AudioGetOutputDevice(),
        AudioGetTreble(), AudioGetVolume(), AudioMuteOnEvent(), AudioSetBass(),
        AudioSetMute(), AudioSetToDefaults(), AudioSetTreble(), AudioSetVolume(),
        OpenMixerSoundComponent(), CloseMixerSoundComponent(), SoundComponentAddSource(),
        SoundComponentGetInfo(), SoundComponentGetSource(), SoundComponentGetSourceData(),
        SoundComponentInitOutputDevice(), SoundComponentPauseSource(),
        SoundComponentPlaySourceBuffer(), SoundComponentRemoveSource(),
        SoundComponentSetInfo(), SoundComponentSetOutput(), SoundComponentSetSource(),
        SoundComponentStartSource(), SoundComponentStopSource(),
        ParseAIFFHeader(), ParseSndHeader(), SoundConverterGetInfo(), SoundConverterSetInfo()
}
{
    Interfaces for Sound Driver, !!! OBSOLETE and NOT SUPPORTED !!!

    These items are no longer defined, but appear here so that someone
    searching the interfaces might find them. If you are using one of these
    items, you must change your code to support the Sound Manager.

        swMode, ftMode, ffMode
        FreeWave, FFSynthRec, Tone, SWSynthRec, Wave, FTSoundRec
        SndCompletionProcPtr
        StartSound, StopSound, SoundDone
        SetSoundVol, GetSoundVol
}
{
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   constants
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
const twelfthRootTwo = 1.05946309435;


const
	soundListRsrc				= $736E6420 (* 'snd ' *);						{ Resource type used by Sound Manager }

	kSimpleBeepID				= 1;							{ reserved resource ID for Simple Beep }

	rate48khz					= $BB800000;					{ 48000.00000 in fixed-point }
	rate44khz					= $AC440000;					{ 44100.00000 in fixed-point }
	rate32khz					= $7D000000;					{ 32000.00000 in fixed-point }
	rate22050hz					= $56220000;					{ 22050.00000 in fixed-point }
	rate22khz					= $56EE8BA3;					{ 22254.54545 in fixed-point }
	rate16khz					= $3E800000;					{ 16000.00000 in fixed-point }
	rate11khz					= $2B7745D1;					{ 11127.27273 in fixed-point }
	rate11025hz					= $2B110000;					{ 11025.00000 in fixed-point }
	rate8khz					= $1F400000;					{  8000.00000 in fixed-point }

	{	synthesizer numbers for SndNewChannel	}
	sampledSynth				= 5;							{ sampled sound synthesizer }

{$ifc CALL_NOT_IN_CARBON}
	squareWaveSynth				= 1;							{ square wave synthesizer }
	waveTableSynth				= 3;							{ wave table synthesizer }
																{ old Sound Manager MACE synthesizer numbers }
	MACE3snthID					= 11;
	MACE6snthID					= 13;

{$endc}  {CALL_NOT_IN_CARBON}

	kMiddleC					= 60;							{ MIDI note value for middle C }

	kNoVolume					= 0;							{ setting for no sound volume }
	kFullVolume					= $0100;						{ 1.0, setting for full hardware output volume }

	stdQLength					= 128;

	dataOffsetFlag				= $8000;

	kUseOptionalOutputDevice	= -1;							{ only for Sound Manager 3.0 or later }

	notCompressed				= 0;							{ compression ID's }
	fixedCompression			= -1;							{ compression ID for fixed-sized compression }
	variableCompression			= -2;							{ compression ID for variable-sized compression }

	twoToOne					= 1;
	eightToThree				= 2;
	threeToOne					= 3;
	sixToOne					= 4;
	sixToOnePacketSize			= 8;
	threeToOnePacketSize		= 16;

	stateBlockSize				= 64;
	leftOverBlockSize			= 32;

	firstSoundFormat			= $0001;						{ general sound format }
	secondSoundFormat			= $0002;						{ special sampled sound format (HyperCard) }

{$ifc CALL_NOT_IN_CARBON}
	dbBufferReady				= $00000001;					{ double buffer is filled }
	dbLastBuffer				= $00000004;					{ last double buffer to play }

{$endc}  {CALL_NOT_IN_CARBON}

	sysBeepDisable				= $0000;						{ SysBeep() enable flags }
	sysBeepEnable				= $01;
	sysBeepSynchronous			= $02;							{ if bit set, make alert sounds synchronous }

	unitTypeNoSelection			= $FFFF;						{ unitTypes for AudioSelection.unitType }
	unitTypeSeconds				= $0000;

	stdSH						= $00;							{ Standard sound header encode value }
	extSH						= $FF;							{ Extended sound header encode value }
	cmpSH						= $FE;							{ Compressed sound header encode value }

	{	command numbers for SndDoCommand and SndDoImmediate	}
	nullCmd						= 0;
	quietCmd					= 3;
	flushCmd					= 4;
	reInitCmd					= 5;
	waitCmd						= 10;
	pauseCmd					= 11;
	resumeCmd					= 12;
	callBackCmd					= 13;
	syncCmd						= 14;
	availableCmd				= 24;
	versionCmd					= 25;
	volumeCmd					= 46;							{ sound manager 3.0 or later only }
	getVolumeCmd				= 47;							{ sound manager 3.0 or later only }
	clockComponentCmd			= 50;							{ sound manager 3.2.1 or later only }
	getClockComponentCmd		= 51;							{ sound manager 3.2.1 or later only }
	scheduledSoundCmd			= 52;							{ sound manager 3.3 or later only }
	linkSoundComponentsCmd		= 53;							{ sound manager 3.3 or later only }
	soundCmd					= 80;
	bufferCmd					= 81;
	rateMultiplierCmd			= 86;
	getRateMultiplierCmd		= 87;

{$ifc CALL_NOT_IN_CARBON}
	{	command numbers for SndDoCommand and SndDoImmediate that are not available for use in Carbon 	}
	initCmd						= 1;
	freeCmd						= 2;
	totalLoadCmd				= 26;
	loadCmd						= 27;
	freqDurationCmd				= 40;
	restCmd						= 41;
	freqCmd						= 42;
	ampCmd						= 43;
	timbreCmd					= 44;
	getAmpCmd					= 45;
	waveTableCmd				= 60;
	phaseCmd					= 61;
	rateCmd						= 82;
	continueCmd					= 83;
	doubleBufferCmd				= 84;
	getRateCmd					= 85;
	sizeCmd						= 90;							{ obsolete command }
	convertCmd					= 91;							{ obsolete MACE command }

{$endc}  {CALL_NOT_IN_CARBON}

{$ifc OLDROUTINENAMES}
	{	channel initialization parameters	}
	waveInitChannelMask			= $07;
	waveInitChannel0			= $04;							{ wave table only, Sound Manager 2.0 and earlier }
	waveInitChannel1			= $05;							{ wave table only, Sound Manager 2.0 and earlier }
	waveInitChannel2			= $06;							{ wave table only, Sound Manager 2.0 and earlier }
	waveInitChannel3			= $07;							{ wave table only, Sound Manager 2.0 and earlier }
	initChan0					= $04;							{ obsolete spelling }
	initChan1					= $05;							{ obsolete spelling }
	initChan2					= $06;							{ obsolete spelling }
	initChan3					= $07;							{ obsolete spelling }

	outsideCmpSH				= 0;							{ obsolete MACE constant }
	insideCmpSH					= 1;							{ obsolete MACE constant }
	aceSuccess					= 0;							{ obsolete MACE constant }
	aceMemFull					= 1;							{ obsolete MACE constant }
	aceNilBlock					= 2;							{ obsolete MACE constant }
	aceBadComp					= 3;							{ obsolete MACE constant }
	aceBadEncode				= 4;							{ obsolete MACE constant }
	aceBadDest					= 5;							{ obsolete MACE constant }
	aceBadCmd					= 6;							{ obsolete MACE constant }

{$endc}  {OLDROUTINENAMES}

	initChanLeft				= $0002;						{ left stereo channel }
	initChanRight				= $0003;						{ right stereo channel }
	initNoInterp				= $0004;						{ no linear interpolation }
	initNoDrop					= $0008;						{ no drop-sample conversion }
	initMono					= $0080;						{ monophonic channel }
	initStereo					= $00C0;						{ stereo channel }
	initMACE3					= $0300;						{ MACE 3:1 }
	initMACE6					= $0400;						{ MACE 6:1 }
	initPanMask					= $0003;						{ mask for right/left pan values }
	initSRateMask				= $0030;						{ mask for sample rate values }
	initStereoMask				= $00C0;						{ mask for mono/stereo values }
	initCompMask				= $FF00;						{ mask for compression IDs }

	{	Get&Set Sound Information Selectors	}
	siActiveChannels			= $63686163 (* 'chac' *);						{ active channels }
	siActiveLevels				= $6C6D6163 (* 'lmac' *);						{ active meter levels }
	siAGCOnOff					= $61676320 (* 'agc ' *);						{ automatic gain control state }
	siAsync						= $6173796E (* 'asyn' *);						{ asynchronous capability }
	siAVDisplayBehavior			= $61766462 (* 'avdb' *);
	siChannelAvailable			= $63686176 (* 'chav' *);						{ number of channels available }
	siCompressionAvailable		= $636D6176 (* 'cmav' *);						{ compression types available }
	siCompressionChannels		= $63706374 (* 'cpct' *);						{ compressor's number of channels }
	siCompressionFactor			= $636D6661 (* 'cmfa' *);						{ current compression factor }
	siCompressionHeader			= $636D6864 (* 'cmhd' *);						{ return compression header }
	siCompressionNames			= $636E616D (* 'cnam' *);						{ compression type names available }
	siCompressionParams			= $65766177 (* 'evaw' *);						{ compression parameters }
	siCompressionSampleRate		= $63707274 (* 'cprt' *);						{ compressor's sample rate }
	siCompressionType			= $636F6D70 (* 'comp' *);						{ current compression type }
	siContinuous				= $636F6E74 (* 'cont' *);						{ continous recording }
	siDecompressionParams		= $77617665 (* 'wave' *);						{ decompression parameters }
	siDeviceBufferInfo			= $6462696E (* 'dbin' *);						{ size of interrupt buffer }
	siDeviceConnected			= $64636F6E (* 'dcon' *);						{ input device connection status }
	siDeviceIcon				= $69636F6E (* 'icon' *);						{ input device icon }
	siDeviceName				= $6E616D65 (* 'name' *);						{ input device name }
	siEQSpectrumBands			= $65717362 (* 'eqsb' *);						{  number of spectrum bands }
	siEQSpectrumLevels			= $65716C76 (* 'eqlv' *);						{  gets spectum meter levels }
	siEQSpectrumOnOff			= $65716C6F (* 'eqlo' *);						{  turn on/off spectum meter levels }
	siEQSpectrumResolution		= $65717273 (* 'eqrs' *);						{  set the resolution of the FFT, 0 = low res (<=16 bands), 1 = high res (16-64 bands) }
	siEQToneControlGain			= $65717467 (* 'eqtg' *);						{  set the bass and treble gain }
	siEQToneControlOnOff		= $65717463 (* 'eqtc' *);						{  turn on equalizer attenuation }
	siHardwareBalance			= $6862616C (* 'hbal' *);
	siHardwareBalanceSteps		= $68626C73 (* 'hbls' *);
	siHardwareBass				= $68626173 (* 'hbas' *);
	siHardwareBassSteps			= $68627374 (* 'hbst' *);
	siHardwareBusy				= $68776273 (* 'hwbs' *);						{ sound hardware is in use }
	siHardwareFormat			= $6877666D (* 'hwfm' *);						{ get hardware format }
	siHardwareMute				= $686D7574 (* 'hmut' *);						{ mute state of all hardware }
	siHardwareMuteNoPrefs		= $686D6E70 (* 'hmnp' *);						{ mute state of all hardware, but don't store in prefs  }
	siHardwareTreble			= $68747262 (* 'htrb' *);
	siHardwareTrebleSteps		= $68777473 (* 'hwts' *);
	siHardwareVolume			= $68766F6C (* 'hvol' *);						{ volume level of all hardware }
	siHardwareVolumeSteps		= $68737470 (* 'hstp' *);						{ number of volume steps for hardware }
	siHeadphoneMute				= $706D7574 (* 'pmut' *);						{ mute state of headphones }
	siHeadphoneVolume			= $70766F6C (* 'pvol' *);						{ volume level of headphones }
	siHeadphoneVolumeSteps		= $68647374 (* 'hdst' *);						{ number of volume steps for headphones }
	siInputAvailable			= $696E6176 (* 'inav' *);						{ input sources available }
	siInputGain					= $6761696E (* 'gain' *);						{ input gain }
	siInputSource				= $736F7572 (* 'sour' *);						{ input source selector }
	siInputSourceNames			= $736E616D (* 'snam' *);						{ input source names }
	siLevelMeterOnOff			= $6C6D6574 (* 'lmet' *);						{ level meter state }
	siModemGain					= $6D676169 (* 'mgai' *);						{ modem input gain }
	siMonitorAvailable			= $6D6E6176 (* 'mnav' *);
	siMonitorSource				= $6D6F6E73 (* 'mons' *);
	siNumberChannels			= $6368616E (* 'chan' *);						{ current number of channels }
	siOptionsDialog				= $6F707464 (* 'optd' *);						{ display options dialog }
	siOSTypeInputSource			= $696E7074 (* 'inpt' *);						{ input source by OSType }
	siOSTypeInputAvailable		= $696E6176 (* 'inav' *);						{ list of available input source OSTypes }
	siOutputDeviceName			= $6F6E616D (* 'onam' *);						{ output device name }
	siPlayThruOnOff				= $706C7468 (* 'plth' *);						{ playthrough state }
	siPostMixerSoundComponent	= $70736D78 (* 'psmx' *);						{ install post-mixer effect }
	siPreMixerSoundComponent	= $70726D78 (* 'prmx' *);						{ install pre-mixer effect }
	siQuality					= $7175616C (* 'qual' *);						{ quality setting }
	siRateMultiplier			= $726D756C (* 'rmul' *);						{ throttle rate setting }
	siRecordingQuality			= $7175616C (* 'qual' *);						{ recording quality }
	siSampleRate				= $73726174 (* 'srat' *);						{ current sample rate }
	siSampleRateAvailable		= $73726176 (* 'srav' *);						{ sample rates available }
	siSampleSize				= $7373697A (* 'ssiz' *);						{ current sample size }
	siSampleSizeAvailable		= $73736176 (* 'ssav' *);						{ sample sizes available }
	siSetupCDAudio				= $73756364 (* 'sucd' *);						{ setup sound hardware for CD audio }
	siSetupModemAudio			= $73756D64 (* 'sumd' *);						{ setup sound hardware for modem audio }
	siSlopeAndIntercept			= $666C6170 (* 'flap' *);						{ floating point variables for conversion }
	siSoundClock				= $73636C6B (* 'sclk' *);
	siUseThisSoundClock			= $73636C63 (* 'sclc' *);						{ sdev uses this to tell the mixer to use his sound clock }
	siSpeakerMute				= $736D7574 (* 'smut' *);						{ mute state of all built-in speaker }
	siSpeakerVolume				= $73766F6C (* 'svol' *);						{ volume level of built-in speaker }
	siSSpCPULoadLimit			= $33646C6C (* '3dll' *);
	siSSpLocalization			= $33646966 (* '3dif' *);
	siSSpSpeakerSetup			= $33647374 (* '3dst' *);
	siStereoInputGain			= $73676169 (* 'sgai' *);						{ stereo input gain }
	siSubwooferMute				= $626D7574 (* 'bmut' *);						{ mute state of sub-woofer }
	siTerminalType				= $74747970 (* 'ttyp' *);						{  usb terminal type  }
	siTwosComplementOnOff		= $74776F73 (* 'twos' *);						{ two's complement state }
	siVendorProduct				= $7670726F (* 'vpro' *);						{  vendor and product ID  }
	siVolume					= $766F6C75 (* 'volu' *);						{ volume level of source }
	siVoxRecordInfo				= $766F7872 (* 'voxr' *);						{ VOX record parameters }
	siVoxStopInfo				= $766F7873 (* 'voxs' *);						{ VOX stop parameters }
	siWideStereo				= $77696465 (* 'wide' *);						{ wide stereo setting }
	siSupportedExtendedFlags	= $6578666C (* 'exfl' *);						{ which flags are supported in Extended sound data structures }
	siRateConverterRollOffSlope	= $72636462 (* 'rcdb' *);						{ the roll-off slope for the rate converter's filter, in whole dB as a long this value is a long whose range is from 20 (worst quality/fastest performance) to 90 (best quality/slowest performance) }
	siOutputLatency				= $6F6C7465 (* 'olte' *);						{ latency of sound output component }

	siCloseDriver				= $636C6F73 (* 'clos' *);						{ reserved for internal use only }
	siInitializeDriver			= $696E6974 (* 'init' *);						{ reserved for internal use only }
	siPauseRecording			= $70617573 (* 'paus' *);						{ reserved for internal use only }
	siUserInterruptProc			= $75736572 (* 'user' *);						{ reserved for internal use only }

	{  input source Types }
	kInvalidSource				= $FFFFFFFF;					{ this source may be returned from GetInfo if no other source is the monitored source }
	kNoSource					= $6E6F6E65 (* 'none' *);						{ no source selection }
	kCDSource					= $63642020 (* 'cd  ' *);						{ internal CD player input }
	kExtMicSource				= $656D6963 (* 'emic' *);						{ external mic input }
	kSoundInSource				= $73696E6A (* 'sinj' *);						{ sound input jack }
	kRCAInSource				= $69726361 (* 'irca' *);						{ RCA jack input }
	kTVFMTunerSource			= $7476666D (* 'tvfm' *);
	kDAVInSource				= $69646176 (* 'idav' *);						{ DAV analog input }
	kIntMicSource				= $696D6963 (* 'imic' *);						{ internal mic input }
	kMediaBaySource				= $6D626179 (* 'mbay' *);						{ media bay input }
	kModemSource				= $6D6F646D (* 'modm' *);						{ modem input (internal modem on desktops, PCI input on PowerBooks) }
	kPCCardSource				= $70636D20 (* 'pcm ' *);						{ PC Card pwm input }
	kZoomVideoSource			= $7A767063 (* 'zvpc' *);						{ zoom video input }
	kDVDSource					= $64766461 (* 'dvda' *);						{  DVD audio input }
	kMicrophoneArray			= $6D696361 (* 'mica' *);						{  microphone array }

	{ Sound Component Types and Subtypes }
	kNoSoundComponentType		= $2A2A2A2A (* '****' *);
	kSoundComponentType			= $73696674 (* 'sift' *);						{ component type }
	kSoundComponentPPCType		= $6E696674 (* 'nift' *);						{ component type for PowerPC code }
	kRate8SubType				= $72617462 (* 'ratb' *);						{ 8-bit rate converter }
	kRate16SubType				= $72617477 (* 'ratw' *);						{ 16-bit rate converter }
	kConverterSubType			= $636F6E76 (* 'conv' *);						{ sample format converter }
	kSndSourceSubType			= $736F7572 (* 'sour' *);						{ generic source component }
	kMixerType					= $6D697872 (* 'mixr' *);
	kMixer8SubType				= $6D697862 (* 'mixb' *);						{ 8-bit mixer }
	kMixer16SubType				= $6D697877 (* 'mixw' *);						{ 16-bit mixer }
	kSoundInputDeviceType		= $73696E70 (* 'sinp' *);						{ sound input component }
	kWaveInSubType				= $77617669 (* 'wavi' *);						{ Windows waveIn }
	kWaveInSnifferSubType		= $7769736E (* 'wisn' *);						{ Windows waveIn sniffer }
	kSoundOutputDeviceType		= $73646576 (* 'sdev' *);						{ sound output component }
	kClassicSubType				= $636C6173 (* 'clas' *);						{ classic hardware, i.e. Mac Plus }
	kASCSubType					= $61736320 (* 'asc ' *);						{ Apple Sound Chip device }
	kDSPSubType					= $64737020 (* 'dsp ' *);						{ DSP device }
	kAwacsSubType				= $61776163 (* 'awac' *);						{ Another of Will's Audio Chips device }
	kGCAwacsSubType				= $61776763 (* 'awgc' *);						{ Awacs audio with Grand Central DMA }
	kSingerSubType				= $73696E67 (* 'sing' *);						{ Singer (via Whitney) based sound }
	kSinger2SubType				= $736E6732 (* 'sng2' *);						{ Singer 2 (via Whitney) for Acme }
	kWhitSubType				= $77686974 (* 'whit' *);						{ Whit sound component for PrimeTime 3 }
	kSoundBlasterSubType		= $73626C73 (* 'sbls' *);						{ Sound Blaster for CHRP }
	kWaveOutSubType				= $7761766F (* 'wavo' *);						{ Windows waveOut }
	kWaveOutSnifferSubType		= $776F736E (* 'wosn' *);						{ Windows waveOut sniffer }
	kDirectSoundSubType			= $64736E64 (* 'dsnd' *);						{ Windows DirectSound }
	kDirectSoundSnifferSubType	= $6473736E (* 'dssn' *);						{ Windows DirectSound sniffer }
	kUNIXsdevSubType			= $756E3178 (* 'un1x' *);						{ UNIX base sdev }
	kUSBSubType					= $75736220 (* 'usb ' *);						{ USB device }
	kBlueBoxSubType				= $62736E64 (* 'bsnd' *);						{ Blue Box sound component }
	kSoundCompressor			= $73636F6D (* 'scom' *);
	kSoundDecompressor			= $73646563 (* 'sdec' *);
	kAudioComponentType			= $6164696F (* 'adio' *);						{ Audio components and sub-types }
	kAwacsPhoneSubType			= $6870686E (* 'hphn' *);
	kAudioVisionSpeakerSubType	= $74656C63 (* 'telc' *);
	kAudioVisionHeadphoneSubType = $74656C68 (* 'telh' *);
	kPhilipsFaderSubType		= $74766176 (* 'tvav' *);
	kSGSToneSubType				= $73677330 (* 'sgs0' *);
	kSoundEffectsType			= $736E6678 (* 'snfx' *);						{ sound effects type }
	kEqualizerSubType			= $6571616C (* 'eqal' *);						{ frequency equalizer }
	kSSpLocalizationSubType		= $736E6433 (* 'snd3' *);

	{ Format Types }
	kSoundNotCompressed			= $4E4F4E45 (* 'NONE' *);						{ sound is not compressed }
	k8BitOffsetBinaryFormat		= $72617720 (* 'raw ' *);						{ 8-bit offset binary }
	k16BitBigEndianFormat		= $74776F73 (* 'twos' *);						{ 16-bit big endian }
	k16BitLittleEndianFormat	= $736F7774 (* 'sowt' *);						{ 16-bit little endian }
	kFloat32Format				= $666C3332 (* 'fl32' *);						{ 32-bit floating point }
	kFloat64Format				= $666C3634 (* 'fl64' *);						{ 64-bit floating point }
	k24BitFormat				= $696E3234 (* 'in24' *);						{ 24-bit SInt16 }
	k32BitFormat				= $696E3332 (* 'in32' *);						{ 32-bit SInt16 }
	k32BitLittleEndianFormat	= $32336E69 (* '23ni' *);						{ 32-bit little endian SInt16  }
	kMACE3Compression			= $4D414333 (* 'MAC3' *);						{ MACE 3:1 }
	kMACE6Compression			= $4D414336 (* 'MAC6' *);						{ MACE 6:1 }
	kCDXA4Compression			= $63647834 (* 'cdx4' *);						{ CD/XA 4:1 }
	kCDXA2Compression			= $63647832 (* 'cdx2' *);						{ CD/XA 2:1 }
	kIMACompression				= $696D6134 (* 'ima4' *);						{ IMA 4:1 }
	kULawCompression			= $756C6177 (* 'ulaw' *);						{ µLaw 2:1 }
	kALawCompression			= $616C6177 (* 'alaw' *);						{ aLaw 2:1 }
	kMicrosoftADPCMFormat		= $6D730002;					{ Microsoft ADPCM - ACM code 2 }
	kDVIIntelIMAFormat			= $6D730011;					{ DVI/Intel IMA ADPCM - ACM code 17 }
	kDVAudioFormat				= $64766361 (* 'dvca' *);						{ DV Audio }
	kQDesignCompression			= $51444D43 (* 'QDMC' *);						{ QDesign music }
	kQDesign2Compression		= $51444D32 (* 'QDM2' *);						{ QDesign2 music }
	kQUALCOMMCompression		= $51636C70 (* 'Qclp' *);						{ QUALCOMM PureVoice }
	kOffsetBinary				= $72617720 (* 'raw ' *);						{ for compatibility }
	kTwosComplement				= $74776F73 (* 'twos' *);						{ for compatibility }
	kLittleEndianFormat			= $736F7774 (* 'sowt' *);						{ for compatibility }
	kMPEGLayer3Format			= $6D730055;					{ MPEG Layer 3, CBR only (pre QT4.1) }
	kFullMPEGLay3Format			= $2E6D7033 (* '.mp3' *);						{ MPEG Layer 3, CBR & VBR (QT4.1 and later) }

{$ifc TARGET_RT_LITTLE_ENDIAN}
	k16BitNativeEndianFormat	= k16BitLittleEndianFormat;
	k16BitNonNativeEndianFormat	= k16BitBigEndianFormat;

{$elsec}
	k16BitNativeEndianFormat	= k16BitBigEndianFormat;
	k16BitNonNativeEndianFormat	= k16BitLittleEndianFormat;

{$endc}  {TARGET_RT_LITTLE_ENDIAN}

	{ Features Flags }
	k8BitRawIn					= $01;							{ data description }
	k8BitTwosIn					= $02;
	k16BitIn					= $04;
	kStereoIn					= $08;
	k8BitRawOut					= $0100;
	k8BitTwosOut				= $0200;
	k16BitOut					= $0400;
	kStereoOut					= $0800;
	kReverse					= $00010000;					{   function description }
	kRateConvert				= $00020000;
	kCreateSoundSource			= $00040000;
	kVMAwareness				= $00200000;					{  component will hold its memory }
	kHighQuality				= $00400000;					{   performance description }
	kNonRealTime				= $00800000;

	{ SoundComponentPlaySourceBuffer action flags }
	kSourcePaused				= $01;
	kPassThrough				= $00010000;
	kNoSoundComponentChain		= $00020000;

	{ SoundParamBlock flags, usefull for OpenMixerSoundComponent }
	kNoMixing					= $01;							{ don't mix source }
	kNoSampleRateConversion		= $02;							{ don't convert sample rate (i.e. 11 kHz -> 22 kHz) }
	kNoSampleSizeConversion		= $04;							{ don't convert sample size (i.e. 16 -> 8) }
	kNoSampleFormatConversion	= $08;							{ don't convert sample format (i.e. 'twos' -> 'raw ') }
	kNoChannelConversion		= $10;							{ don't convert stereo/mono }
	kNoDecompression			= $20;							{ don't decompress (i.e. 'MAC3' -> 'raw ') }
	kNoVolumeConversion			= $40;							{ don't apply volume }
	kNoRealtimeProcessing		= $80;							{ won't run at interrupt time }
	kScheduledSource			= $0100;						{ source is scheduled }
	kNonInterleavedBuffer		= $0200;						{ buffer is not interleaved samples }
	kNonPagingMixer				= $0400;						{ if VM is on, use the non-paging mixer }
	kSoundConverterMixer		= $0800;						{ the mixer is to be used by the SoundConverter }
	kPagingMixer				= $1000;						{ the mixer is to be used as a paging mixer when VM is on }
	kVMAwareMixer				= $2000;						{ passed to the output device when the SM is going to deal with VM safety }
	kExtendedSoundData			= $4000;						{ SoundComponentData record is actually an ExtendedSoundComponentData }

	{ SoundParamBlock quality settings }
	kBestQuality				= $01;							{ use interpolation in rate conversion }

	{ useful bit masks }
	kInputMask					= $000000FF;					{ masks off input bits }
	kOutputMask					= $0000FF00;					{ masks off output bits }
	kOutputShift				= 8;							{ amount output bits are shifted }
	kActionMask					= $00FF0000;					{ masks off action bits }
	kSoundComponentBits			= $00FFFFFF;

	{ audio atom types }
	kAudioFormatAtomType		= $66726D61 (* 'frma' *);
	kAudioEndianAtomType		= $656E6461 (* 'enda' *);
	kAudioVBRAtomType			= $76627261 (* 'vbra' *);
	kAudioTerminatorAtomType	= 0;

	{ siAVDisplayBehavior types }
	kAVDisplayHeadphoneRemove	= 0;							{  monitor does not have a headphone attached }
	kAVDisplayHeadphoneInsert	= 1;							{  monitor has a headphone attached }
	kAVDisplayPlainTalkRemove	= 2;							{  monitor either sending no input through CPU input port or unable to tell if input is coming in }
	kAVDisplayPlainTalkInsert	= 3;							{  monitor sending PlainTalk level microphone source input through sound input port }

	{ Audio Component constants }
																{ Values for whichChannel parameter }
	audioAllChannels			= 0;							{ All channels (usually interpreted as both left and right) }
	audioLeftChannel			= 1;							{ Left channel }
	audioRightChannel			= 2;							{ Right channel }
																{ Values for mute parameter }
	audioUnmuted				= 0;							{ Device is unmuted }
	audioMuted					= 1;							{ Device is muted }
																{ Capabilities flags definitions }
	audioDoesMono				= $00000001;					{ Device supports mono output }
	audioDoesStereo				= $00000002;					{ Device supports stereo output }
	audioDoesIndependentChannels = $00000004;					{ Device supports independent software control of each channel }

	{	Sound Input Qualities	}
	siCDQuality					= $63642020 (* 'cd  ' *);						{ 44.1kHz, stereo, 16 bit }
	siBestQuality				= $62657374 (* 'best' *);						{ 22kHz, mono, 8 bit }
	siBetterQuality				= $62657472 (* 'betr' *);						{ 22kHz, mono, MACE 3:1 }
	siGoodQuality				= $676F6F64 (* 'good' *);						{ 22kHz, mono, MACE 6:1 }
	siNoneQuality				= $6E6F6E65 (* 'none' *);						{ settings don't match any quality for a get call }

	siDeviceIsConnected			= 1;							{ input device is connected and ready for input }
	siDeviceNotConnected		= 0;							{ input device is not connected }
	siDontKnowIfConnected		= -1;							{ can't tell if input device is connected }
	siReadPermission			= 0;							{ permission passed to SPBOpenDevice }
	siWritePermission			= 1;							{ permission passed to SPBOpenDevice }

	{ flags that SoundConverterFillBuffer will return }
	kSoundConverterDidntFillBuffer = $01;						{ set if the converter couldn't completely satisfy a SoundConverterFillBuffer request }
	kSoundConverterHasLeftOverData = $02;						{ set if the converter had left over data after completely satisfying a SoundConverterFillBuffer call }

	{  flags for extendedFlags fields of ExtendedSoundComponentData, ExtendedSoundParamBlock, and ExtendedScheduledSoundHeader }
	kExtendedSoundSampleCountNotValid = $00000001;				{  set if sampleCount of SoundComponentData isn't meaningful; use buffer size instead }
	kExtendedSoundBufferSizeValid = $00000002;					{  set if bufferSize field is valid }

	{
	  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	   typedefs
	  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	}


type
	SndCommandPtr = ^SndCommand;
	SndCommand = packed record
		cmd:					UInt16;
		param1:					SInt16;
		param2:					SInt32;
	end;

	SndChannelPtr = ^SndChannel;
{$ifc TYPED_FUNCTION_POINTERS}
	SndCallBackProcPtr = procedure(chan: SndChannelPtr; var cmd: SndCommand);
{$elsec}
	SndCallBackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SndCallBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SndCallBackUPP = UniversalProcPtr;
{$endc}	
	SndChannel = packed record
		nextChan:				SndChannelPtr;
		firstMod:				Ptr;									{  reserved for the Sound Manager  }
		callBack:				SndCallBackUPP;
		userInfo:				SInt32;
		wait:					SInt32;								{  The following is for internal Sound Manager use only. }
		cmdInProgress:			SndCommand;
		flags:					SInt16;
		qLength:				SInt16;
		qHead:					SInt16;
		qTail:					SInt16;
		queue:					array [0..127] of SndCommand;
	end;


const
	uppSndCallBackProcInfo = $000003C0;
	{
	 *  NewSndCallBackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSndCallBackUPP(userRoutine: SndCallBackProcPtr): SndCallBackUPP; external name '_NewSndCallBackUPP'; { old name was NewSndCallBackProc }
{
 *  DisposeSndCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSndCallBackUPP(userUPP: SndCallBackUPP); external name '_DisposeSndCallBackUPP';
{
 *  InvokeSndCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSndCallBackUPP(chan: SndChannelPtr; var cmd: SndCommand; userRoutine: SndCallBackUPP); external name '_InvokeSndCallBackUPP'; { old name was CallSndCallBackProc }
{MACE structures}

type
	StateBlockPtr = ^StateBlock;
	StateBlock = record
		stateVar:				array [0..63] of SInt16;
	end;

	LeftOverBlockPtr = ^LeftOverBlock;
	LeftOverBlock = record
		count:					UInt32;
		sampleArea:				array [0..31] of SInt8;
	end;

	ModRefPtr = ^ModRef;
	ModRef = record
		modNumber:				UInt16;
		modInit:				SInt32;
	end;

	SndListResourcePtr = ^SndListResource;
	SndListResource = record
		format:					SInt16;
		numModifiers:			SInt16;
		modifierPart:			array [0..0] of ModRef;
		numCommands:			SInt16;
		commandPart:			array [0..0] of SndCommand;
		dataPart:				SInt8;
	end;

	SndListPtr							= ^SndListResource;
	SndListHandle						= ^SndListPtr;
	SndListHndl							= SndListHandle;
	{	HyperCard sound resource format	}
	Snd2ListResourcePtr = ^Snd2ListResource;
	Snd2ListResource = record
		format:					SInt16;
		refCount:				SInt16;
		numCommands:			SInt16;
		commandPart:			array [0..0] of SndCommand;
		dataPart:				SInt8;
	end;

	Snd2ListPtr							= ^Snd2ListResource;
	Snd2ListHandle						= ^Snd2ListPtr;
	Snd2ListHndl						= Snd2ListHandle;
	SoundHeaderPtr = ^SoundHeader;
	SoundHeader = packed record
		samplePtr:				Ptr;									{ if NIL then samples are in sampleArea }
		length:					UInt32;									{ length of sound in bytes }
		sampleRate:				UnsignedFixed;							{ sample rate for this sound }
		loopStart:				UInt32;									{ start of looping portion }
		loopEnd:				UInt32;									{ end of looping portion }
		encode:					UInt8;									{ header encoding }
		baseFrequency:			UInt8;									{ baseFrequency value }
		sampleArea:				packed array [0..0] of UInt8;			{ space for when samples follow directly }
		pad:					UInt8;
	end;

	CmpSoundHeaderPtr = ^CmpSoundHeader;
	CmpSoundHeader = packed record
		samplePtr:				Ptr;									{ if nil then samples are in sample area }
		numChannels:			UInt32;									{ number of channels i.e. mono = 1 }
		sampleRate:				UnsignedFixed;							{ sample rate in Apples Fixed point representation }
		loopStart:				UInt32;									{ loopStart of sound before compression }
		loopEnd:				UInt32;									{ loopEnd of sound before compression }
		encode:					UInt8;									{ data structure used , stdSH, extSH, or cmpSH }
		baseFrequency:			UInt8;									{ same meaning as regular SoundHeader }
		numFrames:				UInt32;									{ length in frames ( packetFrames or sampleFrames ) }
		AIFFSampleRate:			extended80;								{ IEEE sample rate }
		markerChunk:			Ptr;									{ sync track }
		format:					OSType;									{ data format type, was futureUse1 }
		futureUse2:				UInt32;									{ reserved by Apple }
		stateVars:				StateBlockPtr;							{ pointer to State Block }
		leftOverSamples:		LeftOverBlockPtr;						{ used to save truncated samples between compression calls }
		compressionID:			SInt16;								{ 0 means no compression, non zero means compressionID }
		packetSize:				UInt16;									{ number of bits in compressed sample packet }
		snthID:					UInt16;									{ resource ID of Sound Manager snth that contains NRT C/E }
		sampleSize:				UInt16;									{ number of bits in non-compressed sample }
		sampleArea:				packed array [0..0] of UInt8;			{ space for when samples follow directly }
		pad:					UInt8;
	end;

	ExtSoundHeaderPtr = ^ExtSoundHeader;
	ExtSoundHeader = packed record
		samplePtr:				Ptr;									{ if nil then samples are in sample area }
		numChannels:			UInt32;									{ number of channels,  ie mono = 1 }
		sampleRate:				UnsignedFixed;							{ sample rate in Apples Fixed point representation }
		loopStart:				UInt32;									{ same meaning as regular SoundHeader }
		loopEnd:				UInt32;									{ same meaning as regular SoundHeader }
		encode:					UInt8;									{ data structure used , stdSH, extSH, or cmpSH }
		baseFrequency:			UInt8;									{ same meaning as regular SoundHeader }
		numFrames:				UInt32;									{ length in total number of frames }
		AIFFSampleRate:			extended80;								{ IEEE sample rate }
		markerChunk:			Ptr;									{ sync track }
		instrumentChunks:		Ptr;									{ AIFF instrument chunks }
		AESRecording:			Ptr;
		sampleSize:				UInt16;									{ number of bits in sample }
		futureUse1:				UInt16;									{ reserved by Apple }
		futureUse2:				UInt32;									{ reserved by Apple }
		futureUse3:				UInt32;									{ reserved by Apple }
		futureUse4:				UInt32;									{ reserved by Apple }
		sampleArea:				packed array [0..0] of UInt8;			{ space for when samples follow directly }
		pad:					UInt8;
	end;

	SoundHeaderUnionPtr = ^SoundHeaderUnion;
	SoundHeaderUnion = record
		case SInt16 of
		0: (
			stdHeader:			SoundHeader;
			);
		1: (
			cmpHeader:			CmpSoundHeader;
			);
		2: (
			extHeader:			ExtSoundHeader;
			);
	end;

	ConversionBlockPtr = ^ConversionBlock;
	ConversionBlock = record
		destination:			SInt16;
		unused:					SInt16;
		inputPtr:				CmpSoundHeaderPtr;
		outputPtr:				CmpSoundHeaderPtr;
	end;

	{  ScheduledSoundHeader flags }

const
	kScheduledSoundDoScheduled	= $01;
	kScheduledSoundDoCallBack	= $02;
	kScheduledSoundExtendedHdr	= $04;


type
	ScheduledSoundHeaderPtr = ^ScheduledSoundHeader;
	ScheduledSoundHeader = record
		u:						SoundHeaderUnion;
		flags:					SInt32;
		reserved:				SInt16;
		callBackParam1:			SInt16;
		callBackParam2:			SInt32;
		startTime:				TimeRecord;
	end;

	ExtendedScheduledSoundHeaderPtr = ^ExtendedScheduledSoundHeader;
	ExtendedScheduledSoundHeader = record
		u:						SoundHeaderUnion;
		flags:					SInt32;
		reserved:				SInt16;
		callBackParam1:			SInt16;
		callBackParam2:			SInt32;
		startTime:				TimeRecord;
		recordSize:				SInt32;
		extendedFlags:			SInt32;
		bufferSize:				SInt32;
	end;

	SMStatusPtr = ^SMStatus;
	SMStatus = packed record
		smMaxCPULoad:			SInt16;
		smNumChannels:			SInt16;
		smCurCPULoad:			SInt16;
	end;

	SCStatusPtr = ^SCStatus;
	SCStatus = record
		scStartTime:			UnsignedFixed;
		scEndTime:				UnsignedFixed;
		scCurrentTime:			UnsignedFixed;
		scChannelBusy:			boolean;
		scChannelDisposed:		boolean;
		scChannelPaused:		boolean;
		scUnused:				boolean;
		scChannelAttributes:	UInt32;
		scCPULoad:				SInt32;
	end;

	AudioSelectionPtr = ^AudioSelection;
	AudioSelection = packed record
		unitType:				SInt32;
		selStart:				UnsignedFixed;
		selEnd:					UnsignedFixed;
	end;

{$ifc CALL_NOT_IN_CARBON}
	SndDoubleBufferPtr = ^SndDoubleBuffer;
	SndDoubleBuffer = packed record
		dbNumFrames:			SInt32;
		dbFlags:				SInt32;
		dbUserInfo:				array [0..1] of SInt32;
		dbSoundData:			array [0..0] of SInt8;
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	SndDoubleBackProcPtr = procedure(channel: SndChannelPtr; doubleBufferPtr: SndDoubleBufferPtr);
{$elsec}
	SndDoubleBackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SndDoubleBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SndDoubleBackUPP = UniversalProcPtr;
{$endc}	

const
	uppSndDoubleBackProcInfo = $000003C0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewSndDoubleBackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewSndDoubleBackUPP(userRoutine: SndDoubleBackProcPtr): SndDoubleBackUPP; external name '_NewSndDoubleBackUPP'; { old name was NewSndDoubleBackProc }
{
 *  DisposeSndDoubleBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeSndDoubleBackUPP(userUPP: SndDoubleBackUPP); external name '_DisposeSndDoubleBackUPP';
{
 *  InvokeSndDoubleBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InvokeSndDoubleBackUPP(channel: SndChannelPtr; doubleBufferPtr: SndDoubleBufferPtr; userRoutine: SndDoubleBackUPP); external name '_InvokeSndDoubleBackUPP'; { old name was CallSndDoubleBackProc }
{$endc}  {CALL_NOT_IN_CARBON}

type
	SndDoubleBufferHeaderPtr = ^SndDoubleBufferHeader;
	SndDoubleBufferHeader = packed record
		dbhNumChannels:			SInt16;
		dbhSampleSize:			SInt16;
		dbhCompressionID:		SInt16;
		dbhPacketSize:			SInt16;
		dbhSampleRate:			UnsignedFixed;
		dbhBufferPtr:			array [0..1] of SndDoubleBufferPtr;
		dbhDoubleBack:			SndDoubleBackUPP;
	end;

	SndDoubleBufferHeader2Ptr = ^SndDoubleBufferHeader2;
	SndDoubleBufferHeader2 = packed record
		dbhNumChannels:			SInt16;
		dbhSampleSize:			SInt16;
		dbhCompressionID:		SInt16;
		dbhPacketSize:			SInt16;
		dbhSampleRate:			UnsignedFixed;
		dbhBufferPtr:			array [0..1] of SndDoubleBufferPtr;
		dbhDoubleBack:			SndDoubleBackUPP;
		dbhFormat:				OSType;
	end;

{$endc}  {CALL_NOT_IN_CARBON}

	SoundInfoListPtr = ^SoundInfoList;
	SoundInfoList = packed record
		count:					SInt16;
		infoHandle:				Handle;
	end;

	SoundComponentDataPtr = ^SoundComponentData;
	SoundComponentData = record
		flags:					SInt32;
		format:					OSType;
		numChannels:			SInt16;
		sampleSize:				SInt16;
		sampleRate:				UnsignedFixed;
		sampleCount:			SInt32;
		buffer:					Ptr;
		reserved:				SInt32;
	end;

	ExtendedSoundComponentDataPtr = ^ExtendedSoundComponentData;
	ExtendedSoundComponentData = record
		desc:					SoundComponentData;						{ description of sound buffer }
		recordSize:				SInt32;								{ size of this record in bytes }
		extendedFlags:			SInt32;								{ flags for extended record }
		bufferSize:				SInt32;								{ size of buffer in bytes }
	end;

	SoundParamBlockPtr = ^SoundParamBlock;
{$ifc TYPED_FUNCTION_POINTERS}
	SoundParamProcPtr = function(var pb: SoundParamBlockPtr): boolean;
{$elsec}
	SoundParamProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SoundParamUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SoundParamUPP = UniversalProcPtr;
{$endc}	
	SoundParamBlock = record
		recordSize:				SInt32;								{ size of this record in bytes }
		desc:					SoundComponentData;						{ description of sound buffer }
		rateMultiplier:			UnsignedFixed;							{ rate multiplier to apply to sound }
		leftVolume:				SInt16;								{ volumes to apply to sound }
		rightVolume:			SInt16;
		quality:				SInt32;								{ quality to apply to sound }
		filter:					ComponentInstance;						{ filter to apply to sound }
		moreRtn:				SoundParamUPP;							{ routine to call to get more data }
		completionRtn:			SoundParamUPP;							{ routine to call when buffer is complete }
		refCon:					SInt32;								{ user refcon }
		result:					SInt16;								{ result }
	end;

	ExtendedSoundParamBlockPtr = ^ExtendedSoundParamBlock;
	ExtendedSoundParamBlock = record
		pb:						SoundParamBlock;						{ classic SoundParamBlock except recordSize == sizeof(ExtendedSoundParamBlock) }
		reserved:				SInt16;
		extendedFlags:			SInt32;								{ flags }
		bufferSize:				SInt32;								{ size of buffer in bytes }
	end;

	CompressionInfoPtr = ^CompressionInfo;
	CompressionInfo = record
		recordSize:				SInt32;
		format:					OSType;
		compressionID:			SInt16;
		samplesPerPacket:		UInt16;
		bytesPerPacket:			UInt16;
		bytesPerFrame:			UInt16;
		bytesPerSample:			UInt16;
		futureUse1:				UInt16;
	end;

	CompressionInfoHandle				= ^CompressionInfoPtr;
	{ variables for floating point conversion }
	SoundSlopeAndInterceptRecordPtr = ^SoundSlopeAndInterceptRecord;
	SoundSlopeAndInterceptRecord = record
		slope:					Float64;
		intercept:				Float64;
		minClip:				Float64;
		maxClip:				Float64;
	end;

	SoundSlopeAndInterceptPtr			= ^SoundSlopeAndInterceptRecord;
	{ private thing to use as a reference to a Sound Converter }
	SoundConverter    = ^SInt32; { an opaque 32-bit type }
	SoundConverterPtr = ^SoundConverter;  { when a var xx:SoundConverter parameter can be nil, it is changed to xx: SoundConverterPtr }
	{ callback routine to provide data to the Sound Converter }
{$ifc TYPED_FUNCTION_POINTERS}
	SoundConverterFillBufferDataProcPtr = function(var data: SoundComponentDataPtr; refCon: UnivPtr): boolean;
{$elsec}
	SoundConverterFillBufferDataProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SoundConverterFillBufferDataUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SoundConverterFillBufferDataUPP = UniversalProcPtr;
{$endc}	
	{ private thing to use as a reference to a Sound Source }
	SoundSource    = ^SInt32; { an opaque 32-bit type }
	SoundSourcePtr = ^SoundSource;  { when a var xx:SoundSource parameter can be nil, it is changed to xx: SoundSourcePtr }
	SoundComponentLinkPtr = ^SoundComponentLink;
	SoundComponentLink = record
		description:			ComponentDescription;					{ Describes the sound component }
		mixerID:				SoundSource;							{ Reserved by Apple }
		linkID:					SoundSourcePtr;							{ Reserved by Apple }
	end;

	AudioInfoPtr = ^AudioInfo;
	AudioInfo = record
		capabilitiesFlags:		SInt32;								{ Describes device capabilities }
		reserved:				SInt32;								{ Reserved by Apple }
		numVolumeSteps:			UInt16;									{ Number of significant increments between min and max volume }
	end;

	AudioFormatAtomPtr = ^AudioFormatAtom;
	AudioFormatAtom = record
		size:					SInt32;								{  = sizeof(AudioFormatAtom) }
		atomType:				OSType;									{  = kAudioFormatAtomType }
		format:					OSType;
	end;

	AudioEndianAtomPtr = ^AudioEndianAtom;
	AudioEndianAtom = record
		size:					SInt32;								{  = sizeof(AudioEndianAtom) }
		atomType:				OSType;									{  = kAudioEndianAtomType }
		littleEndian:			SInt16;
	end;

	AudioTerminatorAtomPtr = ^AudioTerminatorAtom;
	AudioTerminatorAtom = record
		size:					SInt32;								{  = sizeof(AudioTerminatorAtom) }
		atomType:				OSType;									{  = kAudioTerminatorAtomType }
	end;

	LevelMeterInfoPtr = ^LevelMeterInfo;
	LevelMeterInfo = record
		numChannels:			SInt16;								{  mono or stereo source }
		leftMeter:				SInt8;									{  0-255 range }
		rightMeter:				SInt8;									{  0-255 range }
	end;

	EQSpectrumBandsRecordPtr = ^EQSpectrumBandsRecord;
	EQSpectrumBandsRecord = record
		count:					SInt16;
		frequency:				UnsignedFixedPtr;						{  pointer to array of frequencies }
	end;

	{  Sound Input Structures }
	SPBPtr = ^SPB;
	{	user procedures called by sound input routines	}
{$ifc TYPED_FUNCTION_POINTERS}
	SIInterruptProcPtr = procedure(inParamPtr: SPBPtr; dataBuffer: Ptr; peakAmplitude: SInt16; sampleSize: SInt32);
{$elsec}
	SIInterruptProcPtr = Register68kProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SICompletionProcPtr = procedure(inParamPtr: SPBPtr);
{$elsec}
	SICompletionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SIInterruptUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SIInterruptUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SICompletionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SICompletionUPP = UniversalProcPtr;
{$endc}	
	{	Sound Input Parameter Block	}
	SPB = record
		inRefNum:				SInt32;								{ reference number of sound input device }
		count:					UInt32;									{ number of bytes to record }
		milliseconds:			UInt32;									{ number of milliseconds to record }
		bufferLength:			UInt32;									{ length of buffer in bytes }
		bufferPtr:				Ptr;									{ buffer to store sound data in }
		completionRoutine:		SICompletionUPP;						{ completion routine }
		interruptRoutine:		SIInterruptUPP;							{ interrupt routine }
		userLong:				SInt32;								{ user-defined field }
		error:					OSErr;									{ error }
		unused1:				SInt32;								{ reserved - must be zero }
	end;


const
	uppSoundParamProcInfo = $000000D0;
	uppSoundConverterFillBufferDataProcInfo = $000003D0;
	uppSIInterruptProcInfo = $1C579802;
	uppSICompletionProcInfo = $000000C0;
	{
	 *  NewSoundParamUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSoundParamUPP(userRoutine: SoundParamProcPtr): SoundParamUPP; external name '_NewSoundParamUPP'; { old name was NewSoundParamProc }
{
 *  NewSoundConverterFillBufferDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSoundConverterFillBufferDataUPP(userRoutine: SoundConverterFillBufferDataProcPtr): SoundConverterFillBufferDataUPP; external name '_NewSoundConverterFillBufferDataUPP'; { old name was NewSoundConverterFillBufferDataProc }
{
 *  NewSIInterruptUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSIInterruptUPP(userRoutine: SIInterruptProcPtr): SIInterruptUPP; external name '_NewSIInterruptUPP'; { old name was NewSIInterruptProc }
{
 *  NewSICompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSICompletionUPP(userRoutine: SICompletionProcPtr): SICompletionUPP; external name '_NewSICompletionUPP'; { old name was NewSICompletionProc }
{
 *  DisposeSoundParamUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSoundParamUPP(userUPP: SoundParamUPP); external name '_DisposeSoundParamUPP';
{
 *  DisposeSoundConverterFillBufferDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSoundConverterFillBufferDataUPP(userUPP: SoundConverterFillBufferDataUPP); external name '_DisposeSoundConverterFillBufferDataUPP';
{
 *  DisposeSIInterruptUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSIInterruptUPP(userUPP: SIInterruptUPP); external name '_DisposeSIInterruptUPP';
{
 *  DisposeSICompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSICompletionUPP(userUPP: SICompletionUPP); external name '_DisposeSICompletionUPP';
{
 *  InvokeSoundParamUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSoundParamUPP(var pb: SoundParamBlockPtr; userRoutine: SoundParamUPP): boolean; external name '_InvokeSoundParamUPP'; { old name was CallSoundParamProc }
{
 *  InvokeSoundConverterFillBufferDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSoundConverterFillBufferDataUPP(var data: SoundComponentDataPtr; refCon: UnivPtr; userRoutine: SoundConverterFillBufferDataUPP): boolean; external name '_InvokeSoundConverterFillBufferDataUPP'; { old name was CallSoundConverterFillBufferDataProc }
{
 *  InvokeSIInterruptUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSIInterruptUPP(inParamPtr: SPBPtr; dataBuffer: Ptr; peakAmplitude: SInt16; sampleSize: SInt32; userRoutine: SIInterruptUPP); external name '_InvokeSIInterruptUPP'; { old name was CallSIInterruptProc }
{
 *  InvokeSICompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSICompletionUPP(inParamPtr: SPBPtr; userRoutine: SICompletionUPP); external name '_InvokeSICompletionUPP'; { old name was CallSICompletionProc }
type
{$ifc TYPED_FUNCTION_POINTERS}
	FilePlayCompletionProcPtr = procedure(chan: SndChannelPtr);
{$elsec}
	FilePlayCompletionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	FilePlayCompletionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FilePlayCompletionUPP = UniversalProcPtr;
{$endc}	

const
	uppFilePlayCompletionProcInfo = $000000C0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewFilePlayCompletionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewFilePlayCompletionUPP(userRoutine: FilePlayCompletionProcPtr): FilePlayCompletionUPP; external name '_NewFilePlayCompletionUPP'; { old name was NewFilePlayCompletionProc }
{
 *  DisposeFilePlayCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeFilePlayCompletionUPP(userUPP: FilePlayCompletionUPP); external name '_DisposeFilePlayCompletionUPP';
{
 *  InvokeFilePlayCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InvokeFilePlayCompletionUPP(chan: SndChannelPtr; userRoutine: FilePlayCompletionUPP); external name '_InvokeFilePlayCompletionUPP'; { old name was CallFilePlayCompletionProc }
{$endc}  {CALL_NOT_IN_CARBON}

{
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   prototypes
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
{ Sound Manager routines }
{
 *  SysBeep()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SysBeep(duration: SInt16); external name '_SysBeep';
{
 *  SndDoCommand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndDoCommand(chan: SndChannelPtr; const (*var*) cmd: SndCommand; noWait: boolean): OSErr; external name '_SndDoCommand';
{
 *  SndDoImmediate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndDoImmediate(chan: SndChannelPtr; const (*var*) cmd: SndCommand): OSErr; external name '_SndDoImmediate';
{
 *  SndNewChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndNewChannel(var chan: SndChannelPtr; synth: SInt16; init: SInt32; userRoutine: SndCallBackUPP): OSErr; external name '_SndNewChannel';
{
 *  SndDisposeChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndDisposeChannel(chan: SndChannelPtr; quietNow: boolean): OSErr; external name '_SndDisposeChannel';
{
 *  SndPlay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndPlay(chan: SndChannelPtr; sndHandle: SndListHandle; async: boolean): OSErr; external name '_SndPlay';
{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  SndAddModifier()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndAddModifier(chan: SndChannelPtr; modifier: Ptr; id: SInt16; init: SInt32): OSErr; external name '_SndAddModifier';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}

{$ifc CALL_NOT_IN_CARBON}
{
 *  SndControl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndControl(id: SInt16; var cmd: SndCommand): OSErr; external name '_SndControl';
{ Sound Manager 2.0 and later, uses _SoundDispatch }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SndSoundManagerVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndSoundManagerVersion: NumVersion; external name '_SndSoundManagerVersion';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SndStartFilePlay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndStartFilePlay(chan: SndChannelPtr; fRefNum: SInt16; resNum: SInt16; bufferSize: SInt32; theBuffer: UnivPtr; theSelection: AudioSelectionPtr; theCompletion: FilePlayCompletionUPP; async: boolean): OSErr; external name '_SndStartFilePlay';
{
 *  SndPauseFilePlay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndPauseFilePlay(chan: SndChannelPtr): OSErr; external name '_SndPauseFilePlay';
{
 *  SndStopFilePlay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndStopFilePlay(chan: SndChannelPtr; quietNow: boolean): OSErr; external name '_SndStopFilePlay';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SndChannelStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndChannelStatus(chan: SndChannelPtr; theLength: SInt16; theStatus: SCStatusPtr): OSErr; external name '_SndChannelStatus';
{
 *  SndManagerStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndManagerStatus(theLength: SInt16; theStatus: SMStatusPtr): OSErr; external name '_SndManagerStatus';
{
 *  SndGetSysBeepState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SndGetSysBeepState(var sysBeepState: SInt16); external name '_SndGetSysBeepState';
{
 *  SndSetSysBeepState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndSetSysBeepState(sysBeepState: SInt16): OSErr; external name '_SndSetSysBeepState';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SndPlayDoubleBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndPlayDoubleBuffer(chan: SndChannelPtr; theParams: SndDoubleBufferHeaderPtr): OSErr; external name '_SndPlayDoubleBuffer';
{ MACE compression routines, uses _SoundDispatch }
{
 *  MACEVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function MACEVersion: NumVersion; external name '_MACEVersion';
{
 *  Comp3to1()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Comp3to1(inBuffer: UnivPtr; outBuffer: UnivPtr; cnt: UInt32; inState: StateBlockPtr; outState: StateBlockPtr; numChannels: UInt32; whichChannel: UInt32); external name '_Comp3to1';
{
 *  Exp1to3()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Exp1to3(inBuffer: UnivPtr; outBuffer: UnivPtr; cnt: UInt32; inState: StateBlockPtr; outState: StateBlockPtr; numChannels: UInt32; whichChannel: UInt32); external name '_Exp1to3';
{
 *  Comp6to1()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Comp6to1(inBuffer: UnivPtr; outBuffer: UnivPtr; cnt: UInt32; inState: StateBlockPtr; outState: StateBlockPtr; numChannels: UInt32; whichChannel: UInt32); external name '_Comp6to1';
{
 *  Exp1to6()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure Exp1to6(inBuffer: UnivPtr; outBuffer: UnivPtr; cnt: UInt32; inState: StateBlockPtr; outState: StateBlockPtr; numChannels: UInt32; whichChannel: UInt32); external name '_Exp1to6';
{ Sound Manager 3.0 and later calls, uses _SoundDispatch }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  GetSysBeepVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSysBeepVolume(var level: SInt32): OSErr; external name '_GetSysBeepVolume';
{
 *  SetSysBeepVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSysBeepVolume(level: SInt32): OSErr; external name '_SetSysBeepVolume';
{
 *  GetDefaultOutputVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDefaultOutputVolume(var level: SInt32): OSErr; external name '_GetDefaultOutputVolume';
{
 *  SetDefaultOutputVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetDefaultOutputVolume(level: SInt32): OSErr; external name '_SetDefaultOutputVolume';
{
 *  GetSoundHeaderOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSoundHeaderOffset(sndHandle: SndListHandle; var offset: SInt32): OSErr; external name '_GetSoundHeaderOffset';
{
 *  UnsignedFixedMulDiv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function UnsignedFixedMulDiv(value: UnsignedFixed; multiplier: UnsignedFixed; divisor: UnsignedFixed): UnsignedFixed; external name '_UnsignedFixedMulDiv';
{
 *  GetCompressionInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCompressionInfo(compressionID: SInt16; format: OSType; numChannels: SInt16; sampleSize: SInt16; cp: CompressionInfoPtr): OSErr; external name '_GetCompressionInfo';
{
 *  SetSoundPreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSoundPreference(theType: OSType; var name: Str255; settings: Handle): OSErr; external name '_SetSoundPreference';
{
 *  GetSoundPreference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSoundPreference(theType: OSType; var name: Str255; settings: Handle): OSErr; external name '_GetSoundPreference';
{
 *  OpenMixerSoundComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OpenMixerSoundComponent(outputDescription: SoundComponentDataPtr; outputFlags: SInt32; var mixerComponent: ComponentInstance): OSErr; external name '_OpenMixerSoundComponent';
{
 *  CloseMixerSoundComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CloseMixerSoundComponent(ci: ComponentInstance): OSErr; external name '_CloseMixerSoundComponent';
{ Sound Manager 3.1 and later calls, uses _SoundDispatch }
{
 *  SndGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndGetInfo(chan: SndChannelPtr; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SndGetInfo';
{
 *  SndSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndSetInfo(chan: SndChannelPtr; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SndSetInfo';
{
 *  GetSoundOutputInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSoundOutputInfo(outputDevice: Component; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_GetSoundOutputInfo';
{
 *  SetSoundOutputInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSoundOutputInfo(outputDevice: Component; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SetSoundOutputInfo';
{ Sound Manager 3.2 and later calls, uses _SoundDispatch }
{
 *  GetCompressionName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCompressionName(compressionType: OSType; var compressionName: Str255): OSErr; external name '_GetCompressionName';
{
 *  SoundConverterOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterOpen(const (*var*) inputFormat: SoundComponentData; const (*var*) outputFormat: SoundComponentData; var sc: SoundConverter): OSErr; external name '_SoundConverterOpen';
{
 *  SoundConverterClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterClose(sc: SoundConverter): OSErr; external name '_SoundConverterClose';
{
 *  SoundConverterGetBufferSizes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterGetBufferSizes(sc: SoundConverter; inputBytesTarget: UInt32; var inputFrames: UInt32; var inputBytes: UInt32; var outputBytes: UInt32): OSErr; external name '_SoundConverterGetBufferSizes';
{
 *  SoundConverterBeginConversion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterBeginConversion(sc: SoundConverter): OSErr; external name '_SoundConverterBeginConversion';
{
 *  SoundConverterConvertBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterConvertBuffer(sc: SoundConverter; inputPtr: UnivPtr; inputFrames: UInt32; outputPtr: UnivPtr; var outputFrames: UInt32; var outputBytes: UInt32): OSErr; external name '_SoundConverterConvertBuffer';
{
 *  SoundConverterEndConversion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterEndConversion(sc: SoundConverter; outputPtr: UnivPtr; var outputFrames: UInt32; var outputBytes: UInt32): OSErr; external name '_SoundConverterEndConversion';
{ Sound Manager 3.3 and later calls, uses _SoundDispatch }
{
 *  SoundConverterGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.3 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterGetInfo(sc: SoundConverter; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SoundConverterGetInfo';
{
 *  SoundConverterSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.3 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterSetInfo(sc: SoundConverter; selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SoundConverterSetInfo';
{ Sound Manager 3.6 and later calls, uses _SoundDispatch }
{
 *  SoundConverterFillBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.6 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundConverterFillBuffer(sc: SoundConverter; fillBufferDataUPP: SoundConverterFillBufferDataUPP; fillBufferDataRefCon: UnivPtr; outputBuffer: UnivPtr; outputBufferByteSize: UInt32; var bytesWritten: UInt32; var framesWritten: UInt32; var outputFlags: UInt32): OSErr; external name '_SoundConverterFillBuffer';
{
 *  SoundManagerGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.6 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundManagerGetInfo(selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SoundManagerGetInfo';
{
 *  SoundManagerSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.6 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundManagerSetInfo(selector: OSType; infoPtr: UnivPtr): OSErr; external name '_SoundManagerSetInfo';
{
  Sound Component Functions
   basic sound component functions
}

{
 *  SoundComponentInitOutputDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentInitOutputDevice(ti: ComponentInstance; actions: SInt32): ComponentResult; external name '_SoundComponentInitOutputDevice';
{
 *  SoundComponentSetSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentSetSource(ti: ComponentInstance; sourceID: SoundSource; source: ComponentInstance): ComponentResult; external name '_SoundComponentSetSource';
{
 *  SoundComponentGetSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentGetSource(ti: ComponentInstance; sourceID: SoundSource; var source: ComponentInstance): ComponentResult; external name '_SoundComponentGetSource';
{
 *  SoundComponentGetSourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentGetSourceData(ti: ComponentInstance; var sourceData: SoundComponentDataPtr): ComponentResult; external name '_SoundComponentGetSourceData';
{
 *  SoundComponentSetOutput()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentSetOutput(ti: ComponentInstance; requested: SoundComponentDataPtr; var actual: SoundComponentDataPtr): ComponentResult; external name '_SoundComponentSetOutput';
{  junction methods for the mixer, must be called at non-interrupt level }
{
 *  SoundComponentAddSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentAddSource(ti: ComponentInstance; var sourceID: SoundSource): ComponentResult; external name '_SoundComponentAddSource';
{
 *  SoundComponentRemoveSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentRemoveSource(ti: ComponentInstance; sourceID: SoundSource): ComponentResult; external name '_SoundComponentRemoveSource';
{  info methods }
{
 *  SoundComponentGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentGetInfo(ti: ComponentInstance; sourceID: SoundSource; selector: OSType; infoPtr: UnivPtr): ComponentResult; external name '_SoundComponentGetInfo';
{
 *  SoundComponentSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentSetInfo(ti: ComponentInstance; sourceID: SoundSource; selector: OSType; infoPtr: UnivPtr): ComponentResult; external name '_SoundComponentSetInfo';
{  control methods }
{
 *  SoundComponentStartSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentStartSource(ti: ComponentInstance; count: SInt16; var sources: SoundSource): ComponentResult; external name '_SoundComponentStartSource';
{
 *  SoundComponentStopSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentStopSource(ti: ComponentInstance; count: SInt16; var sources: SoundSource): ComponentResult; external name '_SoundComponentStopSource';
{
 *  SoundComponentPauseSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentPauseSource(ti: ComponentInstance; count: SInt16; var sources: SoundSource): ComponentResult; external name '_SoundComponentPauseSource';
{
 *  SoundComponentPlaySourceBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SoundComponentPlaySourceBuffer(ti: ComponentInstance; sourceID: SoundSource; pb: SoundParamBlockPtr; actions: SInt32): ComponentResult; external name '_SoundComponentPlaySourceBuffer';
{ Audio Components }
{Volume is described as a value between 0 and 1, with 0 indicating minimum
  volume and 1 indicating maximum volume; if the device doesn't support
  software control of volume, then a value of unimpErr is returned, indicating
  that these functions are not supported by the device
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  AudioGetVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetVolume(ac: ComponentInstance; whichChannel: SInt16; var volume: ShortFixed): ComponentResult; external name '_AudioGetVolume';
{
 *  AudioSetVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioSetVolume(ac: ComponentInstance; whichChannel: SInt16; volume: ShortFixed): ComponentResult; external name '_AudioSetVolume';
{If the device doesn't support software control of mute, then a value of unimpErr is
returned, indicating that these functions are not supported by the device.}
{
 *  AudioGetMute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetMute(ac: ComponentInstance; whichChannel: SInt16; var mute: SInt16): ComponentResult; external name '_AudioGetMute';
{
 *  AudioSetMute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioSetMute(ac: ComponentInstance; whichChannel: SInt16; mute: SInt16): ComponentResult; external name '_AudioSetMute';
{AudioSetToDefaults causes the associated device to reset its volume and mute values
(and perhaps other characteristics, e.g. attenuation) to "factory default" settings}
{
 *  AudioSetToDefaults()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioSetToDefaults(ac: ComponentInstance): ComponentResult; external name '_AudioSetToDefaults';
{ This routine is required; it must be implemented by all audio components }

{
 *  AudioGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetInfo(ac: ComponentInstance; info: AudioInfoPtr): ComponentResult; external name '_AudioGetInfo';
{
 *  AudioGetBass()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetBass(ac: ComponentInstance; whichChannel: SInt16; var bass: SInt16): ComponentResult; external name '_AudioGetBass';
{
 *  AudioSetBass()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioSetBass(ac: ComponentInstance; whichChannel: SInt16; bass: SInt16): ComponentResult; external name '_AudioSetBass';
{
 *  AudioGetTreble()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetTreble(ac: ComponentInstance; whichChannel: SInt16; var Treble: SInt16): ComponentResult; external name '_AudioGetTreble';
{
 *  AudioSetTreble()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioSetTreble(ac: ComponentInstance; whichChannel: SInt16; Treble: SInt16): ComponentResult; external name '_AudioSetTreble';
{
 *  AudioGetOutputDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioGetOutputDevice(ac: ComponentInstance; var outputDevice: Component): ComponentResult; external name '_AudioGetOutputDevice';
{ This is routine is private to the AudioVision component.  It enables the watching of the mute key. }
{
 *  AudioMuteOnEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AudioMuteOnEvent(ac: ComponentInstance; muteOnEvent: SInt16): ComponentResult; external name '_AudioMuteOnEvent';
{$endc}  {CALL_NOT_IN_CARBON}


const
	kDelegatedSoundComponentSelectors = $0100;

	{	 Sound Input Manager routines, uses _SoundDispatch 	}
	{
	 *  SPBVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function SPBVersion: NumVersion; external name '_SPBVersion';
{
 *  SndRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SndRecord(filterProc: ModalFilterUPP; corner: Point; quality: OSType; var sndHandle: SndListHandle): OSErr; external name '_SndRecord';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SndRecordToFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SndRecordToFile(filterProc: ModalFilterUPP; corner: Point; quality: OSType; fRefNum: SInt16): OSErr; external name '_SndRecordToFile';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SPBSignInDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBSignInDevice(deviceRefNum: SInt16; const (*var*) deviceName: Str255): OSErr; external name '_SPBSignInDevice';
{
 *  SPBSignOutDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBSignOutDevice(deviceRefNum: SInt16): OSErr; external name '_SPBSignOutDevice';
{
 *  SPBGetIndexedDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBGetIndexedDevice(count: SInt16; var deviceName: Str255; var deviceIconHandle: Handle): OSErr; external name '_SPBGetIndexedDevice';
{
 *  SPBOpenDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBOpenDevice(const (*var*) deviceName: Str255; permission: SInt16; var inRefNum: SInt32): OSErr; external name '_SPBOpenDevice';
{
 *  SPBCloseDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBCloseDevice(inRefNum: SInt32): OSErr; external name '_SPBCloseDevice';
{
 *  SPBRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBRecord(inParamPtr: SPBPtr; asynchFlag: boolean): OSErr; external name '_SPBRecord';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SPBRecordToFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SPBRecordToFile(fRefNum: SInt16; inParamPtr: SPBPtr; asynchFlag: boolean): OSErr; external name '_SPBRecordToFile';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  SPBPauseRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBPauseRecording(inRefNum: SInt32): OSErr; external name '_SPBPauseRecording';
{
 *  SPBResumeRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBResumeRecording(inRefNum: SInt32): OSErr; external name '_SPBResumeRecording';
{
 *  SPBStopRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBStopRecording(inRefNum: SInt32): OSErr; external name '_SPBStopRecording';
{
 *  SPBGetRecordingStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBGetRecordingStatus(inRefNum: SInt32; var recordingStatus: SInt16; var meterLevel: SInt16; var totalSamplesToRecord: UInt32; var numberOfSamplesRecorded: UInt32; var totalMsecsToRecord: UInt32; var numberOfMsecsRecorded: UInt32): OSErr; external name '_SPBGetRecordingStatus';
{
 *  SPBGetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBGetDeviceInfo(inRefNum: SInt32; infoType: OSType; infoData: UnivPtr): OSErr; external name '_SPBGetDeviceInfo';
{
 *  SPBSetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBSetDeviceInfo(inRefNum: SInt32; infoType: OSType; infoData: UnivPtr): OSErr; external name '_SPBSetDeviceInfo';
{
 *  SPBMillisecondsToBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBMillisecondsToBytes(inRefNum: SInt32; var milliseconds: SInt32): OSErr; external name '_SPBMillisecondsToBytes';
{
 *  SPBBytesToMilliseconds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SPBBytesToMilliseconds(inRefNum: SInt32; var byteCount: SInt32): OSErr; external name '_SPBBytesToMilliseconds';
{
 *  SetupSndHeader()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetupSndHeader(sndHandle: SndListHandle; numChannels: SInt16; sampleRate: UnsignedFixed; sampleSize: SInt16; compressionType: OSType; baseNote: SInt16; numBytes: UInt32; var headerLen: SInt16): OSErr; external name '_SetupSndHeader';
{
 *  SetupAIFFHeader()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetupAIFFHeader(fRefNum: SInt16; numChannels: SInt16; sampleRate: UnsignedFixed; sampleSize: SInt16; compressionType: OSType; numBytes: UInt32; numFrames: UInt32): OSErr; external name '_SetupAIFFHeader';
{ Sound Input Manager 1.1 and later calls, uses _SoundDispatch }
{
 *  ParseAIFFHeader()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ParseAIFFHeader(fRefNum: SInt16; var sndInfo: SoundComponentData; var numFrames: UInt32; var dataOffset: UInt32): OSErr; external name '_ParseAIFFHeader';
{
 *  ParseSndHeader()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SoundLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ParseSndHeader(sndHandle: SndListHandle; var sndInfo: SoundComponentData; var numFrames: UInt32; var dataOffset: UInt32): OSErr; external name '_ParseSndHeader';
{$ifc NOT TARGET_OS_MAC OR TARGET_API_MAC_CARBON}
{  Only to be used if you are writing a sound input component; this }
{  is the param block for a read request from the SoundMgr to the   }
{  sound input component.  Not to be confused with the SPB struct   }
{  above, which is the param block for a read request from an app   }
{  to the SoundMgr.                                                 }

type
	SndInputCmpParamPtr = ^SndInputCmpParam;
{$ifc TYPED_FUNCTION_POINTERS}
	SICCompletionProcPtr = procedure(SICParmPtr: SndInputCmpParamPtr);
{$elsec}
	SICCompletionProcPtr = ProcPtr;
{$endc}

	SndInputCmpParam = record
		ioCompletion:			SICCompletionProcPtr;					{  completion routine [pointer] }
		ioInterrupt:			SIInterruptProcPtr;						{  interrupt routine [pointer] }
		ioResult:				OSErr;									{  I/O result code [word] }
		pad:					SInt16;
		ioReqCount:				UInt32;
		ioActCount:				UInt32;
		ioBuffer:				Ptr;
		ioMisc:					Ptr;
	end;

	{
	 *  SndInputReadAsync()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         in version 10.0 and later
	 	}
function SndInputReadAsync(self: ComponentInstance; SICParmPtr: SndInputCmpParamPtr): ComponentResult; external name '_SndInputReadAsync';
{
 *  SndInputReadSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputReadSync(self: ComponentInstance; SICParmPtr: SndInputCmpParamPtr): ComponentResult; external name '_SndInputReadSync';
{
 *  SndInputPauseRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputPauseRecording(self: ComponentInstance): ComponentResult; external name '_SndInputPauseRecording';
{
 *  SndInputResumeRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputResumeRecording(self: ComponentInstance): ComponentResult; external name '_SndInputResumeRecording';
{
 *  SndInputStopRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputStopRecording(self: ComponentInstance): ComponentResult; external name '_SndInputStopRecording';
{
 *  SndInputGetStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputGetStatus(self: ComponentInstance; var recordingStatus: SInt16; var totalSamplesToRecord: UInt32; var numberOfSamplesRecorded: UInt32): ComponentResult; external name '_SndInputGetStatus';
{
 *  SndInputGetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputGetDeviceInfo(self: ComponentInstance; infoType: OSType; infoData: UnivPtr): ComponentResult; external name '_SndInputGetDeviceInfo';
{
 *  SndInputSetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputSetDeviceInfo(self: ComponentInstance; infoType: OSType; infoData: UnivPtr): ComponentResult; external name '_SndInputSetDeviceInfo';
{
 *  SndInputInitHardware()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function SndInputInitHardware(self: ComponentInstance): ComponentResult; external name '_SndInputInitHardware';
{$endc}


{$ALIGN MAC68K}


end.
