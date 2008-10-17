{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

//
// Module Name:
//
//     mmreg.h
//
// Abstract:
//
//     Multimedia Registration
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit mmreg;

interface

uses Windows;

{
  Automatically converted by H2Pas 1.0.0 from mmreg.h
  The following command line parameters were used:
    -d
    -w
    -D
    -l
    mmreg.h
}

{ Define the following to skip definitions }
{ }
{ NOMMIDS      Multimedia IDs are not defined }
{ NONEWWAVE    No new waveform types are defined except WAVEFORMATEX }
{ NONEWRIFF    No new RIFF forms are defined }
{ NOJPEGDIB    No JPEG DIB definitions }
{ NONEWIC      No new Image Compressor types are defined }
{ NOBITMAP     No extended bitmap info header definition }

{$PACKRECORDS 1} //  {#include "pshpack1.h"   /* Assume byte packing throughout */ }


type
     FOURCC = DWORD;         //* a four character code */

{$IFNDEF NOMMIDS}
// manufacturer IDs  
const
     MM_MICROSOFT = 1; //  Microsoft Corporation

     MM_CREATIVE = 2; // Creative Labs, Inc
  {  Media Vision, Inc.  }
     MM_MEDIAVISION = 3;     
  {  Fujitsu Corp.  }
     MM_FUJITSU = 4;     
  {  Artisoft, Inc.  }
     MM_ARTISOFT = 20;     
  {  Turtle Beach, Inc.  }
     MM_TURTLE_BEACH = 21;     
  {  IBM Corporation  }
     MM_IBM = 22;     
  {  Vocaltec LTD.  }
     MM_VOCALTEC = 23;     
  {  Roland  }
     MM_ROLAND = 24;     
  {  DSP Solutions, Inc.  }
     MM_DSP_SOLUTIONS = 25;     
  {  NEC  }
     MM_NEC = 26;     
  {  ATI  }
     MM_ATI = 27;     
  {  Wang Laboratories, Inc  }
     MM_WANGLABS = 28;     
  {  Tandy Corporation  }
     MM_TANDY = 29;     
  {  Voyetra  }
     MM_VOYETRA = 30;     
  {  Antex Electronics Corporation  }
     MM_ANTEX = 31;     
  {  ICL Personal Systems  }
     MM_ICL_PS = 32;     
  {  Intel Corporation  }
     MM_INTEL = 33;     
  {  Advanced Gravis  }
     MM_GRAVIS = 34;     
  {  Video Associates Labs, Inc.  }
     MM_VAL = 35;     
  {  InterActive Inc  }
     MM_INTERACTIVE = 36;     
  {  Yamaha Corporation of America  }
     MM_YAMAHA = 37;     
  {  Everex Systems, Inc  }
     MM_EVEREX = 38;     
  {  Echo Speech Corporation  }
     MM_ECHO = 39;     
  {  Sierra Semiconductor Corp  }
     MM_SIERRA = 40;     
  {  Computer Aided Technologies  }
     MM_CAT = 41;     
  {  APPS Software International  }
     MM_APPS = 42;     
  {  DSP Group, Inc  }
     MM_DSP_GROUP = 43;     
  {  microEngineering Labs  }
     MM_MELABS = 44;     
  {  Computer Friends, Inc.  }
     MM_COMPUTER_FRIENDS = 45;     
  {  ESS Technology  }
     MM_ESS = 46;     
  {  Audio, Inc.  }
     MM_AUDIOFILE = 47;     
  {  Motorola, Inc.  }
     MM_MOTOROLA = 48;     
  {  Canopus, co., Ltd.  }
     MM_CANOPUS = 49;     
  {  Seiko Epson Corporation  }
     MM_EPSON = 50;     
  {  Truevision  }
     MM_TRUEVISION = 51;     
  {  Aztech Labs, Inc.  }
     MM_AZTECH = 52;     
  {  Videologic  }
     MM_VIDEOLOGIC = 53;     
  {  SCALACS  }
     MM_SCALACS = 54;     
  {  Korg Inc.  }
     MM_KORG = 55;     
  {  Audio Processing Technology  }
     MM_APT = 56;     
  {  Integrated Circuit Systems, Inc.  }
     MM_ICS = 57;     
  {  Iterated Systems, Inc.  }
     MM_ITERATEDSYS = 58;     
  {  Metheus  }
     MM_METHEUS = 59;     
  {  Logitech, Inc.  }
     MM_LOGITECH = 60;     
  {  Winnov, Inc.  }
     MM_WINNOV = 61;     
  {  NCR Corporation  }
     MM_NCR = 62;     
  {  EXAN  }
     MM_EXAN = 63;     
  {  AST Research Inc.  }
     MM_AST = 64;     
  {  Willow Pond Corporation  }
     MM_WILLOWPOND = 65;     
  {  Sonic Foundry  }
     MM_SONICFOUNDRY = 66;     
  {  Vitec Multimedia  }
     MM_VITEC = 67;     
  {  MOSCOM Corporation  }
     MM_MOSCOM = 68;     
  {  Silicon Soft, Inc.  }
     MM_SILICONSOFT = 69;     
  {  Supermac  }
     MM_SUPERMAC = 73;     
  {  Audio Processing Technology  }
     MM_AUDIOPT = 74;     
  {  Speech Compression  }
     MM_SPEECHCOMP = 76;     
  {  Ahead, Inc.  }
     MM_AHEAD = 77;     
  {  Dolby Laboratories  }
     MM_DOLBY = 78;     
  {  OKI  }
     MM_OKI = 79;     
  {  AuraVision Corporation  }
     MM_AURAVISION = 80;     
  {  Ing C. Olivetti & C., S.p.A.  }
     MM_OLIVETTI = 81;     
  {  I/O Magic Corporation  }
     MM_IOMAGIC = 82;     
  {  Matsushita Electric Industrial Co., LTD.  }
     MM_MATSUSHITA = 83;     
  {  Control Resources Limited  }
     MM_CONTROLRES = 84;     
  {  Xebec Multimedia Solutions Limited  }
     MM_XEBEC = 85;     
  {  New Media Corporation  }
     MM_NEWMEDIA = 86;     
  {  Natural MicroSystems  }
     MM_NMS = 87;     
  {  Lyrrus Inc.  }
     MM_LYRRUS = 88;     
  {  Compusic  }
     MM_COMPUSIC = 89;     
  {  OPTi Computers Inc.  }
     MM_OPTI = 90;     
  {  Adlib Accessories Inc.  }
     MM_ADLACC = 91;     
  {  Compaq Computer Corp.  }
     MM_COMPAQ = 92;     
  {  Dialogic Corporation  }
     MM_DIALOGIC = 93;     
  {  InSoft, Inc.  }
     MM_INSOFT = 94;     
  {  M.P. Technologies, Inc.  }
     MM_MPTUS = 95;     
  {  Weitek  }
     MM_WEITEK = 96;     
  {  Lernout & Hauspie  }
     MM_LERNOUT_AND_HAUSPIE = 97;     
  {  Quanta Computer Inc.  }
     MM_QCIAR = 98;     
  {  Apple Computer, Inc.  }
     MM_APPLE = 99;     
  {  Digital Equipment Corporation  }
     MM_DIGITAL = 100;     
  {  Mark of the Unicorn  }
     MM_MOTU = 101;     
  {  Workbit Corporation  }
     MM_WORKBIT = 102;     
  {  Ositech Communications Inc.  }
     MM_OSITECH = 103;     
  {  miro Computer Products AG  }
     MM_MIRO = 104;     
  {  Cirrus Logic  }
     MM_CIRRUSLOGIC = 105;     
  {  ISOLUTION  B.V.  }
     MM_ISOLUTION = 106;     
  {  Horizons Technology, Inc  }
     MM_HORIZONS = 107;     
  {  Computer Concepts Ltd  }
     MM_CONCEPTS = 108;     
  {  Voice Technologies Group, Inc.  }
     MM_VTG = 109;     
  {  Radius  }
     MM_RADIUS = 110;     
  {  Rockwell International  }
     MM_ROCKWELL = 111;     
  {  Co. XYZ for testing  }
     MM_XYz = 112;     
  {  Opcode Systems  }
     MM_OPCODE = 113;     
  {  Voxware Inc  }
     MM_VOXWARE = 114;     
  {  Northern Telecom Limited  }
     MM_NORTHERN_TELECOM = 115;     
  {  APICOM  }
     MM_APICOM = 116;     
  {  Grande Software  }
     MM_GRANDE = 117;     
  {  ADDX  }
     MM_ADDX = 118;     
  {  Wildcat Canyon Software  }
     MM_WILDCAT = 119;     
  {  Rhetorex Inc  }
     MM_RHETOREX = 120;     
  {  Brooktree Corporation  }
     MM_BROOKTREE = 121;     
  {  ENSONIQ Corporation  }
     MM_ENSONIQ = 125;     
  {  ///FAST Multimedia AG  }
     MM_FAST = 126;     
  {  NVidia Corporation  }
     MM_NVIDIA = 127;     
  {  OKSORI Co., Ltd.  }
     MM_OKSORI = 128;     
  {  DiAcoustics, Inc.  }
     MM_DIACOUSTICS = 129;     
  {  Gulbransen, Inc.  }
     MM_GULBRANSEN = 130;     
  {  Kay Elemetrics, Inc.  }
     MM_KAY_ELEMETRICS = 131;     
  {  Crystal Semiconductor Corporation  }
     MM_CRYSTAL = 132;     
  {  Splash Studios  }
     MM_SPLASH_STUDIOS = 133;     
  {  Quarterdeck Corporation  }
     MM_QUARTERDECK = 134;     
  {  TDK Corporation  }
     MM_TDK = 135;     
  {  Digital Audio Labs, Inc.  }
     MM_DIGITAL_AUDIO_LABS = 136;     
  {  Seer Systems, Inc.  }
     MM_SEERSYS = 137;     
  {  PictureTel Corporation  }
     MM_PICTURETEL = 138;     
  {  AT&T Microelectronics  }
     MM_ATT_MICROELECTRONICS = 139;     
  {  Osprey Technologies, Inc.  }
     MM_OSPREY = 140;     
  {  Mediatrix Peripherals  }
     MM_MEDIATRIX = 141;     
  {  SounDesignS M.C.S. Ltd.  }
     MM_SOUNDESIGNS = 142;     
  {  A.L. Digital Ltd.  }
     MM_ALDIGITAL = 143;     
  {  Spectrum Signal Processing, Inc.  }
     MM_SPECTRUM_SIGNAL_PROCESSING = 144;     
  {  Electronic Courseware Systems, Inc.  }
     MM_ECS = 145;     
  {  AMD  }
     MM_AMD = 146;     
  {  Core Dynamics  }
     MM_COREDYNAMICS = 147;     
  {  CANAM Computers  }
     MM_CANAM = 148;     
  {  Softsound, Ltd.  }
     MM_SOFTSOUND = 149;     
  {  Norris Communications, Inc.  }
     MM_NORRIS = 150;     
  {  Danka Data Devices  }
     MM_DDD = 151;     
  {  EuPhonics  }
     MM_EUPHONICS = 152;     
  {  Precept Software, Inc.  }
     MM_PRECEPT = 153;     
  {  Crystal Net Corporation  }
     MM_CRYSTAL_NET = 154;     
  {  Chromatic Research, Inc  }
     MM_CHROMATIC = 155;     
  {  Voice Information Systems, Inc  }
     MM_VOICEINFO = 156;     
  {  Vienna Systems  }
     MM_VIENNASYS = 157;     
  {  Connectix Corporation  }
     MM_CONNECTIX = 158;     
  {  Gadget Labs LLC  }
     MM_GADGETLABS = 159;     
  {  Frontier Design Group LLC  }
     MM_FRONTIER = 160;     
  {  Viona Development GmbH  }
     MM_VIONA = 161;     
  {  Casio Computer Co., LTD  }
     MM_CASIO = 162;     
  {  Diamond Multimedia  }
     MM_DIAMONDMM = 163;     
  {  S3  }
     MM_S3 = 164;     
  {  Fraunhofer  }
     MM_FRAUNHOFER_IIS = 172;     


{ MM_MICROSOFT product IDs  }
{$IFNDEF MM_MIDI_MAPPER}
  const
     MM_MIDI_MAPPER = 1; // Midi Mapper
  {  Wave Mapper   }
     MM_WAVE_MAPPER = 2;
  {  Sound Blaster MIDI output port   }
     MM_SNDBLST_MIDIOUT = 3;
  {  Sound Blaster MIDI input port   }
     MM_SNDBLST_MIDIIN = 4;
  {  Sound Blaster internal synth   }
     MM_SNDBLST_SYNTH = 5;
  {  Sound Blaster waveform output   }
     MM_SNDBLST_WAVEOUT = 6;
  {  Sound Blaster waveform input   }
     MM_SNDBLST_WAVEIN = 7;
  {  Ad Lib Compatible synth   }
     MM_ADLIB = 9;
  {  MPU 401 compatible MIDI output port   }
     MM_MPU401_MIDIOUT = 10;
  {  MPU 401 compatible MIDI input port   }
     MM_MPU401_MIDIIN = 11;
  {  Joystick adapter   }
     MM_PC_JOYSTICK = 12;
{$ENDIF MM_MIDI_MAPPER}

  const
     MM_PCSPEAKER_WAVEOUT = 13;  {  PC speaker waveform output   }     
  {  MS Audio Board waveform input   }
     MM_MSFT_WSS_WAVEIN = 14;     
  {  MS Audio Board waveform output   }
     MM_MSFT_WSS_WAVEOUT = 15;     
  {  MS Audio Board  Stereo FM synth   }
     MM_MSFT_WSS_FMSYNTH_STEREO = 16;     
  {  MS Audio Board Mixer Driver   }
     MM_MSFT_WSS_MIXER = 17;     
  {  MS OEM Audio Board waveform input   }
     MM_MSFT_WSS_OEM_WAVEIN = 18;     
  {  MS OEM Audio Board waveform output   }
     MM_MSFT_WSS_OEM_WAVEOUT = 19;     
  {  MS OEM Audio Board Stereo FM Synth   }
     MM_MSFT_WSS_OEM_FMSYNTH_STEREO = 20;     
  {  MS Audio Board Aux. Port   }
     MM_MSFT_WSS_AUX = 21;     
  {  MS OEM Audio Aux Port   }
     MM_MSFT_WSS_OEM_AUX = 22;     
  {  MS Vanilla driver waveform input   }
     MM_MSFT_GENERIC_WAVEIN = 23;     
  {  MS Vanilla driver wavefrom output   }
     MM_MSFT_GENERIC_WAVEOUT = 24;     
  {  MS Vanilla driver MIDI in   }
     MM_MSFT_GENERIC_MIDIIN = 25;     
  {  MS Vanilla driver MIDI  external out   }
     MM_MSFT_GENERIC_MIDIOUT = 26;     
  {  MS Vanilla driver MIDI synthesizer   }
     MM_MSFT_GENERIC_MIDISYNTH = 27;     
  {  MS Vanilla driver aux (line in)   }
     MM_MSFT_GENERIC_AUX_LINE = 28;     
  {  MS Vanilla driver aux (mic)   }
     MM_MSFT_GENERIC_AUX_MIC = 29;     
  {  MS Vanilla driver aux (CD)   }
     MM_MSFT_GENERIC_AUX_CD = 30;     
  {  MS OEM Audio Board Mixer Driver   }
     MM_MSFT_WSS_OEM_MIXER = 31;     
  {  MS Audio Compression Manager   }
     MM_MSFT_MSACM = 32;     
  {  MS ADPCM Codec   }
     MM_MSFT_ACM_MSADPCM = 33;     
  {  IMA ADPCM Codec   }
     MM_MSFT_ACM_IMAADPCM = 34;     
  {  MS Filter   }
     MM_MSFT_ACM_MSFILTER = 35;     
  {  GSM 610 codec   }
     MM_MSFT_ACM_GSM610 = 36;     
  {  G.711 codec   }
     MM_MSFT_ACM_G711 = 37;     
  {  PCM converter   }
     MM_MSFT_ACM_PCM = 38;

     
  { Microsoft Windows Sound System drivers }
  {  Sound Blaster 16 waveform input   }
     MM_WSS_SB16_WAVEIN = 39;     
  {  Sound Blaster 16  waveform output   }
     MM_WSS_SB16_WAVEOUT = 40;     
  {  Sound Blaster 16 midi-in   }
     MM_WSS_SB16_MIDIIN = 41;     
  {  Sound Blaster 16 midi out   }
     MM_WSS_SB16_MIDIOUT = 42;     
  {  Sound Blaster 16 FM Synthesis   }
     MM_WSS_SB16_SYNTH = 43;     
  {  Sound Blaster 16 aux (line in)   }
     MM_WSS_SB16_AUX_LINE = 44;     
  {  Sound Blaster 16 aux (CD)   }
     MM_WSS_SB16_AUX_CD = 45;     
  {  Sound Blaster 16 mixer device   }
     MM_WSS_SB16_MIXER = 46;     
  {  Sound Blaster Pro waveform input   }
     MM_WSS_SBPRO_WAVEIN = 47;     
  {  Sound Blaster Pro waveform output   }
     MM_WSS_SBPRO_WAVEOUT = 48;     
  {  Sound Blaster Pro midi in   }
     MM_WSS_SBPRO_MIDIIN = 49;     
  {  Sound Blaster Pro midi out   }
     MM_WSS_SBPRO_MIDIOUT = 50;     
  {  Sound Blaster Pro FM synthesis   }
     MM_WSS_SBPRO_SYNTH = 51;     
  {  Sound Blaster Pro aux (line in )   }
     MM_WSS_SBPRO_AUX_LINE = 52;     
  {  Sound Blaster Pro aux (CD)   }
     MM_WSS_SBPRO_AUX_CD = 53;     
  {  Sound Blaster Pro mixer   }
     MM_WSS_SBPRO_MIXER = 54;     
  {  WSS NT wave in   }
     MM_MSFT_WSS_NT_WAVEIN = 55;     
  {  WSS NT wave out   }
     MM_MSFT_WSS_NT_WAVEOUT = 56;     
  {  WSS NT FM synth   }
     MM_MSFT_WSS_NT_FMSYNTH_STEREO = 57;     
  {  WSS NT mixer   }
     MM_MSFT_WSS_NT_MIXER = 58;     
  {  WSS NT aux   }
     MM_MSFT_WSS_NT_AUX = 59;     
  {  Sound Blaster 16 waveform input   }
     MM_MSFT_SB16_WAVEIN = 60;     
  {  Sound Blaster 16  waveform output   }
     MM_MSFT_SB16_WAVEOUT = 61;     
  {  Sound Blaster 16 midi-in   }
     MM_MSFT_SB16_MIDIIN = 62;     
  {  Sound Blaster 16 midi out   }
     MM_MSFT_SB16_MIDIOUT = 63;     
  {  Sound Blaster 16 FM Synthesis   }
     MM_MSFT_SB16_SYNTH = 64;     
  {  Sound Blaster 16 aux (line in)   }
     MM_MSFT_SB16_AUX_LINE = 65;     
  {  Sound Blaster 16 aux (CD)   }
     MM_MSFT_SB16_AUX_CD = 66;     
  {  Sound Blaster 16 mixer device   }
     MM_MSFT_SB16_MIXER = 67;     
  {  Sound Blaster Pro waveform input   }
     MM_MSFT_SBPRO_WAVEIN = 68;     
  {  Sound Blaster Pro waveform output   }
     MM_MSFT_SBPRO_WAVEOUT = 69;     
  {  Sound Blaster Pro midi in   }
     MM_MSFT_SBPRO_MIDIIN = 70;     
  {  Sound Blaster Pro midi out   }
     MM_MSFT_SBPRO_MIDIOUT = 71;     
  {  Sound Blaster Pro FM synthesis   }
     MM_MSFT_SBPRO_SYNTH = 72;     
  {  Sound Blaster Pro aux (line in )   }
     MM_MSFT_SBPRO_AUX_LINE = 73;     
  {  Sound Blaster Pro aux (CD)   }
     MM_MSFT_SBPRO_AUX_CD = 74;     
  {  Sound Blaster Pro mixer   }
     MM_MSFT_SBPRO_MIXER = 75;     
  { Yamaha OPL2/OPL3 compatible FM synthesis  }
     MM_MSFT_MSOPL_SYNTH = 76;     
  { Voice Modem Serial Line Wave Input  }
     MM_MSFT_VMDMS_LINE_WAVEIN = 80;     
  { Voice Modem Serial Line Wave Output  }
     MM_MSFT_VMDMS_LINE_WAVEOUT = 81;     
  { Voice Modem Serial Handset Wave Input  }
     MM_MSFT_VMDMS_HANDSET_WAVEIN = 82;     
  { Voice Modem Serial Handset Wave Output  }
     MM_MSFT_VMDMS_HANDSET_WAVEOUT = 83;     
  { Voice Modem Wrapper Line Wave Input  }
     MM_MSFT_VMDMW_LINE_WAVEIN = 84;     
  { Voice Modem Wrapper Line Wave Output  }
     MM_MSFT_VMDMW_LINE_WAVEOUT = 85;     
  { Voice Modem Wrapper Handset Wave Input  }
     MM_MSFT_VMDMW_HANDSET_WAVEIN = 86;     
  { Voice Modem Wrapper Handset Wave Output  }
     MM_MSFT_VMDMW_HANDSET_WAVEOUT = 87;     
  { Voice Modem Wrapper Mixer  }
     MM_MSFT_VMDMW_MIXER = 88;     
  { Voice Modem Game Compatible Wave Device  }
     MM_MSFT_VMDM_GAME_WAVEOUT = 89;     
  { Voice Modem Game Compatible Wave Device  }
     MM_MSFT_VMDM_GAME_WAVEIN = 90;     
  {  }
     MM_MSFT_ACM_MSNAUDIO = 91;     
  {  }
     MM_MSFT_ACM_MSG723 = 92;     
  { Generic id for WDM Audio drivers  }
     MM_MSFT_WDMAUDIO_WAVEOUT = 100;     
  { Generic id for WDM Audio drivers  }
     MM_MSFT_WDMAUDIO_WAVEIN = 101;     
  { Generic id for WDM Audio drivers  }
     MM_MSFT_WDMAUDIO_MIDIOUT = 102;     
  { Generic id for WDM Audio drivers  }
     MM_MSFT_WDMAUDIO_MIDIIN = 103;     
  { Generic id for WDM Audio drivers  }
     MM_MSFT_WDMAUDIO_MIXER = 104;

  { MM_CREATIVE product IDs  }
     MM_CREATIVE_SB15_WAVEIN = 1;  {  SB (r) 1.5 waveform input   }
     MM_CREATIVE_SB20_WAVEIN = 2;
     MM_CREATIVE_SBPRO_WAVEIN = 3;     
     MM_CREATIVE_SBP16_WAVEIN = 4;     
     MM_CREATIVE_PHNBLST_WAVEIN = 5;     
     MM_CREATIVE_SB15_WAVEOUT = 101;     
     MM_CREATIVE_SB20_WAVEOUT = 102;     
     MM_CREATIVE_SBPRO_WAVEOUT = 103;     
     MM_CREATIVE_SBP16_WAVEOUT = 104;     
     MM_CREATIVE_PHNBLST_WAVEOUT = 105;     
  {  SB (r)   }
     MM_CREATIVE_MIDIOUT = 201;     
  {  SB (r)   }
     MM_CREATIVE_MIDIIN = 202;     
  {  SB (r)   }
     MM_CREATIVE_FMSYNTH_MONO = 301;     
  {  SB Pro (r) stereo synthesizer   }
     MM_CREATIVE_FMSYNTH_STEREO = 302;     
     MM_CREATIVE_MIDI_AWE32 = 303;     
  {  SB Pro (r) aux (CD)   }
     MM_CREATIVE_AUX_CD = 401;     
  {  SB Pro (r) aux (Line in )   }
     MM_CREATIVE_AUX_LINE = 402;     
  {  SB Pro (r) aux (mic)   }
     MM_CREATIVE_AUX_MIC = 403;     
     MM_CREATIVE_AUX_MASTER = 404;     
     MM_CREATIVE_AUX_PCSPK = 405;     
     MM_CREATIVE_AUX_WAVE = 406;     
     MM_CREATIVE_AUX_MIDI = 407;     
     MM_CREATIVE_SBPRO_MIXER = 408;     
     MM_CREATIVE_SB16_MIXER = 409;

  { MM_MEDIAVISION product IDs  }
  { Pro Audio Spectrum }
     MM_MEDIAVISION_PROAUDIO = $10;
     MM_PROAUD_MIDIOUT = MM_MEDIAVISION_PROAUDIO+1;
     MM_PROAUD_MIDIIN = MM_MEDIAVISION_PROAUDIO+2;
     MM_PROAUD_SYNTH = MM_MEDIAVISION_PROAUDIO+3;
     MM_PROAUD_WAVEOUT = MM_MEDIAVISION_PROAUDIO+4;
     MM_PROAUD_WAVEIN = MM_MEDIAVISION_PROAUDIO+5;
     MM_PROAUD_MIXER = MM_MEDIAVISION_PROAUDIO+6;
     MM_PROAUD_AUX = MM_MEDIAVISION_PROAUDIO+7;

  { Thunder Board }
     MM_MEDIAVISION_THUNDER = $20;
     MM_THUNDER_SYNTH = MM_MEDIAVISION_THUNDER+3;
     MM_THUNDER_WAVEOUT = MM_MEDIAVISION_THUNDER+4;
     MM_THUNDER_WAVEIN = MM_MEDIAVISION_THUNDER+5;
     MM_THUNDER_AUX = MM_MEDIAVISION_THUNDER+7;

  { Audio Port }
     MM_MEDIAVISION_TPORT = $40;
     MM_TPORT_WAVEOUT = MM_MEDIAVISION_TPORT+1;
     MM_TPORT_WAVEIN = MM_MEDIAVISION_TPORT+2;
     MM_TPORT_SYNTH = MM_MEDIAVISION_TPORT+3;

  { Pro Audio Spectrum Plus }
     MM_MEDIAVISION_PROAUDIO_PLUS = $50;
     MM_PROAUD_PLUS_MIDIOUT = MM_MEDIAVISION_PROAUDIO_PLUS+1;
     MM_PROAUD_PLUS_MIDIIN = MM_MEDIAVISION_PROAUDIO_PLUS+2;
     MM_PROAUD_PLUS_SYNTH = MM_MEDIAVISION_PROAUDIO_PLUS+3;
     MM_PROAUD_PLUS_WAVEOUT = MM_MEDIAVISION_PROAUDIO_PLUS+4;
     MM_PROAUD_PLUS_WAVEIN = MM_MEDIAVISION_PROAUDIO_PLUS+5;
     MM_PROAUD_PLUS_MIXER = MM_MEDIAVISION_PROAUDIO_PLUS+6;
     MM_PROAUD_PLUS_AUX = MM_MEDIAVISION_PROAUDIO_PLUS+7;
          
  { Pro Audio Spectrum 16 }
     MM_MEDIAVISION_PROAUDIO_16 = $60;     
     MM_PROAUD_16_MIDIOUT = MM_MEDIAVISION_PROAUDIO_16+1;     
     MM_PROAUD_16_MIDIIN = MM_MEDIAVISION_PROAUDIO_16+2;     
     MM_PROAUD_16_SYNTH = MM_MEDIAVISION_PROAUDIO_16+3;     
     MM_PROAUD_16_WAVEOUT = MM_MEDIAVISION_PROAUDIO_16+4;     
     MM_PROAUD_16_WAVEIN = MM_MEDIAVISION_PROAUDIO_16+5;     
     MM_PROAUD_16_MIXER = MM_MEDIAVISION_PROAUDIO_16+6;     
     MM_PROAUD_16_AUX = MM_MEDIAVISION_PROAUDIO_16+7;

  { Pro Audio Studio 16 }
     MM_MEDIAVISION_PROSTUDIO_16 = $60;
     MM_STUDIO_16_MIDIOUT = MM_MEDIAVISION_PROSTUDIO_16+1;
     MM_STUDIO_16_MIDIIN = MM_MEDIAVISION_PROSTUDIO_16+2;
     MM_STUDIO_16_SYNTH = MM_MEDIAVISION_PROSTUDIO_16+3;
     MM_STUDIO_16_WAVEOUT = MM_MEDIAVISION_PROSTUDIO_16+4;
     MM_STUDIO_16_WAVEIN = MM_MEDIAVISION_PROSTUDIO_16+5;
     MM_STUDIO_16_MIXER = MM_MEDIAVISION_PROSTUDIO_16+6;
     MM_STUDIO_16_AUX = MM_MEDIAVISION_PROSTUDIO_16+7;

  { CDPC }
     MM_MEDIAVISION_CDPC = $70;
     MM_CDPC_MIDIOUT = MM_MEDIAVISION_CDPC+1;
     MM_CDPC_MIDIIN = MM_MEDIAVISION_CDPC+2;
     MM_CDPC_SYNTH = MM_MEDIAVISION_CDPC+3;
     MM_CDPC_WAVEOUT = MM_MEDIAVISION_CDPC+4;
     MM_CDPC_WAVEIN = MM_MEDIAVISION_CDPC+5;
     MM_CDPC_MIXER = MM_MEDIAVISION_CDPC+6;
     MM_CDPC_AUX = MM_MEDIAVISION_CDPC+7;
     
  { Opus MV 1208 Chipsent }
     MM_MEDIAVISION_OPUS1208 = $80;     
     MM_OPUS401_MIDIOUT = MM_MEDIAVISION_OPUS1208+1;     
     MM_OPUS401_MIDIIN = MM_MEDIAVISION_OPUS1208+2;     
     MM_OPUS1208_SYNTH = MM_MEDIAVISION_OPUS1208+3;     
     MM_OPUS1208_WAVEOUT = MM_MEDIAVISION_OPUS1208+4;     
     MM_OPUS1208_WAVEIN = MM_MEDIAVISION_OPUS1208+5;     
     MM_OPUS1208_MIXER = MM_MEDIAVISION_OPUS1208+6;     
     MM_OPUS1208_AUX = MM_MEDIAVISION_OPUS1208+7;

  { Opus MV 1216 chipset }
     MM_MEDIAVISION_OPUS1216 = $90;
     MM_OPUS1216_MIDIOUT = MM_MEDIAVISION_OPUS1216+1;
     MM_OPUS1216_MIDIIN = MM_MEDIAVISION_OPUS1216+2;
     MM_OPUS1216_SYNTH = MM_MEDIAVISION_OPUS1216+3;
     MM_OPUS1216_WAVEOUT = MM_MEDIAVISION_OPUS1216+4;
     MM_OPUS1216_WAVEIN = MM_MEDIAVISION_OPUS1216+5;
     MM_OPUS1216_MIXER = MM_MEDIAVISION_OPUS1216+6;
     MM_OPUS1216_AUX = MM_MEDIAVISION_OPUS1216+7;

  { MM_ARTISOFT product IDs  }
  {  Artisoft sounding Board waveform input   }
     MM_ARTISOFT_SBWAVEIN = 1;
  {  Artisoft sounding Board waveform output   }
     MM_ARTISOFT_SBWAVEOUT = 2;

  { MM_IBM product IDs  }
  {  IBM M-Motion Auxiliary Device   }
     MM_MMOTION_WAVEAUX = 1;
  {  IBM M-Motion Waveform output   }
     MM_MMOTION_WAVEOUT = 2;
  {  IBM M-Motion  Waveform Input   }
     MM_MMOTION_WAVEIN = 3;
  {  IBM waveform input   }
     MM_IBM_PCMCIA_WAVEIN = 11;
  {  IBM Waveform output   }
     MM_IBM_PCMCIA_WAVEOUT = 12;
  {  IBM Midi Synthesis   }
     MM_IBM_PCMCIA_SYNTH = 13;     
  {  IBM external MIDI in   }
     MM_IBM_PCMCIA_MIDIIN = 14;     
  {  IBM external MIDI out   }
     MM_IBM_PCMCIA_MIDIOUT = 15;     
  {  IBM auxiliary control   }
     MM_IBM_PCMCIA_AUX = 16;     
     MM_IBM_THINKPAD200 = 17;     
     MM_IBM_MWAVE_WAVEIN = 18;     
     MM_IBM_MWAVE_WAVEOUT = 19;     
     MM_IBM_MWAVE_MIXER = 20;     
     MM_IBM_MWAVE_MIDIIN = 21;     
     MM_IBM_MWAVE_MIDIOUT = 22;     
     MM_IBM_MWAVE_AUX = 23;     
     MM_IBM_WC_MIDIOUT = 30;     
     MM_IBM_WC_WAVEOUT = 31;     
     MM_IBM_WC_MIXEROUT = 33;

  { MM_VOCALTEC product IDs  }
     MM_VOCALTEC_WAVEOUT = 1;
     MM_VOCALTEC_WAVEIN = 2;

  { MM_ROLAND product IDs  }
  { MM_ROLAND_RAP10  }
     MM_ROLAND_RAP10_MIDIOUT = 10;
  { MM_ROLAND_RAP10  }
     MM_ROLAND_RAP10_MIDIIN = 11;
  { MM_ROLAND_RAP10  }
     MM_ROLAND_RAP10_SYNTH = 12;
  { MM_ROLAND_RAP10  }
     MM_ROLAND_RAP10_WAVEOUT = 13;     
  { MM_ROLAND_RAP10  }
     MM_ROLAND_RAP10_WAVEIN = 14;     
     MM_ROLAND_MPU401_MIDIOUT = 15;     
     MM_ROLAND_MPU401_MIDIIN = 16;     
     MM_ROLAND_SMPU_MIDIOUTA = 17;     
     MM_ROLAND_SMPU_MIDIOUTB = 18;     
     MM_ROLAND_SMPU_MIDIINA = 19;     
     MM_ROLAND_SMPU_MIDIINB = 20;     
     MM_ROLAND_SC7_MIDIOUT = 21;     
     MM_ROLAND_SC7_MIDIIN = 22;     
     MM_ROLAND_SERIAL_MIDIOUT = 23;     
     MM_ROLAND_SERIAL_MIDIIN = 24;     
     MM_ROLAND_SCP_MIDIOUT = 38;     
     MM_ROLAND_SCP_MIDIIN = 39;     
     MM_ROLAND_SCP_WAVEOUT = 40;     
     MM_ROLAND_SCP_WAVEIN = 41;     
     MM_ROLAND_SCP_MIXER = 42;
     MM_ROLAND_SCP_AUX = 48;

  { MM_DSP_SOLUTIONS product IDs  }
     MM_DSP_SOLUTIONS_WAVEOUT = 1;
     MM_DSP_SOLUTIONS_WAVEIN = 2;
     MM_DSP_SOLUTIONS_SYNTH = 3;
     MM_DSP_SOLUTIONS_AUX = 4;

  { MM_WANGLABS product IDs  }
  {  Input audio wave on CPU board models: Exec 4010, 4030, 3450; PC 251/25c, pc 461/25s , pc 461/33c   }
     MM_WANGLABS_WAVEIN1 = 1;
     MM_WANGLABS_WAVEOUT1 = 2;
          
  { MM_TANDY product IDs  }
     MM_TANDY_VISWAVEIN = 1;     
     MM_TANDY_VISWAVEOUT = 2;     
     MM_TANDY_VISBIOSSYNTH = 3;     
     MM_TANDY_SENS_MMAWAVEIN = 4;     
     MM_TANDY_SENS_MMAWAVEOUT = 5;     
     MM_TANDY_SENS_MMAMIDIIN = 6;     
     MM_TANDY_SENS_MMAMIDIOUT = 7;     
     MM_TANDY_SENS_VISWAVEOUT = 8;     
     MM_TANDY_PSSJWAVEIN = 9;     
     MM_TANDY_PSSJWAVEOUT = 10;
         
  { product IDs  }
  {  HID2 WaveAudio Driver   }
     MM_INTELOPD_WAVEIN = 1;     
  {  HID2   }
     MM_INTELOPD_WAVEOUT = 101;     
  {  HID2 for mixing   }
     MM_INTELOPD_AUX = 401;     
     MM_INTEL_NSPMODEMLINE = 501;

  { MM_INTERACTIVE product IDs  }
     MM_INTERACTIVE_WAVEIN = $45;
     MM_INTERACTIVE_WAVEOUT = $45;

  { MM_YAMAHA product IDs  }
     MM_YAMAHA_GSS_SYNTH = $01;     
     MM_YAMAHA_GSS_WAVEOUT = $02;     
     MM_YAMAHA_GSS_WAVEIN = $03;     
     MM_YAMAHA_GSS_MIDIOUT = $04;     
     MM_YAMAHA_GSS_MIDIIN = $05;     
     MM_YAMAHA_GSS_AUX = $06;     
     MM_YAMAHA_SERIAL_MIDIOUT = $07;     
     MM_YAMAHA_SERIAL_MIDIIN = $08;     
     MM_YAMAHA_OPL3SA_WAVEOUT = $10;     
     MM_YAMAHA_OPL3SA_WAVEIN = $11;     
     MM_YAMAHA_OPL3SA_FMSYNTH = $12;     
     MM_YAMAHA_OPL3SA_YSYNTH = $13;     
     MM_YAMAHA_OPL3SA_MIDIOUT = $14;     
     MM_YAMAHA_OPL3SA_MIDIIN = $15;     
     MM_YAMAHA_OPL3SA_MIXER = $17;
     MM_YAMAHA_OPL3SA_JOYSTICK = $18;

  { MM_EVEREX product IDs  }
     MM_EVEREX_CARRIER = $01;

  { MM_ECHO product IDs  }
     MM_ECHO_SYNTH = $01;
     MM_ECHO_WAVEOUT = $02;
     MM_ECHO_WAVEIN = $03;
     MM_ECHO_MIDIOUT = $04;
     MM_ECHO_MIDIIN = $05;
     MM_ECHO_AUX = $06;

  { MM_SIERRA product IDs  }
     MM_SIERRA_ARIA_MIDIOUT = $14;
     MM_SIERRA_ARIA_MIDIIN = $15;
     MM_SIERRA_ARIA_SYNTH = $16;
     MM_SIERRA_ARIA_WAVEOUT = $17;
     MM_SIERRA_ARIA_WAVEIN = $18;
     MM_SIERRA_ARIA_AUX = $19;
     MM_SIERRA_ARIA_AUX2 = $20;
     MM_SIERRA_QUARTET_WAVEIN = $50;
     MM_SIERRA_QUARTET_WAVEOUT = $51;
     MM_SIERRA_QUARTET_MIDIIN = $52;
     MM_SIERRA_QUARTET_MIDIOUT = $53;
     MM_SIERRA_QUARTET_SYNTH = $54;
     MM_SIERRA_QUARTET_AUX_CD = $55;
     MM_SIERRA_QUARTET_AUX_LINE = $56;
     MM_SIERRA_QUARTET_AUX_MODEM = $57;
     MM_SIERRA_QUARTET_MIXER = $58;

  { MM_CAT product IDs  }
     MM_CAT_WAVEOUT = 1;

  { MM_DSP_GROUP product IDs  }
     MM_DSP_GROUP_TRUESPEECH = $01;

  { MM_MELABS product IDs  }
     MM_MELABS_MIDI2GO = $01;

  { MM_ESS product IDs  }
     MM_ESS_AMWAVEOUT = $01;
     MM_ESS_AMWAVEIN = $02;
     MM_ESS_AMAUX = $03;
     MM_ESS_AMSYNTH = $04;
     MM_ESS_AMMIDIOUT = $05;
     MM_ESS_AMMIDIIN = $06;
     MM_ESS_MIXER = $07;     
     MM_ESS_AUX_CD = $08;     
     MM_ESS_MPU401_MIDIOUT = $09;     
     MM_ESS_MPU401_MIDIIN = $0A;     
     MM_ESS_ES488_WAVEOUT = $10;     
     MM_ESS_ES488_WAVEIN = $11;     
     MM_ESS_ES488_MIXER = $12;     
     MM_ESS_ES688_WAVEOUT = $13;     
     MM_ESS_ES688_WAVEIN = $14;     
     MM_ESS_ES688_MIXER = $15;     
     MM_ESS_ES1488_WAVEOUT = $16;     
     MM_ESS_ES1488_WAVEIN = $17;     
     MM_ESS_ES1488_MIXER = $18;     
     MM_ESS_ES1688_WAVEOUT = $19;     
     MM_ESS_ES1688_WAVEIN = $1A;     
     MM_ESS_ES1688_MIXER = $1B;     
     MM_ESS_ES1788_WAVEOUT = $1C;     
     MM_ESS_ES1788_WAVEIN = $1D;     
     MM_ESS_ES1788_MIXER = $1E;     
     MM_ESS_ES1888_WAVEOUT = $1F;     
     MM_ESS_ES1888_WAVEIN = $20;     
     MM_ESS_ES1888_MIXER = $21;     
     MM_ESS_ES1868_WAVEOUT = $22;     
     MM_ESS_ES1868_WAVEIN = $23;     
     MM_ESS_ES1868_MIXER = $24;     
     MM_ESS_ES1878_WAVEOUT = $25;     
     MM_ESS_ES1878_WAVEIN = $26;     
     MM_ESS_ES1878_MIXER = $27;

  { product IDs  }
     MM_EPS_FMSND = 1;

  { MM_TRUEVISION product IDs  }
     MM_TRUEVISION_WAVEIN1 = 1;
     MM_TRUEVISION_WAVEOUT1 = 2;
          
  { MM_AZTECH product IDs  }
     MM_AZTECH_MIDIOUT = 3;     
     MM_AZTECH_MIDIIN = 4;     
     MM_AZTECH_WAVEIN = 17;     
     MM_AZTECH_WAVEOUT = 18;     
     MM_AZTECH_FMSYNTH = 20;     
     MM_AZTECH_MIXER = 21;     
     MM_AZTECH_PRO16_WAVEIN = 33;     
     MM_AZTECH_PRO16_WAVEOUT = 34;     
     MM_AZTECH_PRO16_FMSYNTH = 38;     
     MM_AZTECH_DSP16_WAVEIN = 65;     
     MM_AZTECH_DSP16_WAVEOUT = 66;     
     MM_AZTECH_DSP16_FMSYNTH = 68;     
     MM_AZTECH_DSP16_WAVESYNTH = 70;     
     MM_AZTECH_NOVA16_WAVEIN = 71;     
     MM_AZTECH_NOVA16_WAVEOUT = 72;     
     MM_AZTECH_NOVA16_MIXER = 73;     
     MM_AZTECH_WASH16_WAVEIN = 74;     
     MM_AZTECH_WASH16_WAVEOUT = 75;     
     MM_AZTECH_WASH16_MIXER = 76;     
     MM_AZTECH_AUX_CD = 401;     
     MM_AZTECH_AUX_LINE = 402;     
     MM_AZTECH_AUX_MIC = 403;     
     MM_AZTECH_AUX = 404;

  { MM_VIDEOLOGIC product IDs  }
     MM_VIDEOLOGIC_MSWAVEIN = 1;
     MM_VIDEOLOGIC_MSWAVEOUT = 2;

  { MM_KORG product IDs  }
     MM_KORG_PCIF_MIDIOUT = 1;
     MM_KORG_PCIF_MIDIIN = 2;

  { MM_APT product IDs  }
     MM_APT_ACE100CD = 1;

  { MM_ICS product IDs  }
  {  MS WSS compatible card and driver   }
     MM_ICS_WAVEDECK_WAVEOUT = 1;     
     MM_ICS_WAVEDECK_WAVEIN = 2;     
     MM_ICS_WAVEDECK_MIXER = 3;     
     MM_ICS_WAVEDECK_AUX = 4;     
     MM_ICS_WAVEDECK_SYNTH = 5;     
     MM_ICS_WAVEDEC_SB_WAVEOUT = 6;     
     MM_ICS_WAVEDEC_SB_WAVEIN = 7;     
     MM_ICS_WAVEDEC_SB_FM_MIDIOUT = 8;     
     MM_ICS_WAVEDEC_SB_MPU401_MIDIOUT = 9;     
     MM_ICS_WAVEDEC_SB_MPU401_MIDIIN = 10;     
     MM_ICS_WAVEDEC_SB_MIXER = 11;     
     MM_ICS_WAVEDEC_SB_AUX = 12;     
     MM_ICS_2115_LITE_MIDIOUT = 13;     
     MM_ICS_2120_LITE_MIDIOUT = 14;

  { MM_ITERATEDSYS product IDs  }
     MM_ITERATEDSYS_FUFCODEC = 1;

  { MM_METHEUS product IDs  }
     MM_METHEUS_ZIPPER = 1;

  { MM_WINNOV product IDs  }
     MM_WINNOV_CAVIAR_WAVEIN = 1;
     MM_WINNOV_CAVIAR_WAVEOUT = 2;
     MM_WINNOV_CAVIAR_VIDC = 3;
  {  Fourcc is CHAM   }
     MM_WINNOV_CAVIAR_CHAMPAGNE = 4;     
  {  Fourcc is YUV8   }
     MM_WINNOV_CAVIAR_YUV8 = 5;

  { MM_NCR product IDs  }
     MM_NCR_BA_WAVEIN = 1;
     MM_NCR_BA_WAVEOUT = 2;
     MM_NCR_BA_SYNTH = 3;
     MM_NCR_BA_AUX = 4;
     MM_NCR_BA_MIXER = 5;

  { MM_VITEC product IDs  }
     MM_VITEC_VMAKER = 1;
     MM_VITEC_VMPRO = 2;

  { MM_MOSCOM product IDs  }
  {  Four Port Voice Processing / Voice Recognition Board   }
     MM_MOSCOM_VPC2400_IN = 1;     
  {  VPC2400  }
     MM_MOSCOM_VPC2400_OUT = 2;

  { MM_SILICONSOFT product IDs  }
  {  Waveform in , high sample rate   }
     MM_SILICONSOFT_SC1_WAVEIN = 1;
  {  Waveform out , high sample rate   }
     MM_SILICONSOFT_SC1_WAVEOUT = 2;
  {  Waveform in 2 channels, high sample rate   }
     MM_SILICONSOFT_SC2_WAVEIN = 3;
  {  Waveform out 2 channels, high sample rate   }
     MM_SILICONSOFT_SC2_WAVEOUT = 4;
  {  Waveform out, self powered, efficient   }
     MM_SILICONSOFT_SOUNDJR2_WAVEOUT = 5;
  {  Waveform in, self powered, efficient   }
     MM_SILICONSOFT_SOUNDJR2PR_WAVEIN = 6;
  {  Waveform out 2 channels, self powered, efficient   }
     MM_SILICONSOFT_SOUNDJR2PR_WAVEOUT = 7;
  {  Waveform in 2 channels, self powered, efficient   }
     MM_SILICONSOFT_SOUNDJR3_WAVEOUT = 8;

  { MM_OLIVETTI product IDs  }
     MM_OLIVETTI_WAVEIN = 1;     
     MM_OLIVETTI_WAVEOUT = 2;     
     MM_OLIVETTI_MIXER = 3;     
     MM_OLIVETTI_AUX = 4;     
     MM_OLIVETTI_MIDIIN = 5;     
     MM_OLIVETTI_MIDIOUT = 6;     
     MM_OLIVETTI_SYNTH = 7;     
     MM_OLIVETTI_JOYSTICK = 8;     
     MM_OLIVETTI_ACM_GSM = 9;     
     MM_OLIVETTI_ACM_ADPCM = 10;     
     MM_OLIVETTI_ACM_CELP = 11;     
     MM_OLIVETTI_ACM_SBC = 12;     
     MM_OLIVETTI_ACM_OPR = 13;

  { MM_IOMAGIC product IDs  }
  {  The I/O Magic Tempo is a PCMCIA Type 2 audio card featuring wave audio
      record and playback, FM synthesizer, and MIDI output.  The I/O Magic
      Tempo WaveOut device supports mono and stereo PCM playback at rates
      of 7350, 11025, 22050, and  44100 samples  }
     MM_IOMAGIC_TEMPO_WAVEOUT = 1;
     MM_IOMAGIC_TEMPO_WAVEIN = 2;
     MM_IOMAGIC_TEMPO_SYNTH = 3;
     MM_IOMAGIC_TEMPO_MIDIOUT = 4;
     MM_IOMAGIC_TEMPO_MXDOUT = 5;
     MM_IOMAGIC_TEMPO_AUXOUT = 6;

  { MM_MATSUSHITA product IDs  }
     MM_MATSUSHITA_WAVEIN = 1;
     MM_MATSUSHITA_WAVEOUT = 2;
     MM_MATSUSHITA_FMSYNTH_STEREO = 3;
     MM_MATSUSHITA_MIXER = 4;
     MM_MATSUSHITA_AUX = 5;

  { MM_NEWMEDIA product IDs  }
  {  WSS Compatible sound card.   }
     MM_NEWMEDIA_WAVJAMMER = 1;

  { MM_LYRRUS product IDs  }
  {  Bridge is a MIDI driver that allows the the Lyrrus G-VOX hardware to
      communicate with Windows base transcription and sequencer applications.
      The driver also provides a mechanism for the user to configure the system
      to their personal playing style.  }
     MM_LYRRUS_BRIDGE_GUITAR = 1;

  { MM_OPTI product IDs  }
     MM_OPTI_M16_FMSYNTH_STEREO = $0001;
     MM_OPTI_M16_MIDIIN = $0002;
     MM_OPTI_M16_MIDIOUT = $0003;
     MM_OPTI_M16_WAVEIN = $0004;
     MM_OPTI_M16_WAVEOUT = $0005;
     MM_OPTI_M16_MIXER = $0006;
     MM_OPTI_M16_AUX = $0007;
     MM_OPTI_P16_FMSYNTH_STEREO = $0010;
     MM_OPTI_P16_MIDIIN = $0011;
     MM_OPTI_P16_MIDIOUT = $0012;
     MM_OPTI_P16_WAVEIN = $0013;
     MM_OPTI_P16_WAVEOUT = $0014;
     MM_OPTI_P16_MIXER = $0015;
     MM_OPTI_P16_AUX = $0016;
     MM_OPTI_M32_WAVEIN = $0020;
     MM_OPTI_M32_WAVEOUT = $0021;
     MM_OPTI_M32_MIDIIN = $0022;
     MM_OPTI_M32_MIDIOUT = $0023;
     MM_OPTI_M32_SYNTH_STEREO = $0024;
     MM_OPTI_M32_MIXER = $0025;
     MM_OPTI_M32_AUX = $0026;

  {  Product IDs for     MM_ADDX    -  ADDX     }
  { MM_ADDX_PCTV_DIGITALMIX  }
     MM_ADDX_PCTV_DIGITALMIX = 1;
  { MM_ADDX_PCTV_WAVEIN  }
     MM_ADDX_PCTV_WAVEIN = 2;
  { MM_ADDX_PCTV_WAVEOUT  }
     MM_ADDX_PCTV_WAVEOUT = 3;
  { MM_ADDX_PCTV_MIXER  }
     MM_ADDX_PCTV_MIXER = 4;
  { MM_ADDX_PCTV_AUX_CD  }
     MM_ADDX_PCTV_AUX_CD = 5;
  { MM_ADDX_PCTV_AUX_LINE  }
     MM_ADDX_PCTV_AUX_LINE = 6;

  {  Product IDs for     MM_AHEAD    -  Ahead, Inc.     }
     MM_AHEAD_MULTISOUND = 1;
     MM_AHEAD_SOUNDBLASTER = 2;
     MM_AHEAD_PROAUDIO = 3;
     MM_AHEAD_GENERIC = 4;

  {  Product IDs for     MM_AMD    -  AMD     }
     MM_AMD_INTERWAVE_WAVEIN = 1;     
     MM_AMD_INTERWAVE_WAVEOUT = 2;     
     MM_AMD_INTERWAVE_SYNTH = 3;     
     MM_AMD_INTERWAVE_MIXER1 = 4;     
     MM_AMD_INTERWAVE_MIXER2 = 5;     
     MM_AMD_INTERWAVE_JOYSTICK = 6;     
     MM_AMD_INTERWAVE_EX_CD = 7;     
     MM_AMD_INTERWAVE_MIDIIN = 8;     
     MM_AMD_INTERWAVE_MIDIOUT = 9;     
     MM_AMD_INTERWAVE_AUX1 = 10;     
     MM_AMD_INTERWAVE_AUX2 = 11;     
     MM_AMD_INTERWAVE_AUX_MIC = 12;     
     MM_AMD_INTERWAVE_AUX_CD = 13;     
     MM_AMD_INTERWAVE_MONO_IN = 14;     
     MM_AMD_INTERWAVE_MONO_OUT = 15;     
     MM_AMD_INTERWAVE_EX_TELEPHONY = 16;     
     MM_AMD_INTERWAVE_WAVEOUT_BASE = 17;     
     MM_AMD_INTERWAVE_WAVEOUT_TREBLE = 18;     
     MM_AMD_INTERWAVE_STEREO_ENHANCED = 19;
     
  {  Product IDs for     MM_AST    -  AST Research Inc.     }
     MM_AST_MODEMWAVE_WAVEIN = 13;     
     MM_AST_MODEMWAVE_WAVEOUT = 14;

  {  Product IDs for     MM_BROOKTREE    -  Brooktree Corporation     }
  { Brooktree PCM Wave Audio In  }
     MM_BTV_WAVEIN = 1;
  { Brooktree PCM Wave Audio Out  }
     MM_BTV_WAVEOUT = 2;
  { Brooktree MIDI In  }
     MM_BTV_MIDIIN = 3;
  { Brooktree MIDI out  }
     MM_BTV_MIDIOUT = 4;
  { Brooktree MIDI FM synth  }
     MM_BTV_MIDISYNTH = 5;
  { Brooktree Line Input  }
     MM_BTV_AUX_LINE = 6;
  { Brooktree Microphone Input  }
     MM_BTV_AUX_MIC = 7;
  { Brooktree CD Input  }
     MM_BTV_AUX_CD = 8;
  { Brooktree PCM Wave in with subcode information  }
     MM_BTV_DIGITALIN = 9;
  { Brooktree PCM Wave out with subcode information  }
     MM_BTV_DIGITALOUT = 10;
  { Brooktree WaveStream  }
     MM_BTV_MIDIWAVESTREAM = 11;
  { Brooktree WSS Mixer driver  }
     MM_BTV_MIXER = 12;

  {  Product IDs for     MM_CANAM    -  CANAM Computers     }
     MM_CANAM_CBXWAVEOUT = 1;
     MM_CANAM_CBXWAVEIN = 2;

  {  Product IDs for     MM_CASIO    -  Casio Computer Co., LTD     }
  { wp150  }
     MM_CASIO_WP150_MIDIOUT = 1;
     MM_CASIO_WP150_MIDIIN = 2;

  {  Product IDs for     MM_COMPAQ    -  Compaq Computer Corp.     }
     MM_COMPAQ_BB_WAVEIN = 1;
     MM_COMPAQ_BB_WAVEOUT = 2;
     MM_COMPAQ_BB_WAVEAUX = 3;

  {  Product IDs for     MM_COREDYNAMICS    -  Core Dynamics     }
  { DynaMax Hi-Rez  }
     MM_COREDYNAMICS_DYNAMIXHR = 1;
  { DynaSonix  }
     MM_COREDYNAMICS_DYNASONIX_SYNTH = 2;
     MM_COREDYNAMICS_DYNASONIX_MIDI_IN = 3;
     MM_COREDYNAMICS_DYNASONIX_MIDI_OUT = 4;
     MM_COREDYNAMICS_DYNASONIX_WAVE_IN = 5;
     MM_COREDYNAMICS_DYNASONIX_WAVE_OUT = 6;
     MM_COREDYNAMICS_DYNASONIX_AUDIO_IN = 7;
     MM_COREDYNAMICS_DYNASONIX_AUDIO_OUT = 8;
  { DynaGrfx  }
     MM_COREDYNAMICS_DYNAGRAFX_VGA = 9;     
     MM_COREDYNAMICS_DYNAGRAFX_WAVE_IN = 10;     
     MM_COREDYNAMICS_DYNAGRAFX_WAVE_OUT = 11;

  {  Product IDs for     MM_CRYSTAL    -  Crystal Semiconductor Corporation     }
     MM_CRYSTAL_CS4232_WAVEIN = 1;
     MM_CRYSTAL_CS4232_WAVEOUT = 2;
     MM_CRYSTAL_CS4232_WAVEMIXER = 3;
     MM_CRYSTAL_CS4232_WAVEAUX_AUX1 = 4;
     MM_CRYSTAL_CS4232_WAVEAUX_AUX2 = 5;
     MM_CRYSTAL_CS4232_WAVEAUX_LINE = 6;
     MM_CRYSTAL_CS4232_WAVEAUX_MONO = 7;
     MM_CRYSTAL_CS4232_WAVEAUX_MASTER = 8;
     MM_CRYSTAL_CS4232_MIDIIN = 9;
     MM_CRYSTAL_CS4232_MIDIOUT = 10;
     MM_CRYSTAL_CS4232_INPUTGAIN_AUX1 = 13;
     MM_CRYSTAL_CS4232_INPUTGAIN_LOOP = 14;

  {  Product IDs for     MM_DDD    -  Danka Data Devices     }
     MM_DDD_MIDILINK_MIDIIN = 1;
     MM_DDD_MIDILINK_MIDIOUT = 2;

  {  Product IDs for     MM_DIACOUSTICS    -  DiAcoustics, Inc.     }
  { Drum Action  }
     MM_DIACOUSTICS_DRUM_ACTION = 1;
          
  {  Product IDs for     MM_DIAMONDMM    -  Diamond Multimedia     }
  { Freedom Audio  }
     MM_DIMD_PLATFORM = 0;     
     MM_DIMD_DIRSOUND = 1;     
     MM_DIMD_VIRTMPU = 2;     
     MM_DIMD_VIRTSB = 3;     
     MM_DIMD_VIRTJOY = 4;     
     MM_DIMD_WAVEIN = 5;     
     MM_DIMD_WAVEOUT = 6;     
     MM_DIMD_MIDIIN = 7;     
     MM_DIMD_MIDIOUT = 8;     
     MM_DIMD_AUX_LINE = 9;     
     MM_DIMD_MIXER = 10;

  {  Product IDs for     MM_DIGITAL_AUDIO_LABS    -  Digital Audio Labs, Inc.     }
     MM_DIGITAL_AUDIO_LABS_V8 = $10;
     MM_DIGITAL_AUDIO_LABS_CPRO = $11;

  {  Product IDs for     MM_DIGITAL    -  Digital Equipment Corporation     }
  { Digital Audio Video Compression Board  }
     MM_DIGITAL_AV320_WAVEIN = 1;
  { Digital Audio Video Compression Board  }
     MM_DIGITAL_AV320_WAVEOUT = 2;
          
  {  Product IDs for     MM_ECS    -  Electronic Courseware Systems, Inc.     }
     MM_ECS_AADF_MIDI_IN = 10;     
     MM_ECS_AADF_MIDI_OUT = 11;     
     MM_ECS_AADF_WAVE2MIDI_IN = 12;

  {  Product IDs for     MM_ENSONIQ    -  ENSONIQ Corporation     }
  { ENSONIQ Soundscape  }
     MM_ENSONIQ_SOUNDSCAPE = $10;
     MM_SOUNDSCAPE_WAVEOUT              = MM_ENSONIQ_SOUNDSCAPE+1;
     MM_SOUNDSCAPE_WAVEOUT_AUX          = MM_ENSONIQ_SOUNDSCAPE+2;
     MM_SOUNDSCAPE_WAVEIN               = MM_ENSONIQ_SOUNDSCAPE+3;
     MM_SOUNDSCAPE_MIDIOUT              = MM_ENSONIQ_SOUNDSCAPE+4;
     MM_SOUNDSCAPE_MIDIIN               = MM_ENSONIQ_SOUNDSCAPE+5;
     MM_SOUNDSCAPE_SYNTH                = MM_ENSONIQ_SOUNDSCAPE+6;
     MM_SOUNDSCAPE_MIXER                = MM_ENSONIQ_SOUNDSCAPE+7;
     MM_SOUNDSCAPE_AUX                  = MM_ENSONIQ_SOUNDSCAPE+8;

    {  Product IDs for     MM_FRONTIER    -  Frontier Design Group LLC     }
    { WaveCenter  }
       MM_FRONTIER_WAVECENTER_MIDIIN = 1;       
       MM_FRONTIER_WAVECENTER_MIDIOUT = 2;       
       MM_FRONTIER_WAVECENTER_WAVEIN = 3;       
       MM_FRONTIER_WAVECENTER_WAVEOUT = 4;

    {  Product IDs for     MM_GADGETLABS    -  Gadget Labs LLC     }
       MM_GADGETLABS_WAVE44_WAVEIN = 1;
       MM_GADGETLABS_WAVE44_WAVEOUT = 2;
       MM_GADGETLABS_WAVE42_WAVEIN = 3;
       MM_GADGETLABS_WAVE42_WAVEOUT = 4;
       MM_GADGETLABS_WAVE4_MIDIIN = 5;
       MM_GADGETLABS_WAVE4_MIDIOUT = 6;

    {  Product IDs for     MM_KAY_ELEMETRICS    -  Kay Elemetrics, Inc.     }
       MM_KAY_ELEMETRICS_CSL = $4300;
       MM_KAY_ELEMETRICS_CSL_DAT = $4308;
       MM_KAY_ELEMETRICS_CSL_4CHANNEL = $4309;

    {  Product IDs for     MM_LERNOUT_AND_HAUSPIE    -  Lernout & Hauspie     }
       MM_LERNOUT_ANDHAUSPIE_LHCODECACM = 1;

    {  Product IDs for     MM_MPTUS    -  M.P. Technologies, Inc.     }
    { Sound Pallette  }
       MM_MPTUS_SPWAVEOUT = 1;

    {  Product IDs for     MM_MOTU    -  Mark of the Unicorn     }
       MM_MOTU_MTP_MIDIOUT_ALL = 100;
       MM_MOTU_MTP_MIDIIN_1 = 101;
       MM_MOTU_MTP_MIDIOUT_1 = 101;
       MM_MOTU_MTP_MIDIIN_2 = 102;
       MM_MOTU_MTP_MIDIOUT_2 = 102;
       MM_MOTU_MTP_MIDIIN_3 = 103;
       MM_MOTU_MTP_MIDIOUT_3 = 103;
       MM_MOTU_MTP_MIDIIN_4 = 104;       
       MM_MOTU_MTP_MIDIOUT_4 = 104;       
       MM_MOTU_MTP_MIDIIN_5 = 105;       
       MM_MOTU_MTP_MIDIOUT_5 = 105;       
       MM_MOTU_MTP_MIDIIN_6 = 106;       
       MM_MOTU_MTP_MIDIOUT_6 = 106;       
       MM_MOTU_MTP_MIDIIN_7 = 107;       
       MM_MOTU_MTP_MIDIOUT_7 = 107;       
       MM_MOTU_MTP_MIDIIN_8 = 108;       
       MM_MOTU_MTP_MIDIOUT_8 = 108;       
       MM_MOTU_MTPII_MIDIOUT_ALL = 200;       
       MM_MOTU_MTPII_MIDIIN_SYNC = 200;       
       MM_MOTU_MTPII_MIDIIN_1 = 201;       
       MM_MOTU_MTPII_MIDIOUT_1 = 201;       
       MM_MOTU_MTPII_MIDIIN_2 = 202;       
       MM_MOTU_MTPII_MIDIOUT_2 = 202;       
       MM_MOTU_MTPII_MIDIIN_3 = 203;       
       MM_MOTU_MTPII_MIDIOUT_3 = 203;       
       MM_MOTU_MTPII_MIDIIN_4 = 204;       
       MM_MOTU_MTPII_MIDIOUT_4 = 204;       
       MM_MOTU_MTPII_MIDIIN_5 = 205;       
       MM_MOTU_MTPII_MIDIOUT_5 = 205;       
       MM_MOTU_MTPII_MIDIIN_6 = 206;       
       MM_MOTU_MTPII_MIDIOUT_6 = 206;       
       MM_MOTU_MTPII_MIDIIN_7 = 207;       
       MM_MOTU_MTPII_MIDIOUT_7 = 207;       
       MM_MOTU_MTPII_MIDIIN_8 = 208;       
       MM_MOTU_MTPII_MIDIOUT_8 = 208;       
       MM_MOTU_MTPII_NET_MIDIIN_1 = 209;       
       MM_MOTU_MTPII_NET_MIDIOUT_1 = 209;       
       MM_MOTU_MTPII_NET_MIDIIN_2 = 210;       
       MM_MOTU_MTPII_NET_MIDIOUT_2 = 210;       
       MM_MOTU_MTPII_NET_MIDIIN_3 = 211;       
       MM_MOTU_MTPII_NET_MIDIOUT_3 = 211;       
       MM_MOTU_MTPII_NET_MIDIIN_4 = 212;       
       MM_MOTU_MTPII_NET_MIDIOUT_4 = 212;       
       MM_MOTU_MTPII_NET_MIDIIN_5 = 213;       
       MM_MOTU_MTPII_NET_MIDIOUT_5 = 213;       
       MM_MOTU_MTPII_NET_MIDIIN_6 = 214;       
       MM_MOTU_MTPII_NET_MIDIOUT_6 = 214;       
       MM_MOTU_MTPII_NET_MIDIIN_7 = 215;       
       MM_MOTU_MTPII_NET_MIDIOUT_7 = 215;       
       MM_MOTU_MTPII_NET_MIDIIN_8 = 216;       
       MM_MOTU_MTPII_NET_MIDIOUT_8 = 216;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_ALL = 300;       
       MM_MOTU_MXP_MIDIIN_SYNC = 300;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_1 = 301;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_1 = 301;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_2 = 302;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_2 = 302;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_3 = 303;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_3 = 303;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_4 = 304;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_4 = 304;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_5 = 305;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_5 = 305;       
       MM_MOTU_MXP_MIDIIN_MIDIIN_6 = 306;       
       MM_MOTU_MXP_MIDIIN_MIDIOUT_6 = 306;       
       MM_MOTU_MXPMPU_MIDIOUT_ALL = 400;       
       MM_MOTU_MXPMPU_MIDIIN_SYNC = 400;       
       MM_MOTU_MXPMPU_MIDIIN_1 = 401;       
       MM_MOTU_MXPMPU_MIDIOUT_1 = 401;       
       MM_MOTU_MXPMPU_MIDIIN_2 = 402;       
       MM_MOTU_MXPMPU_MIDIOUT_2 = 402;       
       MM_MOTU_MXPMPU_MIDIIN_3 = 403;       
       MM_MOTU_MXPMPU_MIDIOUT_3 = 403;       
       MM_MOTU_MXPMPU_MIDIIN_4 = 404;       
       MM_MOTU_MXPMPU_MIDIOUT_4 = 404;       
       MM_MOTU_MXPMPU_MIDIIN_5 = 405;       
       MM_MOTU_MXPMPU_MIDIOUT_5 = 405;       
       MM_MOTU_MXPMPU_MIDIIN_6 = 406;       
       MM_MOTU_MXPMPU_MIDIOUT_6 = 406;       
       MM_MOTU_MXN_MIDIOUT_ALL = 500;       
       MM_MOTU_MXN_MIDIIN_SYNC = 500;       
       MM_MOTU_MXN_MIDIIN_1 = 501;       
       MM_MOTU_MXN_MIDIOUT_1 = 501;       
       MM_MOTU_MXN_MIDIIN_2 = 502;       
       MM_MOTU_MXN_MIDIOUT_2 = 502;       
       MM_MOTU_MXN_MIDIIN_3 = 503;       
       MM_MOTU_MXN_MIDIOUT_3 = 503;       
       MM_MOTU_MXN_MIDIIN_4 = 504;       
       MM_MOTU_MXN_MIDIOUT_4 = 504;       
       MM_MOTU_FLYER_MIDI_IN_SYNC = 600;       
       MM_MOTU_FLYER_MIDI_IN_A = 601;       
       MM_MOTU_FLYER_MIDI_OUT_A = 601;       
       MM_MOTU_FLYER_MIDI_IN_B = 602;       
       MM_MOTU_FLYER_MIDI_OUT_B = 602;       
       MM_MOTU_PKX_MIDI_IN_SYNC = 700;       
       MM_MOTU_PKX_MIDI_IN_A = 701;       
       MM_MOTU_PKX_MIDI_OUT_A = 701;       
       MM_MOTU_PKX_MIDI_IN_B = 702;       
       MM_MOTU_PKX_MIDI_OUT_B = 702;       
       MM_MOTU_DTX_MIDI_IN_SYNC = 800;       
       MM_MOTU_DTX_MIDI_IN_A = 801;       
       MM_MOTU_DTX_MIDI_OUT_A = 801;       
       MM_MOTU_DTX_MIDI_IN_B = 802;       
       MM_MOTU_DTX_MIDI_OUT_B = 802;       
       MM_MOTU_MTPAV_MIDIOUT_ALL = 900;       
       MM_MOTU_MTPAV_MIDIIN_SYNC = 900;       
       MM_MOTU_MTPAV_MIDIIN_1 = 901;       
       MM_MOTU_MTPAV_MIDIOUT_1 = 901;       
       MM_MOTU_MTPAV_MIDIIN_2 = 902;       
       MM_MOTU_MTPAV_MIDIOUT_2 = 902;       
       MM_MOTU_MTPAV_MIDIIN_3 = 903;       
       MM_MOTU_MTPAV_MIDIOUT_3 = 903;       
       MM_MOTU_MTPAV_MIDIIN_4 = 904;       
       MM_MOTU_MTPAV_MIDIOUT_4 = 904;       
       MM_MOTU_MTPAV_MIDIIN_5 = 905;       
       MM_MOTU_MTPAV_MIDIOUT_5 = 905;       
       MM_MOTU_MTPAV_MIDIIN_6 = 906;       
       MM_MOTU_MTPAV_MIDIOUT_6 = 906;       
       MM_MOTU_MTPAV_MIDIIN_7 = 907;       
       MM_MOTU_MTPAV_MIDIOUT_7 = 907;       
       MM_MOTU_MTPAV_MIDIIN_8 = 908;       
       MM_MOTU_MTPAV_MIDIOUT_8 = 908;       
       MM_MOTU_MTPAV_NET_MIDIIN_1 = 909;       
       MM_MOTU_MTPAV_NET_MIDIOUT_1 = 909;       
       MM_MOTU_MTPAV_NET_MIDIIN_2 = 910;       
       MM_MOTU_MTPAV_NET_MIDIOUT_2 = 910;       
       MM_MOTU_MTPAV_NET_MIDIIN_3 = 911;       
       MM_MOTU_MTPAV_NET_MIDIOUT_3 = 911;       
       MM_MOTU_MTPAV_NET_MIDIIN_4 = 912;       
       MM_MOTU_MTPAV_NET_MIDIOUT_4 = 912;       
       MM_MOTU_MTPAV_NET_MIDIIN_5 = 913;       
       MM_MOTU_MTPAV_NET_MIDIOUT_5 = 913;       
       MM_MOTU_MTPAV_NET_MIDIIN_6 = 914;       
       MM_MOTU_MTPAV_NET_MIDIOUT_6 = 914;       
       MM_MOTU_MTPAV_NET_MIDIIN_7 = 915;       
       MM_MOTU_MTPAV_NET_MIDIOUT_7 = 915;       
       MM_MOTU_MTPAV_NET_MIDIIN_8 = 916;       
       MM_MOTU_MTPAV_NET_MIDIOUT_8 = 916;       
       MM_MOTU_MTPAV_MIDIIN_ADAT = 917;       
       MM_MOTU_MTPAV_MIDIOUT_ADAT = 917;

    {  Product IDs for     MM_MIRO    -  miro Computer Products AG     }
    { miroMOVIE pro  }
       MM_MIRO_MOVIEPRO = 1;
    { miroVIDEO D1  }
       MM_MIRO_VIDEOD1 = 2;
    { miroVIDEO DC1 tv  }
       MM_MIRO_VIDEODC1TV = 3;
    { miroVIDEO 10/20 TD  }
       MM_MIRO_VIDEOTD = 4;
       MM_MIRO_DC30_WAVEOUT = 5;
       MM_MIRO_DC30_WAVEIN = 6;
       MM_MIRO_DC30_MIX = 7;

    {  Product IDs for     MM_NEC    -  NEC     }
       MM_NEC_73_86_SYNTH = 5;
       MM_NEC_73_86_WAVEOUT = 6;
       MM_NEC_73_86_WAVEIN = 7;
       MM_NEC_26_SYNTH = 9;
       MM_NEC_MPU401_MIDIOUT = 10;
       MM_NEC_MPU401_MIDIIN = 11;
       MM_NEC_JOYSTICK = 12;

    {  Product IDs for     MM_NORRIS    -  Norris Communications, Inc.     }
       MM_NORRIS_VOICELINK = 1;

    {  Product IDs for     MM_NORTHERN_TELECOM    -  Northern Telecom Limited     }
    {  MPX Audio Card Wave Input Device  }
       MM_NORTEL_MPXAC_WAVEIN = 1;

    { MPX Audio Card Wave Output Device  }
       MM_NORTEL_MPXAC_WAVEOUT = 2;

    {  Product IDs for     MM_NVIDIA    -  NVidia Corporation     }
       MM_NVIDIA_WAVEOUT = 1;
       MM_NVIDIA_WAVEIN = 2;
       MM_NVIDIA_MIDIOUT = 3;
       MM_NVIDIA_MIDIIN = 4;
       MM_NVIDIA_GAMEPORT = 5;
       MM_NVIDIA_MIXER = 6;
       MM_NVIDIA_AUX = 7;

    {  Product IDs for     MM_OKSORI    -  OKSORI Co., Ltd.     }
    { Oksori Base  }
       MM_OKSORI_BASE = 0;
       MM_OKSORI_OSR8_WAVEOUT             = MM_OKSORI_BASE+1;       { Oksori 8bit Wave out  }
       MM_OKSORI_OSR8_WAVEIN              = MM_OKSORI_BASE+2;       { Oksori 8bit Wave in  }
       MM_OKSORI_OSR16_WAVEOUT            = MM_OKSORI_BASE+3;       { Oksori 16 bit Wave out  }
       MM_OKSORI_OSR16_WAVEIN             = MM_OKSORI_BASE+4;       { Oksori 16 bit Wave in  }
       MM_OKSORI_FM_OPL4                  = MM_OKSORI_BASE+5;       { Oksori FM Synth Yamaha OPL4  }
       MM_OKSORI_MIX_MASTER               = MM_OKSORI_BASE+6;       { Oksori DSP Mixer - Master Volume  }
       MM_OKSORI_MIX_WAVE                 = MM_OKSORI_BASE+7;       { Oksori DSP Mixer - Wave Volume  }
       MM_OKSORI_MIX_FM                   = MM_OKSORI_BASE+8;       { Oksori DSP Mixer - FM Volume  }
       MM_OKSORI_MIX_LINE                 = MM_OKSORI_BASE+9;       { Oksori DSP Mixer - Line Volume  }
       MM_OKSORI_MIX_CD                   = MM_OKSORI_BASE+10;      { Oksori DSP Mixer - CD Volume  }
       MM_OKSORI_MIX_MIC                  = MM_OKSORI_BASE+11;      { Oksori DSP Mixer - MIC Volume  }
       MM_OKSORI_MIX_ECHO                 = MM_OKSORI_BASE+12;      { Oksori DSP Mixer - Echo Volume  }
       MM_OKSORI_MIX_AUX1                 = MM_OKSORI_BASE+13;      { Oksori AD1848 - AUX1 Volume  }
       MM_OKSORI_MIX_LINE1                = MM_OKSORI_BASE+14;      { Oksori AD1848 - LINE1 Volume  }
       MM_OKSORI_EXT_MIC1                 = MM_OKSORI_BASE+15;      { Oksori External - One Mic Connect  }
       MM_OKSORI_EXT_MIC2                 = MM_OKSORI_BASE+16;      { Oksori External - Two Mic Connect  }
       MM_OKSORI_MIDIOUT                  = MM_OKSORI_BASE+17;      { Oksori MIDI Out Device  }
       MM_OKSORI_MIDIIN                   = MM_OKSORI_BASE+18;      { Oksori MIDI In Device  }
       MM_OKSORI_MPEG_CDVISION            = MM_OKSORI_BASE+19;      { Oksori CD-Vision MPEG Decoder  }

    {  Product IDs for     MM_OSITECH    -  Ositech Communications Inc.     }
    { Trumpcard  }
       MM_OSITECH_TRUMPCARD = 1;

    {  Product IDs for     MM_OSPREY    -  Osprey Technologies, Inc.     }
       MM_OSPREY_1000WAVEIN = 1;
       MM_OSPREY_1000WAVEOUT = 2;

    {  Product IDs for     MM_QUARTERDECK    -  Quarterdeck Corporation     }
    { Quarterdeck L&H Codec Wave In  }
       MM_QUARTERDECK_LHWAVEIN = 0;
    { Quarterdeck L&H Codec Wave Out  }
       MM_QUARTERDECK_LHWAVEOUT = 1;

    {  Product IDs for     MM_RHETOREX    -  Rhetorex Inc     }
       MM_RHETOREX_WAVEIN = 1;
       MM_RHETOREX_WAVEOUT = 2;

    {  Product IDs for     MM_ROCKWELL    -  Rockwell International     }
       MM_VOICEMIXER = 1;
       ROCKWELL_WA1_WAVEIN = 100;
       ROCKWELL_WA1_WAVEOUT = 101;
       ROCKWELL_WA1_SYNTH = 102;
       ROCKWELL_WA1_MIXER = 103;
       ROCKWELL_WA1_MPU401_IN = 104;
       ROCKWELL_WA1_MPU401_OUT = 105;
       ROCKWELL_WA2_WAVEIN = 200;
       ROCKWELL_WA2_WAVEOUT = 201;
       ROCKWELL_WA2_SYNTH = 202;
       ROCKWELL_WA2_MIXER = 203;
       ROCKWELL_WA2_MPU401_IN = 204;
       ROCKWELL_WA2_MPU401_OUT = 205;
              
    {  Product IDs for     MM_S3    -  S3     }
       MM_S3_WAVEOUT = $1;       
       MM_S3_WAVEIN = $2;       
       MM_S3_MIDIOUT = $3;       
       MM_S3_MIDIIN = $4;       
       MM_S3_FMSYNTH = $5;       
       MM_S3_MIXER = $6;       
       MM_S3_AUX = $7;

    {  Product IDs for     MM_SEERSYS    -  Seer Systems, Inc.     }
       MM_SEERSYS_SEERSYNTH = 1;
       MM_SEERSYS_SEERWAVE = 2;
       MM_SEERSYS_SEERMIX = 3;

    {  Product IDs for     MM_SOFTSOUND    -  Softsound, Ltd.     }
       MM_SOFTSOUND_CODEC = 1;

    {  Product IDs for     MM_SOUNDESIGNS    -  SounDesignS M.C.S. Ltd.     }
       MM_SOUNDESIGNS_WAVEIN = 1;
       MM_SOUNDESIGNS_WAVEOUT = 2;

    {  Product IDs for     MM_SPECTRUM_SIGNAL_PROCESSING    -  Spectrum Signal Processing, Inc.     }
    { Sound Festa Wave In Device  }
       MM_SSP_SNDFESWAVEIN = 1;
    { Sound Festa Wave Out Device  }
       MM_SSP_SNDFESWAVEOUT = 2;
    { Sound Festa MIDI In Device  }
       MM_SSP_SNDFESMIDIIN = 3;
    { Sound Festa MIDI Out Device  }
       MM_SSP_SNDFESMIDIOUT = 4;
    { Sound Festa MIDI Synth Device  }
       MM_SSP_SNDFESSYNTH = 5;
    { Sound Festa Mixer Device  }
       MM_SSP_SNDFESMIX = 6;
    { Sound Festa Auxilliary Device  }
       MM_SSP_SNDFESAUX = 7;

    {  Product IDs for     MM_TDK    -  TDK Corporation     }
       MM_TDK_MW_MIDI_SYNTH = 1;       
       MM_TDK_MW_MIDI_IN = 2;       
       MM_TDK_MW_MIDI_OUT = 3;       
       MM_TDK_MW_WAVE_IN = 4;       
       MM_TDK_MW_WAVE_OUT = 5;       
       MM_TDK_MW_AUX = 6;       
       MM_TDK_MW_MIXER = 10;       
       MM_TDK_MW_AUX_MASTER = 100;       
       MM_TDK_MW_AUX_BASS = 101;       
       MM_TDK_MW_AUX_TREBLE = 102;       
       MM_TDK_MW_AUX_MIDI_VOL = 103;       
       MM_TDK_MW_AUX_WAVE_VOL = 104;       
       MM_TDK_MW_AUX_WAVE_RVB = 105;       
       MM_TDK_MW_AUX_WAVE_CHR = 106;       
       MM_TDK_MW_AUX_VOL = 107;       
       MM_TDK_MW_AUX_RVB = 108;       
       MM_TDK_MW_AUX_CHR = 109;

    {  Product IDs for     MM_TURTLE_BEACH    -  Turtle Beach, Inc.     }
       MM_TBS_TROPEZ_WAVEIN = 37;
       MM_TBS_TROPEZ_WAVEOUT = 38;
       MM_TBS_TROPEZ_AUX1 = 39;
       MM_TBS_TROPEZ_AUX2 = 40;
       MM_TBS_TROPEZ_LINE = 41;
              
    {  Product IDs for     MM_VIENNASYS    -  Vienna Systems     }
       MM_VIENNASYS_TSP_WAVE_DRIVER = 1;
              
    {  Product IDs for     MM_VIONA    -  Viona Development GmbH     }
    { Q-Motion PCI II/Bravado 2000  }
       MM_VIONA_QVINPCI_MIXER = 1;       
       MM_VIONA_QVINPCI_WAVEIN = 2;       
       MM_VIONAQVINPCI_WAVEOUT = 3;
    { Buster  }
       MM_VIONA_BUSTER_MIXER = 4;
    { Cinemaster  }
       MM_VIONA_CINEMASTER_MIXER = 5;
    { Concerto  }
       MM_VIONA_CONCERTO_MIXER = 6;

    {  Product IDs for     MM_WILDCAT    -  Wildcat Canyon Software     }
    { Autoscore  }
       MM_WILDCAT_AUTOSCOREMIDIIN = 1;

    {  Product IDs for     MM_WILLOWPOND    -  Willow Pond Corporation     }
       MM_WILLOWPOND_FMSYNTH_STEREO = 20;
       MM_WILLOWPOND_SNDPORT_WAVEIN = 100;
       MM_WILLOWPOND_SNDPORT_WAVEOUT = 101;
       MM_WILLOWPOND_SNDPORT_MIXER = 102;
       MM_WILLOWPOND_SNDPORT_AUX = 103;
       MM_WILLOWPOND_PH_WAVEIN = 104;
       MM_WILLOWPOND_PH_WAVEOUT = 105;
       MM_WILLOWPOND_PH_MIXER = 106;
       MM_WILLOWPOND_PH_AUX = 107;

    {  Product IDs for     MM_WORKBIT    -  Workbit Corporation     }
    { Harmony Mixer  }
       MM_WORKBIT_MIXER = 1;
    { Harmony Mixer  }
       MM_WORKBIT_WAVEOUT = 2;
    { Harmony Mixer  }
       MM_WORKBIT_WAVEIN = 3;
    { Harmony Mixer  }
       MM_WORKBIT_MIDIIN = 4;
    { Harmony Mixer  }
       MM_WORKBIT_MIDIOUT = 5;
    { Harmony Mixer  }
       MM_WORKBIT_FMSYNTH = 6;
    { Harmony Mixer  }
       MM_WORKBIT_AUX = 7;
       MM_WORKBIT_JOYSTICK = 8;
    {  Product IDs for     MM_FRAUNHOFER_IIS -  Fraunhofer  }
       MM_FHGIIS_MPEGLAYER3 = 10;
{$ENDIF NOMMIDS}


{*////////////////////////////////////////////////////////////////////////// */

/*              INFO LIST CHUNKS (from the Multimedia Programmer's Reference
                                        plus new ones)
*}

const
      RIFFINFO_IARL = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('A')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('L')) shl 24)
                            ); //*Archival location  */

      RIFFINFO_IART = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('A')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Artist  */

      RIFFINFO_ICMS = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('C')) shl 8) or
                             (byte(AnsiChar('M')) shl 16) or
                             (byte(AnsiChar('S'))shl 24)
                            ); //*Commissioned  */

      RIFFINFO_ICMT = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('C')) shl 8) or
                             (byte(AnsiChar('M')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Comments  */

      RIFFINFO_ICOP = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('C')) shl 8) or
                             (byte(AnsiChar('O')) shl 16) or
                             (byte(AnsiChar('P')) shl 24)
                            ); //*Copyright  */

      RIFFINFO_ICRD = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('C')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('D')) shl 24)
                            ); //*Creation date of subject  */

      RIFFINFO_ICRP = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('C')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('P')) shl 24)
                            ); //*Cropped  */

      RIFFINFO_IDIM = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('D')) shl 8) or
                             (byte(AnsiChar('I')) shl 16) or
                             (byte(AnsiChar('M')) shl 24)
                            ); //*Dimensions  */

      RIFFINFO_IDPI = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('D')) shl 8) or
                             (byte(AnsiChar('P')) shl 16) or
                             (byte(AnsiChar('I')) shl 24)
                            ); //*Dots per inch  */

      RIFFINFO_IENG = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('E')) shl 8) or
                             (byte(AnsiChar('N')) shl 16) or
                             (byte(AnsiChar('G')) shl 24)
                            ); //*Engineer  */

      RIFFINFO_IGNR = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('G')) shl 8) or
                             (byte(AnsiChar('N')) shl 16) or
                             (byte(AnsiChar('R')) shl 24)
                            ); //*Genre  */

      RIFFINFO_IKEY = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('K')) shl 8) or
                             (byte(AnsiChar('E')) shl 16) or
                             (byte(AnsiChar('Y')) shl 24)
                            ); //*Keywords  */

      RIFFINFO_ILGT = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('L')) shl 8) or
                             (byte(AnsiChar('G')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Lightness settings  */

      RIFFINFO_IMED = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('M')) shl 8) or
                             (byte(AnsiChar('E')) shl 16) or
                             (byte(AnsiChar('D')) shl 24)
                            ); //*Medium  */

      RIFFINFO_INAM = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('N')) shl 8) or
                             (byte(AnsiChar('A')) shl 16) or
                             (byte(AnsiChar('M')) shl 24)
                            ); //*Name of subject  */

      RIFFINFO_IPLT = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('P')) shl 8) or
                             (byte(AnsiChar('L')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Palette Settings. No. of colors requested.   */

      RIFFINFO_IPRD = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('P')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('D')) shl 24)
                            ); //*Product  */

      RIFFINFO_ISBJ = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('B')) shl 16) or
                             (byte(AnsiChar('J')) shl 24)
                            ); //*Subject description  */

      RIFFINFO_ISFT = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('F')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Software. Name of package used to create file.  */

      RIFFINFO_ISHP = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('H')) shl 16) or
                             (byte(AnsiChar('P')) shl 24)
                            ); //*Sharpness.  */

      RIFFINFO_ISRC = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('C')) shl 24)
                            ); //*Source.   */

      RIFFINFO_ISRF = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('R')) shl 16) or
                             (byte(AnsiChar('F')) shl 24)
                            ); //*Source Form. ie slide, paper  */

      RIFFINFO_ITCH = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('T')) shl 8) or
                             (byte(AnsiChar('C')) shl 16) or
                             (byte(AnsiChar('H')) shl 24)
                            ); //*Technician who digitized the subject.  */

//* New INFO Chunks as of August 30, 1993: */
      RIFFINFO_ISMP = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('S')) shl 8) or
                             (byte(AnsiChar('M')) shl 16) or
                             (byte(AnsiChar('P')) shl 24)
                            ); //*SMPTE time code  */
{* ISMP: SMPTE time code of digitization start point expressed as a NULL terminated
                text string "HH:MM:SS:FF". If performing MCI capture in AVICAP, this
                chunk will be automatically set based on the MCI start time.
*}

      RIFFINFO_IDIT = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('D')) shl 8) or
                             (byte(AnsiChar('I')) shl 16) or
                             (byte(AnsiChar('T')) shl 24)
                            ); //*Digitization Time  */
{* IDIT: "Digitization Time" Specifies the time and date that the digitization commenced.
                The digitization time is contained in an ASCII string which
                contains exactly 26 characters and is in the format
                "Wed Jan 02 02:03:55 1990\n\0".
                The ctime(), asctime(), functions can be used to create strings
                in this format. This chunk is automatically added to the capture
                file based on the current system time at the moment capture is initiated.
*}

{*Template line for new additions
      RIFFINFO_I = FOURCC(byte(AnsiChar('I')) or
                             (byte(AnsiChar('')) shl 8) or
                             (byte(AnsiChar('')) shl 16) or
                             (byte(AnsiChar('')) shl 24)
                            ); // Comment
}
//*/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*/

{$IFNDEF NONEWWAVE}
{ WAVE form wFormatTag IDs  }
const
       WAVE_FORMAT_UNKNOWN = $0000;{  Microsoft Corporation   }
    {  Microsoft Corporation   }
       WAVE_FORMAT_ADPCM = $0002;
    {  Microsoft Corporation   }
       WAVE_FORMAT_IEEE_FLOAT = $0003;       
    {  IEEE754: range (+1, -1]   }
    {  32-bit/64-bit format as defined by  }
    {  MSVC++ float/double type  }
    {  IBM Corporation   }
       WAVE_FORMAT_IBM_CVSD = $0005;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_ALAW = $0006;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_MULAW = $0007;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_WMAVOICE9 = $000a;       
    {  OKI   }
       WAVE_FORMAT_OKI_ADPCM = $0010;       
    {  Intel Corporation   }
       WAVE_FORMAT_DVI_ADPCM = $0011;       
    {  Intel Corporation   }
       WAVE_FORMAT_IMA_ADPCM = WAVE_FORMAT_DVI_ADPCM;       
    {  Videologic   }
       WAVE_FORMAT_MEDIASPACE_ADPCM = $0012;       
    {  Sierra Semiconductor Corp   }
       WAVE_FORMAT_SIERRA_ADPCM = $0013;       
    {  Antex Electronics Corporation   }
       WAVE_FORMAT_G723_ADPCM = $0014;       
    {  DSP Solutions, Inc.   }
       WAVE_FORMAT_DIGISTD = $0015;       
    {  DSP Solutions, Inc.   }
       WAVE_FORMAT_DIGIFIX = $0016;       
    {  Dialogic Corporation   }
       WAVE_FORMAT_DIALOGIC_OKI_ADPCM = $0017;       
    {  Media Vision, Inc.  }
       WAVE_FORMAT_MEDIAVISION_ADPCM = $0018;       
    {  Yamaha Corporation of America   }
       WAVE_FORMAT_YAMAHA_ADPCM = $0020;       
    {  Speech Compression   }
       WAVE_FORMAT_SONARC = $0021;       
    {  DSP Group, Inc   }
       WAVE_FORMAT_DSPGROUP_TRUESPEECH = $0022;       
    {  Echo Speech Corporation   }
       WAVE_FORMAT_ECHOSC1 = $0023;       
    {     }
       WAVE_FORMAT_AUDIOFILE_AF36 = $0024;       
    {  Audio Processing Technology   }
       WAVE_FORMAT_APTX = $0025;       
    {     }
       WAVE_FORMAT_AUDIOFILE_AF10 = $0026;       
    {  Dolby Laboratories   }
       WAVE_FORMAT_DOLBY_AC2 = $0030;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_GSM610 = $0031;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_MSNAUDIO = $0032;       
    {  Antex Electronics Corporation   }
       WAVE_FORMAT_ANTEX_ADPCME = $0033;       
    {  Control Resources Limited   }
       WAVE_FORMAT_CONTROL_RES_VQLPC = $0034;       
    {  DSP Solutions, Inc.   }
       WAVE_FORMAT_DIGIREAL = $0035;       
    {  DSP Solutions, Inc.   }
       WAVE_FORMAT_DIGIADPCM = $0036;       
    {  Control Resources Limited   }
       WAVE_FORMAT_CONTROL_RES_CR10 = $0037;       
    {  Natural MicroSystems   }
       WAVE_FORMAT_NMS_VBXADPCM = $0038;       
    { Crystal Semiconductor IMA ADPCM  }
       WAVE_FORMAT_CS_IMAADPCM = $0039;       
    { Echo Speech Corporation  }
       WAVE_FORMAT_ECHOSC3 = $003A;       
    { Rockwell International  }
       WAVE_FORMAT_ROCKWELL_ADPCM = $003B;       
    { Rockwell International  }
       WAVE_FORMAT_ROCKWELL_DIGITALK = $003C;       
    { Xebec Multimedia Solutions Limited  }
       WAVE_FORMAT_XEBEC = $003D;       
    {  Antex Electronics Corporation   }
       WAVE_FORMAT_G721_ADPCM = $0040;       
    {  Antex Electronics Corporation   }
       WAVE_FORMAT_G728_CELP = $0041;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_MPEG = $0050;       
    {  ISO/MPEG Layer3 Format Tag  }
       WAVE_FORMAT_MPEGLAYER3 = $0055;       
    {  Cirrus Logic   }
       WAVE_FORMAT_CIRRUS = $0060;       
    {  ESS Technology   }
       WAVE_FORMAT_ESPCM = $0061;       
    {  Voxware Inc   }
       WAVE_FORMAT_VOXWARE = $0062;       
    {  Canopus, co., Ltd.   }
       WAVEFORMAT_CANOPUS_ATRAC = $0063;       
    {  APICOM   }
       WAVE_FORMAT_G726_ADPCM = $0064;       
    {  APICOM       }
       WAVE_FORMAT_G722_ADPCM = $0065;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_DSAT = $0066;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_DSAT_DISPLAY = $0067;       
    {  Softsound, Ltd.       }
       WAVE_FORMAT_SOFTSOUND = $0080;       
    {  Rhetorex Inc   }
       WAVE_FORMAT_RHETOREX_ADPCM = $0100;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_MSAUDIO1 = $0160;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_WMAUDIO2 = $0161;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_WMAUDIO3 = $0162;       
    {  Microsoft Corporation   }
       WAVE_FORMAT_WMAUDIO_LOSSLESS = $0163;       
    {  Creative Labs, Inc   }
       WAVE_FORMAT_CREATIVE_ADPCM = $0200;       
    {  Creative Labs, Inc   }
       WAVE_FORMAT_CREATIVE_FASTSPEECH8 = $0202;       
    {  Creative Labs, Inc   }
       WAVE_FORMAT_CREATIVE_FASTSPEECH10 = $0203;       
    {  Quarterdeck Corporation   }
       WAVE_FORMAT_QUARTERDECK = $0220;       
    {  Fujitsu Corp.   }
       WAVE_FORMAT_FM_TOWNS_SND = $0300;       
    {  Brooktree Corporation   }
       WAVE_FORMAT_BTV_DIGITAL = $0400;       
    {  Ing C. Olivetti & C., S.p.A.   }
       WAVE_FORMAT_OLIGSM = $1000;       
    {  Ing C. Olivetti & C., S.p.A.   }
       WAVE_FORMAT_OLIADPCM = $1001;       
    {  Ing C. Olivetti & C., S.p.A.   }
       WAVE_FORMAT_OLICELP = $1002;       
    {  Ing C. Olivetti & C., S.p.A.   }
       WAVE_FORMAT_OLISBC = $1003;       
    {  Ing C. Olivetti & C., S.p.A.   }
       WAVE_FORMAT_OLIOPR = $1004;       
    {  Lernout & Hauspie   }
       WAVE_FORMAT_LH_CODEC = $1100;       
    {  Norris Communications, Inc.   }
       WAVE_FORMAT_NORRIS = $1400;

const
      WAVE_FORMAT_EXTENSIBLE = $FFFE;     { Microsoft  }

    { }
    {  the WAVE_FORMAT_DEVELOPMENT format tag can be used during the }
    {  development phase of a new wave format.  Before shipping, you MUST }
    {  acquire an official format tag from Microsoft. }
    { }

const
       WAVE_FORMAT_DEVELOPMENT = $FFFF;

{$ENDIF NONEWWAVE}

type
{ general waveform format structure (information common to all formats)  }
     waveformat_tag = record
       wFormatTag:word; { format type  }
       nChannels:word; { number of channels (i.e. mono, stereo...)  }
       nSamplesPerSec:DWORD; { sample rate  }
       nAvgBytesPerSec:DWORD; { for buffer estimation  }
       nBlockAlign:word; { block size of data  }
     end;
     WAVEFORMAT = waveformat_tag;
     PWAVEFORMAT = ^WAVEFORMAT;
     NPWAVEFORMAT = ^WAVEFORMAT;
     LPWAVEFORMAT = ^WAVEFORMAT;
     LPCWAVEFORMAT = ^WAVEFORMAT;

    { flags for wFormatTag field of WAVEFORMAT  }
const
      WAVE_FORMAT_PCM = 1;

{ specific waveform format structure for PCM data  }
type
     pcmwaveformat_tag = record
       wf:WAVEFORMAT;
       wBitsPerSample:word;  // corresponds to MCI_WAVE_SET_.... structure
     end;
     PCMWAVEFORMAT = pcmwaveformat_tag;
     PPCMWAVEFORMAT = ^PCMWAVEFORMAT;
     NPPCMWAVEFORMAT = ^PCMWAVEFORMAT;
     LPPCMWAVEFORMAT = ^PCMWAVEFORMAT;

{ general extended waveform format structure
  Use this for all NON PCM formats (information common to all formats)
}

{*
 *  extended waveform format structure used for all non-PCM formats. this
 *  structure is common to all non-PCM formats.
 *}
type
     tWAVEFORMATEX = record
       wFormatTag:word;       //* format type */
       nChannels:word;        //* number of channels (i.e. mono, stereo...) */
       nSamplesPerSec:DWORD;  //* sample rate */
       nAvgBytesPerSec:DWORD; //* for buffer estimation */
       nBlockAlign:word;      //* block size of data */
       wBitsPerSample:word;   //* Number of bits per sample of mono data */
       cbSize:word;           //* The count in bytes of the size of
                              //  extra information (after cbSize) */
     end;
     WAVEFORMATEX = tWAVEFORMATEX;
     PWAVEFORMATEX = ^WAVEFORMATEX;
     NPWAVEFORMATEX = ^WAVEFORMATEX;
     LPWAVEFORMATEX = ^tWAVEFORMATEX;
     LPCWAVEFORMATEX = ^WAVEFORMATEX;

type
     WAVEFORMATEXTENSIBLE = record
       Format:WAVEFORMATEX;
       Samples:record
         case longint of
           0: (wValidBitsPerSample:word);
           1: (wSamplesPerBlock:word);
           2: (wReserved:word);
       end;
       dwChannelMask:DWORD;
       SubFormat:TGUID;
     end;
     PWAVEFORMATEXTENSIBLE = ^WAVEFORMATEXTENSIBLE;

{  Extended PCM waveform format structure based on WAVEFORMATEXTENSIBLE. }
{  Use this for multiple channel and hi-resolution PCM data }
type
     WAVEFORMATPCMEX = WAVEFORMATEXTENSIBLE; { Format.cbSize = 22  }
     PWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;
     NPWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;
     LPWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;

    {  Extended format structure using IEEE Float data and based }
    {  on WAVEFORMATEXTENSIBLE.  Use this for multiple channel }
    {  and hi-resolution PCM data in IEEE floating point format. }

     WAVEFORMATIEEEFLOATEX = WAVEFORMATEXTENSIBLE; { Format.cbSize = 22  }
     PWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;
     NPWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;
     LPWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;

{ Speaker Positions for dwChannelMask in WAVEFORMATEXTENSIBLE: }
const
       SPEAKER_FRONT_LEFT = $1;       
       SPEAKER_FRONT_RIGHT = $2;       
       SPEAKER_FRONT_CENTER = $4;       
       SPEAKER_LOW_FREQUENCY = $8;       
       SPEAKER_BACK_LEFT = $10;       
       SPEAKER_BACK_RIGHT = $20;       
       SPEAKER_FRONT_LEFT_OF_CENTER = $40;       
       SPEAKER_FRONT_RIGHT_OF_CENTER = $80;       
       SPEAKER_BACK_CENTER = $100;       
       SPEAKER_SIDE_LEFT = $200;       
       SPEAKER_SIDE_RIGHT = $400;       
       SPEAKER_TOP_CENTER = $800;       
       SPEAKER_TOP_FRONT_LEFT = $1000;       
       SPEAKER_TOP_FRONT_CENTER = $2000;       
       SPEAKER_TOP_FRONT_RIGHT = $4000;       
       SPEAKER_TOP_BACK_LEFT = $8000;       
       SPEAKER_TOP_BACK_CENTER = $10000;       
       SPEAKER_TOP_BACK_RIGHT = $20000;       
    { Bit mask locations reserved for future use }
       SPEAKER_RESERVED = $7FFC0000;       
    { Used to specify that any possible permutation of speaker configurations }
       SPEAKER_ALL = $80000000;

{$IFNDEF NONEWWAVE}
{ Define data for MS ADPCM  }

type
     adpcmcoef_tag = record
       iCoef1:SmallInt;
       iCoef2:SmallInt;
     end;
     ADPCMCOEFSET = adpcmcoef_tag;
     PADPCMCOEFSET = ^ADPCMCOEFSET;
     NPADPCMCOEFSET = ^ADPCMCOEFSET;
     LPADPCMCOEFSET = ^ADPCMCOEFSET;

type
     adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wSamplesPerBlock:word;
       wNumCoef:word;
       aCoef:array[0..0] of ADPCMCOEFSET;
     end;
     ADPCMWAVEFORMAT = adpcmwaveformat_tag;
     PADPCMWAVEFORMAT = ^ADPCMWAVEFORMAT;
     NPADPCMWAVEFORMAT = ^ADPCMWAVEFORMAT;
     LPADPCMWAVEFORMAT = ^ADPCMWAVEFORMAT;
     
{  Intel's DVI ADPCM structure definitions }
{ }
{      for WAVE_FORMAT_DVI_ADPCM   (0x0011) }

type
     dvi_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wSamplesPerBlock:word;
     end;
     DVIADPCMWAVEFORMAT = dvi_adpcmwaveformat_tag;
     PDVIADPCMWAVEFORMAT = ^DVIADPCMWAVEFORMAT;
     NPDVIADPCMWAVEFORMAT = ^DVIADPCMWAVEFORMAT;
     LPDVIADPCMWAVEFORMAT = ^DVIADPCMWAVEFORMAT;

    { }
    {  IMA endorsed ADPCM structure definitions--note that this is exactly }
    {  the same format as Intel's DVI ADPCM. }
    { }
    {      for WAVE_FORMAT_IMA_ADPCM   (0x0011) }
    { }
    { }

     ima_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wSamplesPerBlock:word;
     end;
     IMAADPCMWAVEFORMAT = ima_adpcmwaveformat_tag;
     PIMAADPCMWAVEFORMAT = ^IMAADPCMWAVEFORMAT;
     NPIMAADPCMWAVEFORMAT = ^IMAADPCMWAVEFORMAT;
     LPIMAADPCMWAVEFORMAT = ^IMAADPCMWAVEFORMAT;

    {
    //VideoLogic's Media Space ADPCM Structure definitions
    // for  WAVE_FORMAT_MEDIASPACE_ADPCM    (0x0012)
    //
    //
     }

     mediaspace_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wRevision:word;
     end;
     MEDIASPACEADPCMWAVEFORMAT = mediaspace_adpcmwaveformat_tag;
     PMEDIASPACEADPCMWAVEFORMAT = ^MEDIASPACEADPCMWAVEFORMAT;
     NPMEDIASPACEADPCMWAVEFORMAT = ^MEDIASPACEADPCMWAVEFORMAT;
     LPMEDIASPACEADPCMWAVEFORMAT = ^MEDIASPACEADPCMWAVEFORMAT;

    {  Sierra Semiconductor }
    { }
    {      for WAVE_FORMAT_SIERRA_ADPCM   (0x0013) }

     sierra_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wRevision:word;
     end;
     SIERRAADPCMWAVEFORMAT = sierra_adpcmwaveformat_tag;
     PSIERRAADPCMWAVEFORMAT = ^SIERRAADPCMWAVEFORMAT;
     NPSIERRAADPCMWAVEFORMAT = ^SIERRAADPCMWAVEFORMAT;
     LPSIERRAADPCMWAVEFORMAT = ^SIERRAADPCMWAVEFORMAT;
     
    {  Antex Electronics  structure definitions }
    { }
    {      for WAVE_FORMAT_G723_ADPCM   (0x0014) }

     g723_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       cbExtraSize:word;
       nAuxBlockSize:word;
     end;
     G723_ADPCMWAVEFORMAT = g723_adpcmwaveformat_tag;
     PG723_ADPCMWAVEFORMAT = ^G723_ADPCMWAVEFORMAT;
     NPG723_ADPCMWAVEFORMAT = ^G723_ADPCMWAVEFORMAT;
     LPG723_ADPCMWAVEFORMAT = ^G723_ADPCMWAVEFORMAT;

    { }
    {  DSP Solutions (formerly DIGISPEECH) structure definitions }
    { }
    {      for WAVE_FORMAT_DIGISTD   (0x0015) }

     digistdwaveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     DIGISTDWAVEFORMAT = digistdwaveformat_tag;
     PDIGISTDWAVEFORMAT = ^DIGISTDWAVEFORMAT;
     NPDIGISTDWAVEFORMAT = ^DIGISTDWAVEFORMAT;
     LPDIGISTDWAVEFORMAT = ^DIGISTDWAVEFORMAT;

    { }
    {  DSP Solutions (formerly DIGISPEECH) structure definitions }
    { }
    {      for WAVE_FORMAT_DIGIFIX   (0x0016) }
    { }
    { }

     digifixwaveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     DIGIFIXWAVEFORMAT = digifixwaveformat_tag;
     PDIGIFIXWAVEFORMAT = ^DIGIFIXWAVEFORMAT;
     NPDIGIFIXWAVEFORMAT = ^DIGIFIXWAVEFORMAT;
     LPDIGIFIXWAVEFORMAT = ^DIGIFIXWAVEFORMAT;

    { }
    {   Dialogic Corporation }
    { WAVEFORMAT_DIALOGIC_OKI_ADPCM   (0x0017) }
    { }

     creative_fastspeechformat_tag = record
       ewf:WAVEFORMATEX;
     end;
     DIALOGICOKIADPCMWAVEFORMAT = creative_fastspeechformat_tag;
     PDIALOGICOKIADPCMWAVEFORMAT = ^DIALOGICOKIADPCMWAVEFORMAT;
     NPDIALOGICOKIADPCMWAVEFORMAT = ^DIALOGICOKIADPCMWAVEFORMAT;
     LPDIALOGICOKIADPCMWAVEFORMAT = ^DIALOGICOKIADPCMWAVEFORMAT;

    { }
    {  Yamaha Compression's ADPCM structure definitions }
    { }
    {      for WAVE_FORMAT_YAMAHA_ADPCM   (0x0020) }
    { }
    { }

     yamaha_adpmcwaveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     YAMAHA_ADPCMWAVEFORMAT = yamaha_adpmcwaveformat_tag;
     PYAMAHA_ADPCMWAVEFORMAT = ^YAMAHA_ADPCMWAVEFORMAT;
    (* near ignored *)
     NPYAMAHA_ADPCMWAVEFORMAT = ^YAMAHA_ADPCMWAVEFORMAT;
    (* far ignored *)
     LPYAMAHA_ADPCMWAVEFORMAT = ^YAMAHA_ADPCMWAVEFORMAT;

    { }
    {  Speech Compression's Sonarc structure definitions }
    { }
    {      for WAVE_FORMAT_SONARC   (0x0021) }
    { }
    { }
     sonarcwaveformat_tag = record
       wfx : WAVEFORMATEX;
       wCompType : word;
     end;
     SONARCWAVEFORMAT = sonarcwaveformat_tag;
     PSONARCWAVEFORMAT = ^SONARCWAVEFORMAT;
     NPSONARCWAVEFORMAT = ^SONARCWAVEFORMAT;
     LPSONARCWAVEFORMAT = SONARCWAVEFORMAT;


    { }
    {  DSP Groups's TRUESPEECH structure definitions }
    { }
    {      for WAVE_FORMAT_DSPGROUP_TRUESPEECH   (0x0022) }
    { }
    { }

     truespeechwaveformat_tag = record
       wfx : WAVEFORMATEX;
       wRevision :word;
       nSamplesPerBlock:word;
       abReserved :array[0..27] of byte;
     end;
     TRUESPEECHWAVEFORMAT = truespeechwaveformat_tag;
     PTRUESPEECHWAVEFORMAT = ^TRUESPEECHWAVEFORMAT;
     NPTRUESPEECHWAVEFORMAT = ^TRUESPEECHWAVEFORMAT;
     LPTRUESPEECHWAVEFORMAT = ^TRUESPEECHWAVEFORMAT;

    { }
    {  Echo Speech Corp structure definitions }
    { }
    {      for WAVE_FORMAT_ECHOSC1   (0x0023) }

     echosc1waveformat_tag = record
       wfx:WAVEFORMATEX;
     end;
     ECHOSC1WAVEFORMAT = echosc1waveformat_tag;
     PECHOSC1WAVEFORMAT = ^ECHOSC1WAVEFORMAT;
     NPECHOSC1WAVEFORMAT = ^ECHOSC1WAVEFORMAT;
     LPECHOSC1WAVEFORMAT = ^ECHOSC1WAVEFORMAT;

    { }
    {  Audiofile Inc.structure definitions }
    { }
    {      for WAVE_FORMAT_AUDIOFILE_AF36   (0x0024) }
    { }
    { }

     audiofile_af36waveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     AUDIOFILE_AF36WAVEFORMAT = audiofile_af36waveformat_tag;
     PAUDIOFILE_AF36WAVEFORMAT = ^AUDIOFILE_AF36WAVEFORMAT;
     NPAUDIOFILE_AF36WAVEFORMAT = ^AUDIOFILE_AF36WAVEFORMAT;
     LPAUDIOFILE_AF36WAVEFORMAT = ^AUDIOFILE_AF36WAVEFORMAT;
     
    { }
    {  Audio Processing Technology structure definitions }
    { }
    {      for WAVE_FORMAT_APTX   (0x0025) }
    { }
    { }
     aptxwaveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     APTXWAVEFORMAT = aptxwaveformat_tag;
     PAPTXWAVEFORMAT = ^APTXWAVEFORMAT;
     NPAPTXWAVEFORMAT = ^APTXWAVEFORMAT;
     LPAPTXWAVEFORMAT = ^APTXWAVEFORMAT;

    { }
    {  Audiofile Inc.structure definitions }
    { }
    {      for WAVE_FORMAT_AUDIOFILE_AF10   (0x0026) }
    { }
    { }
     audiofile_af10waveformat_tag = record
       wfx : WAVEFORMATEX;
     end;
     AUDIOFILE_AF10WAVEFORMAT = audiofile_af10waveformat_tag;
     PAUDIOFILE_AF10WAVEFORMAT = ^AUDIOFILE_AF10WAVEFORMAT;
     NPAUDIOFILE_AF10WAVEFORMAT = ^AUDIOFILE_AF10WAVEFORMAT;
     LPAUDIOFILE_AF10WAVEFORMAT = ^AUDIOFILE_AF10WAVEFORMAT;


    { }
    { Dolby's AC-2 wave format structure definition
               WAVE_FORMAT_DOLBY_AC2    (0x0030) }
    { }
     dolbyac2waveformat_tag = record
       wfx:WAVEFORMATEX;
       nAuxBitsCode:word;
     end;
     DOLBYAC2WAVEFORMAT = dolbyac2waveformat_tag;

    {Microsoft's  }
    { WAVE_FORMAT_GSM 610           0x0031 }
    { }
     gsm610waveformat_tag = record
       wfx : WAVEFORMATEX;
       wSamplesPerBlock : word;
     end;
     GSM610WAVEFORMAT = gsm610waveformat_tag;
     PGSM610WAVEFORMAT = ^GSM610WAVEFORMAT;
     NPGSM610WAVEFORMAT = ^GSM610WAVEFORMAT;
     LPGSM610WAVEFORMAT = ^GSM610WAVEFORMAT;


    { }
    {      Antex Electronics Corp }
    { }
    {      for WAVE_FORMAT_ADPCME                  (0x0033) }
    { }
    { }
     adpcmewaveformat_tag = record
       wfx : WAVEFORMATEX;
       wSamplesPerBlock:word;
     end;
     ADPCMEWAVEFORMAT = adpcmewaveformat_tag;
     PADPCMEWAVEFORMAT = ^ADPCMEWAVEFORMAT;
     NPADPCMEWAVEFORMAT = ^ADPCMEWAVEFORMAT;
     LPADPCMEWAVEFORMAT = ^ADPCMEWAVEFORMAT;


    {       Control Resources Limited  }
    { WAVE_FORMAT_CONTROL_RES_VQLPC                 0x0034 }
    { }

       contres_vqlpcwaveformat_tag = record
            wfx : WAVEFORMATEX;
            wSamplesPerBlock :word;
         end;
       CONTRESVQLPCWAVEFORMAT = contres_vqlpcwaveformat_tag;

       PCONTRESVQLPCWAVEFORMAT = ^CONTRESVQLPCWAVEFORMAT;
    (* near ignored *)

       NPCONTRESVQLPCWAVEFORMAT = ^CONTRESVQLPCWAVEFORMAT;
    (* far ignored *)

       LPCONTRESVQLPCWAVEFORMAT = ^CONTRESVQLPCWAVEFORMAT;

    {      for WAVE_FORMAT_DIGIREAL                   (0x0035) }
    { }
    { }

       digirealwaveformat_tag = record
            wfx : WAVEFORMATEX;
            wSamplesPerBlock : word;
         end;
       DIGIREALWAVEFORMAT = digirealwaveformat_tag;

       PDIGIREALWAVEFORMAT = ^DIGIREALWAVEFORMAT;
       NPDIGIREALWAVEFORMAT = ^DIGIREALWAVEFORMAT;
       LPDIGIREALWAVEFORMAT = ^DIGIREALWAVEFORMAT;

    {  DSP Solutions }
    { }
    {      for WAVE_FORMAT_DIGIADPCM   (0x0036) }
       digiadpcmmwaveformat_tag = record
            wfx : WAVEFORMATEX;
            wSamplesPerBlock:word;
         end;
       DIGIADPCMWAVEFORMAT = digiadpcmmwaveformat_tag;

       PDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;
    (* near ignored *)

       NPDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;
    (* far ignored *)

       LPDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;


    {       Control Resources Limited  }
    { WAVE_FORMAT_CONTROL_RES_CR10          0x0037 }
       contres_cr10waveformat_tag = record
            wfx : WAVEFORMATEX;
            wSamplesPerBlock :word;
         end;
       CONTRESCR10WAVEFORMAT = contres_cr10waveformat_tag;
       PCONTRESCR10WAVEFORMAT = ^CONTRESCR10WAVEFORMAT;
       NPCONTRESCR10WAVEFORMAT = ^CONTRESCR10WAVEFORMAT;
       LPCONTRESCR10WAVEFORMAT = ^CONTRESCR10WAVEFORMAT;

    { }
    {  Natural Microsystems }
    { }
    {      for WAVE_FORMAT_NMS_VBXADPCM   (0x0038) }
       nms_vbxadpcmmwaveformat_tag = record
            wfx : WAVEFORMATEX;
            wSamplesPerBlock :word;
         end;
       NMS_VBXADPCMWAVEFORMAT = nms_vbxadpcmmwaveformat_tag;

       PNMS_VBXADPCMWAVEFORMAT = ^NMS_VBXADPCMWAVEFORMAT;
    (* near ignored *)

       NPNMS_VBXADPCMWAVEFORMAT = ^NMS_VBXADPCMWAVEFORMAT;
    (* far ignored *)

       LPNMS_VBXADPCMWAVEFORMAT = ^NMS_VBXADPCMWAVEFORMAT;


    { }
    {  Antex Electronics  structure definitions }
    { }
    {      for WAVE_FORMAT_G721_ADPCM   (0x0040) }
    { }
    { }

       g721_adpcmwaveformat_tag = record
            wfx : WAVEFORMATEX;
            nAuxBlockSize :word;
         end;
       G721_ADPCMWAVEFORMAT = g721_adpcmwaveformat_tag;

       PG721_ADPCMWAVEFORMAT = ^G721_ADPCMWAVEFORMAT;
    (* near ignored *)

       NPG721_ADPCMWAVEFORMAT = ^G721_ADPCMWAVEFORMAT;
    (* far ignored *)

       LPG721_ADPCMWAVEFORMAT = ^G721_ADPCMWAVEFORMAT;

    { Microsoft MPEG audio WAV definition }
    { }
    {  MPEG-1 audio wave format (audio layer only).   (0x0050)    }
     mpeg1waveformat_tag = record
       wfx:WAVEFORMATEX;
       fwHeadLayer:word;
       dwHeadBitrate:DWORD;
       fwHeadMode:word;
       fwHeadModeExt:word;
       wHeadEmphasis:word;
       fwHeadFlags:word;
       dwPTSLow:DWORD;
       dwPTSHigh:DWORD;
     end;
     MPEG1WAVEFORMAT = mpeg1waveformat_tag;
     PMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;
     NPMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;
     LPMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;

const
       ACM_MPEG_LAYER1 = $0001;       
       ACM_MPEG_LAYER2 = $0002;       
       ACM_MPEG_LAYER3 = $0004;       
       ACM_MPEG_STEREO = $0001;       
       ACM_MPEG_JOINTSTEREO = $0002;       
       ACM_MPEG_DUALCHANNEL = $0004;       
       ACM_MPEG_SINGLECHANNEL = $0008;
       ACM_MPEG_PRIVATEBIT = $0001;       
       ACM_MPEG_COPYRIGHT = $0002;       
       ACM_MPEG_ORIGINALHOME = $0004;       
       ACM_MPEG_PROTECTIONBIT = $0008;       
       ACM_MPEG_ID_MPEG1 = $0010;       

    { MPEG Layer3 WAVEFORMATEX structure }
    { for WAVE_FORMAT_MPEGLAYER3 (0x0055) }
       MPEGLAYER3_WFX_EXTRA_BYTES = 12;
    { WAVE_FORMAT_MPEGLAYER3 format sructure }

type
     mpeglayer3waveformat_tag = record
       wfx:WAVEFORMATEX;
       wID:word;
       fdwFlags:DWORD;
       nBlockSize:WORD;
       nFramesPerBlock:word;
       nCodecDelay:word;
     end;
     MPEGLAYER3WAVEFORMAT = mpeglayer3waveformat_tag;
     PMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
     NPMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
     LPMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;

    {==========================================================================; }

const
       MPEGLAYER3_ID_UNKNOWN = 0;
       MPEGLAYER3_ID_MPEG = 1;
       MPEGLAYER3_ID_CONSTANTFRAMESIZE = 2;
       
       MPEGLAYER3_FLAG_PADDING_ISO = $00000000;
       MPEGLAYER3_FLAG_PADDING_ON = $00000001;
       MPEGLAYER3_FLAG_PADDING_OFF = $00000002;

    {  Creative's ADPCM structure definitions }
    { }
    {      for WAVE_FORMAT_CREATIVE_ADPCM   (0x0200) }

type
     creative_adpcmwaveformat_tag = record
       wfx:WAVEFORMATEX;
       wRevision:word;
     end;
     CREATIVEADPCMWAVEFORMAT = creative_adpcmwaveformat_tag;
     PCREATIVEADPCMWAVEFORMAT = ^CREATIVEADPCMWAVEFORMAT;
     NPCREATIVEADPCMWAVEFORMAT = ^CREATIVEADPCMWAVEFORMAT;
     LPCREATIVEADPCMWAVEFORMAT = ^CREATIVEADPCMWAVEFORMAT;
     
    { }
    {    Creative FASTSPEECH }
    { WAVEFORMAT_CREATIVE_FASTSPEECH8   (0x0202) }
    { }

       creative_fastspeech8format_tag = record
            wfx : WAVEFORMATEX;
            wRevision : word;
         end;
       CREATIVEFASTSPEECH8WAVEFORMAT = creative_fastspeech8format_tag;

       PCREATIVEFASTSPEECH8WAVEFORMAT = ^CREATIVEFASTSPEECH8WAVEFORMAT;
    (* near ignored *)

       NPCREATIVEFASTSPEECH8WAVEFORMAT = ^CREATIVEFASTSPEECH8WAVEFORMAT;
    (* far ignored *)

       LPCREATIVEFASTSPEECH8WAVEFORMAT = ^CREATIVEFASTSPEECH8WAVEFORMAT;

    { }
    {    Creative FASTSPEECH }
    { WAVEFORMAT_CREATIVE_FASTSPEECH10   (0x0203) }
    { }
       creative_fastspeech10format_tag = record
            wfx : WAVEFORMATEX;
            wRevision : word;
         end;
       CREATIVEFASTSPEECH10WAVEFORMAT = creative_fastspeech10format_tag;

       PCREATIVEFASTSPEECH10WAVEFORMAT = ^CREATIVEFASTSPEECH10WAVEFORMAT;
    (* near ignored *)

       NPCREATIVEFASTSPEECH10WAVEFORMAT = ^CREATIVEFASTSPEECH10WAVEFORMAT;
    (* far ignored *)

       LPCREATIVEFASTSPEECH10WAVEFORMAT = ^CREATIVEFASTSPEECH10WAVEFORMAT;

    { }
    {  Fujitsu FM Towns 'SND' structure }
    { }
    {      for WAVE_FORMAT_FMMTOWNS_SND   (0x0300) }
    { }
    { }

       fmtowns_snd_waveformat_tag = record
            wfx : WAVEFORMATEX;
            wRevision : word;
         end;
       FMTOWNS_SND_WAVEFORMAT = fmtowns_snd_waveformat_tag;

       PFMTOWNS_SND_WAVEFORMAT = ^FMTOWNS_SND_WAVEFORMAT;
    (* near ignored *)

       NPFMTOWNS_SND_WAVEFORMAT = ^FMTOWNS_SND_WAVEFORMAT;
    (* far ignored *)

       LPFMTOWNS_SND_WAVEFORMAT = ^FMTOWNS_SND_WAVEFORMAT;

    {  Olivetti structure }
    { }
    {      for WAVE_FORMAT_OLIGSM   (0x1000) }

       oligsmwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       OLIGSMWAVEFORMAT = oligsmwaveformat_tag;

       POLIGSMWAVEFORMAT = ^OLIGSMWAVEFORMAT;
    (* near ignored *)

       NPOLIGSMWAVEFORMAT = ^OLIGSMWAVEFORMAT;
    (* far ignored *)

       LPOLIGSMWAVEFORMAT = ^OLIGSMWAVEFORMAT;
       
    { }
    {  Olivetti structure }
    { }
    {      for WAVE_FORMAT_OLIADPCM   (0x1001) }
    { }
    { }
       oliadpcmwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       OLIADPCMWAVEFORMAT = oliadpcmwaveformat_tag;

       POLIADPCMWAVEFORMAT = ^OLIADPCMWAVEFORMAT;
    (* near ignored *)

       NPOLIADPCMWAVEFORMAT = ^OLIADPCMWAVEFORMAT;
    (* far ignored *)

       LPOLIADPCMWAVEFORMAT = ^OLIADPCMWAVEFORMAT;

    { }
    {  Olivetti structure }
    { }
    {      for WAVE_FORMAT_OLICELP   (0x1002) }
       olicelpwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       OLICELPWAVEFORMAT = olicelpwaveformat_tag;
       POLICELPWAVEFORMAT = ^OLICELPWAVEFORMAT;
       NPOLICELPWAVEFORMAT = ^OLICELPWAVEFORMAT;
       LPOLICELPWAVEFORMAT = ^OLICELPWAVEFORMAT;

    { }
    {  Olivetti structure }
    { }
    {      for WAVE_FORMAT_OLISBC   (0x1003) }
    { }
    { }

       olisbcwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       OLISBCWAVEFORMAT = olisbcwaveformat_tag;

       POLISBCWAVEFORMAT = ^OLISBCWAVEFORMAT;
    (* near ignored *)

       NPOLISBCWAVEFORMAT = ^OLISBCWAVEFORMAT;
    (* far ignored *)

       LPOLISBCWAVEFORMAT = ^OLISBCWAVEFORMAT;

    { }
    {  Olivetti structure }
    { }
    {      for WAVE_FORMAT_OLIOPR   (0x1004) }
    { }
    { }
       olioprwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       OLIOPRWAVEFORMAT = olioprwaveformat_tag;

       POLIOPRWAVEFORMAT = ^OLIOPRWAVEFORMAT;
    (* near ignored *)

       NPOLIOPRWAVEFORMAT = ^OLIOPRWAVEFORMAT;
    (* far ignored *)

       LPOLIOPRWAVEFORMAT = ^OLIOPRWAVEFORMAT;

    { }
    {  Crystal Semiconductor IMA ADPCM format }
    { }
    {      for WAVE_FORMAT_CS_IMAADPCM   (0x0039) }
    { }
    { }

       csimaadpcmwaveformat_tag = record
            wfx : WAVEFORMATEX;
         end;
       CSIMAADPCMWAVEFORMAT = csimaadpcmwaveformat_tag;

       PCSIMAADPCMWAVEFORMAT = ^CSIMAADPCMWAVEFORMAT;
    (* near ignored *)

       NPCSIMAADPCMWAVEFORMAT = ^CSIMAADPCMWAVEFORMAT;
    (* far ignored *)

       LPCSIMAADPCMWAVEFORMAT = ^CSIMAADPCMWAVEFORMAT;

{==========================================================================; }
{ }
{  ACM Wave Filters }
{ }
{ }
{==========================================================================; }
const
       WAVE_FILTER_UNKNOWN = $0000;
       WAVE_FILTER_DEVELOPMENT = $FFFF;

type
     wavefilter_tag = record
       cbStruct:DWORD;                   //* Size of the filter in bytes */
       dwFilterTag:DWORD;                //* filter type */
       fdwFilter:DWORD;                  //* Flags for the filter (Universal Dfns) */
       dwReserved:array[0..4] of DWORD;  //* Reserved for system use */
     end;
     WAVEFILTER = wavefilter_tag;
     PWAVEFILTER = ^WAVEFILTER;
     NPWAVEFILTER = ^WAVEFILTER;
     LPWAVEFILTER = ^WAVEFILTER;

const
       WAVE_FILTER_VOLUME = $0001;

type
     wavefilter_volume_tag = record
       wfltr:WAVEFILTER;
       dwVolume:DWORD;
     end;
     VOLUMEWAVEFILTER = wavefilter_volume_tag;
     PVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
     NPVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
     LPVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
     
const
       WAVE_FILTER_ECHO = $0002;

type
     wavefilter_echo_tag = record
       wfltr:WAVEFILTER;
       dwVolume:DWORD;
       dwDelay:DWORD;
     end;
     ECHOWAVEFILTER = wavefilter_echo_tag;
     PECHOWAVEFILTER = ^ECHOWAVEFILTER;
     NPECHOWAVEFILTER = ^ECHOWAVEFILTER;
     LPECHOWAVEFILTER = ^ECHOWAVEFILTER;

//////////////////////////////////////////////////////////////////////////
//
// New RIFF WAVE Chunks
//
const
      RIFFWAVE_inst = FOURCC(byte(AnsiChar('i')) or
                             (byte(AnsiChar('n')) shl 8) or
                             (byte(AnsiChar('s')) shl 16) or
                             (byte(AnsiChar('t')) shl 24)
                            );

type
     tag_s_RIFFWAVE_inst = record
       bUnshiftedNote:byte;
       chFineTune:ShortInt;
       chGain:ShortInt;
       bLowNote:byte;
       bHighNote:byte;
       bLowVelocity:byte;
       bHighVelocity:byte;
     end;
     s_RIFFWAVE_inst = tag_s_RIFFWAVE_INST;

{$ENDIF} // NONEWWAVE

//////////////////////////////////////////////////////////////////////////
//
// New RIFF Forms
//

{$IFNDEF NONEWRIFF}

// RIFF AVI
//
// AVI file format is specified in a seperate file (AVIFMT.H),
// which is available in the VfW and Win 32 SDK
//

// RIFF CPPO
const
      RIFFCPPO = FOURCC(byte(AnsiChar('C')) or
                        (byte(AnsiChar('P')) shl 8) or
                        (byte(AnsiChar('P')) shl 16) or
                        (byte(AnsiChar('O')) shl 24)
                       );

      RIFFCPPO_objr = FOURCC(byte(AnsiChar('o')) or
                             (byte(AnsiChar('b')) shl 8) or
                             (byte(AnsiChar('j')) shl 16) or
                             (byte(AnsiChar('r')) shl 24)
                            );

      RIFFCPPO_obji = FOURCC(byte(AnsiChar('o')) or
                             (byte(AnsiChar('b')) shl 8) or
                             (byte(AnsiChar('j')) shl 16) or
                             (byte(AnsiChar('i')) shl 24)
                            );

      RIFFCPPO_clsr = FOURCC(byte(AnsiChar('c')) or
                             (byte(AnsiChar('l')) shl 8) or
                             (byte(AnsiChar('s')) shl 16) or
                             (byte(AnsiChar('r')) shl 24)
                            );

      RIFFCPPO_clsi = FOURCC(byte(AnsiChar('c')) or
                             (byte(AnsiChar('l')) shl 8) or
                             (byte(AnsiChar('s')) shl 16) or
                             (byte(AnsiChar('i')) shl 24)
                            );

      RIFFCPPO_mbr  = FOURCC(byte(AnsiChar('m')) or
                             (byte(AnsiChar('b')) shl 8) or
                             (byte(AnsiChar('r')) shl 16) or
                             (byte(AnsiChar(' ')) shl 24)
                            );

      RIFFCPPO_char = FOURCC(byte(AnsiChar('c')) or
                             (byte(AnsiChar('h')) shl 8) or
                             (byte(AnsiChar('a')) shl 16) or
                             (byte(AnsiChar('r')) shl 24)
                            );

      RIFFCPPO_byte = FOURCC(byte(AnsiChar('b')) or
                             (byte(AnsiChar('y')) shl 8) or
                             (byte(AnsiChar('t')) shl 16) or
                             (byte(AnsiChar('e')) shl 24)
                            );
      RIFFCPPO_int  = FOURCC(byte(AnsiChar('i')) or
                             (byte(AnsiChar('n')) shl 8) or
                             (byte(AnsiChar('t')) shl 16) or
                             (byte(AnsiChar(' ')) shl 24)
                            );

      RIFFCPPO_word = FOURCC(byte(AnsiChar('w')) or
                             (byte(AnsiChar('o')) shl 8) or
                             (byte(AnsiChar('r')) shl 16) or
                             (byte(AnsiChar('d')) shl 24)
                            );
      RIFFCPPO_long = FOURCC(byte(AnsiChar('l')) or
                             (byte(AnsiChar('o')) shl 8) or
                             (byte(AnsiChar('n')) shl 16) or
                             (byte(AnsiChar('g')) shl 24)
                            );
      RIFFCPPO_dwrd  = FOURCC(byte(AnsiChar('d')) or
                             (byte(AnsiChar('w')) shl 8) or
                             (byte(AnsiChar('r')) shl 16) or
                             (byte(AnsiChar('d')) shl 24)
                            );
      RIFFCPPO_flt   = FOURCC(byte(AnsiChar('f')) or
                             (byte(AnsiChar('l')) shl 8) or
                             (byte(AnsiChar('t')) shl 16) or
                             (byte(AnsiChar(' ')) shl 24)
                            );
      RIFFCPPO_dbl   = FOURCC(byte(AnsiChar('d')) or
                             (byte(AnsiChar('b')) shl 8) or
                             (byte(AnsiChar('l')) shl 16) or
                             (byte(AnsiChar(' ')) shl 24)
                            );
      RIFFCPPO_str   = FOURCC(byte(AnsiChar('s')) or
                             (byte(AnsiChar('t')) shl 8) or
                             (byte(AnsiChar('r')) shl 16) or
                             (byte(AnsiChar(' ')) shl 24)
                            );

{$ENDIF} // NONEWRIFF


//////////////////////////////////////////////////////////////////////////
//
// DIB Compression Defines
//

const
       BI_BITFIELDS = 3;

const
       QUERYDIBSUPPORT = 3073;
       QDI_SETDIBITS = $0001;
       QDI_GETDIBITS = $0002;
       QDI_DIBTOSCREEN = $0004;
       QDI_STRETCHDIB = $0008;

{ @CESYSGEN IF GWES_PGDI || GWES_MGBASE }

{$IFNDEF NOBITMAP}
type
     tagEXBMINFOHEADER = record
       bmi:BITMAPINFOHEADER;
      // extended BITMAPINFOHEADER fields
       biExtDataOffset:DWORD;
      // Other elements will go here

      // ...
      // Format-specific information
      // biExtDataOffset points here
     end;
     EXBMINFOHEADER = tagEXBMINFOHEADER;
{$ENDIF} // NOBITMAP

{ @CESYSGEN ENDIF }

// New DIB Compression Defines
const
      BICOMP_IBMULTIMOTION  = FOURCC(byte(AnsiChar('U')) or
                                     (byte(AnsiChar('L')) shl 8) or
                                     (byte(AnsiChar('T')) shl 16) or
                                     (byte(AnsiChar('I')) shl 24)
                                    );

      BICOMP_IBMPHOTOMOTION = FOURCC(byte(AnsiChar('P')) or
                                     (byte(AnsiChar('H')) shl 8) or
                                     (byte(AnsiChar('M')) shl 16) or
                                     (byte(AnsiChar('O')) shl 24)
                                    );

      BICOMP_CREATIVEYUV    = FOURCC(byte(AnsiChar('c')) or
                                     (byte(AnsiChar('y')) shl 8) or
                                     (byte(AnsiChar('u')) shl 16) or
                                     (byte(AnsiChar('v')) shl 24)
                                    );


{$IFNDEF NOJPEGDIB}
// New DIB Compression Defines
const
      JPEG_DIB              = FOURCC(byte(AnsiChar('J')) or
                                     (byte(AnsiChar('P')) shl 8) or
                                     (byte(AnsiChar('E')) shl 16) or
                                     (byte(AnsiChar('G')) shl 24)
                                    ); // Still image JPEG DIB biCompression

      MJPG_DIB              = FOURCC(byte(AnsiChar('M')) or
                                     (byte(AnsiChar('J')) shl 8) or
                                     (byte(AnsiChar('P')) shl 16) or
                                     (byte(AnsiChar('G')) shl 24)
                                    ); // Motion JPEG DIB biCompression

{ JPEGProcess Definitions  }
const
       JPEG_PROCESS_BASELINE = 0;    { Baseline DCT  }
    { AVI File format extensions  }
       AVIIF_CONTROLFRAME = $00000200;     { This is a control frame  }
             
    { JIF Marker byte pairs in JPEG Interchange Format sequence  }
    { SOF Huff  - Baseline DCT }
       JIFMK_SOF0 = $FFC0;       
    { SOF Huff  - Extended sequential DCT }
       JIFMK_SOF1 = $FFC1;       
    { SOF Huff  - Progressive DCT }
       JIFMK_SOF2 = $FFC2;       
    { SOF Huff  - Spatial (sequential) lossless }
       JIFMK_SOF3 = $FFC3;       
    { SOF Huff  - Differential sequential DCT }
       JIFMK_SOF5 = $FFC5;       
    { SOF Huff  - Differential progressive DCT }
       JIFMK_SOF6 = $FFC6;       
    { SOF Huff  - Differential spatial }
       JIFMK_SOF7 = $FFC7;       
    { SOF Arith - Reserved for JPEG extensions }
       JIFMK_JPG = $FFC8;       
    { SOF Arith - Extended sequential DCT }
       JIFMK_SOF9 = $FFC9;       
    { SOF Arith - Progressive DCT }
       JIFMK_SOF10 = $FFCA;       
    { SOF Arith - Spatial (sequential) lossless }
       JIFMK_SOF11 = $FFCB;       
    { SOF Arith - Differential sequential DCT }
       JIFMK_SOF13 = $FFCD;       
    { SOF Arith - Differential progressive DCT }
       JIFMK_SOF14 = $FFCE;       
    { SOF Arith - Differential spatial }
       JIFMK_SOF15 = $FFCF;       
    { Define Huffman Table(s)  }
       JIFMK_DHT = $FFC4;       
    { Define Arithmetic coding conditioning(s)  }
       JIFMK_DAC = $FFCC;       
    { Restart with modulo 8 count 0  }
       JIFMK_RST0 = $FFD0;       
    { Restart with modulo 8 count 1  }
       JIFMK_RST1 = $FFD1;       
    { Restart with modulo 8 count 2  }
       JIFMK_RST2 = $FFD2;       
    { Restart with modulo 8 count 3  }
       JIFMK_RST3 = $FFD3;
    { Restart with modulo 8 count 4  }
       JIFMK_RST4 = $FFD4;       
    { Restart with modulo 8 count 5  }
       JIFMK_RST5 = $FFD5;       
    { Restart with modulo 8 count 6  }
       JIFMK_RST6 = $FFD6;       
    { Restart with modulo 8 count 7  }
       JIFMK_RST7 = $FFD7;       
    { Start of Image  }
       JIFMK_SOI = $FFD8;       
    { End of Image  }
       JIFMK_EOI = $FFD9;       
    { Start of Scan  }
       JIFMK_SOS = $FFDA;       
    { Define quantization Table(s)  }
       JIFMK_DQT = $FFDB;       
    { Define Number of Lines  }
       JIFMK_DNL = $FFDC;       
    { Define Restart Interval  }
       JIFMK_DRI = $FFDD;       
    { Define Hierarchical progression  }
       JIFMK_DHP = $FFDE;       
    { Expand Reference Component(s)  }
       JIFMK_EXP = $FFDF;       
    { Application Field 0 }
       JIFMK_APP0 = $FFE0;       
    { Application Field 1 }
       JIFMK_APP1 = $FFE1;       
    { Application Field 2 }
       JIFMK_APP2 = $FFE2;       
    { Application Field 3 }
       JIFMK_APP3 = $FFE3;       
    { Application Field 4 }
       JIFMK_APP4 = $FFE4;       
    { Application Field 5 }
       JIFMK_APP5 = $FFE5;       
    { Application Field 6 }
       JIFMK_APP6 = $FFE6;       
    { Application Field 7 }
       JIFMK_APP7 = $FFE7;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG0 = $FFF0;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG1 = $FFF1;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG2 = $FFF2;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG3 = $FFF3;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG4 = $FFF4;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG5 = $FFF5;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG6 = $FFF6;
    { Reserved for JPEG extensions  }
       JIFMK_JPG7 = $FFF7;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG8 = $FFF8;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG9 = $FFF9;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG10 = $FFFA;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG11 = $FFFB;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG12 = $FFFC;       
    { Reserved for JPEG extensions  }
       JIFMK_JPG13 = $FFFD;       
    { Comment  }
       JIFMK_COM = $FFFE;       
    { for temp private use arith code  }
       JIFMK_TEM = $FF01;       
    { Reserved  }
       JIFMK_RES = $FF02;
    { Zero stuffed byte - entropy data  }
       JIFMK_00 = $FF00;       
    { Fill byte  }
       JIFMK_FF = $FFFF;
       
    { JPEGColorSpaceID Definitions  }
    { Y only component of YCbCr  }
       JPEG_Y = 1;       
    { YCbCr as define by CCIR 601  }
       JPEG_YCbCr = 2;       
    { 3 component RGB  }
       JPEG_RGB = 3;       
    { Structure definitions  }
    { compression-specific fields  }
    { these fields are defined for 'JPEG' and 'MJPG'  }
    { Process specific fields  }

type
     tagJPEGINFOHEADER = record
      // compression-specific fields
      // these fields are defined for 'JPEG' and 'MJPG'
       JPEGSize:DWORD;
       JPEGProcess:DWORD;

      // Process specific fields  
       JPEGColorSpaceID:DWORD;
       JPEGBitsPerSample:DWORD;
       JPEGHSubSampling:DWORD;
       JPEGVSubSampling:DWORD;
     end;
     JPEGINFOHEADER = tagJPEGINFOHEADER;

{$IFDEF MJPGDHTSEG_STORAGE}
// Default DHT Segment
const
     // JPEG DHT Segment for YCrCb omitted from MJPG data
      MJPGDHTSeg:array[0..$01A4-1] of byte =
        ($FF,$C4,$01,$A2,$00,$00,$01,$05,$01,$01,$01,$01,$01,$01,
         $00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,
         $07,$08,$09,$0A,$0B,$01,$00,$03,$01,$01,$01,$01,$01,$01,
         $01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,
         $06,$07,$08,$09,$0A,$0B,$10,$00,$02,$01,$03,$03,$02,$04,
         $03,$05,$05,$04,$04,$00,$00,$01,$7D,$01,$02,$03,$00,$04,
         $11,$05,$12,$21,$31,$41,$06,$13,$51,$61,$07,$22,$71,$14,
         $32,$81,$91,$A1,$08,$23,$42,$B1,$C1,$15,$52,$D1,$F0,$24,
         $33,$62,$72,$82,$09,$0A,$16,$17,$18,$19,$1A,$25,$26,$27,
         $28,$29,$2A,$34,$35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,
         $47,$48,$49,$4A,$53,$54,$55,$56,$57,$58,$59,$5A,$63,$64,
         $65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,$79,$7A,
         $83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,
         $98,$99,$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,
         $B4,$B5,$B6,$B7,$B8,$B9,$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,
         $C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$E1,$E2,$E3,
         $E4,$E5,$E6,$E7,$E8,$E9,$EA,$F1,$F2,$F3,$F4,$F5,$F6,$F7,
         $F8,$F9,$FA,$11,$00,$02,$01,$02,$04,$04,$03,$04,$07,$05,
         $04,$04,$00,$01,$02,$77,$00,$01,$02,$03,$11,$04,$05,$21,
         $31,$06,$12,$41,$51,$07,$61,$71,$13,$22,$32,$81,$08,$14,
         $42,$91,$A1,$B1,$C1,$09,$23,$33,$52,$F0,$15,$62,$72,$D1,
         $0A,$16,$24,$34,$E1,$25,$F1,$17,$18,$19,$1A,$26,$27,$28,
         $29,$2A,$35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,
         $49,$4A,$53,$54,$55,$56,$57,$58,$59,$5A,$63,$64,$65,$66,
         $67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,$79,$7A,$82,$83,
         $84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,
         $99,$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,
         $B5,$B6,$B7,$B8,$B9,$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,
         $CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$E2,$E3,$E4,$E5,
         $E6,$E7,$E8,$E9,$EA,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA
        );

// End DHT default
{$ENDIF} // MJPGDHTSEG_STORAGE

{$ENDIF} // NOJPEGDIB


//////////////////////////////////////////////////////////////////////////
//
// Defined IC types

{$IFNDEF NONEWIC}
const
      ICTYPE_VIDEO    = FOURCC(byte(AnsiChar('v')) or
                               (byte(AnsiChar('i')) shl 8) or
                               (byte(AnsiChar('d')) shl 16) or
                               (byte(AnsiChar('c')) shl 24)
                              );

      ICTYPE_AUDIO    = FOURCC(byte(AnsiChar('a')) or
                               (byte(AnsiChar('u')) shl 8) or
                               (byte(AnsiChar('d')) shl 16) or
                               (byte(AnsiChar('c')) shl 24)
                              );
{$ENDIF} // NONEWIC


//   Misc. FOURCC registration

{* Sierra Semiconductor: RDSP- Confidential RIFF file format
//       for the storage and downloading of DSP
//       code for Audio and communications devices.
*}
const
      FOURCC_RDSP     = FOURCC(byte(AnsiChar('R')) or
                               (byte(AnsiChar('D')) shl 8) or
                               (byte(AnsiChar('S')) shl 16) or
                               (byte(AnsiChar('P')) shl 24)
                              );

{$PACKRECORDS DEFAULT} //   {#include "poppack.h"    /* Revert to default packing */ }

implementation

end.
