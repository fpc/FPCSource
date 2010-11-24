var SMPEGBase : pLibrary;

const
    SMPEGNAME : PChar = 'smpeg.library';

function SMPEG_new(const _file: PChar; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; syscall r12base SMPEGBase 28;
function SMPEG_new_descr(_file: Integer; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; syscall r12base SMPEGBase 34;
function SMPEG_new_data(data: Pointer; size: Integer; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; syscall r12base SMPEGBase 40;
procedure SMPEG_getinfo(mpeg: PSMPEG; info: PSMPEG_Info); syscall r12base SMPEGBase 52;
procedure SMPEG_enableaudio(mpeg: PSMPEG; enable: Integer); syscall r12base SMPEGBase 58;
procedure SMPEG_enablevideo(mpeg: PSMPEG; enable: Integer); syscall r12base SMPEGBase 64;
procedure SMPEG_delete(mpeg: PSMPEG); syscall r12base SMPEGBase 70;
function SMPEG_status(mpeg: PSMPEG): TSMPEGstatus; syscall r12base SMPEGBase 76;
procedure SMPEG_setvolume(mpeg: PSMPEG; volume: Integer); syscall r12base SMPEGBase 82;
procedure SMPEG_setdisplay(mpeg: PSMPEG; dst: PSDL_Surface; surfLock: PSDL_mutex; callback: TSMPEG_DisplayCallback); syscall r12base SMPEGBase 88;
procedure SMPEG_loop(mpeg: PSMPEG; _repeat: Integer); syscall r12base SMPEGBase 94;
procedure SMPEG_scaleXY(mpeg: PSMPEG; width, height: Integer); syscall r12base SMPEGBase 100;
procedure SMPEG_scale(mpeg: PSMPEG; scale: Integer); syscall r12base SMPEGBase 106;
procedure SMPEG_move(mpeg: PSMPEG; x, y: Integer); syscall r12base SMPEGBase 112;
procedure SMPEG_setdisplayregion(mpeg: PSMPEG; x, y, w, h: Integer); syscall r12base SMPEGBase 118;
procedure SMPEG_play(mpeg: PSMPEG); syscall r12base SMPEGBase 124;
procedure SMPEG_pause(mpeg: PSMPEG); syscall r12base SMPEGBase 130;
procedure SMPEG_stop(mpeg: PSMPEG); syscall r12base SMPEGBase 136;
procedure SMPEG_rewind(mpeg: PSMPEG); syscall r12base SMPEGBase 142;
procedure SMPEG_seek(mpeg: PSMPEG; bytes: Integer); syscall r12base SMPEGBase 148;
procedure SMPEG_skip(mpeg: PSMPEG; seconds: single); syscall r12base SMPEGBase 152;
procedure SMPEG_renderFrame(mpeg: PSMPEG; framenum: Integer); syscall r12base SMPEGBase 158;
procedure SMPEG_renderFinal(mpeg: PSMPEG; dst: PSDL_Surface; x, y: Integer); syscall r12base SMPEGBase 164;
function SMPEG_filter(mpeg: PSMPEG; filter: PSMPEG_Filter): PSMPEG_Filter; syscall r12base SMPEGBase 172;
function SMPEG_error(mpeg: PSMPEG): PChar; syscall r12base SMPEGBase 178;
function SMPEG_playAudio(mpeg: PSMPEG; stream: PUInt8; len: Integer): Integer; syscall r12base SMPEGBase 184;
procedure SMPEG_playAudioSDL(mpeg: Pointer; stream: PUInt8; len: Integer); syscall r12base SMPEGBase 190;
function SMPEG_wantedSpec(mpeg: PSMPEG; wanted: PSDL_AudioSpec): Integer; syscall r12base SMPEGBase 196;
procedure SMPEG_actualSpec(mpeg: PSMPEG; spec: PSDL_AudioSpec); syscall r12base SMPEGBase 202;
