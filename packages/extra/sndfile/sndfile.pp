unit sndfile;

interface

{
  Automatically converted by H2Pas 0.99.15 from sndfile.h
  The following command line parameters were used:
    -D
    -p
    -e
    sndfile.h
}

  const
    External_library='sndfile'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

    size_t = Longint;

{$PACKRECORDS C}

     Const
       SF_FORMAT_WAV = $10000;
       SF_FORMAT_AIFF = $20000;
       SF_FORMAT_AU = $30000;
       SF_FORMAT_AULE = $40000;
       SF_FORMAT_RAW = $50000;
       SF_FORMAT_PAF = $60000;
       SF_FORMAT_SVX = $70000;
       SF_FORMAT_NIST = $80000;
       SF_FORMAT_WMA = $90000;
       SF_FORMAT_SMPLTD = $A0000;
       SF_FORMAT_VOC = $B0000;
       SF_FORMAT_SD2 = $C0000;
       SF_FORMAT_REX2 = $D0000;
       SF_FORMAT_IRCAM = $E0000;
       SF_FORMAT_PCM = $0001;
       SF_FORMAT_FLOAT = $0002;
       SF_FORMAT_ULAW = $0003;
       SF_FORMAT_ALAW = $0004;
       SF_FORMAT_IMA_ADPCM = $0005;
       SF_FORMAT_MS_ADPCM = $0006;
       SF_FORMAT_PCM_BE = $0007;
       SF_FORMAT_PCM_LE = $0008;
       SF_FORMAT_PCM_S8 = $0009;
       SF_FORMAT_PCM_U8 = $000A;
       SF_FORMAT_SVX_FIB = $000B;
       SF_FORMAT_SVX_EXP = $000C;
       SF_FORMAT_GSM610 = $000D;
       SF_FORMAT_G721_32 = $000E;
       SF_FORMAT_G723_24 = $000F;
       SF_FORMAT_FLOAT_BE = $0010;
       SF_FORMAT_FLOAT_LE = $0011;
       SF_FORMAT_SUBMASK = $FFFF;
       SF_FORMAT_TYPEMASK = $7FFF0000;

     SF_FORMAT_RAW_BE = SF_FORMAT_PCM_BE;
     SF_FORMAT_RAW_LE = SF_FORMAT_PCM_LE;
     SF_FORMAT_RAW_S8 = SF_FORMAT_PCM_S8;
     SF_FORMAT_RAW_U8 = SF_FORMAT_PCM_U8;

  type

     PSNDFILE = Pointer;

     PSF_INFO = ^SF_INFO;
     SF_INFO = record
          samplerate : dword;
          samples : dword;
          channels : dword;
          pcmbitwidth : dword;
          format : dword;
          sections : dword;
          seekable : dword;
       end;
  function sf_open_read(path:Pchar; sfinfo:PSF_INFO):PSNDFILE;cdecl;external External_library name 'sf_open_read';
  function sf_open_write(path:Pchar; sfinfo:PSF_INFO):PSNDFILE;cdecl;external External_library name 'sf_open_write';

  function sf_perror(sndfile:PSNDFILE):longint;cdecl;external External_library name 'sf_perror';

  function sf_error_str(sndfile:PSNDFILE; str:Pchar; len:size_t):longint;cdecl;external External_library name 'sf_error_str';

  function sf_error_number(errnum:longint; str:Pchar; maxlen:size_t):longint;cdecl;external External_library name 'sf_error_number';

  function sf_get_header_info(sndfile:PSNDFILE; buffer:Pchar; bufferlen:size_t; offset:size_t):size_t;cdecl;external External_library name 'sf_get_header_info';

  function sf_get_lib_version(buffer:Pchar; bufferlen:size_t):size_t;cdecl;external External_library name 'sf_get_lib_version';

  function sf_command(sndfile:PSNDFILE; cmd:Pchar; data:pointer; datasize:longint):longint;cdecl;external External_library name 'sf_command';

  function sf_format_check(info:PSF_INFO):longint;cdecl;external External_library name 'sf_format_check';

  function sf_signal_max(sndfile:PSNDFILE):double;cdecl;external External_library name 'sf_signal_max';

  function sf_seek(sndfile:PSNDFILE; frames:longint; whence:longint):longint;cdecl;external External_library name 'sf_seek';

  function sf_read_raw(sndfile:PSNDFILE; ptr:pointer; bytes:size_t):size_t;cdecl;external External_library name 'sf_read_raw';

  function sf_write_raw(sndfile:PSNDFILE; ptr:pointer; bytes:size_t):size_t;cdecl;external External_library name 'sf_write_raw';

  function sf_readf_short(sndfile:PSNDFILE; ptr:Psmallint; frames:size_t):size_t;cdecl;external External_library name 'sf_readf_short';

  function sf_writef_short(sndfile:PSNDFILE; ptr:Psmallint; frames:size_t):size_t;cdecl;external External_library name 'sf_writef_short';

  function sf_readf_int(sndfile:PSNDFILE; ptr:Plongint; frames:size_t):size_t;cdecl;external External_library name 'sf_readf_int';

  function sf_writef_int(sndfile:PSNDFILE; ptr:Plongint; frames:size_t):size_t;cdecl;external External_library name 'sf_writef_int';

  function sf_readf_float(sndfile:PSNDFILE; ptr:Pdouble; frames:size_t):size_t;cdecl;external External_library name 'sf_readf_float';

  function sf_writef_float(sndfile:PSNDFILE; ptr:Pdouble; frames:size_t):size_t;cdecl;external External_library name 'sf_writef_float';

  function sf_readf_double(sndfile:PSNDFILE; ptr:Pdouble; frames:size_t; normalize:longint):size_t;cdecl;external External_library name 'sf_readf_double';

  function sf_writef_double(sndfile:PSNDFILE; ptr:Pdouble; frames:size_t; normalize:longint):size_t;cdecl;external External_library name 'sf_writef_double';

  function sf_read_short(sndfile:PSNDFILE; ptr:Psmallint; items:size_t):size_t;cdecl;external External_library name 'sf_read_short';

  function sf_write_short(sndfile:PSNDFILE; ptr:Psmallint; items:size_t):size_t;cdecl;external External_library name 'sf_write_short';

  function sf_read_int(sndfile:PSNDFILE; ptr:Plongint; items:size_t):size_t;cdecl;external External_library name 'sf_read_int';

  function sf_write_int(sndfile:PSNDFILE; ptr:Plongint; items:size_t):size_t;cdecl;external External_library name 'sf_write_int';

  function sf_read_float(sndfile:PSNDFILE; ptr:Pdouble; items:size_t):size_t;cdecl;external External_library name 'sf_read_float';

  function sf_write_float(sndfile:PSNDFILE; ptr:Pdouble; items:size_t):size_t;cdecl;external External_library name 'sf_write_float';

  function sf_read_double(sndfile:PSNDFILE; ptr:Pdouble; items:size_t; normalize:longint):size_t;cdecl;external External_library name 'sf_read_double';

  function sf_write_double(sndfile:PSNDFILE; ptr:Pdouble; items:size_t; normalize:longint):size_t;cdecl;external External_library name 'sf_write_double';

  function sf_close(sndfile:PSNDFILE):longint;cdecl;external External_library name 'sf_close';

implementation


end.
