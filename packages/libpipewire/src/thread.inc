
  const
    libpipewire='libpipewire-0.3'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

  Type
    Pspa_thread_utils  = ^Tspa_thread_utils;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  procedure pw_thread_utils_set(impl:Pspa_thread_utils);cdecl;external libpipewire name 'pw_thread_utils_set';

  function pw_thread_utils_get:Pspa_thread_utils;cdecl;external libpipewire name 'pw_thread_utils_get';


