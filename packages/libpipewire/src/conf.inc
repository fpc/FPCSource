
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
    Pchar  = ^Tchar;
    Ppw_properties  = ^Tpw_properties;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  function pw_conf_load_conf(prefix:Pchar; name:Pchar; conf:Ppw_properties):longint;cdecl;external libpipewire name 'pw_conf_load_conf';

  function pw_conf_load_state(prefix:Pchar; name:Pchar; conf:Ppw_properties):longint;cdecl;external libpipewire name 'pw_conf_load_state';

  function pw_conf_save_state(prefix:Pchar; name:Pchar; conf:Ppw_properties):longint;cdecl;external libpipewire name 'pw_conf_save_state';


