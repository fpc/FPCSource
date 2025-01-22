
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
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  function pw_gettext(msgid:Pchar):Pchar;cdecl;external libpipewire name 'pw_gettext';

  function pw_ngettext(msgid:Pchar; msgid_plural:Pchar; n:dword):Pchar;cdecl;external libpipewire name 'pw_ngettext';


