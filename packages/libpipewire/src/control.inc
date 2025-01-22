
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
    Ppw_control  = ^Tpw_control;
    Ppw_control_events  = ^Tpw_control_events;
    Ppw_impl_port  = ^Tpw_impl_port;
    Pspa_hook  = ^Tspa_hook;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  type
    Ppw_control_events = ^Tpw_control_events;
    Tpw_control_events = record
        version : Tuint32_t;
        destroy : procedure (data:pointer);cdecl;
        free : procedure (data:pointer);cdecl;
        linked : procedure (data:pointer; other:Ppw_control);cdecl;
        unlinked : procedure (data:pointer; other:Ppw_control);cdecl;
      end;


  function pw_control_get_port(control:Ppw_control):Ppw_impl_port;cdecl;external libpipewire name 'pw_control_get_port';

  procedure pw_control_add_listener(control:Ppw_control; listener:Pspa_hook; events:Ppw_control_events; data:pointer);cdecl;external libpipewire name 'pw_control_add_listener';


