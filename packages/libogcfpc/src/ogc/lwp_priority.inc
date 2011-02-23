
{$IFDEF OGC_INTERFACE}
const
  LWP_PRIO_MIN = 0;
  LWP_PRIO_MAX = 255;


type
  _priocntrl = record
    minor : pcuint32;
    ready_minor : cuint32;
    ready_major : cuint32;
    block_minor : cuint32;
    block_major : cuint32;
  end;
  prio_cntrl = _priocntrl;
  pprio_cntrl = ^_priocntrl;


var
  _prio_major_bitmap : cuint32; external;
  _prio_bitmap: array [0..0] of cuint32; external;

procedure __lwp_priority_init; cdecl; external;


//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_priority.inl}
//{$endif}
{$ENDIF}

