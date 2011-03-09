{$IFDEF OGC_INTERFACE}
var
  __wkspace_heap : heap_cntrl; external;


procedure __lwp_wkspace_init(size: cuint32); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_wkspace.inl>
//#endif
{$ENDIF}

