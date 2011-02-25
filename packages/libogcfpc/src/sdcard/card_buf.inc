{$IFDEF OGC_INTERFACE}
procedure sdgecko_initBufferPool; cdecl; external;
function sdgecko_allocBuffer: pcuint8; cdecl; external;
procedure sdgecko_freeBuffer(buf: pcuint8); cdecl; external;
{$ENDIF}



