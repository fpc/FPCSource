{$ifdef NDS_INTERFACE}
procedure IC_InvalidateAll(); cdecl; external;
procedure IC_InvalidateRange(const base: pointer; size: cuint32); cdecl; external;
procedure DC_FlushAll(); cdecl; external;
procedure DC_FlushRange(const base: pointer; size: cuint32); cdecl; external;
procedure DC_InvalidateAll(); cdecl; external;
procedure DC_InvalidateRange(const base: pointer; size: cuint32); cdecl; external;
{$endif NDS_INTERFACE}
