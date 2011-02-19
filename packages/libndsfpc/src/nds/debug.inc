{$ifdef NDS_INTERFACE}

procedure nocashWrite(const aMessage: pchar; len: cint); cdecl; external;
procedure nocashMessage(const aMessage: pchar); cdecl; external;

{$endif NDS_INTERFACE}
