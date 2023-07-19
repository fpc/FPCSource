{$ifdef NDS_INTERFACE}

procedure nocashWrite(const aMessage: PAnsiChar; len: cint); cdecl; external;
procedure nocashMessage(const aMessage: PAnsiChar); cdecl; external;

{$endif NDS_INTERFACE}
