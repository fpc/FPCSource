{$MODE OBJFPC}
{$L libgtemacro.o}
unit libmacro; 
interface

procedure gte_ldv0(r0: pdword); stdcall; external;
procedure gte_ldv1(r0: pdword); stdcall; external;
procedure gte_ldv2(r0: pdword); stdcall; external;

procedure gte_rtpt; stdcall; external;
procedure gte_nclip; stdcall; external;
procedure gte_stopz(r0: plongint); stdcall; external;

procedure gte_stsxy0(r0: pdword); stdcall; external;
procedure gte_stsxy3(r0, r1, r2: pdword); stdcall; external;

procedure gte_avsz3; stdcall; external;
procedure gte_avsz4; stdcall; external;

procedure gte_stotz(r0: plongint); stdcall; external;


implementation
begin
end.