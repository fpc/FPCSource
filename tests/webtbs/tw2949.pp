{ Source provided for Free Pascal Bug Report 2949 }
{ Submitted by "Marco (Gory bugs department)" on  2004-02-07 }
{ e-mail:  }

{$ifdef fpc}{$mode Delphi}{$endif}

TYPE
// dummy types
  PSSL_CTX=pchar;

var
  IdSslCtxSetVerifyDepth : procedure(ctx: PSSL_CTX; depth: Integer); cdecl = nil;

begin
end.
