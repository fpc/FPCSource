{ Source provided for Free Pascal Bug Report 2944 }
{ Submitted by "marco (gory bugs department)" on  2004-02-06 }
{ e-mail:  }

{$ifdef fpc}{$mode Delphi}{$endif}
type
  WS2StubEntry = record
    StubProc : Pointer;
    ProcVar : PPointer;
    Name : PChar;
  end;
  LPFN_WSACLEANUP = function : Integer; stdcall;

var  WSACleanup : LPFN_WSACLEANUP;

procedure WS2Stub_WSACleanup;
begin
end;


CONST

  WS2StubEntryCount = 1;
  WS2StubTable : Array [0..WS2StubEntryCount-1] of WS2StubEntry = (
    (StubProc: @WS2Stub_WSACleanup; ProcVar: @@WSACleanup; Name: 'WSACleanup'));

begin
end.
