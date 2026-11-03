{ %target=linux }
{ unixsockets: CMSG_* helpers and IPv6 packet info received with recvmsg }
program tw29111;

{$mode objfpc}

uses
  ctypes, baseunix, sockets, unixsockets;

type
  in6_pktinfo = record
    ipi6_addr: in6_addr;
    ipi6_ifindex: cuint;
  end;
  Pin6_pktinfo = ^in6_pktinfo;

var
  gErrors: Integer = 0;

// Report aMsg and count it as an error when aOK is false.
procedure Check(aOK: Boolean; const aMsg: string);

begin
  if not aOK then
  begin
    Writeln('FAIL: ', aMsg);
    Inc(gErrors);
  end;
end;


// Check CMSG_FIRSTHDR, CMSG_NXTHDR and CMSG_DATA on a hand-built control buffer.
procedure TestControlBuffer;

var
  lBuf: array[0..255] of Byte;
  lMsg: msghdr;
  lFirst, lSecond: Pcmsghdr;

begin
  FillChar(lBuf, SizeOf(lBuf), 0);
  FillChar(lMsg, SizeOf(lMsg), 0);
  lMsg.msg_control := @lBuf;
  lMsg.msg_controllen := 2 * CMSG_SPACE(4);
  lFirst := CMSG_FIRSTHDR(@lMsg);
  Check(lFirst = Pcmsghdr(@lBuf), 'CMSG_FIRSTHDR returns the start of msg_control');
  lFirst^.cmsg_len := CMSG_LEN(4);
  Check(CMSG_DATA(lFirst) = PByte(@lBuf) + SizeOf(cmsghdr), 'CMSG_DATA points right after the header');
  lSecond := CMSG_NXTHDR(@lMsg, lFirst);
  Check(lSecond = Pcmsghdr(PByte(@lBuf) + CMSG_SPACE(4)), 'CMSG_NXTHDR returns the second header');
  if lSecond <> nil then
  begin
    lSecond^.cmsg_len := CMSG_LEN(4);
    Check(CMSG_NXTHDR(@lMsg, lSecond) = nil, 'CMSG_NXTHDR returns nil after the last header');
  end;
  lMsg.msg_controllen := CMSG_SPACE(4);
  Check(CMSG_NXTHDR(@lMsg, lFirst) = nil, 'CMSG_NXTHDR returns nil when no room is left for a next header');
  lMsg.msg_controllen := 2 * CMSG_SPACE(4);
  lFirst^.cmsg_len := SizeOf(cmsghdr) - 1;
  Check(CMSG_NXTHDR(@lMsg, lFirst) = nil, 'CMSG_NXTHDR returns nil for a cmsg_len shorter than the header');
  lFirst^.cmsg_len := 2 * CMSG_SPACE(4) + 1;
  Check(CMSG_NXTHDR(@lMsg, lFirst) = nil, 'CMSG_NXTHDR returns nil for a cmsg_len past the buffer end');
  lMsg.msg_controllen := SizeOf(cmsghdr) - 1;
  Check(CMSG_FIRSTHDR(@lMsg) = nil, 'CMSG_FIRSTHDR returns nil for a buffer shorter than a header');
end;


// Send a datagram over ::1 and read the IPV6_PKTINFO and IPV6_HOPLIMIT control messages.
procedure TestPacketInfo;

const
  Payload = 'tw29111';

var
  lRx, lTx: cint;
  lAddr, lFrom: TInetSockAddr6;
  lLen: TSockLen;
  lOne: cint;
  lData: array[0..63] of AnsiChar;
  lControl: array[0..255] of Byte;
  lIov: iovec;
  lMsg: msghdr;
  lCmsg: Pcmsghdr;
  lReceived: ssize_t;
  lCount: Integer;
  lPktInfo, lHopLimit: Boolean;
  lInfo: Pin6_pktinfo;

begin
  lRx := fpSocket(AF_INET6, SOCK_DGRAM, 0);
  if lRx < 0 then
  begin
    Writeln('IPv6 sockets not available, packet info test skipped');
    Exit;
  end;
  FillChar(lAddr, SizeOf(lAddr), 0);
  lAddr.sin6_family := AF_INET6;
  lAddr.sin6_addr.u6_addr8[15] := 1;
  if fpBind(lRx, @lAddr, SizeOf(lAddr)) <> 0 then
  begin
    Writeln('cannot bind to ::1, packet info test skipped');
    CloseSocket(lRx);
    Exit;
  end;
  lLen := SizeOf(lAddr);
  Check(fpGetSockName(lRx, @lAddr, @lLen) = 0, 'getsockname on the receiving socket');
  lOne := 1;
  Check(fpSetSockOpt(lRx, IPPROTO_IPV6, IPV6_RECVPKTINFO, @lOne, SizeOf(lOne)) = 0, 'setsockopt IPV6_RECVPKTINFO');
  Check(fpSetSockOpt(lRx, IPPROTO_IPV6, IPV6_RECVHOPLIMIT, @lOne, SizeOf(lOne)) = 0, 'setsockopt IPV6_RECVHOPLIMIT');
  lTx := fpSocket(AF_INET6, SOCK_DGRAM, 0);
  Check(fpSendTo(lTx, PAnsiChar(Payload), Length(Payload), 0, @lAddr, SizeOf(lAddr)) = Length(Payload), 'sendto ::1');
  FillChar(lData, SizeOf(lData), 0);
  FillChar(lControl, SizeOf(lControl), 0);
  lIov.iov_base := @lData;
  lIov.iov_len := SizeOf(lData);
  FillChar(lMsg, SizeOf(lMsg), 0);
  lMsg.msg_name := @lFrom;
  lMsg.msg_namelen := SizeOf(lFrom);
  lMsg.msg_iov := @lIov;
  lMsg.msg_iovlen := 1;
  lMsg.msg_control := @lControl;
  lMsg.msg_controllen := SizeOf(lControl);
  lReceived := recvmsg(lRx, @lMsg, 0);
  Check(lReceived = Length(Payload), 'recvmsg returns the datagram length');
  Check(StrPas(lData) = Payload, 'recvmsg returns the payload');
  lCount := 0;
  lPktInfo := False;
  lHopLimit := False;
  lCmsg := CMSG_FIRSTHDR(@lMsg);
  while lCmsg <> nil do
  begin
    Inc(lCount);
    if (lCmsg^.cmsg_level = IPPROTO_IPV6) and (lCmsg^.cmsg_type = IPV6_PKTINFO) then
    begin
      lPktInfo := True;
      lInfo := Pin6_pktinfo(CMSG_DATA(lCmsg));
      Check((lInfo^.ipi6_addr.u6_addr8[15] = 1) and (lInfo^.ipi6_addr.u6_addr32[0] = 0), 'IPV6_PKTINFO destination is ::1');
      Check(lInfo^.ipi6_ifindex > 0, 'IPV6_PKTINFO has an interface index');
    end
    else if (lCmsg^.cmsg_level = IPPROTO_IPV6) and (lCmsg^.cmsg_type = IPV6_HOPLIMIT) then
    begin
      lHopLimit := True;
      Check(Pcint(CMSG_DATA(lCmsg))^ > 0, 'IPV6_HOPLIMIT is positive');
    end;
    lCmsg := CMSG_NXTHDR(@lMsg, lCmsg);
  end;
  Check(lPktInfo, 'IPV6_PKTINFO control message received');
  Check(lHopLimit, 'IPV6_HOPLIMIT control message received');
  Check(lCount = 2, 'two control messages walked with CMSG_NXTHDR');
  CloseSocket(lTx);
  CloseSocket(lRx);
end;


begin
  TestControlBuffer;
  TestPacketInfo;
  if gErrors > 0 then
    Halt(1);
  Writeln('ok');
end.
