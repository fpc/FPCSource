
function CMSG_FIRSTHDR(mhdr: Pmsghdr): Pcmsghdr;
begin
  if mhdr^.msg_controllen >= SizeOf(cmsghdr) then
    Result:=mhdr^.msg_control
  else
    Result:=nil;
end;

function CMSG_NXTHDR(mhdr: Pmsghdr; cmsg: Pcmsghdr): Pcmsghdr;

var
  lRemaining, lNeeded: PtrUInt;

begin
  Result:=nil;
  if cmsg^.cmsg_len<SizeOf(cmsghdr) then
    Exit;
  lRemaining:=PtrUInt(mhdr^.msg_control)+mhdr^.msg_controllen-PtrUInt(cmsg);
  lNeeded:=SizeOf(cmsghdr)+CMSG_ALIGN(cmsg^.cmsg_len)-cmsg^.cmsg_len;
  if (lRemaining<lNeeded) or (lRemaining-lNeeded<cmsg^.cmsg_len) then
    Exit;
  Result:=Pcmsghdr(PByte(cmsg)+CMSG_ALIGN(cmsg^.cmsg_len));
end;

function CMSG_ALIGN(len: size_t): size_t;
begin
  Result:=(len+SizeOf(size_t)-1) and (not(SizeOf(size_t)-1));
end;

function CMSG_SPACE(len: size_t): size_t;
begin
  Result:=CMSG_ALIGN(len)+CMSG_ALIGN(SizeOf(cmsghdr));
end;

function CMSG_LEN(len: size_t): size_t;
begin
  Result:=CMSG_ALIGN(SizeOf(cmsghdr))+len;
end;

function CMSG_DATA(cmsg: Pcmsghdr): PByte;
begin
  Result:=PByte(Ptruint(cmsg) + SizeOf(cmsghdr));
end;

