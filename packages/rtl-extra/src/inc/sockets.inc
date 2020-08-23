{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                     Text File Writeln/ReadLn Support
******************************************************************************}


Procedure OpenSock(var F:Text);
begin
  if textrec(f).handle=UnusedHandle then
   textrec(f).mode:=fmclosed
  else
   case textrec(f).userdata[1] of
     S_OUT : textrec(f).mode:=fmoutput;
     S_IN : textrec(f).mode:=fminput;
   else
    textrec(f).mode:=fmclosed;
   end;
end;



procedure iosock(var f:text);

var r:sizeint;
    def_error:word;

begin
  with textrec(f) do
    begin
      case mode of
        fmoutput:
          begin
            repeat
{$ifdef use_readwrite}
              r:=fpwrite(handle,bufptr^,bufpos);
{$else}
              r:=fpsend(handle,bufptr,bufpos,0);
{$endif}
            until (r<>-1) or (SocketError <> EsockEINTR);
            bufend:=r;
            def_error:=101; {File write error.}
          end;
        fminput:
          begin
            repeat
{$ifdef use_readwrite}
              r:=fpread(handle,bufptr^,bufsize);
{$else}
              r:=fprecv(handle,bufptr,bufsize,0);
{$endif}
            until (r<>-1) or (SocketError <> EsockEINTR);
            bufend:=r;
            def_error:=100; {File read error.}
          end;
      end;
      if r=-1 then
        case SocketError of
          EsockEBADF:
{          EsysENOTSOCK:}   {Why is this constant not defined? (DM)}
            inoutres:=6;    {Invalid file handle.}
          EsockEFAULT:
            inoutres:=217;
          EsockEINVAL:
            inoutres:=218;
        else
          inoutres:=def_error;
        end;
      bufpos:=0;
    end;
end;



Procedure FlushSock(var F:Text);
begin
  if (textrec(f).mode=fmoutput) and (textrec(f).bufpos<>0) then
   begin
     IOSock(f);
     textrec(f).bufpos:=0;
   end;
end;



Procedure CloseSock(var F:text);
begin
  { Nothing special has to be done here }
end;



Procedure Sock2Text(Sock:Longint;Var SockIn,SockOut:Text);
{
 Set up two Pascal Text file descriptors for reading and writing)
}
begin
{ First the reading part.}
  Assign(SockIn,'.');
  Textrec(SockIn).Handle:=Sock;
  Textrec(Sockin).userdata[1]:=S_IN;
  TextRec(SockIn).OpenFunc:=@OpenSock;
  TextRec(SockIn).InOutFunc:=@IOSock;
  TextRec(SockIn).FlushFunc:=@FlushSock;
  TextRec(SockIn).CloseFunc:=@CloseSock;
  TextRec(SockIn).Mode := fmInput;
  Case DefaultTextLineBreakStyle Of
    tlbsLF: TextRec(sockin).LineEnd := #10;
    tlbsCRLF: TextRec(sockin).LineEnd := #13#10;
    tlbsCR: TextRec(sockin).LineEnd := #13;
   End;
{ Now the writing part. }
  Assign(SockOut,'.');
  Textrec(SockOut).Handle:=Sock;
  Textrec(SockOut).userdata[1]:=S_OUT;
  TextRec(SockOut).OpenFunc:=@OpenSock;
  TextRec(SockOut).InOutFunc:=@IOSock;
  TextRec(SockOut).FlushFunc:=@FlushSock;
  TextRec(SockOut).CloseFunc:=@CloseSock;
  TextRec(SockOut).Mode := fmOutput;

   Case DefaultTextLineBreakStyle Of
    tlbsLF: TextRec(sockout).LineEnd := #10;
    tlbsCRLF: TextRec(sockout).LineEnd := #13#10;
    tlbsCR: TextRec(sockout).LineEnd := #13;
   End;
end;


{******************************************************************************
                                Untyped File
******************************************************************************}

Procedure Sock2File(Sock:Longint;Var SockIn,SockOut:File);
begin
{Input}
  Assign(SockIn,'.');
  FileRec(SockIn).Handle:=Sock;
  FileRec(SockIn).RecSize:=1;
  FileRec(Sockin).userdata[1]:=S_IN;
  FileRec(SockIn).Mode := fmInput;

{Output}
  Assign(SockOut,'.');
  FileRec(SockOut).Handle:=Sock;
  FileRec(SockOut).RecSize:=1;
  FileRec(SockOut).userdata[1]:=S_OUT;
  FileRec(SockOut).Mode := fmOutput;
end;

{******************************************************************************
                               InetSock
******************************************************************************}

Function DoAccept(Sock:longint;Var addr:TInetSockAddr):longint;

Var AddrLen : Longint;

begin
  AddrLEn:=SizeOf(Addr);
  repeat
    DoAccept:=fpaccept(Sock,@Addr,@AddrLen);
  until (DoAccept<>-1) or (SocketError <> EsockEINTR);
end;

Function DoConnect(Sock:longint;const addr: TInetSockAddr): Boolean;

var
  res: longint;
begin
  repeat
    res:=fpconnect(Sock,@Addr,SizeOF(TInetSockAddr));
  until (res<>-1) or (SocketError <> EsockEINTR);
  DoConnect:= res = 0;
end;

{$warnings off}

Function Connect(Sock:longint;const addr: TInetSockAddr;var SockIn,SockOut:text):Boolean;

begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2Text(Sock,SockIn,SockOut);
end;

Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean;

begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2File(Sock,SockIn,SockOut);
end;

Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2Text(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;

Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2File(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;

{$warnings on}

type thostaddr= packed array[1..4] of byte;

function htonl( host : cardinal):cardinal; inline;overload;
begin
{$ifdef FPC_BIG_ENDIAN}
  htonl:=host;
{$else}
  htonl:=SwapEndian(host);
{$endif}
end;

Function NToHl (Net : cardinal) : cardinal; inline;overload;
begin
{$ifdef FPC_BIG_ENDIAN}
  ntohl:=net;
{$else}
  ntohl:=SwapEndian(net);
{$endif}
end;

function htons( host : word):word; inline;

begin
{$ifdef FPC_BIG_ENDIAN}
  htons:=host;
{$else}
  htons:=SwapEndian(host);
{$endif}
end;

Function NToHs (Net : word):word; inline;

begin
{$ifdef FPC_BIG_ENDIAN}
  ntohs:=net;
{$else}
  ntohs:=SwapEndian(net);
{$endif}
end;

Type array4int = array[1..4] of byte;

function NetAddrToStr (Entry : in_addr) : AnsiString;

Var Dummy : Ansistring;
    i,j   : Longint;

begin
  NetAddrToStr:='';
  j:=entry.s_addr;
  For I:=1 to 4 do
   begin
     Str(array4int(j)[i],Dummy);
     NetAddrToStr:=NetAddrToStr+Dummy;
     If I<4 Then
       NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function HostAddrToStr (Entry : in_addr) : AnsiString;

Var x: in_addr;

begin
  x.s_addr:=htonl(entry.s_addr);
  HostAddrToStr:=NetAddrToStr(x);
end;

function StrToHostAddr(IP : AnsiString) : in_addr ;
var
  ip4: in_addr;
begin
  ip4.s_addr := 0;
  StrToHostAddr.s_addr:=0;              //:=NoAddress;
  if TryStrToHostAddr(IP, ip4) then
    StrToHostAddr := ip4;
end;

function TryStrToHostAddr(IP: AnsiString; out ip4: in_addr): Boolean;

Const
  AllowedChars = ['.','0'..'9'];

Var
    Dummy : AnsiString;
    I,j,k     : Longint;
    Temp : in_addr;

begin
  TryStrToHostAddr := False;
  ip4.s_addr:=0;              //:=NoAddress;
  for I:=1 to Length(IP) do
    if Not (IP[I] in AllowedChars) then
      exit;
  For I:=1 to 4 do
    begin
      If I<4 Then
        begin
          J:=Pos('.',IP);
          If J=0 then
            exit;
          Dummy:=Copy(IP,1,J-1);
          Delete (IP,1,J);
        end
       else
         Dummy:=IP;
      if Length(Dummy)>3 then
        exit;
      Val (Dummy,k,J);
      If J<>0 then
        exit;
      array4int(temp.s_addr)[i]:=byte(k);
      { check if after wrapping to a byte, our number is
        still the same. if not, it can't be part of an IP. }
      If array4int(temp.s_addr)[i]<>k then
        exit;
   end;
   ip4.s_addr:=ntohl(Temp.s_addr);
   TryStrToHostAddr := True;
end;

function StrToNetAddr(IP : AnsiString) : in_addr;

begin
  StrToNetAddr.s_addr:=htonl(StrToHostAddr(IP).s_addr);
end;

Function HostToNet (Host : in_addr):in_addr;

begin
  HostToNet.s_addr:=htonl(host.s_addr);
end;

Function NetToHost (Net : in_addr) : in_addr;

begin
  NetToHost.s_addr:=ntohl(net.s_addr);
end;

Function HostToNet (Host : Longint) : Longint;

begin
  HostToNet:=htonl(host);
end;

Function NetToHost (Net : Longint) : Longint;

begin
  NetToHost:=ntohl(net);
end;

Function ShortHostToNet (Host : Word) : Word;

begin
  ShortHostToNet:=htons(host);
end;

Function ShortNetToHost (Net : Word) : Word;

begin
  ShortNEtToHost:=ntohs(net);
end;

const digittab : shortstring = ('0123456789ABCDEF');

function lclinttohex (i:longint;digits:longint): ansistring;

begin
  SetLength(lclinttohex,4);
  lclinttohex[4]:=digittab[1+(i and 15)];
  lclinttohex[3]:=digittab[1+((i shr 4) and 15)];
  lclinttohex[2]:=digittab[1+((i shr 8) and 15)];
  lclinttohex[1]:=digittab[1+((i shr 12) and 15)];;
end;

function HostAddrToStr6 (Entry : TIn6_Addr) :ansiString;
var
  i: byte;
  zr1,zr2: set of byte;
  zc1,zc2: byte;
  have_skipped: boolean;
begin
  zr1 := [];
  zr2 := [];
  zc1 := 0;
  zc2 := 0;
  for i := 0 to 7 do begin
    if Entry.u6_addr16[i] = 0 then begin
      include(zr2, i);
      inc(zc2);
    end else begin
      if zc1 < zc2 then begin
        zc1 := zc2;
        zr1 := zr2;
        zc2 := 0; zr2 := [];
      end;
    end;
  end;
  if zc1 < zc2 then begin
    zc1 := zc2;
    zr1 := zr2;
  end;
  SetLength(HostAddrToStr6, 8*5-1);
  SetLength(HostAddrToStr6, 0);
  have_skipped := false;
  for i := 0 to 7 do begin
    if not (i in zr1) then begin
      if have_skipped then begin
        if HostAddrToStr6 = ''
          then HostAddrToStr6 := '::'
          else HostAddrToStr6 := HostAddrToStr6 + ':';
        have_skipped := false;
      end;
      // FIXME: is that shortnettohost really proper there? I wouldn't be too sure...
      HostAddrToStr6 := HostAddrToStr6 +lclIntToHex(ShortNetToHost(Entry.u6_addr16[i]), 1) + ':';
    end else begin
      have_skipped := true;
    end;
  end;
  if have_skipped then
    if HostAddrToStr6 = ''
      then HostAddrToStr6 := '::'
      else HostAddrToStr6 := HostAddrToStr6 + ':';

  if HostAddrToStr6 = '' then HostAddrToStr6 := '::';
  if not (7 in zr1) then
    SetLength(HostAddrToStr6, Length(HostAddrToStr6)-1);
end;

function StrToHostAddr6(IP: AnsiString): in6_addr;
var
  i6: in6_addr;
begin
  i6.u6_addr32[0] := 0;
  i6.u6_addr32[1] := 0;
  i6.u6_addr32[2] := 0;
  i6.u6_addr32[3] := 0;
  StrToHostAddr6.u6_addr32[0] := 0;
  StrToHostAddr6.u6_addr32[1] := 0;
  StrToHostAddr6.u6_addr32[2] := 0;
  StrToHostAddr6.u6_addr32[3] := 0;

  if TryStrToHostAddr6(IP, i6) then
    StrToHostAddr6 := i6;
end;

function TryStrToHostAddr6(IP: AnsiString; out ip6: in6_addr): Boolean;
type
  TCharClass = (cHexDigit, cColon, cDot, cUnknown, cEndStr);
  TParserMode = (pmIPv6, pmIPv4);

  TCharRec = record
    ch: AnsiChar;
    ctype: TCharClass;
  end;

  TToken = record
    s: ShortString;
    tt: TCharClass;
  end;

  function get_char_class(ch: AnsiChar): TCharClass;
  begin
    get_char_class := cUnknown;
    case ch of
      'A' .. 'F', 'a' .. 'f', '0' .. '9': get_char_class := cHexDigit;
      ':': get_char_class := cColon;
      '.': get_char_class := cDot;
    else
      get_char_class := cUnknown;
    end;
  end;

  function is_eos(idx: Cardinal): Boolean;
  begin
    is_eos := (idx < 1) or (idx > Length(IP));
  end;

  function next_char(idx: Cardinal): TCharRec;
  begin
    next_char.ctype := cUnknown;
    if is_eos(idx) then
    begin
      next_char.ch := '-';
      next_char.ctype := cEndStr;
    end
    else
    begin
      next_char.ch := IP[idx];
      next_char.ctype := get_char_class(next_char.ch);
    end;
  end;

  function next_token(var idx: Cardinal): TToken;
  var
    rch: TCharRec;
    prv: TCharClass;
  begin
    next_token.s := '';
    next_token.tt := cUnknown;
    rch := next_char(idx);
    prv := rch.ctype;
    next_token.tt := rch.ctype;
    while (rch.ctype <> cEndStr) and (rch.ctype = prv) do
    begin
      next_token.s := next_token.s + rch.ch;
      Inc(idx);
      rch := next_char(idx);
    end;
  end;

  function convert_hextet(const s: ShortString; var res: Word): Boolean;
  var
    tmpval,valcode: Word;
  begin
    convert_hextet := False;
    if Length(s) > 4 then exit;
    Val('0x'+s,tmpval,valcode);
    if valcode <> 0 then exit;
    res := htons(tmpval);
    convert_hextet := True;
  end;

  function convert_octet(const s: ShortString; var res: Byte): Boolean;
  var
    tmpval: Word;
    valcode: Word;
  begin
    convert_octet := False;
    if Length(s) > 3 then exit;
    Val(s,tmpval,valcode);
    if valcode <> 0 then exit;
    if tmpval > 255 then exit;
    res := tmpval;
    convert_octet := True;
  end;

var
  tkn, ptkn: TToken;
  idx: Cardinal;
  hextet_arr: array[0 .. 7] of Word = (0,0,0,0,0,0,0,0);
  hextet_idx, octet_idx,coll_start_idx: byte;
  octet_arr: array[0 .. 3] of byte = (0,0,0,0);
  coll_zero_seen: Boolean = False;
  parser_mode: TParserMode = pmIPv6;
  tmpval: Word = 0;
  tmpByte: Byte = 0;

begin
  TryStrToHostAddr6 := False;
  ip6.u6_addr32[0] := 0;
  ip6.u6_addr32[1] := 0;
  ip6.u6_addr32[2] := 0;
  ip6.u6_addr32[3] := 0;

  if (Length(IP) > 45) or (Length(IP) < 2) then exit;

  hextet_idx := 0;
  coll_start_idx := 0;
  octet_idx := 0;
  idx := 1;

  ptkn.s := '';
  ptkn.tt := cUnknown;

  tkn := next_token(idx);
  while (tkn.tt <> cEndStr) do
  begin
    case tkn.tt of
      cHexDigit:
        begin
          case parser_mode of
            pmIPv6:
              begin
                if (hextet_idx <= 7) and (convert_hextet(tkn.s, tmpval)) then
                begin
                  hextet_arr[hextet_idx] := tmpval;
                  Inc(hextet_idx);
                end
                else
                  exit; // too many hextets, or invalid hextet.
              end;
            pmIPv4:
              begin
                if (octet_idx <= 3) and (convert_octet(tkn.s, tmpByte)) then
                begin
                  octet_arr[octet_idx] := tmpByte;
                  Inc(octet_idx);
                end
                else
                  exit; // too many octets, or invalid octet.
              end;
          end;
        end;

      cColon:
        begin
          if (parser_mode = pmIPv4) or (Length(tkn.s) > 2) then exit;
          if Length(tkn.s) = 2 then
          begin
            // if we saw a collapsed sequence before, or if we've already
            // seen 8 hextets.
            if coll_zero_seen or (hextet_idx > 7) then exit;
            coll_zero_seen := True;
            coll_start_idx := hextet_idx;
            Inc(hextet_idx);
          end
          else if Length(tkn.s) = 1 then
          begin
            // is this single colon the first token? if so, address is invalid.
            // if the prev token is cUnknown, then this must be the first token.
            if ptkn.tt = cUnknown then exit;
          end;
        end;

      cDot:
        begin
          if Length(tkn.s) > 1 then exit;

          // By the time we see the first dot, the first octet of the IPv4
          // address has already been processed as an IPv6 hextet. we have
          // to backtrack to remove that value from hextet_arr
          // and reprocess the value as ipv4.
          if parser_mode = pmIPv6 then
          begin
            if ptkn.tt = cHexDigit then
            begin
              Dec(hextet_idx);
              hextet_arr[hextet_idx] := 0;

              if (octet_idx <= 3) and (convert_octet(ptkn.s, tmpByte)) then
              begin
                octet_arr[octet_idx] := tmpByte;
                Inc(octet_idx);
              end
              else
                exit; // too many octets, or invalid octet.
            end
            else // dot preceded by something other than digit
              exit;
            parser_mode := pmIPv4;
          end;
        end;

      cUnknown:
        exit;
    end;
    ptkn := tkn;
    tkn := next_token(idx);
  end;

  // if we finished on a . or :, the address is invalid.
  if (ptkn.tt = cDot) or ((ptkn.tt = cColon) and (Length(ptkn.s) = 1)) then
    exit;

  // if there's an ipv4 addr, add its octets onto the end
  // of the ipv6 hextet array. we have to convert the bytes to
  // words.
  if (parser_mode = pmIPv4) then
  begin
    if (octet_idx = 4) and (hextet_idx <= 6) then
    begin
      tmpval := (octet_arr[0] shl 8) + (octet_arr[1]);
      hextet_arr[hextet_idx] := htons(tmpval);
      Inc(hextet_idx);
      tmpval := (octet_arr[2] shl 8) + (octet_arr[3]);
      hextet_arr[hextet_idx] := htons(tmpval);
      Inc(hextet_idx);
    end
    else
      exit; // invalid no of ipv4 octets, or not enough room for them.
  end;

  // finish line is in sight. if we have a collapsed-zeroes sequence
  // then we must fill that in now.
  if coll_zero_seen then
  begin
    for tmpByte := 0 to coll_start_idx do
      ip6.u6_addr16[tmpByte] := hextet_arr[tmpByte];

    // hextet_idx-1 points to the final byte we processed, in the hextet_arr
    // array. starting there, reading back to coll_start_idx, we copy these
    // words to the end of the Result array, with word hextet_idx-1 going at
    // the end of the Result array, hextet_idx-2 going to the end - 1 of Result,
    // and so on.
    // NOTE: optimization note -- a memmove/memcpy equivalent could help here.
    tmpByte := hextet_idx-1;
    idx := 7;
    while tmpByte > coll_start_idx do
    begin
      ip6.u6_addr16[idx] := hextet_arr[tmpByte];
      Dec(tmpByte);
      Dec(idx);
    end;
  end
  else
  begin
    // no collapsed zeroes. we must have exactly 8 words then, or we're short.
    // NOTE: optimization note: memmove/memcpy equivalent could help here.
    if hextet_idx < 8 then exit;
    for tmpByte := 0 to 7 do
      ip6.u6_addr16[tmpByte] := hextet_arr[tmpByte];
  end;
  TryStrToHostAddr6 := True;
end;

function NetAddrToStr6 (Entry : TIn6_Addr) : ansiString;
begin
  netaddrtostr6 := HostAddrToStr6((Entry));
end;

function StrToNetAddr6(IP : ansiString) : TIn6_Addr;
begin
  StrToNetAddr6 := StrToHostAddr6(IP);
end;


