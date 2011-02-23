
{$IFDEF OGC_INTERFACE}
type
  _bd_addr = packed record
    addr: array [0..5] of cuint8;
  end;
  p_bd_addr = ^_bd_addr;

const
  BD_ADDR_LEN = 6;
  BD_ADDR_ANY: _bd_addr = (addr:(0, 0, 0, 0, 0, 0););
  BD_ADDR_LOCAL: _bd_addr = (addr:(0, 0, 0, $ff, $ff, $ff););

procedure BD_ADDR(var bdaddr: _bd_addr; a, b, c, d, e, f: cint); inline;
function bd_addr_cmp(addr1, addr2: _bd_addr): boolean; inline;
//TODO: USE memcpy????
procedure bd_addr_set(addr1, addr2: _bd_addr); inline;
{$ENDIF}



{$IFDEF OGC_IMPLEMENTATION}
procedure BD_ADDR(var bdaddr: _bd_addr; a, b, c, d, e, f: cint); inline;
begin
  bdaddr.addr[0] := a;
	bdaddr.addr[1] := b;
	bdaddr.addr[2] := c;
	bdaddr.addr[3] := d;
	bdaddr.addr[4] := e;
	bdaddr.addr[5] := f;
end;

function bd_addr_cmp(addr1, addr2: _bd_addr): boolean; inline;
begin
  bd_addr_cmp := (addr1.addr[0] = addr2.addr[0]) and
				   (addr1.addr[1] = addr2.addr[1]) and
           (addr1.addr[2] = addr2.addr[2]) and
           (addr1.addr[3] = addr2.addr[3]) and
           (addr1.addr[4] = addr2.addr[4]) and
           (addr1.addr[5] = addr2.addr[5]);
end;

//TODO: USE memcpy????
procedure bd_addr_set(addr1, addr2: _bd_addr); inline;
begin
  addr1.addr[0] := addr2.addr[0];
  addr1.addr[1] := addr2.addr[1];
  addr1.addr[2] := addr2.addr[2];
  addr1.addr[3] := addr2.addr[3];
  addr1.addr[4] := addr2.addr[4];
  addr1.addr[5] := addr2.addr[5];
end;
{$ENDIF}


