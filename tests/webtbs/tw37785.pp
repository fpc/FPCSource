{ %cpu=x86_64 }
{$asmmode intel}

procedure tymm; assembler; nostackframe;
  asm
    vcvttpd2dq xmm0,ymmword ptr [rsp]
    vcvtpd2dq xmm0,ymmword ptr [rdx]
    vcvtpd2ps xmm0,ymmword ptr [rdx]
    vcvtpd2dq xmm0,ymmword ptr [rcx+r10];
    vbroadcastsd ymm1,qword ptr [rip]
    vmovsd xmm0,qword ptr[rip]
    vorpd ymm1,ymm2,ymm3
    vmovsd  xmm0,qword ptr [rcx]
  end;

const
  tymm_expected: array[0..43] of byte = (
    $c5, $fd, $e6, $04, $24,
    $c5, $ff, $e6, $02,
    $c5, $fd, $5a, $02,
    $c4, $a1, $7f, $e6, $04, $11,
    $c4, $e2, $7d, $19, $0d, $00, $00, $00, $00,
    $c5, $fb, $10, $05, $00, $00, $00, $00,
    $c5, $ed, $56, $cb,
    $c5, $fb, $10, $01
  );

procedure check(const id: string; const expected: array of byte; p: pointer);
var
  i : longint;
begin
  for i:=0 to high(expected) do
    if expected[i]<>pbyte(p)[i] then
      begin
        writeln(id, ' mismatch at offset $',hexstr(i,4), ', expected=$',hexstr(expected[i],2),' actual=$',hexstr(pbyte(p)[i],2));
        halt(1);
      end;
end;

begin
  check('tymm', tymm_expected, @tymm);
  writeln('ok');
end.
