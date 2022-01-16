{ %cpu=xtensa }
{ %norun }
unit mac16test;

interface

procedure testMAC16;

implementation

procedure testMAC16; assembler;
asm
  mula.aa.ll a3, a4
  mula.ad.ll a3, m2                // my in [m2, m3]
  mula.da.ll m1, a3                // mx in [m0, m1]
  mula.dd.ll m1, m3                // mx in [m0, m1], my in [m2, m3]
  mula.da.ll.lddec m1, a5, m3, a6  // mw in [m0..m3], mx in [m0, m1], my in [m2, m3]
  mula.dd.ll.ldinc m3, a5, m0, m2  // mw in [m0..m3], mx in [m0, m1], my in [m2, m3]
  muls.aa.hh a4, a5
  muls.ad.hl a4, m2                // my in [m2, m3]
  muls.da.lh m0, a4                // mx in [m0, m1]
  muls.dd.hl m0, m2                // mx in [m0, m1], my in [m2, m3]
end;

end.

