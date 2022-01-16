{%cpu=aarch64}

{ based on the arm64-simd-ldst.s test from LLVM

The LLVM Project is under the Apache License v2.0 with LLVM Exceptions
}

{$mode objfpc}

type
  tinstrdata = record
    bytes: array[0..3] of byte;
    str: ansistring;
  end;


procedure ld1st1_multiple; assembler; nostackframe;
asm
  ld1.8b {v0}, [x1]
  ld1.8b {v0, v1}, [x1]
  ld1.8b {v0, v1, v2}, [x1]
  ld1.8b {v0, v1, v2, v3}, [x1]

  ld1.8b {v3}, [x1]
  ld1.8b {v3, v4}, [x2]
  ld1.8b {v4, v5, v6}, [x3]
  ld1.8b {v7, v8, v9, v10}, [x4]

  ld1.16b {v0}, [x1]
  ld1.16b {v0, v1}, [x1]
  ld1.16b {v0, v1, v2}, [x1]
  ld1.16b {v0, v1, v2, v3}, [x1]

  ld1.4h {v0}, [x1]
  ld1.4h {v0, v1}, [x1]
  ld1.4h {v0, v1, v2}, [x1]
  ld1.4h {v0, v1, v2, v3}, [x1]

  ld1.8h {v0}, [x1]
  ld1.8h {v0, v1}, [x1]
  ld1.8h {v0, v1, v2}, [x1]
  ld1.8h {v0, v1, v2, v3}, [x1]

  ld1.2s {v0}, [x1]
  ld1.2s {v0, v1}, [x1]
  ld1.2s {v0, v1, v2}, [x1]
  ld1.2s {v0, v1, v2, v3}, [x1]

  ld1.4s {v0}, [x1]
  ld1.4s {v0, v1}, [x1]
  ld1.4s {v0, v1, v2}, [x1]
  ld1.4s {v0, v1, v2, v3}, [x1]

  ld1.1d {v0}, [x1]
  ld1.1d {v0, v1}, [x1]
  ld1.1d {v0, v1, v2}, [x1]
  ld1.1d {v0, v1, v2, v3}, [x1]

  ld1.2d {v0}, [x1]
  ld1.2d {v0, v1}, [x1]
  ld1.2d {v0, v1, v2}, [x1]
  ld1.2d {v0, v1, v2, v3}, [x1]

  st1.8b {v0}, [x1]
  st1.8b {v0, v1}, [x1]
  st1.8b {v0, v1, v2}, [x1]
  st1.8b {v0, v1, v2, v3}, [x1]

  st1.16b {v0}, [x1]
  st1.16b {v0, v1}, [x1]
  st1.16b {v0, v1, v2}, [x1]
  st1.16b {v0, v1, v2, v3}, [x1]

  st1.4h {v0}, [x1]
  st1.4h {v0, v1}, [x1]
  st1.4h {v0, v1, v2}, [x1]
  st1.4h {v0, v1, v2, v3}, [x1]

  st1.8h {v0}, [x1]
  st1.8h {v0, v1}, [x1]
  st1.8h {v0, v1, v2}, [x1]
  st1.8h {v0, v1, v2, v3}, [x1]

  st1.2s {v0}, [x1]
  st1.2s {v0, v1}, [x1]
  st1.2s {v0, v1, v2}, [x1]
  st1.2s {v0, v1, v2, v3}, [x1]

  st1.4s {v0}, [x1]
  st1.4s {v0, v1}, [x1]
  st1.4s {v0, v1, v2}, [x1]
  st1.4s {v0, v1, v2, v3}, [x1]

  st1.1d {v0}, [x1]
  st1.1d {v0, v1}, [x1]
  st1.1d {v0, v1, v2}, [x1]
  st1.1d {v0, v1, v2, v3}, [x1]

  st1.2d {v0}, [x1]
  st1.2d {v0, v1}, [x1]
  st1.2d {v0, v1, v2}, [x1]
  st1.2d {v0, v1, v2, v3}, [x1]

  st1.2d {v5}, [x1]
  st1.2d {v7, v8}, [x10]
  st1.2d {v11, v12, v13}, [x1]
  st1.2d {v28, v29, v30, v31}, [x13]
end;

procedure ld2st2_multiple; assembler; nostackframe;
asm
  ld2.8b {v4, v5}, [x19]
  ld2.16b {v4, v5}, [x19]
  ld2.4h {v4, v5}, [x19]
  ld2.8h {v4, v5}, [x19]
  ld2.2s {v4, v5}, [x19]
  ld2.4s {v4, v5}, [x19]
  ld2.2d {v4, v5}, [x19]

  st2.8b {v4, v5}, [x19]
  st2.16b {v4, v5}, [x19]
  st2.4h {v4, v5}, [x19]
  st2.8h {v4, v5}, [x19]
  st2.2s {v4, v5}, [x19]
  st2.4s {v4, v5}, [x19]
  st2.2d {v4, v5}, [x19]
end;

procedure ld3st3_multiple; assembler; nostackframe;
asm
    ld3.8b {v4, v5, v6}, [x19]
    ld3.16b {v4, v5, v6}, [x19]
    ld3.4h {v4, v5, v6}, [x19]
    ld3.8h {v4, v5, v6}, [x19]
    ld3.2s {v4, v5, v6}, [x19]
    ld3.4s {v4, v5, v6}, [x19]
    ld3.2d {v4, v5, v6}, [x19]

    ld3.8b {v9, v10, v11}, [x9]
    ld3.16b {v14, v15, v16}, [x19]
    ld3.4h {v24, v25, v26}, [x29]
    ld3.8h {v30, v31, v0}, [x9]
    ld3.2s {v2, v3, v4}, [x19]
    ld3.4s {v4, v5, v6}, [x29]
    ld3.2d {v7, v8, v9}, [x9]

    st3.8b {v4, v5, v6}, [x19]
    st3.16b {v4, v5, v6}, [x19]
    st3.4h {v4, v5, v6}, [x19]
    st3.8h {v4, v5, v6}, [x19]
    st3.2s {v4, v5, v6}, [x19]
    st3.4s {v4, v5, v6}, [x19]
    st3.2d {v4, v5, v6}, [x19]

    st3.8b {v10, v11, v12}, [x9]
    st3.16b {v14, v15, v16}, [x19]
    st3.4h {v24, v25, v26}, [x29]
    st3.8h {v30, v31, v0}, [x9]
    st3.2s {v2, v3, v4}, [x19]
    st3.4s {v7, v8, v9}, [x29]
    st3.2d {v4, v5, v6}, [x9]
end;

procedure ld4st4_multiple; assembler; nostackframe;
asm
    ld4.8b {v4, v5, v6, v7}, [x19]
    ld4.16b {v4, v5, v6, v7}, [x19]
    ld4.4h {v4, v5, v6, v7}, [x19]
    ld4.8h {v4, v5, v6, v7}, [x19]
    ld4.2s {v4, v5, v6, v7}, [x19]
    ld4.4s {v4, v5, v6, v7}, [x19]
    ld4.2d {v4, v5, v6, v7}, [x19]

    st4.8b {v4, v5, v6, v7}, [x19]
    st4.16b {v4, v5, v6, v7}, [x19]
    st4.4h {v4, v5, v6, v7}, [x19]
    st4.8h {v4, v5, v6, v7}, [x19]
    st4.2s {v4, v5, v6, v7}, [x19]
    st4.4s {v4, v5, v6, v7}, [x19]
    st4.2d {v4, v5, v6, v7}, [x19]
end;


//-----------------------------------------------------------------------------
// Post-increment versions.
//----------------------------------------------------------------------------

procedure ld1st1_multiple_post; assembler; nostackframe;
asm
  ld1.8b {v0}, [x1], x15
  ld1.8b {v0, v1}, [x1], x15
  ld1.8b {v0, v1, v2}, [x1], x15
  ld1.8b {v0, v1, v2, v3}, [x1], x15

  ld1.16b {v0}, [x1], x15
  ld1.16b {v0, v1}, [x1], x15
  ld1.16b {v0, v1, v2}, [x1], x15
  ld1.16b {v0, v1, v2, v3}, [x1], x15

  ld1.4h {v0}, [x1], x15
  ld1.4h {v0, v1}, [x1], x15
  ld1.4h {v0, v1, v2}, [x1], x15
  ld1.4h {v0, v1, v2, v3}, [x1], x15

  ld1.8h {v0}, [x1], x15
  ld1.8h {v0, v1}, [x1], x15
  ld1.8h {v0, v1, v2}, [x1], x15
  ld1.8h {v0, v1, v2, v3}, [x1], x15

  ld1.2s {v0}, [x1], x15
  ld1.2s {v0, v1}, [x1], x15
  ld1.2s {v0, v1, v2}, [x1], x15
  ld1.2s {v0, v1, v2, v3}, [x1], x15

  ld1.4s {v0}, [x1], x15
  ld1.4s {v0, v1}, [x1], x15
  ld1.4s {v0, v1, v2}, [x1], x15
  ld1.4s {v0, v1, v2, v3}, [x1], x15

  ld1.1d {v0}, [x1], x15
  ld1.1d {v0, v1}, [x1], x15
  ld1.1d {v0, v1, v2}, [x1], x15
  ld1.1d {v0, v1, v2, v3}, [x1], x15

  ld1.2d {v0}, [x1], x15
  ld1.2d {v0, v1}, [x1], x15
  ld1.2d {v0, v1, v2}, [x1], x15
  ld1.2d {v0, v1, v2, v3}, [x1], x15

  st1.8b {v0}, [x1], x15
  st1.8b {v0, v1}, [x1], x15
  st1.8b {v0, v1, v2}, [x1], x15
  st1.8b {v0, v1, v2, v3}, [x1], x15

  st1.16b {v0}, [x1], x15
  st1.16b {v0, v1}, [x1], x15
  st1.16b {v0, v1, v2}, [x1], x15
  st1.16b {v0, v1, v2, v3}, [x1], x15

  st1.4h {v0}, [x1], x15
  st1.4h {v0, v1}, [x1], x15
  st1.4h {v0, v1, v2}, [x1], x15
  st1.4h {v0, v1, v2, v3}, [x1], x15

  st1.8h {v0}, [x1], x15
  st1.8h {v0, v1}, [x1], x15
  st1.8h {v0, v1, v2}, [x1], x15
  st1.8h {v0, v1, v2, v3}, [x1], x15

  st1.2s {v0}, [x1], x15
  st1.2s {v0, v1}, [x1], x15
  st1.2s {v0, v1, v2}, [x1], x15
  st1.2s {v0, v1, v2, v3}, [x1], x15

  st1.4s {v0}, [x1], x15
  st1.4s {v0, v1}, [x1], x15
  st1.4s {v0, v1, v2}, [x1], x15
  st1.4s {v0, v1, v2, v3}, [x1], x15

  st1.1d {v0}, [x1], x15
  st1.1d {v0, v1}, [x1], x15
  st1.1d {v0, v1, v2}, [x1], x15
  st1.1d {v0, v1, v2, v3}, [x1], x15

  st1.2d {v0}, [x1], x15
  st1.2d {v0, v1}, [x1], x15
  st1.2d {v0, v1, v2}, [x1], x15
  st1.2d {v0, v1, v2, v3}, [x1], x15

  ld1.8b {v0}, [x1], #8
  ld1.8b {v0, v1}, [x1], #16
  ld1.8b {v0, v1, v2}, [x1], #24
  ld1.8b {v0, v1, v2, v3}, [x1], #32

  ld1.16b {v0}, [x1], #16
  ld1.16b {v0, v1}, [x1], #32
  ld1.16b {v0, v1, v2}, [x1], #48
  ld1.16b {v0, v1, v2, v3}, [x1], #64

  ld1.4h {v0}, [x1], #8
  ld1.4h {v0, v1}, [x1], #16
  ld1.4h {v0, v1, v2}, [x1], #24
  ld1.4h {v0, v1, v2, v3}, [x1], #32

  ld1.8h {v0}, [x1], #16
  ld1.8h {v0, v1}, [x1], #32
  ld1.8h {v0, v1, v2}, [x1], #48
  ld1.8h {v0, v1, v2, v3}, [x1], #64

  ld1.2s {v0}, [x1], #8
  ld1.2s {v0, v1}, [x1], #16
  ld1.2s {v0, v1, v2}, [x1], #24
  ld1.2s {v0, v1, v2, v3}, [x1], #32

  ld1.4s {v0}, [x1], #16
  ld1.4s {v0, v1}, [x1], #32
  ld1.4s {v0, v1, v2}, [x1], #48
  ld1.4s {v0, v1, v2, v3}, [x1], #64

  ld1.1d {v0}, [x1], #8
  ld1.1d {v0, v1}, [x1], #16
  ld1.1d {v0, v1, v2}, [x1], #24
  ld1.1d {v0, v1, v2, v3}, [x1], #32

  ld1.2d {v0}, [x1], #16
  ld1.2d {v0, v1}, [x1], #32
  ld1.2d {v0, v1, v2}, [x1], #48
  ld1.2d {v0, v1, v2, v3}, [x1], #64

  st1.8b {v0}, [x1], #8
  st1.8b {v0, v1}, [x1], #16
  st1.8b {v0, v1, v2}, [x1], #24
  st1.8b {v0, v1, v2, v3}, [x1], #32

  st1.16b {v0}, [x1], #16
  st1.16b {v0, v1}, [x1], #32
  st1.16b {v0, v1, v2}, [x1], #48
  st1.16b {v0, v1, v2, v3}, [x1], #64

  st1.4h {v0}, [x1], #8
  st1.4h {v0, v1}, [x1], #16
  st1.4h {v0, v1, v2}, [x1], #24
  st1.4h {v0, v1, v2, v3}, [x1], #32

  st1.8h {v0}, [x1], #16
  st1.8h {v0, v1}, [x1], #32
  st1.8h {v0, v1, v2}, [x1], #48
  st1.8h {v0, v1, v2, v3}, [x1], #64

  st1.2s {v0}, [x1], #8
  st1.2s {v0, v1}, [x1], #16
  st1.2s {v0, v1, v2}, [x1], #24
  st1.2s {v0, v1, v2, v3}, [x1], #32

  st1.4s {v0}, [x1], #16
  st1.4s {v0, v1}, [x1], #32
  st1.4s {v0, v1, v2}, [x1], #48
  st1.4s {v0, v1, v2, v3}, [x1], #64

  st1.1d {v0}, [x1], #8
  st1.1d {v0, v1}, [x1], #16
  st1.1d {v0, v1, v2}, [x1], #24
  st1.1d {v0, v1, v2, v3}, [x1], #32

  st1.2d {v0}, [x1], #16
  st1.2d {v0, v1}, [x1], #32
  st1.2d {v0, v1, v2}, [x1], #48
  st1.2d {v0, v1, v2, v3}, [x1], #64
end;

procedure ld2st2_multiple_post; assembler; nostackframe;
asm
  ld2.8b {v0, v1}, [x1], x15
  ld2.16b {v0, v1}, [x1], x15
  ld2.4h {v0, v1}, [x1], x15
  ld2.8h {v0, v1}, [x1], x15
  ld2.2s {v0, v1}, [x1], x15
  ld2.4s {v0, v1}, [x1], x15
  ld2.2d {v0, v1}, [x1], x15

  st2.8b {v0, v1}, [x1], x15
  st2.16b {v0, v1}, [x1], x15
  st2.4h {v0, v1}, [x1], x15
  st2.8h {v0, v1}, [x1], x15
  st2.2s {v0, v1}, [x1], x15
  st2.4s {v0, v1}, [x1], x15
  st2.2d {v0, v1}, [x1], x15

  ld2.8b {v0, v1}, [x1], #16
  ld2.16b {v0, v1}, [x1], #32
  ld2.4h {v0, v1}, [x1], #16
  ld2.8h {v0, v1}, [x1], #32
  ld2.2s {v0, v1}, [x1], #16
  ld2.4s {v0, v1}, [x1], #32
  ld2.2d {v0, v1}, [x1], #32

  st2.8b {v0, v1}, [x1], #16
  st2.16b {v0, v1}, [x1], #32
  st2.4h {v0, v1}, [x1], #16
  st2.8h {v0, v1}, [x1], #32
  st2.2s {v0, v1}, [x1], #16
  st2.4s {v0, v1}, [x1], #32
  st2.2d {v0, v1}, [x1], #32
end;

procedure ld3st3_multiple_post; assembler; nostackframe;
asm
  ld3.8b {v0, v1, v2}, [x1], x15
  ld3.16b {v0, v1, v2}, [x1], x15
  ld3.4h {v0, v1, v2}, [x1], x15
  ld3.8h {v0, v1, v2}, [x1], x15
  ld3.2s {v0, v1, v2}, [x1], x15
  ld3.4s {v0, v1, v2}, [x1], x15
  ld3.2d {v0, v1, v2}, [x1], x15

  st3.8b {v0, v1, v2}, [x1], x15
  st3.16b {v0, v1, v2}, [x1], x15
  st3.4h {v0, v1, v2}, [x1], x15
  st3.8h {v0, v1, v2}, [x1], x15
  st3.2s {v0, v1, v2}, [x1], x15
  st3.4s {v0, v1, v2}, [x1], x15
  st3.2d {v0, v1, v2}, [x1], x15

  ld3.8b {v0, v1, v2}, [x1], #24
  ld3.16b {v0, v1, v2}, [x1], #48
  ld3.4h {v0, v1, v2}, [x1], #24
  ld3.8h {v0, v1, v2}, [x1], #48
  ld3.2s {v0, v1, v2}, [x1], #24
  ld3.4s {v0, v1, v2}, [x1], #48
  ld3.2d {v0, v1, v2}, [x1], #48

  st3.8b {v0, v1, v2}, [x1], #24
  st3.16b {v0, v1, v2}, [x1], #48
  st3.4h {v0, v1, v2}, [x1], #24
  st3.8h {v0, v1, v2}, [x1], #48
  st3.2s {v0, v1, v2}, [x1], #24
  st3.4s {v0, v1, v2}, [x1], #48
  st3.2d {v0, v1, v2}, [x1], #48
end;

procedure ld4st4_multiple_post; assembler; nostackframe;
asm
  ld4.8b {v0, v1, v2, v3}, [x1], x15
  ld4.16b {v0, v1, v2, v3}, [x1], x15
  ld4.4h {v0, v1, v2, v3}, [x1], x15
  ld4.8h {v0, v1, v2, v3}, [x1], x15
  ld4.2s {v0, v1, v2, v3}, [x1], x15
  ld4.4s {v0, v1, v2, v3}, [x1], x15
  ld4.2d {v0, v1, v2, v3}, [x1], x15

  st4.8b {v0, v1, v2, v3}, [x1], x15
  st4.16b {v0, v1, v2, v3}, [x1], x15
  st4.4h {v0, v1, v2, v3}, [x1], x15
  st4.8h {v0, v1, v2, v3}, [x1], x15
  st4.2s {v0, v1, v2, v3}, [x1], x15
  st4.4s {v0, v1, v2, v3}, [x1], x15
  st4.2d {v0, v1, v2, v3}, [x1], x15

  ld4.8b {v0, v1, v2, v3}, [x1], #32
  ld4.16b {v0, v1, v2, v3}, [x1], #64
  ld4.4h {v0, v1, v2, v3}, [x1], #32
  ld4.8h {v0, v1, v2, v3}, [x1], #64
  ld4.2s {v0, v1, v2, v3}, [x1], #32
  ld4.4s {v0, v1, v2, v3}, [x1], #64
  ld4.2d {v0, v1, v2, v3}, [x1], #64

  st4.8b {v0, v1, v2, v3}, [x1], #32
  st4.16b {v0, v1, v2, v3}, [x1], #64
  st4.4h {v0, v1, v2, v3}, [x1], #32
  st4.8h {v0, v1, v2, v3}, [x1], #64
  st4.2s {v0, v1, v2, v3}, [x1], #32
  st4.4s {v0, v1, v2, v3}, [x1], #64
  st4.2d {v0, v1, v2, v3}, [x1], #64
end;


procedure ld1r; assembler; nostackframe;
asm
  ld1r.8b {v4}, [x2]
  ld1r.8b {v4}, [x2], x3
  ld1r.16b {v4}, [x2]
  ld1r.16b {v4}, [x2], x3
  ld1r.4h {v4}, [x2]
  ld1r.4h {v4}, [x2], x3
  ld1r.8h {v4}, [x2]
  ld1r.8h {v4}, [x2], x3
  ld1r.2s {v4}, [x2]
  ld1r.2s {v4}, [x2], x3
  ld1r.4s {v4}, [x2]
  ld1r.4s {v4}, [x2], x3
  ld1r.1d {v4}, [x2]
  ld1r.1d {v4}, [x2], x3
  ld1r.2d {v4}, [x2]
  ld1r.2d {v4}, [x2], x3

  ld1r.8b {v4}, [x2], #1
  ld1r.16b {v4}, [x2], #1
  ld1r.4h {v4}, [x2], #2
  ld1r.8h {v4}, [x2], #2
  ld1r.2s {v4}, [x2], #4
  ld1r.4s {v4}, [x2], #4
  ld1r.1d {v4}, [x2], #8
  ld1r.2d {v4}, [x2], #8
end;

procedure ld2r; assembler; nostackframe;
asm
  ld2r.8b {v4, v5}, [x2]
  ld2r.8b {v4, v5}, [x2], x3
  ld2r.16b {v4, v5}, [x2]
  ld2r.16b {v4, v5}, [x2], x3
  ld2r.4h {v4, v5}, [x2]
  ld2r.4h {v4, v5}, [x2], x3
  ld2r.8h {v4, v5}, [x2]
  ld2r.8h {v4, v5}, [x2], x3
  ld2r.2s {v4, v5}, [x2]
  ld2r.2s {v4, v5}, [x2], x3
  ld2r.4s {v4, v5}, [x2]
  ld2r.4s {v4, v5}, [x2], x3
  ld2r.1d {v4, v5}, [x2]
  ld2r.1d {v4, v5}, [x2], x3
  ld2r.2d {v4, v5}, [x2]
  ld2r.2d {v4, v5}, [x2], x3

  ld2r.8b {v4, v5}, [x2], #2
  ld2r.16b {v4, v5}, [x2], #2
  ld2r.4h {v4, v5}, [x2], #4
  ld2r.8h {v4, v5}, [x2], #4
  ld2r.2s {v4, v5}, [x2], #8
  ld2r.4s {v4, v5}, [x2], #8
  ld2r.1d {v4, v5}, [x2], #16
  ld2r.2d {v4, v5}, [x2], #16
end;

procedure ld3r; assembler; nostackframe;
asm
  ld3r.8b {v4, v5, v6}, [x2]
  ld3r.8b {v4, v5, v6}, [x2], x3
  ld3r.16b {v4, v5, v6}, [x2]
  ld3r.16b {v4, v5, v6}, [x2], x3
  ld3r.4h {v4, v5, v6}, [x2]
  ld3r.4h {v4, v5, v6}, [x2], x3
  ld3r.8h {v4, v5, v6}, [x2]
  ld3r.8h {v4, v5, v6}, [x2], x3
  ld3r.2s {v4, v5, v6}, [x2]
  ld3r.2s {v4, v5, v6}, [x2], x3
  ld3r.4s {v4, v5, v6}, [x2]
  ld3r.4s {v4, v5, v6}, [x2], x3
  ld3r.1d {v4, v5, v6}, [x2]
  ld3r.1d {v4, v5, v6}, [x2], x3
  ld3r.2d {v4, v5, v6}, [x2]
  ld3r.2d {v4, v5, v6}, [x2], x3

  ld3r.8b {v4, v5, v6}, [x2], #3
  ld3r.16b {v4, v5, v6}, [x2], #3
  ld3r.4h {v4, v5, v6}, [x2], #6
  ld3r.8h {v4, v5, v6}, [x2], #6
  ld3r.2s {v4, v5, v6}, [x2], #12
  ld3r.4s {v4, v5, v6}, [x2], #12
  ld3r.1d {v4, v5, v6}, [x2], #24
  ld3r.2d {v4, v5, v6}, [x2], #24
end;

procedure ld4r; assembler; nostackframe;
asm
  ld4r.8b {v4, v5, v6, v7}, [x2]
  ld4r.8b {v4, v5, v6, v7}, [x2], x3
  ld4r.16b {v4, v5, v6, v7}, [x2]
  ld4r.16b {v4, v5, v6, v7}, [x2], x3
  ld4r.4h {v4, v5, v6, v7}, [x2]
  ld4r.4h {v4, v5, v6, v7}, [x2], x3
  ld4r.8h {v4, v5, v6, v7}, [x2]
  ld4r.8h {v4, v5, v6, v7}, [x2], x3
  ld4r.2s {v4, v5, v6, v7}, [x2]
  ld4r.2s {v4, v5, v6, v7}, [x2], x3
  ld4r.4s {v4, v5, v6, v7}, [x2]
  ld4r.4s {v4, v5, v6, v7}, [x2], x3
  ld4r.1d {v4, v5, v6, v7}, [x2]
  ld4r.1d {v4, v5, v6, v7}, [x2], x3
  ld4r.2d {v4, v5, v6, v7}, [x2]
  ld4r.2d {v4, v5, v6, v7}, [x2], x3

  ld4r.8b {v4, v5, v6, v7}, [x2], #4
  ld4r.16b {v5, v6, v7, v8}, [x2], #4
  ld4r.4h {v6, v7, v8, v9}, [x2], #8
  ld4r.8h {v1, v2, v3, v4}, [x2], #8
  ld4r.2s {v2, v3, v4, v5}, [x2], #16
  ld4r.4s {v3, v4, v5, v6}, [x2], #16
  ld4r.1d {v0, v1, v2, v3}, [x2], #32
  ld4r.2d {v4, v5, v6, v7}, [x2], #32
end;


procedure ld1; assembler; nostackframe;
asm
  ld1.b {v4}[13], [x3]
  ld1.h {v4}[2], [x3]
  ld1.s {v4}[2], [x3]
  ld1.d {v4}[1], [x3]
  ld1.b {v4}[13], [x3], x5
  ld1.h {v4}[2], [x3], x5
  ld1.s {v4}[2], [x3], x5
  ld1.d {v4}[1], [x3], x5
  ld1.b {v4}[13], [x3], #1
  ld1.h {v4}[2], [x3], #2
  ld1.s {v4}[2], [x3], #4
  ld1.d {v4}[1], [x3], #8
end;

procedure ld2; assembler; nostackframe;
asm
  ld2.b {v4, v5}[13], [x3]
  ld2.h {v4, v5}[2], [x3]
  ld2.s {v4, v5}[2], [x3]
  ld2.d {v4, v5}[1], [x3]
  ld2.b {v4, v5}[13], [x3], x5
  ld2.h {v4, v5}[2], [x3], x5
  ld2.s {v4, v5}[2], [x3], x5
  ld2.d {v4, v5}[1], [x3], x5
  ld2.b {v4, v5}[13], [x3], #2
  ld2.h {v4, v5}[2], [x3], #4
  ld2.s {v4, v5}[2], [x3], #8
  ld2.d {v4, v5}[1], [x3], #16
end;

procedure ld3; assembler; nostackframe;
asm
  ld3.b {v4, v5, v6}[13], [x3]
  ld3.h {v4, v5, v6}[2], [x3]
  ld3.s {v4, v5, v6}[2], [x3]
  ld3.d {v4, v5, v6}[1], [x3]
  ld3.b {v4, v5, v6}[13], [x3], x5
  ld3.h {v4, v5, v6}[2], [x3], x5
  ld3.s {v4, v5, v6}[2], [x3], x5
  ld3.d {v4, v5, v6}[1], [x3], x5
  ld3.b {v4, v5, v6}[13], [x3], #3
  ld3.h {v4, v5, v6}[2], [x3], #6
  ld3.s {v4, v5, v6}[2], [x3], #12
  ld3.d {v4, v5, v6}[1], [x3], #24
end;

procedure ld4; assembler; nostackframe;
asm
  ld4.b {v4, v5, v6, v7}[13], [x3]
  ld4.h {v4, v5, v6, v7}[2], [x3]
  ld4.s {v4, v5, v6, v7}[2], [x3]
  ld4.d {v4, v5, v6, v7}[1], [x3]
  ld4.b {v4, v5, v6, v7}[13], [x3], x5
  ld4.h {v4, v5, v6, v7}[2], [x3], x5
  ld4.s {v4, v5, v6, v7}[2], [x3], x5
  ld4.d {v4, v5, v6, v7}[1], [x3], x5
  ld4.b {v4, v5, v6, v7}[13], [x3], #4
  ld4.h {v4, v5, v6, v7}[2], [x3], #8
  ld4.s {v4, v5, v6, v7}[2], [x3], #16
  ld4.d {v4, v5, v6, v7}[1], [x3], #32
end;

procedure st1; assembler; nostackframe;
asm
  st1.b {v4}[13], [x3]
  st1.h {v4}[2], [x3]
  st1.s {v4}[2], [x3]
  st1.d {v4}[1], [x3]
  st1.b {v4}[13], [x3], x5
  st1.h {v4}[2], [x3], x5
  st1.s {v4}[2], [x3], x5
  st1.d {v4}[1], [x3], x5
  st1.b {v4}[13], [x3], #1
  st1.h {v4}[2], [x3], #2
  st1.s {v4}[2], [x3], #4
  st1.d {v4}[1], [x3], #8
end;

procedure st2; assembler; nostackframe;
asm
  st2.b {v4, v5}[13], [x3]
  st2.h {v4, v5}[2], [x3]
  st2.s {v4, v5}[2], [x3]
  st2.d {v4, v5}[1], [x3]
  st2.b {v4, v5}[13], [x3], x5
  st2.h {v4, v5}[2], [x3], x5
  st2.s {v4, v5}[2], [x3], x5
  st2.d {v4, v5}[1], [x3], x5
  st2.b {v4, v5}[13], [x3], #2
  st2.h {v4, v5}[2], [x3], #4
  st2.s {v4, v5}[2], [x3], #8
  st2.d {v4, v5}[1], [x3], #16
end;

procedure st3; assembler; nostackframe;
asm
  st3.b {v4, v5, v6}[13], [x3]
  st3.h {v4, v5, v6}[2], [x3]
  st3.s {v4, v5, v6}[2], [x3]
  st3.d {v4, v5, v6}[1], [x3]
  st3.b {v4, v5, v6}[13], [x3], x5
  st3.h {v4, v5, v6}[2], [x3], x5
  st3.s {v4, v5, v6}[2], [x3], x5
  st3.d {v4, v5, v6}[1], [x3], x5
  st3.b {v4, v5, v6}[13], [x3], #3
  st3.h {v4, v5, v6}[2], [x3], #6
  st3.s {v4, v5, v6}[2], [x3], #12
  st3.d {v4, v5, v6}[1], [x3], #24
end;

procedure st4; assembler; nostackframe;
asm
  st4.b {v4, v5, v6, v7}[13], [x3]
  st4.h {v4, v5, v6, v7}[2], [x3]
  st4.s {v4, v5, v6, v7}[2], [x3]
  st4.d {v4, v5, v6, v7}[1], [x3]
  st4.b {v4, v5, v6, v7}[13], [x3], x5
  st4.h {v4, v5, v6, v7}[2], [x3], x5
  st4.s {v4, v5, v6, v7}[2], [x3], x5
  st4.d {v4, v5, v6, v7}[1], [x3], x5
  st4.b {v4, v5, v6, v7}[13], [x3], #4
  st4.h {v4, v5, v6, v7}[2], [x3], #8
  st4.s {v4, v5, v6, v7}[2], [x3], #16
  st4.d {v4, v5, v6, v7}[1], [x3], #32
end;


//---------
// ARM verbose syntax equivalents to the above.
//---------
procedure verbose_syntax; assembler; nostackframe;
asm
  ld1 { v1.8b }, [x1]
  ld1 { v2.8b, v3.8b }, [x1]
  ld1 { v3.8b, v4.8b, v5.8b }, [x1]
  ld1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1]

  ld1 { v1.16b }, [x1]
  ld1 { v2.16b, v3.16b }, [x1]
  ld1 { v3.16b, v4.16b, v5.16b }, [x1]
  ld1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1]

  ld1 { v1.4h }, [x1]
  ld1 { v2.4h, v3.4h }, [x1]
  ld1 { v3.4h, v4.4h, v5.4h }, [x1]
  ld1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1]

  ld1 { v1.8h }, [x1]
  ld1 { v2.8h, v3.8h }, [x1]
  ld1 { v3.8h, v4.8h, v5.8h }, [x1]
  ld1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1]

  ld1 { v1.2s }, [x1]
  ld1 { v2.2s, v3.2s }, [x1]
  ld1 { v3.2s, v4.2s, v5.2s }, [x1]
  ld1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1]

  ld1 { v1.4s }, [x1]
  ld1 { v2.4s, v3.4s }, [x1]
  ld1 { v3.4s, v4.4s, v5.4s }, [x1]
  ld1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1]

  ld1 { v1.1d }, [x1]
  ld1 { v2.1d, v3.1d }, [x1]
  ld1 { v3.1d, v4.1d, v5.1d }, [x1]
  ld1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1]

  ld1 { v1.2d }, [x1]
  ld1 { v2.2d, v3.2d }, [x1]
  ld1 { v3.2d, v4.2d, v5.2d }, [x1]
  ld1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1]

  st1 { v1.8b }, [x1]
  st1 { v2.8b, v3.8b }, [x1]
  st1 { v3.8b, v4.8b, v5.8b }, [x1]
  st1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1]

  st1 { v1.16b }, [x1]
  st1 { v2.16b, v3.16b }, [x1]
  st1 { v3.16b, v4.16b, v5.16b }, [x1]
  st1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1]

  st1 { v1.4h }, [x1]
  st1 { v2.4h, v3.4h }, [x1]
  st1 { v3.4h, v4.4h, v5.4h }, [x1]
  st1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1]

  st1 { v1.8h }, [x1]
  st1 { v2.8h, v3.8h }, [x1]
  st1 { v3.8h, v4.8h, v5.8h }, [x1]
  st1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1]

  st1 { v1.2s }, [x1]
  st1 { v2.2s, v3.2s }, [x1]
  st1 { v3.2s, v4.2s, v5.2s }, [x1]
  st1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1]

  st1 { v1.4s }, [x1]
  st1 { v2.4s, v3.4s }, [x1]
  st1 { v3.4s, v4.4s, v5.4s }, [x1]
  st1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1]

  st1 { v1.1d }, [x1]
  st1 { v2.1d, v3.1d }, [x1]
  st1 { v3.1d, v4.1d, v5.1d }, [x1]
  st1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1]

  st1 { v1.2d }, [x1]
  st1 { v2.2d, v3.2d }, [x1]
  st1 { v3.2d, v4.2d, v5.2d }, [x1]
  st1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1]

  ld2 { v3.8b, v4.8b }, [x19]
  ld2 { v3.16b, v4.16b }, [x19]
  ld2 { v3.4h, v4.4h }, [x19]
  ld2 { v3.8h, v4.8h }, [x19]
  ld2 { v3.2s, v4.2s }, [x19]
  ld2 { v3.4s, v4.4s }, [x19]
  ld2 { v3.2d, v4.2d }, [x19]

  st2 { v3.8b, v4.8b }, [x19]
  st2 { v3.16b, v4.16b }, [x19]
  st2 { v3.4h, v4.4h }, [x19]
  st2 { v3.8h, v4.8h }, [x19]
  st2 { v3.2s, v4.2s }, [x19]
  st2 { v3.4s, v4.4s }, [x19]
  st2 { v3.2d, v4.2d }, [x19]

  ld3 { v2.8b, v3.8b, v4.8b }, [x19]
  ld3 { v2.16b, v3.16b, v4.16b }, [x19]
  ld3 { v2.4h, v3.4h, v4.4h }, [x19]
  ld3 { v2.8h, v3.8h, v4.8h }, [x19]
  ld3 { v2.2s, v3.2s, v4.2s }, [x19]
  ld3 { v2.4s, v3.4s, v4.4s }, [x19]
  ld3 { v2.2d, v3.2d, v4.2d }, [x19]

  st3 { v2.8b, v3.8b, v4.8b }, [x19]
  st3 { v2.16b, v3.16b, v4.16b }, [x19]
  st3 { v2.4h, v3.4h, v4.4h }, [x19]
  st3 { v2.8h, v3.8h, v4.8h }, [x19]
  st3 { v2.2s, v3.2s, v4.2s }, [x19]
  st3 { v2.4s, v3.4s, v4.4s }, [x19]
  st3 { v2.2d, v3.2d, v4.2d }, [x19]

  ld4 { v2.8b, v3.8b, v4.8b, v5.8b }, [x19]
  ld4 { v2.16b, v3.16b, v4.16b, v5.16b }, [x19]
  ld4 { v2.4h, v3.4h, v4.4h, v5.4h }, [x19]
  ld4 { v2.8h, v3.8h, v4.8h, v5.8h }, [x19]
  ld4 { v2.2s, v3.2s, v4.2s, v5.2s }, [x19]
  ld4 { v2.4s, v3.4s, v4.4s, v5.4s }, [x19]
  ld4 { v2.2d, v3.2d, v4.2d, v5.2d }, [x19]

  st4 { v2.8b, v3.8b, v4.8b, v5.8b }, [x19]
  st4 { v2.16b, v3.16b, v4.16b, v5.16b }, [x19]
  st4 { v2.4h, v3.4h, v4.4h, v5.4h }, [x19]
  st4 { v2.8h, v3.8h, v4.8h, v5.8h }, [x19]
  st4 { v2.2s, v3.2s, v4.2s, v5.2s }, [x19]
  st4 { v2.4s, v3.4s, v4.4s, v5.4s }, [x19]
  st4 { v2.2d, v3.2d, v4.2d, v5.2d }, [x19]

  ld1 { v1.8b }, [x1], x15
  ld1 { v2.8b, v3.8b }, [x1], x15
  ld1 { v3.8b, v4.8b, v5.8b }, [x1], x15
  ld1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], x15

  ld1 { v1.16b }, [x1], x15
  ld1 { v2.16b, v3.16b }, [x1], x15
  ld1 { v3.16b, v4.16b, v5.16b }, [x1], x15
  ld1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], x15

  ld1 { v1.4h }, [x1], x15
  ld1 { v2.4h, v3.4h }, [x1], x15
  ld1 { v3.4h, v4.4h, v5.4h }, [x1], x15
  ld1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], x15

  ld1 { v1.8h }, [x1], x15
  ld1 { v2.8h, v3.8h }, [x1], x15
  ld1 { v3.8h, v4.8h, v5.8h }, [x1], x15
  ld1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], x15

  ld1 { v1.2s }, [x1], x15
  ld1 { v2.2s, v3.2s }, [x1], x15
  ld1 { v3.2s, v4.2s, v5.2s }, [x1], x15
  ld1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], x15

  ld1 { v1.4s }, [x1], x15
  ld1 { v2.4s, v3.4s }, [x1], x15
  ld1 { v3.4s, v4.4s, v5.4s }, [x1], x15
  ld1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], x15

  ld1 { v1.1d }, [x1], x15
  ld1 { v2.1d, v3.1d }, [x1], x15
  ld1 { v3.1d, v4.1d, v5.1d }, [x1], x15
  ld1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1], x15

  ld1 { v1.2d }, [x1], x15
  ld1 { v2.2d, v3.2d }, [x1], x15
  ld1 { v3.2d, v4.2d, v5.2d }, [x1], x15
  ld1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], x15

  st1 { v1.8b }, [x1], x15
  st1 { v2.8b, v3.8b }, [x1], x15
  st1 { v3.8b, v4.8b, v5.8b }, [x1], x15
  st1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], x15

  st1 { v1.16b }, [x1], x15
  st1 { v2.16b, v3.16b }, [x1], x15
  st1 { v3.16b, v4.16b, v5.16b }, [x1], x15
  st1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], x15

  st1 { v1.4h }, [x1], x15
  st1 { v2.4h, v3.4h }, [x1], x15
  st1 { v3.4h, v4.4h, v5.4h }, [x1], x15
  st1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], x15

  st1 { v1.8h }, [x1], x15
  st1 { v2.8h, v3.8h }, [x1], x15
  st1 { v3.8h, v4.8h, v5.8h }, [x1], x15
  st1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], x15

  st1 { v1.2s }, [x1], x15
  st1 { v2.2s, v3.2s }, [x1], x15
  st1 { v3.2s, v4.2s, v5.2s }, [x1], x15
  st1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], x15

  st1 { v1.4s }, [x1], x15
  st1 { v2.4s, v3.4s }, [x1], x15
  st1 { v3.4s, v4.4s, v5.4s }, [x1], x15
  st1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], x15

  st1 { v1.1d }, [x1], x15
  st1 { v2.1d, v3.1d }, [x1], x15
  st1 { v3.1d, v4.1d, v5.1d }, [x1], x15
  st1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1], x15

  st1 { v1.2d }, [x1], x15
  st1 { v2.2d, v3.2d }, [x1], x15
  st1 { v3.2d, v4.2d, v5.2d }, [x1], x15
  st1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], x15

  ld1 { v1.8b }, [x1], #8
  ld1 { v2.8b, v3.8b }, [x1], #16
  ld1 { v3.8b, v4.8b, v5.8b }, [x1], #24
  ld1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], #32

  ld1 { v1.16b }, [x1], #16
  ld1 { v2.16b, v3.16b }, [x1], #32
  ld1 { v3.16b, v4.16b, v5.16b }, [x1], #48
  ld1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], #64

  ld1 { v1.4h }, [x1], #8
  ld1 { v2.4h, v3.4h }, [x1], #16
  ld1 { v3.4h, v4.4h, v5.4h }, [x1], #24
  ld1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], #32

  ld1 { v1.8h }, [x1], #16
  ld1 { v2.8h, v3.8h }, [x1], #32
  ld1 { v3.8h, v4.8h, v5.8h }, [x1], #48
  ld1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], #64

  ld1 { v1.2s }, [x1], #8
  ld1 { v2.2s, v3.2s }, [x1], #16
  ld1 { v3.2s, v4.2s, v5.2s }, [x1], #24
  ld1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], #32

  ld1 { v1.4s }, [x1], #16
  ld1 { v2.4s, v3.4s }, [x1], #32
  ld1 { v3.4s, v4.4s, v5.4s }, [x1], #48
  ld1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], #64

  ld1 { v1.1d }, [x1], #8
  ld1 { v2.1d, v3.1d }, [x1], #16
  ld1 { v3.1d, v4.1d, v5.1d }, [x1], #24
  ld1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1], #32

  ld1 { v1.2d }, [x1], #16
  ld1 { v2.2d, v3.2d }, [x1], #32
  ld1 { v3.2d, v4.2d, v5.2d }, [x1], #48
  ld1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], #64

  st1 { v1.8b }, [x1], #8
  st1 { v2.8b, v3.8b }, [x1], #16
  st1 { v3.8b, v4.8b, v5.8b }, [x1], #24
  st1 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], #32

  st1 { v1.16b }, [x1], #16
  st1 { v2.16b, v3.16b }, [x1], #32
  st1 { v3.16b, v4.16b, v5.16b }, [x1], #48
  st1 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], #64

  st1 { v1.4h }, [x1], #8
  st1 { v2.4h, v3.4h }, [x1], #16
  st1 { v3.4h, v4.4h, v5.4h }, [x1], #24
  st1 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], #32

  st1 { v1.8h }, [x1], #16
  st1 { v2.8h, v3.8h }, [x1], #32
  st1 { v3.8h, v4.8h, v5.8h }, [x1], #48
  st1 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], #64

  st1 { v1.2s }, [x1], #8
  st1 { v2.2s, v3.2s }, [x1], #16
  st1 { v3.2s, v4.2s, v5.2s }, [x1], #24
  st1 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], #32

  st1 { v1.4s }, [x1], #16
  st1 { v2.4s, v3.4s }, [x1], #32
  st1 { v3.4s, v4.4s, v5.4s }, [x1], #48
  st1 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], #64

  st1 { v1.1d }, [x1], #8
  st1 { v2.1d, v3.1d }, [x1], #16
  st1 { v3.1d, v4.1d, v5.1d }, [x1], #24
  st1 { v7.1d, v8.1d, v9.1d, v10.1d }, [x1], #32

  st1 { v1.2d }, [x1], #16
  st1 { v2.2d, v3.2d }, [x1], #32
  st1 { v3.2d, v4.2d, v5.2d }, [x1], #48
  st1 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], #64

  ld2 { v2.8b, v3.8b }, [x1], x15
  ld2 { v2.16b, v3.16b }, [x1], x15
  ld2 { v2.4h, v3.4h }, [x1], x15
  ld2 { v2.8h, v3.8h }, [x1], x15
  ld2 { v2.2s, v3.2s }, [x1], x15
  ld2 { v2.4s, v3.4s }, [x1], x15
  ld2 { v2.2d, v3.2d }, [x1], x15

  st2 { v2.8b, v3.8b }, [x1], x15
  st2 { v2.16b, v3.16b }, [x1], x15
  st2 { v2.4h, v3.4h }, [x1], x15
  st2 { v2.8h, v3.8h }, [x1], x15
  st2 { v2.2s, v3.2s }, [x1], x15
  st2 { v2.4s, v3.4s }, [x1], x15
  st2 { v2.2d, v3.2d }, [x1], x15

  ld2 { v2.8b, v3.8b }, [x1], #16
  ld2 { v2.16b, v3.16b }, [x1], #32
  ld2 { v2.4h, v3.4h }, [x1], #16
  ld2 { v2.8h, v3.8h }, [x1], #32
  ld2 { v2.2s, v3.2s }, [x1], #16
  ld2 { v2.4s, v3.4s }, [x1], #32
  ld2 { v2.2d, v3.2d }, [x1], #32

  st2 { v2.8b, v3.8b }, [x1], #16
  st2 { v2.16b, v3.16b }, [x1], #32
  st2 { v2.4h, v3.4h }, [x1], #16
  st2 { v2.8h, v3.8h }, [x1], #32
  st2 { v2.2s, v3.2s }, [x1], #16
  st2 { v2.4s, v3.4s }, [x1], #32
  st2 { v2.2d, v3.2d }, [x1], #32

  ld3 { v3.8b, v4.8b, v5.8b }, [x1], x15
  ld3 { v3.16b, v4.16b, v5.16b }, [x1], x15
  ld3 { v3.4h, v4.4h, v5.4h }, [x1], x15
  ld3 { v3.8h, v4.8h, v5.8h }, [x1], x15
  ld3 { v3.2s, v4.2s, v5.2s }, [x1], x15
  ld3 { v3.4s, v4.4s, v5.4s }, [x1], x15
  ld3 { v3.2d, v4.2d, v5.2d }, [x1], x15

  st3 { v3.8b, v4.8b, v5.8b }, [x1], x15
  st3 { v3.16b, v4.16b, v5.16b }, [x1], x15
  st3 { v3.4h, v4.4h, v5.4h }, [x1], x15
  st3 { v3.8h, v4.8h, v5.8h }, [x1], x15
  st3 { v3.2s, v4.2s, v5.2s }, [x1], x15
  st3 { v3.4s, v4.4s, v5.4s }, [x1], x15
  st3 { v3.2d, v4.2d, v5.2d }, [x1], x15
  ld3 { v3.8b, v4.8b, v5.8b }, [x1], #24

  ld3 { v3.16b, v4.16b, v5.16b }, [x1], #48
  ld3 { v3.4h, v4.4h, v5.4h }, [x1], #24
  ld3 { v3.8h, v4.8h, v5.8h }, [x1], #48
  ld3 { v3.2s, v4.2s, v5.2s }, [x1], #24
  ld3 { v3.4s, v4.4s, v5.4s }, [x1], #48
  ld3 { v3.2d, v4.2d, v5.2d }, [x1], #48

  st3 { v3.8b, v4.8b, v5.8b }, [x1], #24
  st3 { v3.16b, v4.16b, v5.16b }, [x1], #48
  st3 { v3.4h, v4.4h, v5.4h }, [x1], #24
  st3 { v3.8h, v4.8h, v5.8h }, [x1], #48
  st3 { v3.2s, v4.2s, v5.2s }, [x1], #24
  st3 { v3.4s, v4.4s, v5.4s }, [x1], #48
  st3 { v3.2d, v4.2d, v5.2d }, [x1], #48

  ld4 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], x15
  ld4 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], x15
  ld4 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], x15
  ld4 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], x15
  ld4 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], x15
  ld4 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], x15
  ld4 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], x15

  st4 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], x15
  st4 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], x15
  st4 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], x15
  st4 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], x15
  st4 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], x15
  st4 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], x15
  st4 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], x15

  ld4 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], #32
  ld4 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], #64
  ld4 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], #32
  ld4 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], #64
  ld4 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], #32
  ld4 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], #64
  ld4 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], #64

  st4 { v4.8b, v5.8b, v6.8b, v7.8b }, [x1], #32
  st4 { v4.16b, v5.16b, v6.16b, v7.16b }, [x1], #64
  st4 { v7.4h, v8.4h, v9.4h, v10.4h }, [x1], #32
  st4 { v7.8h, v8.8h, v9.8h, v10.8h }, [x1], #64
  st4 { v7.2s, v8.2s, v9.2s, v10.2s }, [x1], #32
  st4 { v7.4s, v8.4s, v9.4s, v10.4s }, [x1], #64
  st4 { v7.2d, v8.2d, v9.2d, v10.2d }, [x1], #64


  ld1r { v12.8b }, [x2]
  ld1r { v12.8b }, [x2], x3
  ld1r { v12.16b }, [x2]
  ld1r { v12.16b }, [x2], x3
  ld1r { v12.4h }, [x2]
  ld1r { v12.4h }, [x2], x3
  ld1r { v12.8h }, [x2]
  ld1r { v12.8h }, [x2], x3
  ld1r { v12.2s }, [x2]
  ld1r { v12.2s }, [x2], x3
  ld1r { v12.4s }, [x2]
  ld1r { v12.4s }, [x2], x3
  ld1r { v12.1d }, [x2]
  ld1r { v12.1d }, [x2], x3
  ld1r { v12.2d }, [x2]
  ld1r { v12.2d }, [x2], x3

  ld1r { v12.8b }, [x2], #1
  ld1r { v12.16b }, [x2], #1
  ld1r { v12.4h }, [x2], #2
  ld1r { v12.8h }, [x2], #2
  ld1r { v12.2s }, [x2], #4
  ld1r { v12.4s }, [x2], #4
  ld1r { v12.1d }, [x2], #8
  ld1r { v12.2d }, [x2], #8
  ld2r { v3.8b, v4.8b }, [x2]
  ld2r { v3.8b, v4.8b }, [x2], x3
  ld2r { v3.16b, v4.16b }, [x2]
  ld2r { v3.16b, v4.16b }, [x2], x3
  ld2r { v3.4h, v4.4h }, [x2]
  ld2r { v3.4h, v4.4h }, [x2], x3
  ld2r { v3.8h, v4.8h }, [x2]
  ld2r { v3.8h, v4.8h }, [x2], x3
  ld2r { v3.2s, v4.2s }, [x2]
  ld2r { v3.2s, v4.2s }, [x2], x3
  ld2r { v3.4s, v4.4s }, [x2]
  ld2r { v3.4s, v4.4s }, [x2], x3
  ld2r { v3.1d, v4.1d }, [x2]
  ld2r { v3.1d, v4.1d }, [x2], x3
  ld2r { v3.2d, v4.2d }, [x2]
  ld2r { v3.2d, v4.2d }, [x2], x3

  ld2r { v3.8b, v4.8b }, [x2], #2
  ld2r { v3.16b, v4.16b }, [x2], #2
  ld2r { v3.4h, v4.4h }, [x2], #4
  ld2r { v3.8h, v4.8h }, [x2], #4
  ld2r { v3.2s, v4.2s }, [x2], #8
  ld2r { v3.4s, v4.4s }, [x2], #8
  ld2r { v3.1d, v4.1d }, [x2], #16
  ld2r { v3.2d, v4.2d }, [x2], #16

  ld3r { v2.8b, v3.8b, v4.8b }, [x2]
  ld3r { v2.8b, v3.8b, v4.8b }, [x2], x3
  ld3r { v2.16b, v3.16b, v4.16b }, [x2]
  ld3r { v2.16b, v3.16b, v4.16b }, [x2], x3
  ld3r { v2.4h, v3.4h, v4.4h }, [x2]
  ld3r { v2.4h, v3.4h, v4.4h }, [x2], x3
  ld3r { v2.8h, v3.8h, v4.8h }, [x2]
  ld3r { v2.8h, v3.8h, v4.8h }, [x2], x3
  ld3r { v2.2s, v3.2s, v4.2s }, [x2]
  ld3r { v2.2s, v3.2s, v4.2s }, [x2], x3
  ld3r { v2.4s, v3.4s, v4.4s }, [x2]
  ld3r { v2.4s, v3.4s, v4.4s }, [x2], x3
  ld3r { v2.1d, v3.1d, v4.1d }, [x2]
  ld3r { v2.1d, v3.1d, v4.1d }, [x2], x3
  ld3r { v2.2d, v3.2d, v4.2d }, [x2]
  ld3r { v2.2d, v3.2d, v4.2d }, [x2], x3

  ld3r { v2.8b, v3.8b, v4.8b }, [x2], #3
  ld3r { v2.16b, v3.16b, v4.16b }, [x2], #3
  ld3r { v2.4h, v3.4h, v4.4h }, [x2], #6
  ld3r { v2.8h, v3.8h, v4.8h }, [x2], #6
  ld3r { v2.2s, v3.2s, v4.2s }, [x2], #12
  ld3r { v2.4s, v3.4s, v4.4s }, [x2], #12
  ld3r { v2.1d, v3.1d, v4.1d }, [x2], #24
  ld3r { v2.2d, v3.2d, v4.2d }, [x2], #24

  ld4r { v2.8b, v3.8b, v4.8b, v5.8b }, [x2]
  ld4r { v2.8b, v3.8b, v4.8b, v5.8b }, [x2], x3
  ld4r { v2.16b, v3.16b, v4.16b, v5.16b }, [x2]
  ld4r { v2.16b, v3.16b, v4.16b, v5.16b }, [x2], x3
  ld4r { v2.4h, v3.4h, v4.4h, v5.4h }, [x2]
  ld4r { v2.4h, v3.4h, v4.4h, v5.4h }, [x2], x3
  ld4r { v2.8h, v3.8h, v4.8h, v5.8h }, [x2]
  ld4r { v2.8h, v3.8h, v4.8h, v5.8h }, [x2], x3
  ld4r { v2.2s, v3.2s, v4.2s, v5.2s }, [x2]
  ld4r { v2.2s, v3.2s, v4.2s, v5.2s }, [x2], x3
  ld4r { v2.4s, v3.4s, v4.4s, v5.4s }, [x2]
  ld4r { v2.4s, v3.4s, v4.4s, v5.4s }, [x2], x3
  ld4r { v2.1d, v3.1d, v4.1d, v5.1d }, [x2]
  ld4r { v2.1d, v3.1d, v4.1d, v5.1d }, [x2], x3
  ld4r { v2.2d, v3.2d, v4.2d, v5.2d }, [x2]
  ld4r { v2.2d, v3.2d, v4.2d, v5.2d }, [x2], x3

  ld4r { v2.8b, v3.8b, v4.8b, v5.8b }, [x2], #4
  ld4r { v2.16b, v3.16b, v4.16b, v5.16b }, [x2], #4
  ld4r { v2.4h, v3.4h, v4.4h, v5.4h }, [x2], #8
  ld4r { v2.8h, v3.8h, v4.8h, v5.8h }, [x2], #8
  ld4r { v2.2s, v3.2s, v4.2s, v5.2s }, [x2], #16
  ld4r { v2.4s, v3.4s, v4.4s, v5.4s }, [x2], #16
  ld4r { v2.1d, v3.1d, v4.1d, v5.1d }, [x2], #32
  ld4r { v2.2d, v3.2d, v4.2d, v5.2d }, [x2], #32

  ld1 { v6.b }[13], [x3]
  ld1 { v6.h }[2], [x3]
  ld1 { v6.s }[2], [x3]
  ld1 { v6.d }[1], [x3]
  ld1 { v6.b }[13], [x3], x5
  ld1 { v6.h }[2], [x3], x5
  ld1 { v6.s }[2], [x3], x5
  ld1 { v6.d }[1], [x3], x5
  ld1 { v6.b }[13], [x3], #1
  ld1 { v6.h }[2], [x3], #2
  ld1 { v6.s }[2], [x3], #4
  ld1 { v6.d }[1], [x3], #8

  ld2 { v5.b, v6.b }[13], [x3]
  ld2 { v5.h, v6.h }[2], [x3]
  ld2 { v5.s, v6.s }[2], [x3]
  ld2 { v5.d, v6.d }[1], [x3]
  ld2 { v5.b, v6.b }[13], [x3], x5
  ld2 { v5.h, v6.h }[2], [x3], x5
  ld2 { v5.s, v6.s }[2], [x3], x5
  ld2 { v5.d, v6.d }[1], [x3], x5
  ld2 { v5.b, v6.b }[13], [x3], #2
  ld2 { v5.h, v6.h }[2], [x3], #4
  ld2 { v5.s, v6.s }[2], [x3], #8
  ld2 { v5.d, v6.d }[1], [x3], #16

  ld3 { v7.b, v8.b, v9.b }[13], [x3]
  ld3 { v7.h, v8.h, v9.h }[2], [x3]
  ld3 { v7.s, v8.s, v9.s }[2], [x3]
  ld3 { v7.d, v8.d, v9.d }[1], [x3]
  ld3 { v7.b, v8.b, v9.b }[13], [x3], x5
  ld3 { v7.h, v8.h, v9.h }[2], [x3], x5
  ld3 { v7.s, v8.s, v9.s }[2], [x3], x5
  ld3 { v7.d, v8.d, v9.d }[1], [x3], x5
  ld3 { v7.b, v8.b, v9.b }[13], [x3], #3
  ld3 { v7.h, v8.h, v9.h }[2], [x3], #6
  ld3 { v7.s, v8.s, v9.s }[2], [x3], #12
  ld3 { v7.d, v8.d, v9.d }[1], [x3], #24

  ld4 { v7.b, v8.b, v9.b, v10.b }[13], [x3]
  ld4 { v7.h, v8.h, v9.h, v10.h }[2], [x3]
  ld4 { v7.s, v8.s, v9.s, v10.s }[2], [x3]
  ld4 { v7.d, v8.d, v9.d, v10.d }[1], [x3]
  ld4 { v7.b, v8.b, v9.b, v10.b }[13], [x3], x5
  ld4 { v7.h, v8.h, v9.h, v10.h }[2], [x3], x5
  ld4 { v7.s, v8.s, v9.s, v10.s }[2], [x3], x5
  ld4 { v7.d, v8.d, v9.d, v10.d }[1], [x3], x5
  ld4 { v7.b, v8.b, v9.b, v10.b }[13], [x3], #4
  ld4 { v7.h, v8.h, v9.h, v10.h }[2], [x3], #8
  ld4 { v7.s, v8.s, v9.s, v10.s }[2], [x3], #16
  ld4 { v7.d, v8.d, v9.d, v10.d }[1], [x3], #32

  st1 { v6.b }[13], [x3]
  st1 { v6.h }[2], [x3]
  st1 { v6.s }[2], [x3]
  st1 { v6.d }[1], [x3]
  st1 { v6.b }[13], [x3], x5
  st1 { v6.h }[2], [x3], x5
  st1 { v6.s }[2], [x3], x5
  st1 { v6.d }[1], [x3], x5
  st1 { v6.b }[13], [x3], #1
  st1 { v6.h }[2], [x3], #2
  st1 { v6.s }[2], [x3], #4
  st1 { v6.d }[1], [x3], #8


  st2 { v5.b, v6.b }[13], [x3]
  st2 { v5.h, v6.h }[2], [x3]
  st2 { v5.s, v6.s }[2], [x3]
  st2 { v5.d, v6.d }[1], [x3]
  st2 { v5.b, v6.b }[13], [x3], x5
  st2 { v5.h, v6.h }[2], [x3], x5
  st2 { v5.s, v6.s }[2], [x3], x5
  st2 { v5.d, v6.d }[1], [x3], x5
  st2 { v5.b, v6.b }[13], [x3], #2
  st2 { v5.h, v6.h }[2], [x3], #4
  st2 { v5.s, v6.s }[2], [x3], #8
  st2 { v5.d, v6.d }[1], [x3], #16

  st3 { v7.b, v8.b, v9.b }[13], [x3]
  st3 { v7.h, v8.h, v9.h }[2], [x3]
  st3 { v7.s, v8.s, v9.s }[2], [x3]
  st3 { v7.d, v8.d, v9.d }[1], [x3]
  st3 { v7.b, v8.b, v9.b }[13], [x3], x5
  st3 { v7.h, v8.h, v9.h }[2], [x3], x5
  st3 { v7.s, v8.s, v9.s }[2], [x3], x5
  st3 { v7.d, v8.d, v9.d }[1], [x3], x5
  st3 { v7.b, v8.b, v9.b }[13], [x3], #3
  st3 { v7.h, v8.h, v9.h }[2], [x3], #6
  st3 { v7.s, v8.s, v9.s }[2], [x3], #12
  st3 { v7.d, v8.d, v9.d }[1], [x3], #24

  st4 { v7.b, v8.b, v9.b, v10.b }[13], [x3]
  st4 { v7.h, v8.h, v9.h, v10.h }[2], [x3]
  st4 { v7.s, v8.s, v9.s, v10.s }[2], [x3]
  st4 { v7.d, v8.d, v9.d, v10.d }[1], [x3]
  st4 { v7.b, v8.b, v9.b, v10.b }[13], [x3], x5
  st4 { v7.h, v8.h, v9.h, v10.h }[2], [x3], x5
  st4 { v7.s, v8.s, v9.s, v10.s }[2], [x3], x5
  st4 { v7.d, v8.d, v9.d, v10.d }[1], [x3], x5
  st4 { v7.b, v8.b, v9.b, v10.b }[13], [x3], #4
  st4 { v7.h, v8.h, v9.h, v10.h }[2], [x3], #8
  st4 { v7.s, v8.s, v9.s, v10.s }[2], [x3], #16
  st4 { v7.d, v8.d, v9.d, v10.d }[1], [x3], #32
end;

{$j-}
const
  ld1st1_multiple_data: array[1..72] of tinstrdata = (
    (bytes: ($20,$70,$40,$0c); str: 'ld1.8b	{ v0 }, [x1]'),
    (bytes: ($20,$a0,$40,$0c); str: 'ld1.8b	{ v0, v1 }, [x1]'),
    (bytes: ($20,$60,$40,$0c); str: 'ld1.8b	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$20,$40,$0c); str: 'ld1.8b	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($23,$70,$40,$0c); str: 'ld1.8b { v3 }, [x1]'),
    (bytes: ($43,$a0,$40,$0c); str: 'ld1.8b { v3, v4 }, [x2]'),
    (bytes: ($64,$60,$40,$0c); str: 'ld1.8b { v4, v5, v6 }, [x3]'),
    (bytes: ($87,$20,$40,$0c); str: 'ld1.8b { v7, v8, v9, v10 }, [x4]'),
    (bytes: ($20,$70,$40,$4c); str: 'ld1.16b	{ v0 }, [x1]'),
    (bytes: ($20,$a0,$40,$4c); str: 'ld1.16b	{ v0, v1 }, [x1]'),
    (bytes: ($20,$60,$40,$4c); str: 'ld1.16b	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$20,$40,$4c); str: 'ld1.16b	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$74,$40,$0c); str: 'ld1.4h	{ v0 }, [x1]'),
    (bytes: ($20,$a4,$40,$0c); str: 'ld1.4h	{ v0, v1 }, [x1]'),
    (bytes: ($20,$64,$40,$0c); str: 'ld1.4h	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$24,$40,$0c); str: 'ld1.4h	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$74,$40,$4c); str: 'ld1.8h	{ v0 }, [x1]'),
    (bytes: ($20,$a4,$40,$4c); str: 'ld1.8h	{ v0, v1 }, [x1]'),
    (bytes: ($20,$64,$40,$4c); str: 'ld1.8h	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$24,$40,$4c); str: 'ld1.8h	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$78,$40,$0c); str: 'ld1.2s	{ v0 }, [x1]'),
    (bytes: ($20,$a8,$40,$0c); str: 'ld1.2s	{ v0, v1 }, [x1]'),
    (bytes: ($20,$68,$40,$0c); str: 'ld1.2s	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$28,$40,$0c); str: 'ld1.2s	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$78,$40,$4c); str: 'ld1.4s	{ v0 }, [x1]'),
    (bytes: ($20,$a8,$40,$4c); str: 'ld1.4s	{ v0, v1 }, [x1]'),
    (bytes: ($20,$68,$40,$4c); str: 'ld1.4s	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$28,$40,$4c); str: 'ld1.4s	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$7c,$40,$0c); str: 'ld1.1d	{ v0 }, [x1]'),
    (bytes: ($20,$ac,$40,$0c); str: 'ld1.1d	{ v0, v1 }, [x1]'),
    (bytes: ($20,$6c,$40,$0c); str: 'ld1.1d	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$2c,$40,$0c); str: 'ld1.1d	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$7c,$40,$4c); str: 'ld1.2d	{ v0 }, [x1]'),
    (bytes: ($20,$ac,$40,$4c); str: 'ld1.2d	{ v0, v1 }, [x1]'),
    (bytes: ($20,$6c,$40,$4c); str: 'ld1.2d	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$2c,$40,$4c); str: 'ld1.2d	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$70,$00,$0c); str: 'st1.8b	{ v0 }, [x1]'),
    (bytes: ($20,$a0,$00,$0c); str: 'st1.8b	{ v0, v1 }, [x1]'),
    (bytes: ($20,$60,$00,$0c); str: 'st1.8b	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$20,$00,$0c); str: 'st1.8b	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$70,$00,$4c); str: 'st1.16b	{ v0 }, [x1]'),
    (bytes: ($20,$a0,$00,$4c); str: 'st1.16b	{ v0, v1 }, [x1]'),
    (bytes: ($20,$60,$00,$4c); str: 'st1.16b	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$20,$00,$4c); str: 'st1.16b	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$74,$00,$0c); str: 'st1.4h	{ v0 }, [x1]'),
    (bytes: ($20,$a4,$00,$0c); str: 'st1.4h	{ v0, v1 }, [x1]'),
    (bytes: ($20,$64,$00,$0c); str: 'st1.4h	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$24,$00,$0c); str: 'st1.4h	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$74,$00,$4c); str: 'st1.8h	{ v0 }, [x1]'),
    (bytes: ($20,$a4,$00,$4c); str: 'st1.8h	{ v0, v1 }, [x1]'),
    (bytes: ($20,$64,$00,$4c); str: 'st1.8h	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$24,$00,$4c); str: 'st1.8h	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$78,$00,$0c); str: 'st1.2s	{ v0 }, [x1]'),
    (bytes: ($20,$a8,$00,$0c); str: 'st1.2s	{ v0, v1 }, [x1]'),
    (bytes: ($20,$68,$00,$0c); str: 'st1.2s	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$28,$00,$0c); str: 'st1.2s	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$78,$00,$4c); str: 'st1.4s	{ v0 }, [x1]'),
    (bytes: ($20,$a8,$00,$4c); str: 'st1.4s	{ v0, v1 }, [x1]'),
    (bytes: ($20,$68,$00,$4c); str: 'st1.4s	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$28,$00,$4c); str: 'st1.4s	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$7c,$00,$0c); str: 'st1.1d	{ v0 }, [x1]'),
    (bytes: ($20,$ac,$00,$0c); str: 'st1.1d	{ v0, v1 }, [x1]'),
    (bytes: ($20,$6c,$00,$0c); str: 'st1.1d	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$2c,$00,$0c); str: 'st1.1d	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($20,$7c,$00,$4c); str: 'st1.2d	{ v0 }, [x1]'),
    (bytes: ($20,$ac,$00,$4c); str: 'st1.2d	{ v0, v1 }, [x1]'),
    (bytes: ($20,$6c,$00,$4c); str: 'st1.2d	{ v0, v1, v2 }, [x1]'),
    (bytes: ($20,$2c,$00,$4c); str: 'st1.2d	{ v0, v1, v2, v3 }, [x1]'),
    (bytes: ($25,$7c,$00,$4c); str: 'st1.2d { v5 }, [x1]'),
    (bytes: ($47,$ad,$00,$4c); str: 'st1.2d { v7, v8 }, [x10]'),
    (bytes: ($2b,$6c,$00,$4c); str: 'st1.2d { v11, v12, v13 }, [x1]'),
    (bytes: ($bc,$2d,$00,$4c); str: 'st1.2d { v28, v29, v30, v31 }, [x13]')
  );

  ld2st2_multiple_data: array[1..14] of tinstrdata = (
    (bytes: ($64,$82,$40,$0c); str: 'ld2.8b { v4, v5 }, [x19]'),
    (bytes: ($64,$82,$40,$4c); str: 'ld2.16b { v4, v5 }, [x19]'),
    (bytes: ($64,$86,$40,$0c); str: 'ld2.4h { v4, v5 }, [x19]'),
    (bytes: ($64,$86,$40,$4c); str: 'ld2.8h { v4, v5 }, [x19]'),
    (bytes: ($64,$8a,$40,$0c); str: 'ld2.2s { v4, v5 }, [x19]'),
    (bytes: ($64,$8a,$40,$4c); str: 'ld2.4s { v4, v5 }, [x19]'),
    (bytes: ($64,$8e,$40,$4c); str: 'ld2.2d { v4, v5 }, [x19]'),
    (bytes: ($64,$82,$00,$0c); str: 'st2.8b { v4, v5 }, [x19]'),
    (bytes: ($64,$82,$00,$4c); str: 'st2.16b { v4, v5 }, [x19]'),
    (bytes: ($64,$86,$00,$0c); str: 'st2.4h { v4, v5 }, [x19]'),
    (bytes: ($64,$86,$00,$4c); str: 'st2.8h { v4, v5 }, [x19]'),
    (bytes: ($64,$8a,$00,$0c); str: 'st2.2s { v4, v5 }, [x19]'),
    (bytes: ($64,$8a,$00,$4c); str: 'st2.4s { v4, v5 }, [x19]'),
    (bytes: ($64,$8e,$00,$4c); str: 'st2.2d { v4, v5 }, [x19]')
  );

  ld3st3_multiple_data: array[1..28] of tinstrdata = (
    (bytes: ($64,$42,$40,$0c); str: 'ld3.8b { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$42,$40,$4c); str: 'ld3.16b { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$46,$40,$0c); str: 'ld3.4h { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$46,$40,$4c); str: 'ld3.8h { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4a,$40,$0c); str: 'ld3.2s { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4a,$40,$4c); str: 'ld3.4s { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4e,$40,$4c); str: 'ld3.2d { v4, v5, v6 }, [x19]'),
    (bytes: ($29,$41,$40,$0c); str: 'ld3.8b { v9, v10, v11 }, [x9]'),
    (bytes: ($6e,$42,$40,$4c); str: 'ld3.16b { v14, v15, v16 }, [x19]'),
    (bytes: ($b8,$47,$40,$0c); str: 'ld3.4h { v24, v25, v26 }, [x29]'),
    (bytes: ($3e,$45,$40,$4c); str: 'ld3.8h { v30, v31, v0 }, [x9]'),
    (bytes: ($62,$4a,$40,$0c); str: 'ld3.2s { v2, v3, v4 }, [x19]'),
    (bytes: ($a4,$4b,$40,$4c); str: 'ld3.4s { v4, v5, v6 }, [x29]'),
    (bytes: ($27,$4d,$40,$4c); str: 'ld3.2d { v7, v8, v9 }, [x9]'),
    (bytes: ($64,$42,$00,$0c); str: 'st3.8b { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$42,$00,$4c); str: 'st3.16b { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$46,$00,$0c); str: 'st3.4h { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$46,$00,$4c); str: 'st3.8h { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4a,$00,$0c); str: 'st3.2s { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4a,$00,$4c); str: 'st3.4s { v4, v5, v6 }, [x19]'),
    (bytes: ($64,$4e,$00,$4c); str: 'st3.2d { v4, v5, v6 }, [x19]'),
    (bytes: ($2a,$41,$00,$0c); str: 'st3.8b { v10, v11, v12 }, [x9]'),
    (bytes: ($6e,$42,$00,$4c); str: 'st3.16b { v14, v15, v16 }, [x19]'),
    (bytes: ($b8,$47,$00,$0c); str: 'st3.4h { v24, v25, v26 }, [x29]'),
    (bytes: ($3e,$45,$00,$4c); str: 'st3.8h { v30, v31, v0 }, [x9]'),
    (bytes: ($62,$4a,$00,$0c); str: 'st3.2s { v2, v3, v4 }, [x19]'),
    (bytes: ($a7,$4b,$00,$4c); str: 'st3.4s { v7, v8, v9 }, [x29]'),
    (bytes: ($24,$4d,$00,$4c); str: 'st3.2d { v4, v5, v6 }, [x9]')
  );

  ld4st4_multiple_data: array[1..14] of tinstrdata = (
    (bytes: ($64,$02,$40,$0c); str: 'ld4.8b { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$02,$40,$4c); str: 'ld4.16b { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$06,$40,$0c); str: 'ld4.4h { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$06,$40,$4c); str: 'ld4.8h { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0a,$40,$0c); str: 'ld4.2s { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0a,$40,$4c); str: 'ld4.4s { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0e,$40,$4c); str: 'ld4.2d { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$02,$00,$0c); str: 'st4.8b { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$02,$00,$4c); str: 'st4.16b { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$06,$00,$0c); str: 'st4.4h { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$06,$00,$4c); str: 'st4.8h { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0a,$00,$0c); str: 'st4.2s { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0a,$00,$4c); str: 'st4.4s { v4, v5, v6, v7 }, [x19]'),
    (bytes: ($64,$0e,$00,$4c); str: 'st4.2d { v4, v5, v6, v7 }, [x19]')
  );

  ld1st1_multiple_post_data: array[1..128] of tinstrdata = (
    (bytes: ($20,$70,$cf,$0c); str: 'ld1.8b { v0 }, [x1], x15'),
    (bytes: ($20,$a0,$cf,$0c); str: 'ld1.8b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$60,$cf,$0c); str: 'ld1.8b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$20,$cf,$0c); str: 'ld1.8b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$70,$cf,$4c); str: 'ld1.16b { v0 }, [x1], x15'),
    (bytes: ($20,$a0,$cf,$4c); str: 'ld1.16b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$60,$cf,$4c); str: 'ld1.16b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$20,$cf,$4c); str: 'ld1.16b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$74,$cf,$0c); str: 'ld1.4h { v0 }, [x1], x15'),
    (bytes: ($20,$a4,$cf,$0c); str: 'ld1.4h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$64,$cf,$0c); str: 'ld1.4h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$24,$cf,$0c); str: 'ld1.4h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$74,$cf,$4c); str: 'ld1.8h { v0 }, [x1], x15'),
    (bytes: ($20,$a4,$cf,$4c); str: 'ld1.8h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$64,$cf,$4c); str: 'ld1.8h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$24,$cf,$4c); str: 'ld1.8h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$78,$cf,$0c); str: 'ld1.2s { v0 }, [x1], x15'),
    (bytes: ($20,$a8,$cf,$0c); str: 'ld1.2s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$68,$cf,$0c); str: 'ld1.2s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$28,$cf,$0c); str: 'ld1.2s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$78,$cf,$4c); str: 'ld1.4s { v0 }, [x1], x15'),
    (bytes: ($20,$a8,$cf,$4c); str: 'ld1.4s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$68,$cf,$4c); str: 'ld1.4s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$28,$cf,$4c); str: 'ld1.4s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$7c,$cf,$0c); str: 'ld1.1d { v0 }, [x1], x15'),
    (bytes: ($20,$ac,$cf,$0c); str: 'ld1.1d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$6c,$cf,$0c); str: 'ld1.1d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$2c,$cf,$0c); str: 'ld1.1d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$7c,$cf,$4c); str: 'ld1.2d { v0 }, [x1], x15'),
    (bytes: ($20,$ac,$cf,$4c); str: 'ld1.2d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$6c,$cf,$4c); str: 'ld1.2d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$2c,$cf,$4c); str: 'ld1.2d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$70,$8f,$0c); str: 'st1.8b { v0 }, [x1], x15'),
    (bytes: ($20,$a0,$8f,$0c); str: 'st1.8b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$60,$8f,$0c); str: 'st1.8b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$20,$8f,$0c); str: 'st1.8b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$70,$8f,$4c); str: 'st1.16b { v0 }, [x1], x15'),
    (bytes: ($20,$a0,$8f,$4c); str: 'st1.16b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$60,$8f,$4c); str: 'st1.16b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$20,$8f,$4c); str: 'st1.16b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$74,$8f,$0c); str: 'st1.4h { v0 }, [x1], x15'),
    (bytes: ($20,$a4,$8f,$0c); str: 'st1.4h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$64,$8f,$0c); str: 'st1.4h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$24,$8f,$0c); str: 'st1.4h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$74,$8f,$4c); str: 'st1.8h { v0 }, [x1], x15'),
    (bytes: ($20,$a4,$8f,$4c); str: 'st1.8h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$64,$8f,$4c); str: 'st1.8h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$24,$8f,$4c); str: 'st1.8h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$78,$8f,$0c); str: 'st1.2s { v0 }, [x1], x15'),
    (bytes: ($20,$a8,$8f,$0c); str: 'st1.2s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$68,$8f,$0c); str: 'st1.2s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$28,$8f,$0c); str: 'st1.2s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$78,$8f,$4c); str: 'st1.4s { v0 }, [x1], x15'),
    (bytes: ($20,$a8,$8f,$4c); str: 'st1.4s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$68,$8f,$4c); str: 'st1.4s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$28,$8f,$4c); str: 'st1.4s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$7c,$8f,$0c); str: 'st1.1d { v0 }, [x1], x15'),
    (bytes: ($20,$ac,$8f,$0c); str: 'st1.1d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$6c,$8f,$0c); str: 'st1.1d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$2c,$8f,$0c); str: 'st1.1d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$7c,$8f,$4c); str: 'st1.2d { v0 }, [x1], x15'),
    (bytes: ($20,$ac,$8f,$4c); str: 'st1.2d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$6c,$8f,$4c); str: 'st1.2d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$2c,$8f,$4c); str: 'st1.2d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$70,$df,$0c); str: 'ld1.8b { v0 }, [x1], #8'),
    (bytes: ($20,$a0,$df,$0c); str: 'ld1.8b { v0, v1 }, [x1], #16'),
    (bytes: ($20,$60,$df,$0c); str: 'ld1.8b { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$20,$df,$0c); str: 'ld1.8b { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$70,$df,$4c); str: 'ld1.16b { v0 }, [x1], #16'),
    (bytes: ($20,$a0,$df,$4c); str: 'ld1.16b { v0, v1 }, [x1], #32'),
    (bytes: ($20,$60,$df,$4c); str: 'ld1.16b { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$20,$df,$4c); str: 'ld1.16b { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$74,$df,$0c); str: 'ld1.4h { v0 }, [x1], #8'),
    (bytes: ($20,$a4,$df,$0c); str: 'ld1.4h { v0, v1 }, [x1], #16'),
    (bytes: ($20,$64,$df,$0c); str: 'ld1.4h { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$24,$df,$0c); str: 'ld1.4h { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$74,$df,$4c); str: 'ld1.8h { v0 }, [x1], #16'),
    (bytes: ($20,$a4,$df,$4c); str: 'ld1.8h { v0, v1 }, [x1], #32'),
    (bytes: ($20,$64,$df,$4c); str: 'ld1.8h { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$24,$df,$4c); str: 'ld1.8h { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$78,$df,$0c); str: 'ld1.2s { v0 }, [x1], #8'),
    (bytes: ($20,$a8,$df,$0c); str: 'ld1.2s { v0, v1 }, [x1], #16'),
    (bytes: ($20,$68,$df,$0c); str: 'ld1.2s { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$28,$df,$0c); str: 'ld1.2s { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$78,$df,$4c); str: 'ld1.4s { v0 }, [x1], #16'),
    (bytes: ($20,$a8,$df,$4c); str: 'ld1.4s { v0, v1 }, [x1], #32'),
    (bytes: ($20,$68,$df,$4c); str: 'ld1.4s { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$28,$df,$4c); str: 'ld1.4s { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$7c,$df,$0c); str: 'ld1.1d { v0 }, [x1], #8'),
    (bytes: ($20,$ac,$df,$0c); str: 'ld1.1d { v0, v1 }, [x1], #16'),
    (bytes: ($20,$6c,$df,$0c); str: 'ld1.1d { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$2c,$df,$0c); str: 'ld1.1d { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$7c,$df,$4c); str: 'ld1.2d { v0 }, [x1], #16'),
    (bytes: ($20,$ac,$df,$4c); str: 'ld1.2d { v0, v1 }, [x1], #32'),
    (bytes: ($20,$6c,$df,$4c); str: 'ld1.2d { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$2c,$df,$4c); str: 'ld1.2d { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$70,$9f,$0c); str: 'st1.8b { v0 }, [x1], #8'),
    (bytes: ($20,$a0,$9f,$0c); str: 'st1.8b { v0, v1 }, [x1], #16'),
    (bytes: ($20,$60,$9f,$0c); str: 'st1.8b { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$20,$9f,$0c); str: 'st1.8b { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$70,$9f,$4c); str: 'st1.16b { v0 }, [x1], #16'),
    (bytes: ($20,$a0,$9f,$4c); str: 'st1.16b { v0, v1 }, [x1], #32'),
    (bytes: ($20,$60,$9f,$4c); str: 'st1.16b { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$20,$9f,$4c); str: 'st1.16b { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$74,$9f,$0c); str: 'st1.4h { v0 }, [x1], #8'),
    (bytes: ($20,$a4,$9f,$0c); str: 'st1.4h { v0, v1 }, [x1], #16'),
    (bytes: ($20,$64,$9f,$0c); str: 'st1.4h { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$24,$9f,$0c); str: 'st1.4h { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$74,$9f,$4c); str: 'st1.8h { v0 }, [x1], #16'),
    (bytes: ($20,$a4,$9f,$4c); str: 'st1.8h { v0, v1 }, [x1], #32'),
    (bytes: ($20,$64,$9f,$4c); str: 'st1.8h { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$24,$9f,$4c); str: 'st1.8h { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$78,$9f,$0c); str: 'st1.2s { v0 }, [x1], #8'),
    (bytes: ($20,$a8,$9f,$0c); str: 'st1.2s { v0, v1 }, [x1], #16'),
    (bytes: ($20,$68,$9f,$0c); str: 'st1.2s { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$28,$9f,$0c); str: 'st1.2s { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$78,$9f,$4c); str: 'st1.4s { v0 }, [x1], #16'),
    (bytes: ($20,$a8,$9f,$4c); str: 'st1.4s { v0, v1 }, [x1], #32'),
    (bytes: ($20,$68,$9f,$4c); str: 'st1.4s { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$28,$9f,$4c); str: 'st1.4s { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$7c,$9f,$0c); str: 'st1.1d { v0 }, [x1], #8'),
    (bytes: ($20,$ac,$9f,$0c); str: 'st1.1d { v0, v1 }, [x1], #16'),
    (bytes: ($20,$6c,$9f,$0c); str: 'st1.1d { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$2c,$9f,$0c); str: 'st1.1d { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$7c,$9f,$4c); str: 'st1.2d { v0 }, [x1], #16'),
    (bytes: ($20,$ac,$9f,$4c); str: 'st1.2d { v0, v1 }, [x1], #32'),
    (bytes: ($20,$6c,$9f,$4c); str: 'st1.2d { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$2c,$9f,$4c); str: 'st1.2d { v0, v1, v2, v3 }, [x1], #64')
  );

  ld2st2_multiple_post_data: array[1..28] of tinstrdata = (
    (bytes: ($20,$80,$cf,$0c); str: 'ld2.8b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$80,$cf,$4c); str: 'ld2.16b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$84,$cf,$0c); str: 'ld2.4h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$84,$cf,$4c); str: 'ld2.8h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$88,$cf,$0c); str: 'ld2.2s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$88,$cf,$4c); str: 'ld2.4s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$8c,$cf,$4c); str: 'ld2.2d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$80,$8f,$0c); str: 'st2.8b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$80,$8f,$4c); str: 'st2.16b { v0, v1 }, [x1], x15'),
    (bytes: ($20,$84,$8f,$0c); str: 'st2.4h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$84,$8f,$4c); str: 'st2.8h { v0, v1 }, [x1], x15'),
    (bytes: ($20,$88,$8f,$0c); str: 'st2.2s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$88,$8f,$4c); str: 'st2.4s { v0, v1 }, [x1], x15'),
    (bytes: ($20,$8c,$8f,$4c); str: 'st2.2d { v0, v1 }, [x1], x15'),
    (bytes: ($20,$80,$df,$0c); str: 'ld2.8b { v0, v1 }, [x1], #16'),
    (bytes: ($20,$80,$df,$4c); str: 'ld2.16b { v0, v1 }, [x1], #32'),
    (bytes: ($20,$84,$df,$0c); str: 'ld2.4h { v0, v1 }, [x1], #16'),
    (bytes: ($20,$84,$df,$4c); str: 'ld2.8h { v0, v1 }, [x1], #32'),
    (bytes: ($20,$88,$df,$0c); str: 'ld2.2s { v0, v1 }, [x1], #16'),
    (bytes: ($20,$88,$df,$4c); str: 'ld2.4s { v0, v1 }, [x1], #32'),
    (bytes: ($20,$8c,$df,$4c); str: 'ld2.2d { v0, v1 }, [x1], #32'),
    (bytes: ($20,$80,$9f,$0c); str: 'st2.8b { v0, v1 }, [x1], #16'),
    (bytes: ($20,$80,$9f,$4c); str: 'st2.16b { v0, v1 }, [x1], #32'),
    (bytes: ($20,$84,$9f,$0c); str: 'st2.4h { v0, v1 }, [x1], #16'),
    (bytes: ($20,$84,$9f,$4c); str: 'st2.8h { v0, v1 }, [x1], #32'),
    (bytes: ($20,$88,$9f,$0c); str: 'st2.2s { v0, v1 }, [x1], #16'),
    (bytes: ($20,$88,$9f,$4c); str: 'st2.4s { v0, v1 }, [x1], #32'),
    (bytes: ($20,$8c,$9f,$4c); str: 'st2.2d { v0, v1 }, [x1], #32')
  );

  ld3st3_multiple_post_data: array[1..28] of tinstrdata = (
    (bytes: ($20,$40,$cf,$0c); str: 'ld3.8b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$40,$cf,$4c); str: 'ld3.16b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$44,$cf,$0c); str: 'ld3.4h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$44,$cf,$4c); str: 'ld3.8h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$48,$cf,$0c); str: 'ld3.2s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$48,$cf,$4c); str: 'ld3.4s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$4c,$cf,$4c); str: 'ld3.2d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$40,$8f,$0c); str: 'st3.8b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$40,$8f,$4c); str: 'st3.16b { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$44,$8f,$0c); str: 'st3.4h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$44,$8f,$4c); str: 'st3.8h { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$48,$8f,$0c); str: 'st3.2s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$48,$8f,$4c); str: 'st3.4s { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$4c,$8f,$4c); str: 'st3.2d { v0, v1, v2 }, [x1], x15'),
    (bytes: ($20,$40,$df,$0c); str: 'ld3.8b { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$40,$df,$4c); str: 'ld3.16b { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$44,$df,$0c); str: 'ld3.4h { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$44,$df,$4c); str: 'ld3.8h { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$48,$df,$0c); str: 'ld3.2s { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$48,$df,$4c); str: 'ld3.4s { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$4c,$df,$4c); str: 'ld3.2d { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$40,$9f,$0c); str: 'st3.8b { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$40,$9f,$4c); str: 'st3.16b { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$44,$9f,$0c); str: 'st3.4h { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$44,$9f,$4c); str: 'st3.8h { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$48,$9f,$0c); str: 'st3.2s { v0, v1, v2 }, [x1], #24'),
    (bytes: ($20,$48,$9f,$4c); str: 'st3.4s { v0, v1, v2 }, [x1], #48'),
    (bytes: ($20,$4c,$9f,$4c); str: 'st3.2d { v0, v1, v2 }, [x1], #48')
  );

  ld4st4_multiple_post_data: array[1..28] of tinstrdata = (
    (bytes: ($20,$00,$cf,$0c); str: 'ld4.8b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$00,$cf,$4c); str: 'ld4.16b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$04,$cf,$0c); str: 'ld4.4h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$04,$cf,$4c); str: 'ld4.8h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$08,$cf,$0c); str: 'ld4.2s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$08,$cf,$4c); str: 'ld4.4s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$0c,$cf,$4c); str: 'ld4.2d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$00,$8f,$0c); str: 'st4.8b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$00,$8f,$4c); str: 'st4.16b { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$04,$8f,$0c); str: 'st4.4h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$04,$8f,$4c); str: 'st4.8h { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$08,$8f,$0c); str: 'st4.2s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$08,$8f,$4c); str: 'st4.4s { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$0c,$8f,$4c); str: 'st4.2d { v0, v1, v2, v3 }, [x1], x15'),
    (bytes: ($20,$00,$df,$0c); str: 'ld4.8b { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$00,$df,$4c); str: 'ld4.16b { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$04,$df,$0c); str: 'ld4.4h { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$04,$df,$4c); str: 'ld4.8h { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$08,$df,$0c); str: 'ld4.2s { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$08,$df,$4c); str: 'ld4.4s { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$0c,$df,$4c); str: 'ld4.2d { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$00,$9f,$0c); str: 'st4.8b { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$00,$9f,$4c); str: 'st4.16b { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$04,$9f,$0c); str: 'st4.4h { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$04,$9f,$4c); str: 'st4.8h { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$08,$9f,$0c); str: 'st4.2s { v0, v1, v2, v3 }, [x1], #32'),
    (bytes: ($20,$08,$9f,$4c); str: 'st4.4s { v0, v1, v2, v3 }, [x1], #64'),
    (bytes: ($20,$0c,$9f,$4c); str: 'st4.2d { v0, v1, v2, v3 }, [x1], #64')
  );

  ld1r_data: array[1..24] of tinstrdata = (
    (bytes: ($44,$c0,$40,$0d); str: 'ld1r.8b { v4 }, [x2]'),
    (bytes: ($44,$c0,$c3,$0d); str: 'ld1r.8b { v4 }, [x2], x3'),
    (bytes: ($44,$c0,$40,$4d); str: 'ld1r.16b { v4 }, [x2]'),
    (bytes: ($44,$c0,$c3,$4d); str: 'ld1r.16b { v4 }, [x2], x3'),
    (bytes: ($44,$c4,$40,$0d); str: 'ld1r.4h { v4 }, [x2]'),
    (bytes: ($44,$c4,$c3,$0d); str: 'ld1r.4h { v4 }, [x2], x3'),
    (bytes: ($44,$c4,$40,$4d); str: 'ld1r.8h { v4 }, [x2]'),
    (bytes: ($44,$c4,$c3,$4d); str: 'ld1r.8h { v4 }, [x2], x3'),
    (bytes: ($44,$c8,$40,$0d); str: 'ld1r.2s { v4 }, [x2]'),
    (bytes: ($44,$c8,$c3,$0d); str: 'ld1r.2s { v4 }, [x2], x3'),
    (bytes: ($44,$c8,$40,$4d); str: 'ld1r.4s { v4 }, [x2]'),
    (bytes: ($44,$c8,$c3,$4d); str: 'ld1r.4s { v4 }, [x2], x3'),
    (bytes: ($44,$cc,$40,$0d); str: 'ld1r.1d { v4 }, [x2]'),
    (bytes: ($44,$cc,$c3,$0d); str: 'ld1r.1d { v4 }, [x2], x3'),
    (bytes: ($44,$cc,$40,$4d); str: 'ld1r.2d { v4 }, [x2]'),
    (bytes: ($44,$cc,$c3,$4d); str: 'ld1r.2d { v4 }, [x2], x3'),
    (bytes: ($44,$c0,$df,$0d); str: 'ld1r.8b { v4 }, [x2], #1'),
    (bytes: ($44,$c0,$df,$4d); str: 'ld1r.16b { v4 }, [x2], #1'),
    (bytes: ($44,$c4,$df,$0d); str: 'ld1r.4h { v4 }, [x2], #2'),
    (bytes: ($44,$c4,$df,$4d); str: 'ld1r.8h { v4 }, [x2], #2'),
    (bytes: ($44,$c8,$df,$0d); str: 'ld1r.2s { v4 }, [x2], #4'),
    (bytes: ($44,$c8,$df,$4d); str: 'ld1r.4s { v4 }, [x2], #4'),
    (bytes: ($44,$cc,$df,$0d); str: 'ld1r.1d { v4 }, [x2], #8'),
    (bytes: ($44,$cc,$df,$4d); str: 'ld1r.2d { v4 }, [x2], #8')
  );

  ld2r_data: array[1..24] of tinstrdata = (
    (bytes: ($44,$c0,$60,$0d); str: 'ld2r.8b { v4, v5 }, [x2]'),
    (bytes: ($44,$c0,$e3,$0d); str: 'ld2r.8b { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c0,$60,$4d); str: 'ld2r.16b { v4, v5 }, [x2]'),
    (bytes: ($44,$c0,$e3,$4d); str: 'ld2r.16b { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c4,$60,$0d); str: 'ld2r.4h { v4, v5 }, [x2]'),
    (bytes: ($44,$c4,$e3,$0d); str: 'ld2r.4h { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c4,$60,$4d); str: 'ld2r.8h { v4, v5 }, [x2]'),
    (bytes: ($44,$c4,$e3,$4d); str: 'ld2r.8h { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c8,$60,$0d); str: 'ld2r.2s { v4, v5 }, [x2]'),
    (bytes: ($44,$c8,$e3,$0d); str: 'ld2r.2s { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c8,$60,$4d); str: 'ld2r.4s { v4, v5 }, [x2]'),
    (bytes: ($44,$c8,$e3,$4d); str: 'ld2r.4s { v4, v5 }, [x2], x3'),
    (bytes: ($44,$cc,$60,$0d); str: 'ld2r.1d { v4, v5 }, [x2]'),
    (bytes: ($44,$cc,$e3,$0d); str: 'ld2r.1d { v4, v5 }, [x2], x3'),
    (bytes: ($44,$cc,$60,$4d); str: 'ld2r.2d { v4, v5 }, [x2]'),
    (bytes: ($44,$cc,$e3,$4d); str: 'ld2r.2d { v4, v5 }, [x2], x3'),
    (bytes: ($44,$c0,$ff,$0d); str: 'ld2r.8b { v4, v5 }, [x2], #2'),
    (bytes: ($44,$c0,$ff,$4d); str: 'ld2r.16b { v4, v5 }, [x2], #2'),
    (bytes: ($44,$c4,$ff,$0d); str: 'ld2r.4h { v4, v5 }, [x2], #4'),
    (bytes: ($44,$c4,$ff,$4d); str: 'ld2r.8h { v4, v5 }, [x2], #4'),
    (bytes: ($44,$c8,$ff,$0d); str: 'ld2r.2s { v4, v5 }, [x2], #8'),
    (bytes: ($44,$c8,$ff,$4d); str: 'ld2r.4s { v4, v5 }, [x2], #8'),
    (bytes: ($44,$cc,$ff,$0d); str: 'ld2r.1d { v4, v5 }, [x2], #16'),
    (bytes: ($44,$cc,$ff,$4d); str: 'ld2r.2d { v4, v5 }, [x2], #16')
  );

  ld3r_data: array[1..24] of tinstrdata = (
    (bytes: ($44,$e0,$40,$0d); str: 'ld3r.8b { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e0,$c3,$0d); str: 'ld3r.8b { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e0,$40,$4d); str: 'ld3r.16b { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e0,$c3,$4d); str: 'ld3r.16b { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e4,$40,$0d); str: 'ld3r.4h { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e4,$c3,$0d); str: 'ld3r.4h { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e4,$40,$4d); str: 'ld3r.8h { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e4,$c3,$4d); str: 'ld3r.8h { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e8,$40,$0d); str: 'ld3r.2s { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e8,$c3,$0d); str: 'ld3r.2s { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e8,$40,$4d); str: 'ld3r.4s { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$e8,$c3,$4d); str: 'ld3r.4s { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$ec,$40,$0d); str: 'ld3r.1d { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$ec,$c3,$0d); str: 'ld3r.1d { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$ec,$40,$4d); str: 'ld3r.2d { v4, v5, v6 }, [x2]'),
    (bytes: ($44,$ec,$c3,$4d); str: 'ld3r.2d { v4, v5, v6 }, [x2], x3'),
    (bytes: ($44,$e0,$df,$0d); str: 'ld3r.8b { v4, v5, v6 }, [x2], #3'),
    (bytes: ($44,$e0,$df,$4d); str: 'ld3r.16b { v4, v5, v6 }, [x2], #3'),
    (bytes: ($44,$e4,$df,$0d); str: 'ld3r.4h { v4, v5, v6 }, [x2], #6'),
    (bytes: ($44,$e4,$df,$4d); str: 'ld3r.8h { v4, v5, v6 }, [x2], #6'),
    (bytes: ($44,$e8,$df,$0d); str: 'ld3r.2s { v4, v5, v6 }, [x2], #12'),
    (bytes: ($44,$e8,$df,$4d); str: 'ld3r.4s { v4, v5, v6 }, [x2], #12'),
    (bytes: ($44,$ec,$df,$0d); str: 'ld3r.1d { v4, v5, v6 }, [x2], #24'),
    (bytes: ($44,$ec,$df,$4d); str: 'ld3r.2d { v4, v5, v6 }, [x2], #24')
  );

  ld4r_data: array[1..24] of tinstrdata = (
    (bytes: ($44,$e0,$60,$0d); str: 'ld4r.8b { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e0,$e3,$0d); str: 'ld4r.8b { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e0,$60,$4d); str: 'ld4r.16b { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e0,$e3,$4d); str: 'ld4r.16b { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e4,$60,$0d); str: 'ld4r.4h { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e4,$e3,$0d); str: 'ld4r.4h { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e4,$60,$4d); str: 'ld4r.8h { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e4,$e3,$4d); str: 'ld4r.8h { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e8,$60,$0d); str: 'ld4r.2s { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e8,$e3,$0d); str: 'ld4r.2s { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e8,$60,$4d); str: 'ld4r.4s { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$e8,$e3,$4d); str: 'ld4r.4s { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$ec,$60,$0d); str: 'ld4r.1d { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$ec,$e3,$0d); str: 'ld4r.1d { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$ec,$60,$4d); str: 'ld4r.2d { v4, v5, v6, v7 }, [x2]'),
    (bytes: ($44,$ec,$e3,$4d); str: 'ld4r.2d { v4, v5, v6, v7 }, [x2], x3'),
    (bytes: ($44,$e0,$ff,$0d); str: 'ld4r.8b { v4, v5, v6, v7 }, [x2], #4'),
    (bytes: ($45,$e0,$ff,$4d); str: 'ld4r.16b { v5, v6, v7, v8 }, [x2], #4'),
    (bytes: ($46,$e4,$ff,$0d); str: 'ld4r.4h { v6, v7, v8, v9 }, [x2], #8'),
    (bytes: ($41,$e4,$ff,$4d); str: 'ld4r.8h { v1, v2, v3, v4 }, [x2], #8'),
    (bytes: ($42,$e8,$ff,$0d); str: 'ld4r.2s { v2, v3, v4, v5 }, [x2], #16'),
    (bytes: ($43,$e8,$ff,$4d); str: 'ld4r.4s { v3, v4, v5, v6 }, [x2], #16'),
    (bytes: ($40,$ec,$ff,$0d); str: 'ld4r.1d { v0, v1, v2, v3 }, [x2], #32'),
    (bytes: ($44,$ec,$ff,$4d); str: 'ld4r.2d { v4, v5, v6, v7 }, [x2], #32')
  );

  ld1_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$14,$40,$4d); str: 'ld1.b { v4 }[13], [x3]'),
    (bytes: ($64,$50,$40,$0d); str: 'ld1.h { v4 }[2], [x3]'),
    (bytes: ($64,$80,$40,$4d); str: 'ld1.s { v4 }[2], [x3]'),
    (bytes: ($64,$84,$40,$4d); str: 'ld1.d { v4 }[1], [x3]'),
    (bytes: ($64,$14,$c5,$4d); str: 'ld1.b { v4 }[13], [x3], x5'),
    (bytes: ($64,$50,$c5,$0d); str: 'ld1.h { v4 }[2], [x3], x5'),
    (bytes: ($64,$80,$c5,$4d); str: 'ld1.s { v4 }[2], [x3], x5'),
    (bytes: ($64,$84,$c5,$4d); str: 'ld1.d { v4 }[1], [x3], x5'),
    (bytes: ($64,$14,$df,$4d); str: 'ld1.b { v4 }[13], [x3], #1'),
    (bytes: ($64,$50,$df,$0d); str: 'ld1.h { v4 }[2], [x3], #2'),
    (bytes: ($64,$80,$df,$4d); str: 'ld1.s { v4 }[2], [x3], #4'),
    (bytes: ($64,$84,$df,$4d); str: 'ld1.d { v4 }[1], [x3], #8')
  );

  ld2_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$14,$60,$4d); str: 'ld2.b { v4, v5 }[13], [x3]'),
    (bytes: ($64,$50,$60,$0d); str: 'ld2.h { v4, v5 }[2], [x3]'),
    (bytes: ($64,$80,$60,$4d); str: 'ld2.s { v4, v5 }[2], [x3]'),
    (bytes: ($64,$84,$60,$4d); str: 'ld2.d { v4, v5 }[1], [x3]'),
    (bytes: ($64,$14,$e5,$4d); str: 'ld2.b { v4, v5 }[13], [x3], x5'),
    (bytes: ($64,$50,$e5,$0d); str: 'ld2.h { v4, v5 }[2], [x3], x5'),
    (bytes: ($64,$80,$e5,$4d); str: 'ld2.s { v4, v5 }[2], [x3], x5'),
    (bytes: ($64,$84,$e5,$4d); str: 'ld2.d { v4, v5 }[1], [x3], x5'),
    (bytes: ($64,$14,$ff,$4d); str: 'ld2.b { v4, v5 }[13], [x3], #2'),
    (bytes: ($64,$50,$ff,$0d); str: 'ld2.h { v4, v5 }[2], [x3], #4'),
    (bytes: ($64,$80,$ff,$4d); str: 'ld2.s { v4, v5 }[2], [x3], #8'),
    (bytes: ($64,$84,$ff,$4d); str: 'ld2.d { v4, v5 }[1], [x3], #16')
  );

  ld3_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$34,$40,$4d); str: 'ld3.b { v4, v5, v6 }[13], [x3]'),
    (bytes: ($64,$70,$40,$0d); str: 'ld3.h { v4, v5, v6 }[2], [x3]'),
    (bytes: ($64,$a0,$40,$4d); str: 'ld3.s { v4, v5, v6 }[2], [x3]'),
    (bytes: ($64,$a4,$40,$4d); str: 'ld3.d { v4, v5, v6 }[1], [x3]'),
    (bytes: ($64,$34,$c5,$4d); str: 'ld3.b { v4, v5, v6 }[13], [x3], x5'),
    (bytes: ($64,$70,$c5,$0d); str: 'ld3.h { v4, v5, v6 }[2], [x3], x5'),
    (bytes: ($64,$a0,$c5,$4d); str: 'ld3.s { v4, v5, v6 }[2], [x3], x5'),
    (bytes: ($64,$a4,$c5,$4d); str: 'ld3.d { v4, v5, v6 }[1], [x3], x5'),
    (bytes: ($64,$34,$df,$4d); str: 'ld3.b { v4, v5, v6 }[13], [x3], #3'),
    (bytes: ($64,$70,$df,$0d); str: 'ld3.h { v4, v5, v6 }[2], [x3], #6'),
    (bytes: ($64,$a0,$df,$4d); str: 'ld3.s { v4, v5, v6 }[2], [x3], #12'),
    (bytes: ($64,$a4,$df,$4d); str: 'ld3.d { v4, v5, v6 }[1], [x3], #24')
  );

  ld4_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$34,$60,$4d); str: 'ld4.b { v4, v5, v6, v7 }[13], [x3]'),
    (bytes: ($64,$70,$60,$0d); str: 'ld4.h { v4, v5, v6, v7 }[2], [x3]'),
    (bytes: ($64,$a0,$60,$4d); str: 'ld4.s { v4, v5, v6, v7 }[2], [x3]'),
    (bytes: ($64,$a4,$60,$4d); str: 'ld4.d { v4, v5, v6, v7 }[1], [x3]'),
    (bytes: ($64,$34,$e5,$4d); str: 'ld4.b { v4, v5, v6, v7 }[13], [x3], x5'),
    (bytes: ($64,$70,$e5,$0d); str: 'ld4.h { v4, v5, v6, v7 }[2], [x3], x5'),
    (bytes: ($64,$a0,$e5,$4d); str: 'ld4.s { v4, v5, v6, v7 }[2], [x3], x5'),
    (bytes: ($64,$a4,$e5,$4d); str: 'ld4.d { v4, v5, v6, v7 }[1], [x3], x5'),
    (bytes: ($64,$34,$ff,$4d); str: 'ld4.b { v4, v5, v6, v7 }[13], [x3], #4'),
    (bytes: ($64,$70,$ff,$0d); str: 'ld4.h { v4, v5, v6, v7 }[2], [x3], #8'),
    (bytes: ($64,$a0,$ff,$4d); str: 'ld4.s { v4, v5, v6, v7 }[2], [x3], #16'),
    (bytes: ($64,$a4,$ff,$4d); str: 'ld4.d { v4, v5, v6, v7 }[1], [x3], #32')
  );

  st1_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$14,$00,$4d); str: 'st1.b { v4 }[13], [x3]'),
    (bytes: ($64,$50,$00,$0d); str: 'st1.h { v4 }[2], [x3]'),
    (bytes: ($64,$80,$00,$4d); str: 'st1.s { v4 }[2], [x3]'),
    (bytes: ($64,$84,$00,$4d); str: 'st1.d { v4 }[1], [x3]'),
    (bytes: ($64,$14,$85,$4d); str: 'st1.b { v4 }[13], [x3], x5'),
    (bytes: ($64,$50,$85,$0d); str: 'st1.h { v4 }[2], [x3], x5'),
    (bytes: ($64,$80,$85,$4d); str: 'st1.s { v4 }[2], [x3], x5'),
    (bytes: ($64,$84,$85,$4d); str: 'st1.d { v4 }[1], [x3], x5'),
    (bytes: ($64,$14,$9f,$4d); str: 'st1.b { v4 }[13], [x3], #1'),
    (bytes: ($64,$50,$9f,$0d); str: 'st1.h { v4 }[2], [x3], #2'),
    (bytes: ($64,$80,$9f,$4d); str: 'st1.s { v4 }[2], [x3], #4'),
    (bytes: ($64,$84,$9f,$4d); str: 'st1.d { v4 }[1], [x3], #8')
  );

  st2_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$14,$20,$4d); str: 'st2.b { v4, v5 }[13], [x3]'),
    (bytes: ($64,$50,$20,$0d); str: 'st2.h { v4, v5 }[2], [x3]'),
    (bytes: ($64,$80,$20,$4d); str: 'st2.s { v4, v5 }[2], [x3]'),
    (bytes: ($64,$84,$20,$4d); str: 'st2.d { v4, v5 }[1], [x3]'),
    (bytes: ($64,$14,$a5,$4d); str: 'st2.b { v4, v5 }[13], [x3], x5'),
    (bytes: ($64,$50,$a5,$0d); str: 'st2.h { v4, v5 }[2], [x3], x5'),
    (bytes: ($64,$80,$a5,$4d); str: 'st2.s { v4, v5 }[2], [x3], x5'),
    (bytes: ($64,$84,$a5,$4d); str: 'st2.d { v4, v5 }[1], [x3], x5'),
    (bytes: ($64,$14,$bf,$4d); str: 'st2.b { v4, v5 }[13], [x3], #2'),
    (bytes: ($64,$50,$bf,$0d); str: 'st2.h { v4, v5 }[2], [x3], #4'),
    (bytes: ($64,$80,$bf,$4d); str: 'st2.s { v4, v5 }[2], [x3], #8'),
    (bytes: ($64,$84,$bf,$4d); str: 'st2.d { v4, v5 }[1], [x3], #16')
  );

  st3_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$34,$00,$4d); str: 'st3.b { v4, v5, v6 }[13], [x3]'),
    (bytes: ($64,$70,$00,$0d); str: 'st3.h { v4, v5, v6 }[2], [x3]'),
    (bytes: ($64,$a0,$00,$4d); str: 'st3.s { v4, v5, v6 }[2], [x3]'),
    (bytes: ($64,$a4,$00,$4d); str: 'st3.d { v4, v5, v6 }[1], [x3]'),
    (bytes: ($64,$34,$85,$4d); str: 'st3.b { v4, v5, v6 }[13], [x3], x5'),
    (bytes: ($64,$70,$85,$0d); str: 'st3.h { v4, v5, v6 }[2], [x3], x5'),
    (bytes: ($64,$a0,$85,$4d); str: 'st3.s { v4, v5, v6 }[2], [x3], x5'),
    (bytes: ($64,$a4,$85,$4d); str: 'st3.d { v4, v5, v6 }[1], [x3], x5'),
    (bytes: ($64,$34,$9f,$4d); str: 'st3.b { v4, v5, v6 }[13], [x3], #3'),
    (bytes: ($64,$70,$9f,$0d); str: 'st3.h { v4, v5, v6 }[2], [x3], #6'),
    (bytes: ($64,$a0,$9f,$4d); str: 'st3.s { v4, v5, v6 }[2], [x3], #12'),
    (bytes: ($64,$a4,$9f,$4d); str: 'st3.d { v4, v5, v6 }[1], [x3], #24')
  );

  st4_data: array[1..12] of tinstrdata = (
    (bytes: ($64,$34,$20,$4d); str: 'st4.b { v4, v5, v6, v7 }[13], [x3]'),
    (bytes: ($64,$70,$20,$0d); str: 'st4.h { v4, v5, v6, v7 }[2], [x3]'),
    (bytes: ($64,$a0,$20,$4d); str: 'st4.s { v4, v5, v6, v7 }[2], [x3]'),
    (bytes: ($64,$a4,$20,$4d); str: 'st4.d { v4, v5, v6, v7 }[1], [x3]'),
    (bytes: ($64,$34,$a5,$4d); str: 'st4.b { v4, v5, v6, v7 }[13], [x3], x5'),
    (bytes: ($64,$70,$a5,$0d); str: 'st4.h { v4, v5, v6, v7 }[2], [x3], x5'),
    (bytes: ($64,$a0,$a5,$4d); str: 'st4.s { v4, v5, v6, v7 }[2], [x3], x5'),
    (bytes: ($64,$a4,$a5,$4d); str: 'st4.d { v4, v5, v6, v7 }[1], [x3], x5'),
    (bytes: ($64,$34,$bf,$4d); str: 'st4.b { v4, v5, v6, v7 }[13], [x3], #4'),
    (bytes: ($64,$70,$bf,$0d); str: 'st4.h { v4, v5, v6, v7 }[2], [x3], #8'),
    (bytes: ($64,$a0,$bf,$4d); str: 'st4.s { v4, v5, v6, v7 }[2], [x3], #16'),
    (bytes: ($64,$a4,$bf,$4d); str: 'st4.d { v4, v5, v6, v7 }[1], [x3], #32')
  );

  verbose_syntax_data: array[1..510] of tinstrdata = (
    (bytes: ($21,$70,$40,$0c); str: 'ld1.8b	{ v1 }, [x1]'),
    (bytes: ($22,$a0,$40,$0c); str: 'ld1.8b	{ v2, v3 }, [x1]'),
    (bytes: ($23,$60,$40,$0c); str: 'ld1.8b	{ v3, v4, v5 }, [x1]'),
    (bytes: ($24,$20,$40,$0c); str: 'ld1.8b	{ v4, v5, v6, v7 }, [x1]'),
    (bytes: ($21,$70,$40,$4c); str: 'ld1.16b	{ v1 }, [x1]'),
    (bytes: ($22,$a0,$40,$4c); str: 'ld1.16b	{ v2, v3 }, [x1]'),
    (bytes: ($23,$60,$40,$4c); str: 'ld1.16b	{ v3, v4, v5 }, [x1]'),
    (bytes: ($24,$20,$40,$4c); str: 'ld1.16b	{ v4, v5, v6, v7 }, [x1]'),
    (bytes: ($21,$74,$40,$0c); str: 'ld1.4h	{ v1 }, [x1]'),
    (bytes: ($22,$a4,$40,$0c); str: 'ld1.4h	{ v2, v3 }, [x1]'),
    (bytes: ($23,$64,$40,$0c); str: 'ld1.4h	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$24,$40,$0c); str: 'ld1.4h	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$74,$40,$4c); str: 'ld1.8h	{ v1 }, [x1]'),
    (bytes: ($22,$a4,$40,$4c); str: 'ld1.8h	{ v2, v3 }, [x1]'),
    (bytes: ($23,$64,$40,$4c); str: 'ld1.8h	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$24,$40,$4c); str: 'ld1.8h	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$78,$40,$0c); str: 'ld1.2s	{ v1 }, [x1]'),
    (bytes: ($22,$a8,$40,$0c); str: 'ld1.2s	{ v2, v3 }, [x1]'),
    (bytes: ($23,$68,$40,$0c); str: 'ld1.2s	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$28,$40,$0c); str: 'ld1.2s	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$78,$40,$4c); str: 'ld1.4s	{ v1 }, [x1]'),
    (bytes: ($22,$a8,$40,$4c); str: 'ld1.4s	{ v2, v3 }, [x1]'),
    (bytes: ($23,$68,$40,$4c); str: 'ld1.4s	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$28,$40,$4c); str: 'ld1.4s	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$7c,$40,$0c); str: 'ld1.1d	{ v1 }, [x1]'),
    (bytes: ($22,$ac,$40,$0c); str: 'ld1.1d	{ v2, v3 }, [x1]'),
    (bytes: ($23,$6c,$40,$0c); str: 'ld1.1d	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$2c,$40,$0c); str: 'ld1.1d	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$7c,$40,$4c); str: 'ld1.2d	{ v1 }, [x1]'),
    (bytes: ($22,$ac,$40,$4c); str: 'ld1.2d	{ v2, v3 }, [x1]'),
    (bytes: ($23,$6c,$40,$4c); str: 'ld1.2d	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$2c,$40,$4c); str: 'ld1.2d	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$70,$00,$0c); str: 'st1.8b	{ v1 }, [x1]'),
    (bytes: ($22,$a0,$00,$0c); str: 'st1.8b	{ v2, v3 }, [x1]'),
    (bytes: ($23,$60,$00,$0c); str: 'st1.8b	{ v3, v4, v5 }, [x1]'),
    (bytes: ($24,$20,$00,$0c); str: 'st1.8b	{ v4, v5, v6, v7 }, [x1]'),
    (bytes: ($21,$70,$00,$4c); str: 'st1.16b	{ v1 }, [x1]'),
    (bytes: ($22,$a0,$00,$4c); str: 'st1.16b	{ v2, v3 }, [x1]'),
    (bytes: ($23,$60,$00,$4c); str: 'st1.16b	{ v3, v4, v5 }, [x1]'),
    (bytes: ($24,$20,$00,$4c); str: 'st1.16b	{ v4, v5, v6, v7 }, [x1]'),
    (bytes: ($21,$74,$00,$0c); str: 'st1.4h	{ v1 }, [x1]'),
    (bytes: ($22,$a4,$00,$0c); str: 'st1.4h	{ v2, v3 }, [x1]'),
    (bytes: ($23,$64,$00,$0c); str: 'st1.4h	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$24,$00,$0c); str: 'st1.4h	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$74,$00,$4c); str: 'st1.8h	{ v1 }, [x1]'),
    (bytes: ($22,$a4,$00,$4c); str: 'st1.8h	{ v2, v3 }, [x1]'),
    (bytes: ($23,$64,$00,$4c); str: 'st1.8h	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$24,$00,$4c); str: 'st1.8h	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$78,$00,$0c); str: 'st1.2s	{ v1 }, [x1]'),
    (bytes: ($22,$a8,$00,$0c); str: 'st1.2s	{ v2, v3 }, [x1]'),
    (bytes: ($23,$68,$00,$0c); str: 'st1.2s	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$28,$00,$0c); str: 'st1.2s	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$78,$00,$4c); str: 'st1.4s	{ v1 }, [x1]'),
    (bytes: ($22,$a8,$00,$4c); str: 'st1.4s	{ v2, v3 }, [x1]'),
    (bytes: ($23,$68,$00,$4c); str: 'st1.4s	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$28,$00,$4c); str: 'st1.4s	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$7c,$00,$0c); str: 'st1.1d	{ v1 }, [x1]'),
    (bytes: ($22,$ac,$00,$0c); str: 'st1.1d	{ v2, v3 }, [x1]'),
    (bytes: ($23,$6c,$00,$0c); str: 'st1.1d	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$2c,$00,$0c); str: 'st1.1d	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($21,$7c,$00,$4c); str: 'st1.2d	{ v1 }, [x1]'),
    (bytes: ($22,$ac,$00,$4c); str: 'st1.2d	{ v2, v3 }, [x1]'),
    (bytes: ($23,$6c,$00,$4c); str: 'st1.2d	{ v3, v4, v5 }, [x1]'),
    (bytes: ($27,$2c,$00,$4c); str: 'st1.2d	{ v7, v8, v9, v10 }, [x1]'),
    (bytes: ($63,$82,$40,$0c); str: 'ld2.8b	{ v3, v4 }, [x19]'),
    (bytes: ($63,$82,$40,$4c); str: 'ld2.16b	{ v3, v4 }, [x19]'),
    (bytes: ($63,$86,$40,$0c); str: 'ld2.4h	{ v3, v4 }, [x19]'),
    (bytes: ($63,$86,$40,$4c); str: 'ld2.8h	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8a,$40,$0c); str: 'ld2.2s	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8a,$40,$4c); str: 'ld2.4s	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8e,$40,$4c); str: 'ld2.2d	{ v3, v4 }, [x19]'),
    (bytes: ($63,$82,$00,$0c); str: 'st2.8b	{ v3, v4 }, [x19]'),
    (bytes: ($63,$82,$00,$4c); str: 'st2.16b { v3, v4 }, [x19]'),
    (bytes: ($63,$86,$00,$0c); str: 'st2.4h	{ v3, v4 }, [x19]'),
    (bytes: ($63,$86,$00,$4c); str: 'st2.8h	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8a,$00,$0c); str: 'st2.2s	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8a,$00,$4c); str: 'st2.4s	{ v3, v4 }, [x19]'),
    (bytes: ($63,$8e,$00,$4c); str: 'st2.2d	{ v3, v4 }, [x19]'),
    (bytes: ($62,$42,$40,$0c); str: 'ld3.8b	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$42,$40,$4c); str: 'ld3.16b	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$46,$40,$0c); str: 'ld3.4h	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$46,$40,$4c); str: 'ld3.8h	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4a,$40,$0c); str: 'ld3.2s	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4a,$40,$4c); str: 'ld3.4s	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4e,$40,$4c); str: 'ld3.2d	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$42,$00,$0c); str: 'st3.8b	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$42,$00,$4c); str: 'st3.16b	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$46,$00,$0c); str: 'st3.4h	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$46,$00,$4c); str: 'st3.8h	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4a,$00,$0c); str: 'st3.2s	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4a,$00,$4c); str: 'st3.4s	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$4e,$00,$4c); str: 'st3.2d	{ v2, v3, v4 }, [x19]'),
    (bytes: ($62,$02,$40,$0c); str: 'ld4.8b	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$02,$40,$4c); str: 'ld4.16b	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$06,$40,$0c); str: 'ld4.4h	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$06,$40,$4c); str: 'ld4.8h	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0a,$40,$0c); str: 'ld4.2s	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0a,$40,$4c); str: 'ld4.4s	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0e,$40,$4c); str: 'ld4.2d	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$02,$00,$0c); str: 'st4.8b	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$02,$00,$4c); str: 'st4.16b	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$06,$00,$0c); str: 'st4.4h	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$06,$00,$4c); str: 'st4.8h	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0a,$00,$0c); str: 'st4.2s	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0a,$00,$4c); str: 'st4.4s	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($62,$0e,$00,$4c); str: 'st4.2d	{ v2, v3, v4, v5 }, [x19]'),
    (bytes: ($21,$70,$cf,$0c); str: 'ld1.8b	{ v1 }, [x1], x15'),
    (bytes: ($22,$a0,$cf,$0c); str: 'ld1.8b	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$60,$cf,$0c); str: 'ld1.8b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($24,$20,$cf,$0c); str: 'ld1.8b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($21,$70,$cf,$4c); str: 'ld1.16b	{ v1 }, [x1], x15'),
    (bytes: ($22,$a0,$cf,$4c); str: 'ld1.16b	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$60,$cf,$4c); str: 'ld1.16b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($24,$20,$cf,$4c); str: 'ld1.16b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($21,$74,$cf,$0c); str: 'ld1.4h	{ v1 }, [x1], x15'),
    (bytes: ($22,$a4,$cf,$0c); str: 'ld1.4h	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$64,$cf,$0c); str: 'ld1.4h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$24,$cf,$0c); str: 'ld1.4h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$74,$cf,$4c); str: 'ld1.8h	{ v1 }, [x1], x15'),
    (bytes: ($22,$a4,$cf,$4c); str: 'ld1.8h	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$64,$cf,$4c); str: 'ld1.8h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$24,$cf,$4c); str: 'ld1.8h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$78,$cf,$0c); str: 'ld1.2s	{ v1 }, [x1], x15'),
    (bytes: ($22,$a8,$cf,$0c); str: 'ld1.2s	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$68,$cf,$0c); str: 'ld1.2s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$28,$cf,$0c); str: 'ld1.2s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$78,$cf,$4c); str: 'ld1.4s	{ v1 }, [x1], x15'),
    (bytes: ($22,$a8,$cf,$4c); str: 'ld1.4s	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$68,$cf,$4c); str: 'ld1.4s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$28,$cf,$4c); str: 'ld1.4s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$7c,$cf,$0c); str: 'ld1.1d	{ v1 }, [x1], x15'),
    (bytes: ($22,$ac,$cf,$0c); str: 'ld1.1d	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$6c,$cf,$0c); str: 'ld1.1d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$2c,$cf,$0c); str: 'ld1.1d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$7c,$cf,$4c); str: 'ld1.2d	{ v1 }, [x1], x15'),
    (bytes: ($22,$ac,$cf,$4c); str: 'ld1.2d	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$6c,$cf,$4c); str: 'ld1.2d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$2c,$cf,$4c); str: 'ld1.2d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$70,$8f,$0c); str: 'st1.8b	{ v1 }, [x1], x15'),
    (bytes: ($22,$a0,$8f,$0c); str: 'st1.8b	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$60,$8f,$0c); str: 'st1.8b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($24,$20,$8f,$0c); str: 'st1.8b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($21,$70,$8f,$4c); str: 'st1.16b	{ v1 }, [x1], x15'),
    (bytes: ($22,$a0,$8f,$4c); str: 'st1.16b	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$60,$8f,$4c); str: 'st1.16b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($24,$20,$8f,$4c); str: 'st1.16b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($21,$74,$8f,$0c); str: 'st1.4h	{ v1 }, [x1], x15'),
    (bytes: ($22,$a4,$8f,$0c); str: 'st1.4h	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$64,$8f,$0c); str: 'st1.4h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$24,$8f,$0c); str: 'st1.4h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$74,$8f,$4c); str: 'st1.8h	{ v1 }, [x1], x15'),
    (bytes: ($22,$a4,$8f,$4c); str: 'st1.8h	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$64,$8f,$4c); str: 'st1.8h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$24,$8f,$4c); str: 'st1.8h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$78,$8f,$0c); str: 'st1.2s	{ v1 }, [x1], x15'),
    (bytes: ($22,$a8,$8f,$0c); str: 'st1.2s	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$68,$8f,$0c); str: 'st1.2s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$28,$8f,$0c); str: 'st1.2s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$78,$8f,$4c); str: 'st1.4s	{ v1 }, [x1], x15'),
    (bytes: ($22,$a8,$8f,$4c); str: 'st1.4s	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$68,$8f,$4c); str: 'st1.4s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$28,$8f,$4c); str: 'st1.4s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$7c,$8f,$0c); str: 'st1.1d	{ v1 }, [x1], x15'),
    (bytes: ($22,$ac,$8f,$0c); str: 'st1.1d	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$6c,$8f,$0c); str: 'st1.1d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$2c,$8f,$0c); str: 'st1.1d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$7c,$8f,$4c); str: 'st1.2d	{ v1 }, [x1], x15'),
    (bytes: ($22,$ac,$8f,$4c); str: 'st1.2d	{ v2, v3 }, [x1], x15'),
    (bytes: ($23,$6c,$8f,$4c); str: 'st1.2d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($27,$2c,$8f,$4c); str: 'st1.2d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($21,$70,$df,$0c); str: 'ld1.8b	{ v1 }, [x1], #8'),
    (bytes: ($22,$a0,$df,$0c); str: 'ld1.8b	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$60,$df,$0c); str: 'ld1.8b	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($24,$20,$df,$0c); str: 'ld1.8b	{ v4, v5, v6, v7 }, [x1], #32'),
    (bytes: ($21,$70,$df,$4c); str: 'ld1.16b	{ v1 }, [x1], #16'),
    (bytes: ($22,$a0,$df,$4c); str: 'ld1.16b	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$60,$df,$4c); str: 'ld1.16b	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($24,$20,$df,$4c); str: 'ld1.16b	{ v4, v5, v6, v7 }, [x1], #64'),
    (bytes: ($21,$74,$df,$0c); str: 'ld1.4h	{ v1 }, [x1], #8'),
    (bytes: ($22,$a4,$df,$0c); str: 'ld1.4h	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$64,$df,$0c); str: 'ld1.4h	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$24,$df,$0c); str: 'ld1.4h	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$74,$df,$4c); str: 'ld1.8h	{ v1 }, [x1], #16'),
    (bytes: ($22,$a4,$df,$4c); str: 'ld1.8h	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$64,$df,$4c); str: 'ld1.8h	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$24,$df,$4c); str: 'ld1.8h	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($21,$78,$df,$0c); str: 'ld1.2s	{ v1 }, [x1], #8'),
    (bytes: ($22,$a8,$df,$0c); str: 'ld1.2s	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$68,$df,$0c); str: 'ld1.2s	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$28,$df,$0c); str: 'ld1.2s	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$78,$df,$4c); str: 'ld1.4s	{ v1 }, [x1], #16'),
    (bytes: ($22,$a8,$df,$4c); str: 'ld1.4s	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$68,$df,$4c); str: 'ld1.4s	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$28,$df,$4c); str: 'ld1.4s	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($21,$7c,$df,$0c); str: 'ld1.1d	{ v1 }, [x1], #8'),
    (bytes: ($22,$ac,$df,$0c); str: 'ld1.1d	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$6c,$df,$0c); str: 'ld1.1d	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$2c,$df,$0c); str: 'ld1.1d	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$7c,$df,$4c); str: 'ld1.2d	{ v1 }, [x1], #16'),
    (bytes: ($22,$ac,$df,$4c); str: 'ld1.2d	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$6c,$df,$4c); str: 'ld1.2d	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$2c,$df,$4c); str: 'ld1.2d	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($21,$70,$9f,$0c); str: 'st1.8b	{ v1 }, [x1], #8'),
    (bytes: ($22,$a0,$9f,$0c); str: 'st1.8b	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$60,$9f,$0c); str: 'st1.8b	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($24,$20,$9f,$0c); str: 'st1.8b	{ v4, v5, v6, v7 }, [x1], #32'),
    (bytes: ($21,$70,$9f,$4c); str: 'st1.16b	{ v1 }, [x1], #16'),
    (bytes: ($22,$a0,$9f,$4c); str: 'st1.16b	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$60,$9f,$4c); str: 'st1.16b	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($24,$20,$9f,$4c); str: 'st1.16b	{ v4, v5, v6, v7 }, [x1], #64'),
    (bytes: ($21,$74,$9f,$0c); str: 'st1.4h	{ v1 }, [x1], #8'),
    (bytes: ($22,$a4,$9f,$0c); str: 'st1.4h	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$64,$9f,$0c); str: 'st1.4h	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$24,$9f,$0c); str: 'st1.4h	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$74,$9f,$4c); str: 'st1.8h	{ v1 }, [x1], #16'),
    (bytes: ($22,$a4,$9f,$4c); str: 'st1.8h	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$64,$9f,$4c); str: 'st1.8h	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$24,$9f,$4c); str: 'st1.8h	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($21,$78,$9f,$0c); str: 'st1.2s	{ v1 }, [x1], #8'),
    (bytes: ($22,$a8,$9f,$0c); str: 'st1.2s	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$68,$9f,$0c); str: 'st1.2s	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$28,$9f,$0c); str: 'st1.2s	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$78,$9f,$4c); str: 'st1.4s	{ v1 }, [x1], #16'),
    (bytes: ($22,$a8,$9f,$4c); str: 'st1.4s	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$68,$9f,$4c); str: 'st1.4s	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$28,$9f,$4c); str: 'st1.4s	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($21,$7c,$9f,$0c); str: 'st1.1d	{ v1 }, [x1], #8'),
    (bytes: ($22,$ac,$9f,$0c); str: 'st1.1d	{ v2, v3 }, [x1], #16'),
    (bytes: ($23,$6c,$9f,$0c); str: 'st1.1d	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($27,$2c,$9f,$0c); str: 'st1.1d	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($21,$7c,$9f,$4c); str: 'st1.2d	{ v1 }, [x1], #16'),
    (bytes: ($22,$ac,$9f,$4c); str: 'st1.2d	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$6c,$9f,$4c); str: 'st1.2d	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($27,$2c,$9f,$4c); str: 'st1.2d	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($22,$80,$cf,$0c); str: 'ld2.8b	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$80,$cf,$4c); str: 'ld2.16b	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$84,$cf,$0c); str: 'ld2.4h	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$84,$cf,$4c); str: 'ld2.8h	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$88,$cf,$0c); str: 'ld2.2s	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$88,$cf,$4c); str: 'ld2.4s	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$8c,$cf,$4c); str: 'ld2.2d	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$80,$8f,$0c); str: 'st2.8b	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$80,$8f,$4c); str: 'st2.16b	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$84,$8f,$0c); str: 'st2.4h	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$84,$8f,$4c); str: 'st2.8h	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$88,$8f,$0c); str: 'st2.2s	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$88,$8f,$4c); str: 'st2.4s	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$8c,$8f,$4c); str: 'st2.2d	{ v2, v3 }, [x1], x15'),
    (bytes: ($22,$80,$df,$0c); str: 'ld2.8b	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$80,$df,$4c); str: 'ld2.16b	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$84,$df,$0c); str: 'ld2.4h	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$84,$df,$4c); str: 'ld2.8h	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$88,$df,$0c); str: 'ld2.2s	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$88,$df,$4c); str: 'ld2.4s	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$8c,$df,$4c); str: 'ld2.2d	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$80,$9f,$0c); str: 'st2.8b	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$80,$9f,$4c); str: 'st2.16b	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$84,$9f,$0c); str: 'st2.4h	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$84,$9f,$4c); str: 'st2.8h	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$88,$9f,$0c); str: 'st2.2s	{ v2, v3 }, [x1], #16'),
    (bytes: ($22,$88,$9f,$4c); str: 'st2.4s	{ v2, v3 }, [x1], #32'),
    (bytes: ($22,$8c,$9f,$4c); str: 'st2.2d	{ v2, v3 }, [x1], #32'),
    (bytes: ($23,$40,$cf,$0c); str: 'ld3.8b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$40,$cf,$4c); str: 'ld3.16b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$44,$cf,$0c); str: 'ld3.4h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$44,$cf,$4c); str: 'ld3.8h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$48,$cf,$0c); str: 'ld3.2s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$48,$cf,$4c); str: 'ld3.4s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$4c,$cf,$4c); str: 'ld3.2d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$40,$8f,$0c); str: 'st3.8b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$40,$8f,$4c); str: 'st3.16b	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$44,$8f,$0c); str: 'st3.4h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$44,$8f,$4c); str: 'st3.8h	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$48,$8f,$0c); str: 'st3.2s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$48,$8f,$4c); str: 'st3.4s	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$4c,$8f,$4c); str: 'st3.2d	{ v3, v4, v5 }, [x1], x15'),
    (bytes: ($23,$40,$df,$0c); str: 'ld3.8b	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$40,$df,$4c); str: 'ld3.16b	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$44,$df,$0c); str: 'ld3.4h	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$44,$df,$4c); str: 'ld3.8h	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$48,$df,$0c); str: 'ld3.2s	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$48,$df,$4c); str: 'ld3.4s	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$4c,$df,$4c); str: 'ld3.2d	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$40,$9f,$0c); str: 'st3.8b	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$40,$9f,$4c); str: 'st3.16b	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$44,$9f,$0c); str: 'st3.4h	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$44,$9f,$4c); str: 'st3.8h	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$48,$9f,$0c); str: 'st3.2s	{ v3, v4, v5 }, [x1], #24'),
    (bytes: ($23,$48,$9f,$4c); str: 'st3.4s	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($23,$4c,$9f,$4c); str: 'st3.2d	{ v3, v4, v5 }, [x1], #48'),
    (bytes: ($24,$00,$cf,$0c); str: 'ld4.8b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($24,$00,$cf,$4c); str: 'ld4.16b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($27,$04,$cf,$0c); str: 'ld4.4h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$04,$cf,$4c); str: 'ld4.8h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$08,$cf,$0c); str: 'ld4.2s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$08,$cf,$4c); str: 'ld4.4s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$0c,$cf,$4c); str: 'ld4.2d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($24,$00,$8f,$0c); str: 'st4.8b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($24,$00,$8f,$4c); str: 'st4.16b	{ v4, v5, v6, v7 }, [x1], x15'),
    (bytes: ($27,$04,$8f,$0c); str: 'st4.4h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$04,$8f,$4c); str: 'st4.8h	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$08,$8f,$0c); str: 'st4.2s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$08,$8f,$4c); str: 'st4.4s	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($27,$0c,$8f,$4c); str: 'st4.2d	{ v7, v8, v9, v10 }, [x1], x15'),
    (bytes: ($24,$00,$df,$0c); str: 'ld4.8b	{ v4, v5, v6, v7 }, [x1], #32'),
    (bytes: ($24,$00,$df,$4c); str: 'ld4.16b	{ v4, v5, v6, v7 }, [x1], #64'),
    (bytes: ($27,$04,$df,$0c); str: 'ld4.4h	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($27,$04,$df,$4c); str: 'ld4.8h	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($27,$08,$df,$0c); str: 'ld4.2s	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($27,$08,$df,$4c); str: 'ld4.4s	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($27,$0c,$df,$4c); str: 'ld4.2d	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($24,$00,$9f,$0c); str: 'st4.8b	{ v4, v5, v6, v7 }, [x1], #32'),
    (bytes: ($24,$00,$9f,$4c); str: 'st4.16b	{ v4, v5, v6, v7 }, [x1], #64'),
    (bytes: ($27,$04,$9f,$0c); str: 'st4.4h	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($27,$04,$9f,$4c); str: 'st4.8h	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($27,$08,$9f,$0c); str: 'st4.2s	{ v7, v8, v9, v10 }, [x1], #32'),
    (bytes: ($27,$08,$9f,$4c); str: 'st4.4s	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($27,$0c,$9f,$4c); str: 'st4.2d	{ v7, v8, v9, v10 }, [x1], #64'),
    (bytes: ($4c,$c0,$40,$0d); str: 'ld1r.8b	{ v12 }, [x2]'),
    (bytes: ($4c,$c0,$c3,$0d); str: 'ld1r.8b	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c0,$40,$4d); str: 'ld1r.16b	{ v12 }, [x2]'),
    (bytes: ($4c,$c0,$c3,$4d); str: 'ld1r.16b	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c4,$40,$0d); str: 'ld1r.4h	{ v12 }, [x2]'),
    (bytes: ($4c,$c4,$c3,$0d); str: 'ld1r.4h	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c4,$40,$4d); str: 'ld1r.8h	{ v12 }, [x2]'),
    (bytes: ($4c,$c4,$c3,$4d); str: 'ld1r.8h	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c8,$40,$0d); str: 'ld1r.2s	{ v12 }, [x2]'),
    (bytes: ($4c,$c8,$c3,$0d); str: 'ld1r.2s	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c8,$40,$4d); str: 'ld1r.4s	{ v12 }, [x2]'),
    (bytes: ($4c,$c8,$c3,$4d); str: 'ld1r.4s	{ v12 }, [x2], x3'),
    (bytes: ($4c,$cc,$40,$0d); str: 'ld1r.1d	{ v12 }, [x2]'),
    (bytes: ($4c,$cc,$c3,$0d); str: 'ld1r.1d	{ v12 }, [x2], x3'),
    (bytes: ($4c,$cc,$40,$4d); str: 'ld1r.2d	{ v12 }, [x2]'),
    (bytes: ($4c,$cc,$c3,$4d); str: 'ld1r.2d	{ v12 }, [x2], x3'),
    (bytes: ($4c,$c0,$df,$0d); str: 'ld1r.8b	{ v12 }, [x2], #1'),
    (bytes: ($4c,$c0,$df,$4d); str: 'ld1r.16b	{ v12 }, [x2], #1'),
    (bytes: ($4c,$c4,$df,$0d); str: 'ld1r.4h	{ v12 }, [x2], #2'),
    (bytes: ($4c,$c4,$df,$4d); str: 'ld1r.8h	{ v12 }, [x2], #2'),
    (bytes: ($4c,$c8,$df,$0d); str: 'ld1r.2s	{ v12 }, [x2], #4'),
    (bytes: ($4c,$c8,$df,$4d); str: 'ld1r.4s	{ v12 }, [x2], #4'),
    (bytes: ($4c,$cc,$df,$0d); str: 'ld1r.1d	{ v12 }, [x2], #8'),
    (bytes: ($4c,$cc,$df,$4d); str: 'ld1r.2d	{ v12 }, [x2], #8'),
    (bytes: ($43,$c0,$60,$0d); str: 'ld2r.8b	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c0,$e3,$0d); str: 'ld2r.8b	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c0,$60,$4d); str: 'ld2r.16b	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c0,$e3,$4d); str: 'ld2r.16b	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c4,$60,$0d); str: 'ld2r.4h	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c4,$e3,$0d); str: 'ld2r.4h	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c4,$60,$4d); str: 'ld2r.8h	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c4,$e3,$4d); str: 'ld2r.8h	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c8,$60,$0d); str: 'ld2r.2s	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c8,$e3,$0d); str: 'ld2r.2s	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c8,$60,$4d); str: 'ld2r.4s	{ v3, v4 }, [x2]'),
    (bytes: ($43,$c8,$e3,$4d); str: 'ld2r.4s	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$cc,$60,$0d); str: 'ld2r.1d	{ v3, v4 }, [x2]'),
    (bytes: ($43,$cc,$e3,$0d); str: 'ld2r.1d	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$cc,$60,$4d); str: 'ld2r.2d	{ v3, v4 }, [x2]'),
    (bytes: ($43,$cc,$e3,$4d); str: 'ld2r.2d	{ v3, v4 }, [x2], x3'),
    (bytes: ($43,$c0,$ff,$0d); str: 'ld2r.8b	{ v3, v4 }, [x2], #2'),
    (bytes: ($43,$c0,$ff,$4d); str: 'ld2r.16b	{ v3, v4 }, [x2], #2'),
    (bytes: ($43,$c4,$ff,$0d); str: 'ld2r.4h	{ v3, v4 }, [x2], #4'),
    (bytes: ($43,$c4,$ff,$4d); str: 'ld2r.8h	{ v3, v4 }, [x2], #4'),
    (bytes: ($43,$c8,$ff,$0d); str: 'ld2r.2s	{ v3, v4 }, [x2], #8'),
    (bytes: ($43,$c8,$ff,$4d); str: 'ld2r.4s	{ v3, v4 }, [x2], #8'),
    (bytes: ($43,$cc,$ff,$0d); str: 'ld2r.1d	{ v3, v4 }, [x2], #16'),
    (bytes: ($43,$cc,$ff,$4d); str: 'ld2r.2d	{ v3, v4 }, [x2], #16'),
    (bytes: ($42,$e0,$40,$0d); str: 'ld3r.8b	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e0,$c3,$0d); str: 'ld3r.8b	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e0,$40,$4d); str: 'ld3r.16b	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e0,$c3,$4d); str: 'ld3r.16b	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e4,$40,$0d); str: 'ld3r.4h	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e4,$c3,$0d); str: 'ld3r.4h	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e4,$40,$4d); str: 'ld3r.8h	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e4,$c3,$4d); str: 'ld3r.8h	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e8,$40,$0d); str: 'ld3r.2s	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e8,$c3,$0d); str: 'ld3r.2s	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e8,$40,$4d); str: 'ld3r.4s	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$e8,$c3,$4d); str: 'ld3r.4s	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$ec,$40,$0d); str: 'ld3r.1d	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$ec,$c3,$0d); str: 'ld3r.1d	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$ec,$40,$4d); str: 'ld3r.2d	{ v2, v3, v4 }, [x2]'),
    (bytes: ($42,$ec,$c3,$4d); str: 'ld3r.2d	{ v2, v3, v4 }, [x2], x3'),
    (bytes: ($42,$e0,$df,$0d); str: 'ld3r.8b	{ v2, v3, v4 }, [x2], #3'),
    (bytes: ($42,$e0,$df,$4d); str: 'ld3r.16b	{ v2, v3, v4 }, [x2], #3'),
    (bytes: ($42,$e4,$df,$0d); str: 'ld3r.4h	{ v2, v3, v4 }, [x2], #6'),
    (bytes: ($42,$e4,$df,$4d); str: 'ld3r.8h	{ v2, v3, v4 }, [x2], #6'),
    (bytes: ($42,$e8,$df,$0d); str: 'ld3r.2s	{ v2, v3, v4 }, [x2], #12'),
    (bytes: ($42,$e8,$df,$4d); str: 'ld3r.4s	{ v2, v3, v4 }, [x2], #12'),
    (bytes: ($42,$ec,$df,$0d); str: 'ld3r.1d	{ v2, v3, v4 }, [x2], #24'),
    (bytes: ($42,$ec,$df,$4d); str: 'ld3r.2d	{ v2, v3, v4 }, [x2], #24'),
    (bytes: ($42,$e0,$60,$0d); str: 'ld4r.8b	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e0,$e3,$0d); str: 'ld4r.8b	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e0,$60,$4d); str: 'ld4r.16b	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e0,$e3,$4d); str: 'ld4r.16b	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e4,$60,$0d); str: 'ld4r.4h	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e4,$e3,$0d); str: 'ld4r.4h	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e4,$60,$4d); str: 'ld4r.8h	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e4,$e3,$4d); str: 'ld4r.8h	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e8,$60,$0d); str: 'ld4r.2s	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e8,$e3,$0d); str: 'ld4r.2s	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e8,$60,$4d); str: 'ld4r.4s	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$e8,$e3,$4d); str: 'ld4r.4s	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$ec,$60,$0d); str: 'ld4r.1d	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$ec,$e3,$0d); str: 'ld4r.1d	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$ec,$60,$4d); str: 'ld4r.2d	{ v2, v3, v4, v5 }, [x2]'),
    (bytes: ($42,$ec,$e3,$4d); str: 'ld4r.2d	{ v2, v3, v4, v5 }, [x2], x3'),
    (bytes: ($42,$e0,$ff,$0d); str: 'ld4r.8b	{ v2, v3, v4, v5 }, [x2], #4'),
    (bytes: ($42,$e0,$ff,$4d); str: 'ld4r.16b	{ v2, v3, v4, v5 }, [x2], #4'),
    (bytes: ($42,$e4,$ff,$0d); str: 'ld4r.4h	{ v2, v3, v4, v5 }, [x2], #8'),
    (bytes: ($42,$e4,$ff,$4d); str: 'ld4r.8h	{ v2, v3, v4, v5 }, [x2], #8'),
    (bytes: ($42,$e8,$ff,$0d); str: 'ld4r.2s	{ v2, v3, v4, v5 }, [x2], #16'),
    (bytes: ($42,$e8,$ff,$4d); str: 'ld4r.4s	{ v2, v3, v4, v5 }, [x2], #16'),
    (bytes: ($42,$ec,$ff,$0d); str: 'ld4r.1d	{ v2, v3, v4, v5 }, [x2], #32'),
    (bytes: ($42,$ec,$ff,$4d); str: 'ld4r.2d	{ v2, v3, v4, v5 }, [x2], #32'),
    (bytes: ($66,$14,$40,$4d); str: 'ld1.b	{ v6 }[13], [x3]'),
    (bytes: ($66,$50,$40,$0d); str: 'ld1.h	{ v6 }[2], [x3]'),
    (bytes: ($66,$80,$40,$4d); str: 'ld1.s	{ v6 }[2], [x3]'),
    (bytes: ($66,$84,$40,$4d); str: 'ld1.d	{ v6 }[1], [x3]'),
    (bytes: ($66,$14,$c5,$4d); str: 'ld1.b	{ v6 }[13], [x3], x5'),
    (bytes: ($66,$50,$c5,$0d); str: 'ld1.h	{ v6 }[2], [x3], x5'),
    (bytes: ($66,$80,$c5,$4d); str: 'ld1.s	{ v6 }[2], [x3], x5'),
    (bytes: ($66,$84,$c5,$4d); str: 'ld1.d	{ v6 }[1], [x3], x5'),
    (bytes: ($66,$14,$df,$4d); str: 'ld1.b	{ v6 }[13], [x3], #1'),
    (bytes: ($66,$50,$df,$0d); str: 'ld1.h	{ v6 }[2], [x3], #2'),
    (bytes: ($66,$80,$df,$4d); str: 'ld1.s	{ v6 }[2], [x3], #4'),
    (bytes: ($66,$84,$df,$4d); str: 'ld1.d	{ v6 }[1], [x3], #8'),
    (bytes: ($65,$14,$60,$4d); str: 'ld2.b	{ v5, v6 }[13], [x3]'),
    (bytes: ($65,$50,$60,$0d); str: 'ld2.h	{ v5, v6 }[2], [x3]'),
    (bytes: ($65,$80,$60,$4d); str: 'ld2.s	{ v5, v6 }[2], [x3]'),
    (bytes: ($65,$84,$60,$4d); str: 'ld2.d	{ v5, v6 }[1], [x3]'),
    (bytes: ($65,$14,$e5,$4d); str: 'ld2.b	{ v5, v6 }[13], [x3], x5'),
    (bytes: ($65,$50,$e5,$0d); str: 'ld2.h	{ v5, v6 }[2], [x3], x5'),
    (bytes: ($65,$80,$e5,$4d); str: 'ld2.s	{ v5, v6 }[2], [x3], x5'),
    (bytes: ($65,$84,$e5,$4d); str: 'ld2.d	{ v5, v6 }[1], [x3], x5'),
    (bytes: ($65,$14,$ff,$4d); str: 'ld2.b	{ v5, v6 }[13], [x3], #2'),
    (bytes: ($65,$50,$ff,$0d); str: 'ld2.h	{ v5, v6 }[2], [x3], #4'),
    (bytes: ($65,$80,$ff,$4d); str: 'ld2.s	{ v5, v6 }[2], [x3], #8'),
    (bytes: ($65,$84,$ff,$4d); str: 'ld2.d	{ v5, v6 }[1], [x3], #16'),
    (bytes: ($67,$34,$40,$4d); str: 'ld3.b	{ v7, v8, v9 }[13], [x3]'),
    (bytes: ($67,$70,$40,$0d); str: 'ld3.h	{ v7, v8, v9 }[2], [x3]'),
    (bytes: ($67,$a0,$40,$4d); str: 'ld3.s	{ v7, v8, v9 }[2], [x3]'),
    (bytes: ($67,$a4,$40,$4d); str: 'ld3.d	{ v7, v8, v9 }[1], [x3]'),
    (bytes: ($67,$34,$c5,$4d); str: 'ld3.b	{ v7, v8, v9 }[13], [x3], x5'),
    (bytes: ($67,$70,$c5,$0d); str: 'ld3.h	{ v7, v8, v9 }[2], [x3], x5'),
    (bytes: ($67,$a0,$c5,$4d); str: 'ld3.s	{ v7, v8, v9 }[2], [x3], x5'),
    (bytes: ($67,$a4,$c5,$4d); str: 'ld3.d	{ v7, v8, v9 }[1], [x3], x5'),
    (bytes: ($67,$34,$df,$4d); str: 'ld3.b	{ v7, v8, v9 }[13], [x3], #3'),
    (bytes: ($67,$70,$df,$0d); str: 'ld3.h	{ v7, v8, v9 }[2], [x3], #6'),
    (bytes: ($67,$a0,$df,$4d); str: 'ld3.s	{ v7, v8, v9 }[2], [x3], #12'),
    (bytes: ($67,$a4,$df,$4d); str: 'ld3.d	{ v7, v8, v9 }[1], [x3], #24'),
    (bytes: ($67,$34,$60,$4d); str: 'ld4.b	{ v7, v8, v9, v10 }[13], [x3]'),
    (bytes: ($67,$70,$60,$0d); str: 'ld4.h	{ v7, v8, v9, v10 }[2], [x3]'),
    (bytes: ($67,$a0,$60,$4d); str: 'ld4.s	{ v7, v8, v9, v10 }[2], [x3]'),
    (bytes: ($67,$a4,$60,$4d); str: 'ld4.d	{ v7, v8, v9, v10 }[1], [x3]'),
    (bytes: ($67,$34,$e5,$4d); str: 'ld4.b	{ v7, v8, v9, v10 }[13], [x3], x5'),
    (bytes: ($67,$70,$e5,$0d); str: 'ld4.h	{ v7, v8, v9, v10 }[2], [x3], x5'),
    (bytes: ($67,$a0,$e5,$4d); str: 'ld4.s	{ v7, v8, v9, v10 }[2], [x3], x5'),
    (bytes: ($67,$a4,$e5,$4d); str: 'ld4.d	{ v7, v8, v9, v10 }[1], [x3], x5'),
    (bytes: ($67,$34,$ff,$4d); str: 'ld4.b	{ v7, v8, v9, v10 }[13], [x3], #4'),
    (bytes: ($67,$70,$ff,$0d); str: 'ld4.h	{ v7, v8, v9, v10 }[2], [x3], #8'),
    (bytes: ($67,$a0,$ff,$4d); str: 'ld4.s	{ v7, v8, v9, v10 }[2], [x3], #16'),
    (bytes: ($67,$a4,$ff,$4d); str: 'ld4.d	{ v7, v8, v9, v10 }[1], [x3], #32'),
    (bytes: ($66,$14,$00,$4d); str: 'st1.b	{ v6 }[13], [x3]'),
    (bytes: ($66,$50,$00,$0d); str: 'st1.h	{ v6 }[2], [x3]'),
    (bytes: ($66,$80,$00,$4d); str: 'st1.s	{ v6 }[2], [x3]'),
    (bytes: ($66,$84,$00,$4d); str: 'st1.d	{ v6 }[1], [x3]'),
    (bytes: ($66,$14,$85,$4d); str: 'st1.b	{ v6 }[13], [x3], x5'),
    (bytes: ($66,$50,$85,$0d); str: 'st1.h	{ v6 }[2], [x3], x5'),
    (bytes: ($66,$80,$85,$4d); str: 'st1.s	{ v6 }[2], [x3], x5'),
    (bytes: ($66,$84,$85,$4d); str: 'st1.d	{ v6 }[1], [x3], x5'),
    (bytes: ($66,$14,$9f,$4d); str: 'st1.b	{ v6 }[13], [x3], #1'),
    (bytes: ($66,$50,$9f,$0d); str: 'st1.h	{ v6 }[2], [x3], #2'),
    (bytes: ($66,$80,$9f,$4d); str: 'st1.s	{ v6 }[2], [x3], #4'),
    (bytes: ($66,$84,$9f,$4d); str: 'st1.d	{ v6 }[1], [x3], #8'),
    (bytes: ($65,$14,$20,$4d); str: 'st2.b	{ v5, v6 }[13], [x3]'),
    (bytes: ($65,$50,$20,$0d); str: 'st2.h	{ v5, v6 }[2], [x3]'),
    (bytes: ($65,$80,$20,$4d); str: 'st2.s	{ v5, v6 }[2], [x3]'),
    (bytes: ($65,$84,$20,$4d); str: 'st2.d	{ v5, v6 }[1], [x3]'),
    (bytes: ($65,$14,$a5,$4d); str: 'st2.b	{ v5, v6 }[13], [x3], x5'),
    (bytes: ($65,$50,$a5,$0d); str: 'st2.h	{ v5, v6 }[2], [x3], x5'),
    (bytes: ($65,$80,$a5,$4d); str: 'st2.s	{ v5, v6 }[2], [x3], x5'),
    (bytes: ($65,$84,$a5,$4d); str: 'st2.d	{ v5, v6 }[1], [x3], x5'),
    (bytes: ($65,$14,$bf,$4d); str: 'st2.b	{ v5, v6 }[13], [x3], #2'),
    (bytes: ($65,$50,$bf,$0d); str: 'st2.h	{ v5, v6 }[2], [x3], #4'),
    (bytes: ($65,$80,$bf,$4d); str: 'st2.s	{ v5, v6 }[2], [x3], #8'),
    (bytes: ($65,$84,$bf,$4d); str: 'st2.d	{ v5, v6 }[1], [x3], #16'),
    (bytes: ($67,$34,$00,$4d); str: 'st3.b	{ v7, v8, v9 }[13], [x3]'),
    (bytes: ($67,$70,$00,$0d); str: 'st3.h	{ v7, v8, v9 }[2], [x3]'),
    (bytes: ($67,$a0,$00,$4d); str: 'st3.s	{ v7, v8, v9 }[2], [x3]'),
    (bytes: ($67,$a4,$00,$4d); str: 'st3.d	{ v7, v8, v9 }[1], [x3]'),
    (bytes: ($67,$34,$85,$4d); str: 'st3.b	{ v7, v8, v9 }[13], [x3], x5'),
    (bytes: ($67,$70,$85,$0d); str: 'st3.h	{ v7, v8, v9 }[2], [x3], x5'),
    (bytes: ($67,$a0,$85,$4d); str: 'st3.s	{ v7, v8, v9 }[2], [x3], x5'),
    (bytes: ($67,$a4,$85,$4d); str: 'st3.d	{ v7, v8, v9 }[1], [x3], x5'),
    (bytes: ($67,$34,$9f,$4d); str: 'st3.b	{ v7, v8, v9 }[13], [x3], #3'),
    (bytes: ($67,$70,$9f,$0d); str: 'st3.h	{ v7, v8, v9 }[2], [x3], #6'),
    (bytes: ($67,$a0,$9f,$4d); str: 'st3.s	{ v7, v8, v9 }[2], [x3], #12'),
    (bytes: ($67,$a4,$9f,$4d); str: 'st3.d	{ v7, v8, v9 }[1], [x3], #24'),
    (bytes: ($67,$34,$20,$4d); str: 'st4.b	{ v7, v8, v9, v10 }[13], [x3]'),
    (bytes: ($67,$70,$20,$0d); str: 'st4.h	{ v7, v8, v9, v10 }[2], [x3]'),
    (bytes: ($67,$a0,$20,$4d); str: 'st4.s	{ v7, v8, v9, v10 }[2], [x3]'),
    (bytes: ($67,$a4,$20,$4d); str: 'st4.d	{ v7, v8, v9, v10 }[1], [x3]'),
    (bytes: ($67,$34,$a5,$4d); str: 'st4.b	{ v7, v8, v9, v10 }[13], [x3], x5'),
    (bytes: ($67,$70,$a5,$0d); str: 'st4.h	{ v7, v8, v9, v10 }[2], [x3], x5'),
    (bytes: ($67,$a0,$a5,$4d); str: 'st4.s	{ v7, v8, v9, v10 }[2], [x3], x5'),
    (bytes: ($67,$a4,$a5,$4d); str: 'st4.d	{ v7, v8, v9, v10 }[1], [x3], x5'),
    (bytes: ($67,$34,$bf,$4d); str: 'st4.b	{ v7, v8, v9, v10 }[13], [x3], #4'),
    (bytes: ($67,$70,$bf,$0d); str: 'st4.h	{ v7, v8, v9, v10 }[2], [x3], #8'),
    (bytes: ($67,$a0,$bf,$4d); str: 'st4.s	{ v7, v8, v9, v10 }[2], [x3], #16'),
    (bytes: ($67,$a4,$bf,$4d); str: 'st4.d	{ v7, v8, v9, v10 }[1], [x3], #32')
  );

function check(const name: string; proc: codepointer; const checkdata: array of tinstrdata): boolean;
  var
    i, j: longint;
  begin
    result:=true;
    for i:=low(checkdata) to high(checkdata) do
      begin
        for j:=low(checkdata[i].bytes) to high(checkdata[i].bytes) do
          if byte((proc+i*4+j)^) <> checkdata[i].bytes[j] then
            begin
              writeln('Mismatch in procedure ', name, ' for instruction ', i, ': ', checkdata[i].str);
              result:=false;
            end;
      end;
  end;

var
  error: boolean;
begin
  error:=not check('ld1st1_multiple', @ld1st1_multiple, ld1st1_multiple_data);
  error:=not check('ld2st2_multiple', @ld2st2_multiple, ld2st2_multiple_data) or error;
  error:=not check('ld3st3_multiple', @ld3st3_multiple, ld3st3_multiple_data) or error;
  error:=not check('ld4st4_multiple', @ld4st4_multiple, ld4st4_multiple_data) or error;
  error:=not check('ld1st1_multiple_post', @ld1st1_multiple_post, ld1st1_multiple_post_data) or error;
  error:=not check('ld2st2_multiple_post', @ld2st2_multiple_post, ld2st2_multiple_post_data) or error;
  error:=not check('ld3st3_multiple_post', @ld3st3_multiple_post, ld3st3_multiple_post_data) or error;
  error:=not check('ld4st4_multiple_post', @ld4st4_multiple_post, ld4st4_multiple_post_data) or error;
  error:=not check('ld1', @ld1, ld1_data) or error;
  error:=not check('ld2', @ld2, ld2_data) or error;
  error:=not check('ld3', @ld3, ld3_data) or error;
  error:=not check('ld4', @ld4, ld4_data) or error;
  error:=not check('ld1r', @ld1r, ld1r_data) or error;
  error:=not check('ld2r', @ld2r, ld2r_data) or error;
  error:=not check('ld3r', @ld3r, ld3r_data) or error;
  error:=not check('ld4r', @ld4r, ld4r_data) or error;
  error:=not check('st1', @st1, st1_data) or error;
  error:=not check('st2', @st2, st2_data) or error;
  error:=not check('st3', @st3, st3_data) or error;
  error:=not check('st4', @st4, st4_data) or error;
  error:=not check('verbose_syntax', @verbose_syntax, verbose_syntax_data) or error;
  halt(ord(error));
end.
