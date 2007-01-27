{ %opt=-Ooregvar }

{$ifdef fpc}
{$packset 1}
{$endif fpc}
{$z1}

type
  ta = set of 0..7;
  tb = set of 0..15;
  tc = set of 0..23;
  td = set of 0..31;
  te = set of 0..127;

var
  ve: te;
  vd: td;
  vc: tc;
  vb: tb;
  va: ta;
  b1,b2,b3: byte;
begin
  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  va := [3..4];
  vb := va;
  if b1 <> $ff then
    halt(1);
  if va <> [3..4] then
    halt(1);
  if vb <> [3..4] then
    halt(1);
  if vc <> [] then
    halt(1);
  if vd <> [] then
    halt(1);
  if ve <> [] then
    halt(1);
  vc := va;
  if b1 <> $ff then
    halt(1);
  if va <> [3..4] then
    halt(1);
  if vb <> [3..4] then
    halt(1);
  if vc <> [3..4] then
    halt(1);
  if vd <> [] then
    halt(1);
  if ve <> [] then
    halt(1);
  vd := va;
  if b1 <> $ff then
    halt(1);
  if va <> [3..4] then
    halt(1);
  if vb <> [3..4] then
    halt(1);
  if vc <> [3..4] then
    halt(1);
  if vd <> [3..4] then
    halt(1);
  if ve <> [] then
    halt(1);
  ve := va;
  if b1 <> $ff then
    halt(1);
  if va <> [3..4] then
    halt(1);
  if vb <> [3..4] then
    halt(1);
  if vc <> [3..4] then
    halt(1);
  if vd <> [3..4] then
    halt(1);
  if ve <> [3..4] then
    halt(1);

  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vb := [0,2,3];
  va := vb;
  if b1 <> $ff then
    halt(2);
  if va <> [0,2,3] then
    halt(2);
  if vb <> [0,2,3] then
    halt(2);
  if vc <> [] then
    halt(2);
  if vd <> [] then
    halt(2);
  if ve <> [] then
    halt(2);
  vc := vb;
  if b1 <> $ff then
    halt(2);
  if va <> [0,2,3] then
    halt(2);
  if vb <> [0,2,3] then
    halt(2);
  if vc <> [0,2,3] then
    halt(2);
  if vd <> [] then
    halt(2);
  if ve <> [] then
    halt(2);
  vd := vb;
  if b1 <> $ff then
    halt(2);
  if va <> [0,2,3] then
    halt(2);
  if vb <> [0,2,3] then
    halt(2);
  if vc <> [0,2,3] then
    halt(2);
  if vd <> [0,2,3] then
    halt(2);
  if ve <> [] then
    halt(2);
  ve := vb;
  if va <> [0,2,3] then
    halt(2);
  if vb <> [0,2,3] then
    halt(2);
  if vc <> [0,2,3] then
    halt(2);
  if vd <> [0,2,3] then
    halt(2);
  if ve <> [0,2,3] then
    halt(2);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vc := [5,6,7];
  va := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [5,6,7] then
    halt(3);
  if vc <> [5,6,7] then
    halt(3);
  if vb <> [] then
    halt(3);
  if vd <> [] then
    halt(3);
  if ve <> [] then
    halt(3);
  vb := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [5,6,7] then
    halt(3);
  if vb <> [5,6,7] then
    halt(3);
  if vc <> [5,6,7] then
    halt(3);
  if vd <> [] then
    halt(3);
  if ve <> [] then
    halt(3);
  vd := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [5,6,7] then
    halt(3);
  if vb <> [5,6,7] then
    halt(3);
  if vc <> [5,6,7] then
    halt(3);
  if vd <> [5,6,7] then
    halt(3);
  if ve <> [] then
    halt(3);
  ve := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [5,6,7] then
    halt(3);
  if vb <> [5,6,7] then
    halt(3);
  if vc <> [5,6,7] then
    halt(3);
  if vd <> [5,6,7] then
    halt(3);
  if ve <> [5,6,7] then
    halt(3);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vd := [1,3,5];
  va := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [1,3,5] then
    halt(4);
  if vd <> [1,3,5] then
    halt(4);
  if vc <> [] then
    halt(4);
  if vb <> [] then
    halt(4);
  if ve <> [] then
    halt(4);
  vb := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [1,3,5] then
    halt(4);
  if vb <> [1,3,5] then
    halt(4);
  if vd <> [1,3,5] then
    halt(4);
  if vc <> [] then
    halt(4);
  if ve <> [] then
    halt(4);
  vc := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [1,3,5] then
    halt(4);
  if vb <> [1,3,5] then
    halt(4);
  if vc <> [1,3,5] then
    halt(4);
  if vd <> [1,3,5] then
    halt(4);
  if ve <> [] then
    halt(4);
  ve := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [1,3,5] then
    halt(4);
  if vb <> [1,3,5] then
    halt(4);
  if vc <> [1,3,5] then
    halt(4);
  if vd <> [1,3,5] then
    halt(4);
  if ve <> [1,3,5] then
    halt(4);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  ve := [0,7];
  va := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,7] then
    halt(5);
  if ve <> [0,7] then
    halt(5);
  if vc <> [] then
    halt(5);
  if vd <> [] then
    halt(5);
  if vb <> [] then
    halt(5);
  vb := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,7] then
    halt(5);
  if vb <> [0,7] then
    halt(5);
  if ve <> [0,7] then
    halt(5);
  if vd <> [] then
    halt(5);
  if vc <> [] then
    halt(5);
  vc := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,7] then
    halt(5);
  if vb <> [0,7] then
    halt(5);
  if vc <> [0,7] then
    halt(5);
  if ve <> [0,7] then
    halt(5);
  if vd <> [] then
    halt(5);
  vd := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,7] then
    halt(5);
  if vb <> [0,7] then
    halt(5);
  if vc <> [0,7] then
    halt(5);
  if vd <> [0,7] then
    halt(5);
  if ve <> [0,7] then
    halt(5);

end.
