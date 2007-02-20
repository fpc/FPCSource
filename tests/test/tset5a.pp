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
  r: packed record
    ve: te;
    vd: td;
    vc: tc;
    vb: tb;
    va: ta;
    b1,b2,b3: byte;
  end;
begin
  r.b1 := $ff;
  r.b2 := $ff;
  r.b3 := $ff;
  r.va := [];
  r.vb := [];
  r.vc := [];
  r.vd := [];
  r.ve := [];

  r.va := [3..4];
  r.vb := r.va;
  if r.b1 <> $ff then
    halt(1);
  if r.va <> [3..4] then
    halt(1);
  if r.vb <> [3..4] then
    halt(1);
  if r.vc <> [] then
    halt(1);
  if r.vd <> [] then
    halt(1);
  if r.ve <> [] then
    halt(1);
  r.vc := r.va;
  if r.b1 <> $ff then
    halt(1);
  if r.va <> [3..4] then
    halt(1);
  if r.vb <> [3..4] then
    halt(1);
  if r.vc <> [3..4] then
    halt(1);
  if r.vd <> [] then
    halt(1);
  if r.ve <> [] then
    halt(1);
  r.vd := r.va;
  if r.b1 <> $ff then
    halt(1);
  if r.va <> [3..4] then
    halt(1);
  if r.vb <> [3..4] then
    halt(1);
  if r.vc <> [3..4] then
    halt(1);
  if r.vd <> [3..4] then
    halt(1);
  if r.ve <> [] then
    halt(1);
  r.ve := r.va;
  if r.b1 <> $ff then
    halt(1);
  if r.va <> [3..4] then
    halt(1);
  if r.vb <> [3..4] then
    halt(1);
  if r.vc <> [3..4] then
    halt(1);
  if r.vd <> [3..4] then
    halt(1);
  if r.ve <> [3..4] then
    halt(1);

  r.b1 := $ff;
  r.b2 := $ff;
  r.b3 := $ff;
  r.va := [];
  r.vb := [];
  r.vc := [];
  r.vd := [];
  r.ve := [];

  r.vb := [0,2,3];
  r.va := r.vb;
  if r.b1 <> $ff then
    halt(2);
  if r.va <> [0,2,3] then
    halt(2);
  if r.vb <> [0,2,3] then
    halt(2);
  if r.vc <> [] then
    halt(2);
  if r.vd <> [] then
    halt(2);
  if r.ve <> [] then
    halt(2);
  r.vc := r.vb;
  if r.b1 <> $ff then
    halt(2);
  if r.va <> [0,2,3] then
    halt(2);
  if r.vb <> [0,2,3] then
    halt(2);
  if r.vc <> [0,2,3] then
    halt(2);
  if r.vd <> [] then
    halt(2);
  if r.ve <> [] then
    halt(2);
  r.vd := r.vb;
  if r.b1 <> $ff then
    halt(2);
  if r.va <> [0,2,3] then
    halt(2);
  if r.vb <> [0,2,3] then
    halt(2);
  if r.vc <> [0,2,3] then
    halt(2);
  if r.vd <> [0,2,3] then
    halt(2);
  if r.ve <> [] then
    halt(2);
  r.ve := r.vb;
  if r.va <> [0,2,3] then
    halt(2);
  if r.vb <> [0,2,3] then
    halt(2);
  if r.vc <> [0,2,3] then
    halt(2);
  if r.vd <> [0,2,3] then
    halt(2);
  if r.ve <> [0,2,3] then
    halt(2);


  r.b1 := $ff;
  r.b2 := $ff;
  r.b3 := $ff;
  r.va := [];
  r.vb := [];
  r.vc := [];
  r.vd := [];
  r.ve := [];

  r.vc := [5,6,7];
  r.va := r.vc;
  if r.b1 <> $ff then
    halt(3);
  if r.va <> [5,6,7] then
    halt(3);
  if r.vc <> [5,6,7] then
    halt(3);
  if r.vb <> [] then
    halt(3);
  if r.vd <> [] then
    halt(3);
  if r.ve <> [] then
    halt(3);
  r.vb := r.vc;
  if r.b1 <> $ff then
    halt(3);
  if r.va <> [5,6,7] then
    halt(3);
  if r.vb <> [5,6,7] then
    halt(3);
  if r.vc <> [5,6,7] then
    halt(3);
  if r.vd <> [] then
    halt(3);
  if r.ve <> [] then
    halt(3);
  r.vd := r.vc;
  if r.b1 <> $ff then
    halt(3);
  if r.va <> [5,6,7] then
    halt(3);
  if r.vb <> [5,6,7] then
    halt(3);
  if r.vc <> [5,6,7] then
    halt(3);
  if r.vd <> [5,6,7] then
    halt(3);
  if r.ve <> [] then
    halt(3);
  r.ve := r.vc;
  if r.b1 <> $ff then
    halt(3);
  if r.va <> [5,6,7] then
    halt(3);
  if r.vb <> [5,6,7] then
    halt(3);
  if r.vc <> [5,6,7] then
    halt(3);
  if r.vd <> [5,6,7] then
    halt(3);
  if r.ve <> [5,6,7] then
    halt(3);


  r.b1 := $ff;
  r.b2 := $ff;
  r.b3 := $ff;
  r.va := [];
  r.vb := [];
  r.vc := [];
  r.vd := [];
  r.ve := [];

  r.vd := [1,3,5];
  r.va := r.vd;
  if r.b1 <> $ff then
    halt(4);
  if r.va <> [1,3,5] then
    halt(4);
  if r.vd <> [1,3,5] then
    halt(4);
  if r.vc <> [] then
    halt(4);
  if r.vb <> [] then
    halt(4);
  if r.ve <> [] then
    halt(4);
  r.vb := r.vd;
  if r.b1 <> $ff then
    halt(4);
  if r.va <> [1,3,5] then
    halt(4);
  if r.vb <> [1,3,5] then
    halt(4);
  if r.vd <> [1,3,5] then
    halt(4);
  if r.vc <> [] then
    halt(4);
  if r.ve <> [] then
    halt(4);
  r.vc := r.vd;
  if r.b1 <> $ff then
    halt(4);
  if r.va <> [1,3,5] then
    halt(4);
  if r.vb <> [1,3,5] then
    halt(4);
  if r.vc <> [1,3,5] then
    halt(4);
  if r.vd <> [1,3,5] then
    halt(4);
  if r.ve <> [] then
    halt(4);
  r.ve := r.vd;
  if r.b1 <> $ff then
    halt(4);
  if r.va <> [1,3,5] then
    halt(4);
  if r.vb <> [1,3,5] then
    halt(4);
  if r.vc <> [1,3,5] then
    halt(4);
  if r.vd <> [1,3,5] then
    halt(4);
  if r.ve <> [1,3,5] then
    halt(4);


  r.b1 := $ff;
  r.b2 := $ff;
  r.b3 := $ff;
  r.va := [];
  r.vb := [];
  r.vc := [];
  r.vd := [];
  r.ve := [];

  r.ve := [0,7];
  r.va := r.ve;
  if r.b1 <> $ff then
    halt(5);
  if r.va <> [0,7] then
    halt(5);
  if r.ve <> [0,7] then
    halt(5);
  if r.vc <> [] then
    halt(5);
  if r.vd <> [] then
    halt(5);
  if r.vb <> [] then
    halt(5);
  r.vb := r.ve;
  if r.b1 <> $ff then
    halt(5);
  if r.va <> [0,7] then
    halt(5);
  if r.vb <> [0,7] then
    halt(5);
  if r.ve <> [0,7] then
    halt(5);
  if r.vd <> [] then
    halt(5);
  if r.vc <> [] then
    halt(5);
  r.vc := r.ve;
  if r.b1 <> $ff then
    halt(5);
  if r.va <> [0,7] then
    halt(5);
  if r.vb <> [0,7] then
    halt(5);
  if r.vc <> [0,7] then
    halt(5);
  if r.ve <> [0,7] then
    halt(5);
  if r.vd <> [] then
    halt(5);
  r.vd := r.ve;
  if r.b1 <> $ff then
    halt(5);
  if r.va <> [0,7] then
    halt(5);
  if r.vb <> [0,7] then
    halt(5);
  if r.vc <> [0,7] then
    halt(5);
  if r.vd <> [0,7] then
    halt(5);
  if r.ve <> [0,7] then
    halt(5);

end.
