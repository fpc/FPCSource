{ %opt=-Ooregvar }

{$ifdef fpc}
{$packset 1}
{$endif fpc}
{$z1}

type
  ta = set of 0..700;
  tb = set of 0..1500;
  tc = set of 0..2300;
  td = set of 0..3100;
  te = set of 0..12700;

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

  va := [300..400];
  vb := va;
  if b1 <> $ff then
    halt(1);
  if va <> [300..400] then
    halt(1);
  if vb <> [300..400] then
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
  if va <> [300..400] then
    halt(1);
  if vb <> [300..400] then
    halt(1);
  if vc <> [300..400] then
    halt(1);
  if vd <> [] then
    halt(1);
  if ve <> [] then
    halt(1);
  vd := va;
  if b1 <> $ff then
    halt(1);
  if va <> [300..400] then
    halt(1);
  if vb <> [300..400] then
    halt(1);
  if vc <> [300..400] then
    halt(1);
  if vd <> [300..400] then
    halt(1);
  if ve <> [] then
    halt(1);
  ve := va;
  if b1 <> $ff then
    halt(1);
  if va <> [300..400] then
    halt(1);
  if vb <> [300..400] then
    halt(1);
  if vc <> [300..400] then
    halt(1);
  if vd <> [300..400] then
    halt(1);
  if ve <> [300..400] then
    halt(1);

  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vb := [0,200,300];
  va := vb;
  if b1 <> $ff then
    halt(2);
  if va <> [0,200,300] then
    halt(2);
  if vb <> [0,200,300] then
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
  if va <> [0,200,300] then
    halt(2);
  if vb <> [0,200,300] then
    halt(2);
  if vc <> [0,200,300] then
    halt(2);
  if vd <> [] then
    halt(2);
  if ve <> [] then
    halt(2);
  vd := vb;
  if b1 <> $ff then
    halt(2);
  if va <> [0,200,300] then
    halt(2);
  if vb <> [0,200,300] then
    halt(2);
  if vc <> [0,200,300] then
    halt(2);
  if vd <> [0,200,300] then
    halt(2);
  if ve <> [] then
    halt(2);
  ve := vb;
  if va <> [0,200,300] then
    halt(2);
  if vb <> [0,200,300] then
    halt(2);
  if vc <> [0,200,300] then
    halt(2);
  if vd <> [0,200,300] then
    halt(2);
  if ve <> [0,200,300] then
    halt(2);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vc := [500,600,700];
  va := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [500,600,700] then
    halt(3);
  if vc <> [500,600,700] then
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
  if va <> [500,600,700] then
    halt(3);
  if vb <> [500,600,700] then
    halt(3);
  if vc <> [500,600,700] then
    halt(3);
  if vd <> [] then
    halt(3);
  if ve <> [] then
    halt(3);
  vd := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [500,600,700] then
    halt(3);
  if vb <> [500,600,700] then
    halt(3);
  if vc <> [500,600,700] then
    halt(3);
  if vd <> [500,600,700] then
    halt(3);
  if ve <> [] then
    halt(3);
  ve := vc;
  if b1 <> $ff then
    halt(3);
  if va <> [500,600,700] then
    halt(3);
  if vb <> [500,600,700] then
    halt(3);
  if vc <> [500,600,700] then
    halt(3);
  if vd <> [500,600,700] then
    halt(3);
  if ve <> [500,600,700] then
    halt(3);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  vd := [100,300,500];
  va := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [100,300,500] then
    halt(4);
  if vd <> [100,300,500] then
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
  if va <> [100,300,500] then
    halt(4);
  if vb <> [100,300,500] then
    halt(4);
  if vd <> [100,300,500] then
    halt(4);
  if vc <> [] then
    halt(4);
  if ve <> [] then
    halt(4);
  vc := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [100,300,500] then
    halt(4);
  if vb <> [100,300,500] then
    halt(4);
  if vc <> [100,300,500] then
    halt(4);
  if vd <> [100,300,500] then
    halt(4);
  if ve <> [] then
    halt(4);
  ve := vd;
  if b1 <> $ff then
    halt(4);
  if va <> [100,300,500] then
    halt(4);
  if vb <> [100,300,500] then
    halt(4);
  if vc <> [100,300,500] then
    halt(4);
  if vd <> [100,300,500] then
    halt(4);
  if ve <> [100,300,500] then
    halt(4);


  b1 := $ff;
  b2 := $ff;
  b3 := $ff;
  va := [];
  vb := [];
  vc := [];
  vd := [];
  ve := [];

  ve := [0,700];
  va := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,700] then
    halt(5);
  if ve <> [0,700] then
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
  if va <> [0,700] then
    halt(5);
  if vb <> [0,700] then
    halt(5);
  if ve <> [0,700] then
    halt(5);
  if vd <> [] then
    halt(5);
  if vc <> [] then
    halt(5);
  vc := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,700] then
    halt(5);
  if vb <> [0,700] then
    halt(5);
  if vc <> [0,700] then
    halt(5);
  if ve <> [0,700] then
    halt(5);
  if vd <> [] then
    halt(5);
  vd := ve;
  if b1 <> $ff then
    halt(5);
  if va <> [0,700] then
    halt(5);
  if vb <> [0,700] then
    halt(5);
  if vc <> [0,700] then
    halt(5);
  if vd <> [0,700] then
    halt(5);
  if ve <> [0,700] then
    halt(5);
  writeln('ok');
end.
