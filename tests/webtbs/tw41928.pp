{$modeswitch advancedrecords}

type
  TSVGMatrix = record
    a,b : Double;
    constructor Create(aA, aB: Double);
  end;

constructor TSVGMatrix.Create(aA, aB: Double);
begin
  a := aA;
  b := aB;
end;

var
  aSVG : TSVGMatrix;
begin
  aSVG:=TSVGMatrix.Create(1.0,0.0);
end.
