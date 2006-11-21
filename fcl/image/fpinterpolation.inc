{ TFPCustomInterpolation }

procedure TFPCustomInterpolation.Initialize(aimage: TFPCustomImage; acanvas: TFPCustomCanvas);
begin
  fimage := aimage;
  fcanvas := acanvas;
end;

{ TFPBaseInterpolation }

type

  TInterpolationContribution = record
    weight : double;
    place : integer;
  end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

procedure TFPBaseInterpolation.Horizontal (width : integer);
var x,y,r : integer;
  start, stop, maxcontribs : integer;
  center, re,gr,bl, density : double;
  contributions : array[0..10] of TInterpolationContribution;
  dif, w, gamma, a : double;
  c : TFPColor;
begin
  for x := 0 to width-1 do
    begin
    center := x * xfactor;
    start := round (center-xsupport);
    if start < 0 then
      start := 0;
    stop := round(center+xsupport);
    if stop >= image.Width then
      stop := image.Width-1;
    density := 0.0;
    maxcontribs := -1;
    for r := start to stop do
      begin
      dif := r - center;
      w := Filter (dif);
      if w > 0.0 then
        begin
        inc (maxcontribs);
        with contributions[maxcontribs] do
          begin
          weight := w;
          density := density + w;
          place := r;
          end;
        end;
      end;
    if (density <> 0.0) and (density <> 1.0) then
      begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
      end;
    for y := 0 to image.height-1 do
      begin
      gamma := 0.0;
      re := 0.0;
      gr := 0.0;
      bl := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
          with image.colors[place,y] do
            begin
            a := weight * alpha / $FFFF;
            re := re + a * image.colors[place,y].red;
            gr := gr + a * image.colors[place,y].green;
            bl := bl + a * image.colors[place,y].blue;
            gamma := gamma + a;
            end;
      with c do
        begin
        red := ColorRound (re);
        green := ColorRound (gr);
        blue := ColorRound (bl);
        alpha := ColorRound (gamma * $FFFF) ;
        end;
      tempimage.colors[x,y] := c;
      end;
    end;
end;

procedure TFPBaseInterpolation.vertical(dx,dy,width,height: integer);
var x,y,r : integer;
  start, stop, maxcontribs : integer;
  center, re,gr,bl, density : double;
  contributions : array[0..10] of TInterpolationContribution;
  dif, w, gamma, a : double;
  c : TFPColor;
begin
  for y := 0 to height-1 do
    begin
    center := y * yfactor;
    start := round (center-ysupport);
    if start < 0 then
      start := 0;
    stop := round(center+ysupport);
    if stop >= tempimage.height then
      stop := tempimage.height-1;
    density := 0.0;
    maxcontribs := -1;
    for r := start to stop do
      begin
      dif := r - center;
      w := Filter (dif);
      if w > 0.0 then
        begin
        inc (maxcontribs);
        with contributions[maxcontribs] do
          begin
          weight := w;
          density := density + w;
          place := r;
          end;
        end;
      end;
    if (density <> 0.0) and (density <> 1.0) then
      begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
      end;
    for x := 0 to width-1 do
      begin
      gamma := 0.0;
      re := 0.0;
      gr := 0.0;
      bl := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
          with tempimage.colors[x,place] do
            begin
            a := weight * alpha / $FFFF;
            re := re + a * red;
            gr := gr + a * green;
            bl := bl + a * blue;
            gamma := gamma + a;
            end;
      with c do
        begin
        red := ColorRound (re);
        green := ColorRound (gr);
        blue := ColorRound (bl);
        alpha := ColorRound (gamma * $FFFF);
        end;
      canvas.colors[x+dx,y+dy] := c;
      end;
    end;
end;

procedure TFPBaseInterpolation.Execute(x, y, w, h: integer);
var maxy : integer;
    rx,ry : integer;
begin
  tempimage := TFPMemoryImage.Create (w,image.height);
  tempimage.UsePalette := false;
  xfactor := image.Width / w;
  yfactor := image.Height / h;
  if xfactor > 1.0 then
    xsupport := MaxSupport
  else
    xsupport := xfactor * MaxSupport;
  if yfactor > 1.0 then
    ysupport := MaxSupport
  else
    ysupport := yfactor * MaxSupport;
  Horizontal (w);
  Vertical (x,y,w,h);
end;

{ TMitchelInterpolation }

function TMitchelInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TMitchelInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;

