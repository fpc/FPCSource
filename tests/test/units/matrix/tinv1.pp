uses
  matrix;

procedure do_error(i : longint);
  begin
    writeln('Error: ',i);
    halt(1);
  end;

procedure test_matrix2_extended;
var
  m1,m2,m3 : tmatrix2_extended;
  i,j : longint;
begin
  m1.data[0,0]:=1;
  m1.data[0,1]:=1;
  m1.data[1,0]:=1;
  m1.data[1,1]:=-1;
  m1:=m1/sqrt(2);
  m2:=m1.inverse(m1.determinant);

  m3:=m1.transpose;
  { m1^(-1) must be equal m1^T because m1 is orthogonal }
  for i:=0 to high(m2.data) do
    for j:=0 to high(m2.data[0]) do
      if abs(m2.data[i,j]-m3.data[i,j])>1e-10 then
        begin
          writeln(m2.data[i,j],'<>',m3.data[i,j]);
          do_error(2002);
        end;
end;

procedure test_matrix3_extended;
var
  m1,m2,m3 : tmatrix3_extended;
  i,j : longint;
begin
  m1.data[0,0]:=2;
  m1.data[0,1]:=-2;
  m1.data[0,2]:=1;
  m1.data[1,0]:=1;
  m1.data[1,1]:=2;
  m1.data[1,2]:=2;
  m1.data[2,0]:=2;
  m1.data[2,1]:=1;
  m1.data[2,2]:=-2;
  m1:=m1/3;
  m2:=m1.inverse(m1.determinant);

  m3:=m1.transpose;
  { m1^(-1) must be equal m1^T because m1 is orthogonal }
  for i:=0 to high(m2.data) do
    for j:=0 to high(m2.data[0]) do
      if abs(m2.data[i,j]-m3.data[i,j])>1e-10 then
        begin
          writeln(m2.data[i,j],'<>',m3.data[i,j]);
          do_error(3002);
        end;
end;

procedure test_matrix4_extended;
var
  m1,m2,m3 : tmatrix4_extended;
  i,j : longint;
begin
  m1.data[0,0]:=2;
  m1.data[0,1]:=-2;
  m1.data[0,2]:=1;
  m1.data[0,3]:=7;
  m1.data[1,0]:=1;
  m1.data[1,1]:=2;
  m1.data[1,2]:=2;
  m1.data[1,3]:=5;
  m1.data[2,0]:=2;
  m1.data[2,1]:=1;
  m1.data[2,2]:=-2;
  m1.data[2,3]:=-1;
  m1.data[3,0]:=8;
  m1.data[3,1]:=-9;
  m1.data[3,2]:=2;
  m1.data[3,3]:=-1;
  m2:=m1.inverse(m1.determinant);

  m3:=m1*m2;
  for i:=0 to high(m3.data) do
    for j:=0 to high(m3.data[0]) do
      if (i<>j) and (abs(m3.data[i,j])>1e-10) or
        (i=j) and (abs(m3.data[i,j]-1)>1e-10) then
        begin
          writeln(i,' ',j,m3.data[i,j]);
          do_error(4002);
        end;
end;

begin
  test_matrix2_extended;
  test_matrix3_extended;
  test_matrix4_extended;
  writeln('ok');
end.
