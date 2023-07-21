program tw40358;

{$packset 1}

type
  regval = 0..47;
  regset = set of regval;

procedure print_regset(const rs : regset);
var
  r : regval;
begin
  Write('rs=[');
  for r in rs do
    begin
      Write(',',ord(r));
      { 39 is not in the constant sets below
        but it is equal to 7+32 }
      if r=39 then
        begin
          WriteLn('...');
          WriteLn('Wrong code generaed!');
          halt(1);
        end;
    end;
  WriteLn(']');
end;

var
  rs : regset;

begin
  rs:=[1,3,38,46];
  WriteLn('We should get [,1,3,38,46]');
  print_regset(rs);
  rs:=[5,7,28];
  WriteLn('We should get [,5,7,28]');
  print_regset(rs);
  WriteLn('ok');
end.
