{ %opt=-O-}
type
  TIpHtmlElemMarginStyle = (
      hemsAuto, // use default
      hemsPx    // pixel
    );
  TIpHtmlElemMargin = record
      Style: TIpHtmlElemMarginStyle;
      Size: single;
    end;

var
  tmp2 :TIpHtmlElemMargin;

function test_fn:TIpHtmlElemMargin;
  var
    tmp1 :TIpHtmlElemMargin;
  begin
    tmp1.Size := 3.123;
    tmp1.Style := hemsPx;
    test_fn := tmp1;
  end;

begin
  tmp2 := test_fn();
  if(tmp2.Style <> hemsPx)then
    halt(1);
  if(Abs(tmp2.Size - 3.123) > 0.01)then
    halt(2);
  writeln('ok ');
end.
