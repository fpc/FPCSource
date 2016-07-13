type
  TTestCase = record
    group: char;
    dividend, divider: int64;   // source
    quotient, remainder: int64; // expected result
  end;

const
  test_cases: array [0..40] of TTestCase =(
    // #30240
    ( group:'-'; dividend: 2000000000000; divider: 2000000000001; quotient: 0; remainder: 2000000000000 ),
    //.Lbig_divisor (with carry at the end)
    ( group:'a'; dividend: 8375316585208858139; divider:-7333902439715991;    quotient:-1141;      remainder: 7333901492912408 ),
    ( group:'a'; dividend: 7056323922322693051; divider:-2740063521509;       quotient:-2575240;   remainder: 2739191855891 ),
    ( group:'a'; dividend: 8271196811549967915; divider: 25285028838;         quotient: 327118345; remainder: 24786134805 ),
    ( group:'a'; dividend: 3431221233848454052; divider:-3431221234088342633; quotient: 0;         remainder: 3431221233848454052 ),
    ( group:'a'; dividend:-8585295120939781742; divider:-23751612046;         quotient: 361461575; remainder:-22003649292 ),
    ( group:'a'; dividend:-6683243686137656212; divider: 40354827467772;      quotient:-165611;    remainder:-40354372467520 ),
    ( group:'a'; dividend:-6963003432881308676; divider:-1740750858595018939; quotient: 3;         remainder:-1740750857096251859 ),
    ( group:'a'; dividend: 3589102502730131736; divider: 2718092484398;       quotient: 1320448;   remainder: 2717891761432 ),
    ( group:'a'; dividend:-9069664486668623813; divider:-177626955280;        quotient: 51060180;  remainder:-177219873413 ),
    ( group:'a'; dividend:-8708789282907437996; divider:-280928686587236007;  quotient: 30;        remainder:-280928685290357786 ),
    //.Lbig_divisor (without carry)
    ( group:'b'; dividend:-5478163896315828857; divider:-9281215814;          quotient: 590242055; remainder:-1361971087 ),
    ( group:'b'; dividend: 7101201960831283575; divider: 9474016311;          quotient: 749545042; remainder: 7094103513 ),
    ( group:'b'; dividend: 3960011864586540874; divider:-2123266079007095486; quotient:-1;         remainder: 1836745785579445388 ),
    ( group:'b'; dividend: 6707823169352057382; divider:-7764081918;          quotient:-863955743; remainder: 7173502308 ),
    ( group:'b'; dividend: 5698168712416449358; divider: 4542747269964;       quotient: 1254344;   remainder: 930820725742 ),
    ( group:'b'; dividend: 3759351913822964708; divider:-56344208198167;      quotient:-66721;     remainder: 9998633064301 ),
    ( group:'b'; dividend:-7764588773457981677; divider: 27146308080993374;   quotient:-286;       remainder:-744662293876713 ),
    ( group:'b'; dividend:-5098584499810065147; divider:-1033450244405508;    quotient: 4933;      remainder:-574444157694183 ),
    ( group:'b'; dividend: 7767592121360637078; divider:-2706907408679000905; quotient:-2;         remainder: 2353777304002635268 ),
    ( group:'b'; dividend: 3900260326859439920; divider:-4529352981664096387; quotient: 0;         remainder: 3900260326859439920 ),
    //.Ltwo_divs
    ( group:'c'; dividend:-3189721586398362144; divider:-477575983;  quotient: 6678982402;    remainder:-323510978 ),
    ( group:'c'; dividend:-6272627659376899240; divider:-365611917;  quotient: 17156518613;   remainder:-231788119 ),
    ( group:'c'; dividend: 8347107135342446860; divider: 1114829022; quotient: 7487342875;    remainder: 627528610 ),
    ( group:'c'; dividend: 7002068931434460610; divider: 404820846;  quotient: 17296710385;   remainder: 361774900 ),
    ( group:'c'; dividend: 8293431318282107842; divider:-718398042;  quotient:-11544340092;   remainder: 7207978 ),
    ( group:'c'; dividend:-6808260689000200821; divider:-1501534265; quotient: 4534202680;    remainder:-525370621 ),
    ( group:'c'; dividend: 7674745939185655069; divider:-1699384892; quotient:-4516190520;    remainder: 104031229 ),
    ( group:'c'; dividend: 6431190513421618316; divider: 3333080;    quotient: 1929503796315; remainder: 18116 ),
    ( group:'c'; dividend: 2124140687535160173; divider: 37711397;   quotient: 56326226459;   remainder: 27906950 ),
    ( group:'c'; dividend:-3811970536696094994; divider:-43355849;   quotient: 87922866801;   remainder:-24825945 ),
    // one division
    ( group:'d'; dividend:-569298819287740717;  divider: 623930358;  quotient:-912439684;  remainder:-596213845 ),
    ( group:'d'; dividend: 990400595808799715;  divider:-1625588531; quotient:-609256633;  remainder: 768323592 ),
    ( group:'d'; dividend:-580252789917085737;  divider:-354226044;  quotient: 1638086187; remainder:-165031509 ),
    ( group:'d'; dividend: 1933675428811294466; divider: 1189844258; quotient: 1625150027; remainder: 796799500 ),
    ( group:'d'; dividend: 548675153951135484;  divider:-335038546;  quotient:-1637647848; remainder: 97186476 ),
    ( group:'d'; dividend:-844891682720642266;  divider: 1058118666; quotient:-798484810;  remainder:-742178806 ),
    ( group:'d'; dividend:-759434744728515761;  divider:-733407613;  quotient: 1035487948; remainder:-495567637 ),
    ( group:'d'; dividend: 13655828961120164;   divider: 15582697;   quotient: 876345664;  remainder: 11744356 ),
    ( group:'d'; dividend: 14609195521567996;   divider:-38440672;   quotient:-380045268;  remainder: 29227900 ),
    ( group:'d'; dividend:-402022804788005296;  divider:-254071284;  quotient: 1582322875; remainder:-234183796 )
  );

var
  i, errors: integer;
  vq, vr: int64;

begin
  errors := 0;
  for i := low(test_cases) to high(test_cases) do
    begin
      vq := test_cases[i].dividend div test_cases[i].divider;
      vr := test_cases[i].dividend mod test_cases[i].divider;
      if vq*test_cases[i].divider+vr=test_cases[i].dividend then
        if vq=test_cases[i].quotient then
          if vr=test_cases[i].remainder then
            continue;
      inc(errors);
      writeln('Error [',test_cases[i].group,']: ',test_cases[i].dividend,'/',test_cases[i].divider);
      writeln('  q=',vq,' r=',vr);
      writeln('  expected q=',test_cases[i].quotient,' r=',test_cases[i].remainder);
    end;
  if errors=0 then
    writeln('Pass')
  else
    begin
      writeln('Fail (',errors,' errors)');
      halt(1);
    end;
end.
