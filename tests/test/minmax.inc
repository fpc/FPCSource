uses
  Math;

procedure TestFail(ErrCode: LongInt); noreturn;
  begin
    WriteLn('FAIL on test ', ErrCode);
    Halt(ErrCode);
  end;

procedure TestSingle;

  function Min1(a, b: Single): Single; inline;
    begin
      if a < b then
        Result := a
      else
        Result := b;
    end;


  function Max1(a, b: Single): Single; inline;
    begin
      if a > b then
        Result := a
      else
        Result := b;
    end;


  function Min2(a, b: Single): Single; inline;
    begin
      if a <= b then
        Result := a
      else
        Result := b;
    end;


  function Max2(a, b: Single): Single; inline;
    begin
      if a >= b then
        Result := a
      else
        Result := b;
    end;


  function Min3(a, b: Single): Single; inline;
    begin
      Result := b;
      if a < Result then
        Result := a;
    end;


  function Max3(a, b: Single): Single; inline;
    begin
      Result := b;
      if a > Result then
        Result := a;
    end;


  function Min4(a, b: Single): Single; inline;
    begin
      Result := b;
      if a <= Result then
        Result := a;
    end;


  function Max4(a, b: Single): Single; inline;
    begin
      Result := b;
      if a >= Result then
        Result := a;
    end;

  var
    v1,v3,vNaN : Single;

  begin
    v1:=1;
    v3:=3;
    if Min1(1,3)<>1 then
      TestFail(1);
    if Max1(1,3)<>3 then
      TestFail(2);
    if Min2(1,3)<>1 then
      TestFail(3);
    if Max2(1,3)<>3 then
      TestFail(4);
    if Min3(1,3)<>1 then
      TestFail(5);
    if Max3(1,3)<>3 then
      TestFail(6);
    if Min3(1,3)<>1 then
      TestFail(7);
    if Max3(1,3)<>3 then
      TestFail(8);
    if Min1(1,v3)<>1 then
      TestFail(11);
    if Max1(1,v3)<>3 then
      TestFail(12);
    if Min2(1,v3)<>1 then
      TestFail(13);
    if Max2(1,v3)<>3 then
      TestFail(14);
    if Min3(1,v3)<>1 then
      TestFail(15);
    if Max3(1,v3)<>3 then
      TestFail(16);
    if Min4(1,v3)<>1 then
      TestFail(17);
    if Max4(1,v3)<>3 then
      TestFail(18);
    if Min1(1,v3)<>1 then
      TestFail(21);
    if Max1(1,v3)<>v3 then
      TestFail(22);
    if Min2(1,v3)<>1 then
      TestFail(23);
    if Max2(1,v3)<>v3 then
      TestFail(24);
    if Min3(1,v3)<>1 then
      TestFail(25);
    if Max3(1,v3)<>v3 then
      TestFail(26);
    if Min4(1,v3)<>1 then
      TestFail(27);
    if Max4(1,v3)<>v3 then
      TestFail(28);
    if Min1(v1,v3)<>v1 then
      TestFail(31);
    if Max1(v1,v3)<>v3 then
      TestFail(32);
    if Min2(v1,v3)<>v1 then
      TestFail(33);
    if Max2(v1,v3)<>v3 then
      TestFail(34);
    if Min3(v1,v3)<>v1 then
      TestFail(35);
    if Max3(v1,v3)<>v3 then
      TestFail(36);
    if Min4(v1,v3)<>v1 then
      TestFail(37);
    if Max4(v1,v3)<>v3 then
      TestFail(38);
    SetExceptionMask([exInvalidOp]);
    vNaN:=NaN;
    if not(IsNaN(Min1(v1,vNaN))) then
      TestFail(41);
    if Min1(NaN,v1)<>v1 then
      TestFail(42);
    if not(IsNaN(Max1(v1,vNaN))) then
      TestFail(43);
    if Max1(vNaN,v3)<>v3 then
      TestFail(44);
    if not(IsNaN(Min2(v1,vNaN))) then
      TestFail(45);
    if Min2(vNaN,v3)<>v3 then
      TestFail(46);
    if not(IsNaN(Max2(v1,vNaN))) then
      TestFail(47);
    if Max2(vNaN,v3)<>v3 then
      TestFail(48);
    if not(IsNaN(Min3(v1,vNaN))) then
      TestFail(49);
    if Min3(NaN,v1)<>v1 then
      TestFail(50);
    if not(IsNaN(Max3(v1,vNaN))) then
      TestFail(51);
    if Max3(vNaN,v3)<>v3 then
      TestFail(52);
    if not(IsNaN(Min4(v1,vNaN))) then
      TestFail(53);
    if Min4(vNaN,v3)<>v3 then
      TestFail(54);
    if not(IsNaN(Max4(v1,vNaN))) then
      TestFail(55);
    if Max4(vNaN,v3)<>v3 then
      TestFail(56);
    SetExceptionMask([]);
  end;

procedure TestDouble;

  function Min1(a, b: Double): Double; inline;
    begin
      if a < b then
        Result := a
      else
        Result := b;
    end;


  function Max1(a, b: Double): Double; inline;
    begin
      if a > b then
        Result := a
      else
        Result := b;
    end;


  function Min2(a, b: Double): Double; inline;
    begin
      if a <= b then
        Result := a
      else
        Result := b;
    end;


  function Max2(a, b: Double): Double; inline;
    begin
      if a >= b then
        Result := a
      else
        Result := b;
    end;


  function Min3(a, b: Double): Double; inline;
    begin
      Result := b;
      if a < b then
        Result := a;
    end;


  function Max3(a, b: Double): Double; inline;
    begin
      Result := b;
      if a > b then
        Result := a;
    end;


  function Min4(a, b: Double): Double; inline;
    begin
      Result := b;
      if a <= b then
        Result := a;
    end;


  function Max4(a, b: Double): Double; inline;
    begin
      Result := b;
      if a >= b then
        Result := a;
    end;

  var
    v1,v3,vNaN : Double;

  begin
    v1:=1;
    v3:=3;
    if Min1(1,3)<>1 then
      TestFail(101);
    if Max1(1,3)<>3 then
      TestFail(102);
    if Min2(1,3)<>1 then
      TestFail(103);
    if Max2(1,3)<>3 then
      TestFail(104);
    if Min3(1,3)<>1 then
      TestFail(105);
    if Max3(1,3)<>3 then
      TestFail(106);
    if Min3(1,3)<>1 then
      TestFail(107);
    if Max3(1,3)<>3 then
      TestFail(108);
    if Min1(1,v3)<>1 then
      TestFail(111);
    if Max1(1,v3)<>3 then
      TestFail(112);
    if Min2(1,v3)<>1 then
      TestFail(113);
    if Max2(1,v3)<>3 then
      TestFail(114);
    if Min3(1,v3)<>1 then
      TestFail(115);
    if Max3(1,v3)<>3 then
      TestFail(116);
    if Min4(1,v3)<>1 then
      TestFail(117);
    if Max4(1,v3)<>3 then
      TestFail(118);
    if Min1(1,v3)<>1 then
      TestFail(121);
    if Max1(1,v3)<>v3 then
      TestFail(122);
    if Min2(1,v3)<>1 then
      TestFail(123);
    if Max2(1,v3)<>v3 then
      TestFail(124);
    if Min3(1,v3)<>1 then
      TestFail(125);
    if Max3(1,v3)<>v3 then
      TestFail(126);
    if Min4(1,v3)<>1 then
      TestFail(127);
    if Max4(1,v3)<>v3 then
      TestFail(128);
    if Min1(v1,v3)<>v1 then
      TestFail(131);
    if Max1(v1,v3)<>v3 then
      TestFail(132);
    if Min2(v1,v3)<>v1 then
      TestFail(133);
    if Max2(v1,v3)<>v3 then
      TestFail(134);
    if Min3(v1,v3)<>v1 then
      TestFail(135);
    if Max3(v1,v3)<>v3 then
      TestFail(136);
    if Min4(v1,v3)<>v1 then
      TestFail(137);
    if Max4(v1,v3)<>v3 then
      TestFail(138);
    SetExceptionMask([exInvalidOp]);
    vNaN:=NaN;
    if not(IsNaN(Min1(v1,vNaN))) then
      TestFail(141);
    if Min1(NaN,v1)<>v1 then
      TestFail(142);
    if not(IsNaN(Max1(v1,vNaN))) then
      TestFail(143);
    if Max1(vNaN,v3)<>v3 then
      TestFail(144);
    if not(IsNaN(Min2(v1,vNaN))) then
      TestFail(145);
    if Min2(vNaN,v3)<>v3 then
      TestFail(146);
    if not(IsNaN(Max2(v1,vNaN))) then
      TestFail(147);
    if Max2(vNaN,v3)<>v3 then
      TestFail(148);
    if not(IsNaN(Min3(v1,vNaN))) then
      TestFail(149);
    if Min3(NaN,v1)<>v1 then
      TestFail(150);
    if not(IsNaN(Max3(v1,vNaN))) then
      TestFail(151);
    if Max3(vNaN,v3)<>v3 then
      TestFail(152);
    if not(IsNaN(Min4(v1,vNaN))) then
      TestFail(153);
    if Min4(vNaN,v3)<>v3 then
      TestFail(154);
    if not(IsNaN(Max4(v1,vNaN))) then
      TestFail(155);
    if Max4(vNaN,v3)<>v3 then
      TestFail(156);
    SetExceptionMask([]);
  end;


procedure TestDWord;

  function Min1(a, b: DWord): DWord; inline;
    begin
      if a < b then
        Result := a
      else
        Result := b;
    end;


  function Max1(a, b: DWord): DWord; inline;
    begin
      if a > b then
        Result := a
      else
        Result := b;
    end;


  function Min2(a, b: DWord): DWord; inline;
    begin
      if a <= b then
        Result := a
      else
        Result := b;
    end;


  function Max2(a, b: DWord): DWord; inline;
    begin
      if a >= b then
        Result := a
      else
        Result := b;
    end;


  function Min3(a, b: DWord): DWord; inline;
    begin
      Result := b;
      if a < b then
        Result := a;
    end;


  function Max3(a, b: DWord): DWord; inline;
    begin
      Result := b;
      if a > b then
        Result := a;
    end;


  function Min4(a, b: DWord): DWord; inline;
    begin
      Result := b;
      if a <= b then
        Result := a;
    end;


  function Max4(a, b: Double): Double; inline;
    begin
      Result := b;
      if a >= b then
        Result := a;
    end;

  var
    v1,v3 : DWord;

  begin
    v1:=1;
    v3:=3;
    if Min1(1,3)<>1 then
      TestFail(201);
    if Max1(1,3)<>3 then
      TestFail(202);
    if Min2(1,3)<>1 then
      TestFail(203);
    if Max2(1,3)<>3 then
      TestFail(204);
    if Min3(1,3)<>1 then
      TestFail(205);
    if Max3(1,3)<>3 then
      TestFail(206);
    if Min3(1,3)<>1 then
      TestFail(207);
    if Max3(1,3)<>3 then
      TestFail(208);
    if Min1(1,v3)<>1 then
      TestFail(211);
    if Max1(1,v3)<>3 then
      TestFail(212);
    if Min2(1,v3)<>1 then
      TestFail(213);
    if Max2(1,v3)<>3 then
      TestFail(214);
    if Min3(1,v3)<>1 then
      TestFail(215);
    if Max3(1,v3)<>3 then
      TestFail(216);
    if Min4(1,v3)<>1 then
      TestFail(217);
    if Max4(1,v3)<>3 then
      TestFail(218);
    if Min1(1,v3)<>1 then
      TestFail(221);
    if Max1(1,v3)<>v3 then
      TestFail(222);
    if Min2(1,v3)<>1 then
      TestFail(223);
    if Max2(1,v3)<>v3 then
      TestFail(224);
    if Min3(1,v3)<>1 then
      TestFail(225);
    if Max3(1,v3)<>v3 then
      TestFail(226);
    if Min4(1,v3)<>1 then
      TestFail(227);
    if Max4(1,v3)<>v3 then
      TestFail(228);
    if Min1(v1,v3)<>v1 then
      TestFail(231);
    if Max1(v1,v3)<>v3 then
      TestFail(232);
    if Min2(v1,v3)<>v1 then
      TestFail(233);
    if Max2(v1,v3)<>v3 then
      TestFail(234);
    if Min3(v1,v3)<>v1 then
      TestFail(235);
    if Max3(v1,v3)<>v3 then
      TestFail(236);
    if Min4(v1,v3)<>v1 then
      TestFail(237);
    if Max4(v1,v3)<>v3 then
      TestFail(238);
  end;

procedure TestLongint;

  function Min1(a, b: Longint): Longint; inline;
    begin
      if a < b then
        Result := a
      else
        Result := b;
    end;


  function Max1(a, b: Longint): Longint; inline;
    begin
      if a > b then
        Result := a
      else
        Result := b;
    end;

  function Min2(a, b: Longint): Longint; inline;
    begin
      if a <= b then
        Result := a
      else
        Result := b;
    end;


  function Max2(a, b: Longint): Longint; inline;
    begin
      if a >= b then
        Result := a
      else
        Result := b;
    end;


  function Min3(a, b: Longint): Longint; inline;
    begin
      Result := b;
      if a < b then
        Result := a;
    end;


  function Max3(a, b: Longint): Longint; inline;
    begin
      Result := b;
      if a > b then
        Result := a;
    end;


  function Min4(a, b: Longint): Longint; inline;
    begin
      Result := b;
      if a <= b then
        Result := a;
    end;


  function Max4(a, b: Longint): Longint; inline;
    begin
      Result := b;
      if a >= b then
        Result := a;
    end;

  var
    v1,v3 : Longint;

  begin
    v1:=1;
    v3:=3;
    if Min1(1,3)<>1 then
      TestFail(301);
    if Max1(1,3)<>3 then
      TestFail(302);
    if Min2(1,3)<>1 then
      TestFail(303);
    if Max2(1,3)<>3 then
      TestFail(304);
    if Min3(1,3)<>1 then
      TestFail(305);
    if Max3(1,3)<>3 then
      TestFail(306);
    if Min3(1,3)<>1 then
      TestFail(307);
    if Max3(1,3)<>3 then
      TestFail(308);
    if Min1(1,v3)<>1 then
      TestFail(311);
    if Max1(1,v3)<>3 then
      TestFail(312);
    if Min2(1,v3)<>1 then
      TestFail(313);
    if Max2(1,v3)<>3 then
      TestFail(314);
    if Min3(1,v3)<>1 then
      TestFail(315);
    if Max3(1,v3)<>3 then
      TestFail(316);
    if Min4(1,v3)<>1 then
      TestFail(317);
    if Max4(1,v3)<>3 then
      TestFail(318);
    if Min1(1,v3)<>1 then
      TestFail(321);
    if Max1(1,v3)<>v3 then
      TestFail(322);
    if Min2(1,v3)<>1 then
      TestFail(323);
    if Max2(1,v3)<>v3 then
      TestFail(324);
    if Min3(1,v3)<>1 then
      TestFail(325);
    if Max3(1,v3)<>v3 then
      TestFail(326);
    if Min4(1,v3)<>1 then
      TestFail(327);
    if Max4(1,v3)<>v3 then
      TestFail(328);
    if Min1(v1,v3)<>v1 then
      TestFail(331);
    if Max1(v1,v3)<>v3 then
      TestFail(332);
    if Min2(v1,v3)<>v1 then
      TestFail(333);
    if Max2(v1,v3)<>v3 then
      TestFail(334);
    if Min3(v1,v3)<>v1 then
      TestFail(335);
    if Max3(v1,v3)<>v3 then
      TestFail(336);
    if Min4(v1,v3)<>v1 then
      TestFail(337);
    if Max4(v1,v3)<>v3 then
      TestFail(338);
  end;

  var
    v1,v3 : Longint;

  function Min1: Longint; inline;
    begin
      if v1 < v3 then
        Result := v1
      else
        Result := v3;
    end;


  function Max1: Longint; inline;
    begin
      if v1 > v3 then
        Result := v1
      else
        Result := v3;
    end;

var
    c1 : Longint = 1;
    c3 : Longint = 3;

  function Min11: Longint; inline;
    begin
      if c1 < c3 then
        Result := c1
      else
        Result := c3;
    end;


  function Max11: Longint; inline;
    begin
      if c1 > c3 then
        Result := c1
      else
        Result := c3;
    end;


procedure TestLongintGlobal;

  begin
    v1:=1;
    v3:=3;
    if Min1<>1 then
      TestFail(1301);
    if Max1<>3 then
      TestFail(1302);

    if Min11<>1 then
      TestFail(1311);
    if Max11<>3 then
      TestFail(1312);
  end;


begin
  TestSingle;
  TestDouble;
  TestDWord;
  TestLongint;
  TestLongintGlobal;
  WriteLn('ok');
end.
