program tandorxorassign1;

uses
  uandorxorassign;

{$R-,Q-}

procedure Check(Value, ExpectedValue: QWord);
begin
  if Value <> ExpectedValue then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

var
  gr: record
    b, b2: Byte;
    w, w2: Word;
    d, d2: DWord;
    q, q2: QWord;
  end;

procedure Test_And_Ref_Const;
begin
  gr.b := $5A;
  AndAssignByte(gr.b, $4F);
  Check(gr.b, $4A);

  gr.w := $5A7E;
  AndAssignWord(gr.w, $4F23);
  Check(gr.w, $4A22);

  gr.d := $5A7EFF44;
  AndAssignDWord(gr.d, $4F23E768);
  Check(gr.d, $4A22E740);

  gr.q := $5A7EFF4455AAFF00;
  AndAssignQWord(gr.q, $4F23E7680FF05A78);
  Check(gr.q, $4A22E74005A05A00);

  gr.q := $5A7EFF4455AAFF00;
  AndAssignQWord(gr.q, $CF23E7680FF05A78);
  Check(gr.q, $4A22E74005A05A00);
end;

procedure Test_And_Ref_Ref;
begin
  gr.b := $5A;
  gr.b2 := $4F;
  AndAssignByte(gr.b, gr.b2);
  Check(gr.b, $4A);

  gr.w := $5A7E;
  gr.w2 := $4F23;
  AndAssignWord(gr.w, gr.w2);
  Check(gr.w, $4A22);

  gr.d := $5A7EFF44;
  gr.d2 := $4F23E768;
  AndAssignDWord(gr.d, gr.d2);
  Check(gr.d, $4A22E740);

  gr.q := $5A7EFF4455AAFF00;
  gr.q2 := $4F23E7680FF05A78;
  AndAssignQWord(gr.q, gr.q2);
  Check(gr.q, $4A22E74005A05A00);
end;

procedure Test_And_RegVar_Const;
var
  b: Byte;
  w: Word;
  d: DWord;
  q: QWord;
begin
  b := $5A;
  AndAssignByte(b, $4F);
  Check(b, $4A);

  w := $5A7E;
  AndAssignWord(w, $4F23);
  Check(w, $4A22);

  d := $5A7EFF44;
  AndAssignDWord(d, $4F23E768);
  Check(d, $4A22E740);

  q := $5A7EFF4455AAFF00;
  AndAssignQWord(q, $4F23E7680FF05A78);
  Check(q, $4A22E74005A05A00);

  q := $5A7EFF4455AAFF00;
  AndAssignQWord(q, $CF23E7680FF05A78);
  Check(q, $4A22E74005A05A00);
end;

procedure Test_And_RegVar_RegVar;
var
  b, b2: Byte;
  w, w2: Word;
  d, d2: DWord;
  q, q2: QWord;
begin
  b := $5A;
  b2 := $4F;
  AndAssignByte(b, b2);
  Check(b, $4A);

  w := $5A7E;
  w2 := $4F23;
  AndAssignWord(w, w2);
  Check(w, $4A22);

  d := $5A7EFF44;
  d2 := $4F23E768;
  AndAssignDWord(d, d2);
  Check(d, $4A22E740);

  q := $5A7EFF4455AAFF00;
  q2 := $4F23E7680FF05A78;
  AndAssignQWord(q, q2);
  Check(q, $4A22E74005A05A00);
end;

procedure Test_Or_Ref_Const;
begin
  gr.b := $5A;
  OrAssignByte(gr.b, $4F);
  Check(gr.b, $5F);

  gr.w := $5A7E;
  OrAssignWord(gr.w, $4F23);
  Check(gr.w, $5F7F);

  gr.d := $5A7EFF44;
  OrAssignDWord(gr.d, $4F23E768);
  Check(gr.d, $5F7FFF6C);

  gr.q := $5A7EFF4455AAFF00;
  OrAssignQWord(gr.q, $4F23E7680FF05A78);
  Check(gr.q, $5F7FFF6C5FFAFF78);

  gr.q := $5A7EFF4455AAFF00;
  OrAssignQWord(gr.q, $CF23E7680FF05A78);
  Check(gr.q, $DF7FFF6C5FFAFF78);
end;

procedure Test_Or_Ref_Ref;
begin
  gr.b := $5A;
  gr.b2 := $4F;
  OrAssignByte(gr.b, gr.b2);
  Check(gr.b, $5F);

  gr.w := $5A7E;
  gr.w2 := $4F23;
  OrAssignWord(gr.w, gr.w2);
  Check(gr.w, $5F7F);

  gr.d := $5A7EFF44;
  gr.d2 := $4F23E768;
  OrAssignDWord(gr.d, gr.d2);
  Check(gr.d, $5F7FFF6C);

  gr.q := $5A7EFF4455AAFF00;
  gr.q2 := $4F23E7680FF05A78;
  OrAssignQWord(gr.q, gr.q2);
  Check(gr.q, $5F7FFF6C5FFAFF78);
end;

procedure Test_Or_RegVar_Const;
var
  b: Byte;
  w: Word;
  d: DWord;
  q: QWord;
begin
  b := $5A;
  OrAssignByte(b, $4F);
  Check(b, $5F);

  w := $5A7E;
  OrAssignWord(w, $4F23);
  Check(w, $5F7F);

  d := $5A7EFF44;
  OrAssignDWord(d, $4F23E768);
  Check(d, $5F7FFF6C);

  q := $5A7EFF4455AAFF00;
  OrAssignQWord(q, $4F23E7680FF05A78);
  Check(q, $5F7FFF6C5FFAFF78);

  q := $5A7EFF4455AAFF00;
  OrAssignQWord(q, $CF23E7680FF05A78);
  Check(q, $DF7FFF6C5FFAFF78);
end;

procedure Test_Or_RegVar_RegVar;
var
  b, b2: Byte;
  w, w2: Word;
  d, d2: DWord;
  q, q2: QWord;
begin
  b := $5A;
  b2 := $4F;
  OrAssignByte(b, b2);
  Check(b, $5F);

  w := $5A7E;
  w2 := $4F23;
  OrAssignWord(w, w2);
  Check(w, $5F7F);

  d := $5A7EFF44;
  d2 := $4F23E768;
  OrAssignDWord(d, d2);
  Check(d, $5F7FFF6C);

  q := $5A7EFF4455AAFF00;
  q2 := $4F23E7680FF05A78;
  OrAssignQWord(q, q2);
  Check(q, $5F7FFF6C5FFAFF78);
end;

procedure Test_Xor_Ref_Const;
begin
  gr.b := $5A;
  XorAssignByte(gr.b, $4F);
  Check(gr.b, $15);

  gr.w := $5A7E;
  XorAssignWord(gr.w, $4F23);
  Check(gr.w, $155D);

  gr.d := $5A7EFF44;
  XorAssignDWord(gr.d, $4F23E768);
  Check(gr.d, $155D182C);

  gr.q := $5A7EFF4455AAFF00;
  XorAssignQWord(gr.q, $4F23E7680FF05A78);
  Check(gr.q, $155D182C5A5AA578);

  gr.q := $5A7EFF4455AAFF00;
  XorAssignQWord(gr.q, $CF23E7680FF05A78);
  Check(gr.q, $955D182C5A5AA578);
end;

procedure Test_Xor_Ref_Ref;
begin
  gr.b := $5A;
  gr.b2 := $4F;
  XorAssignByte(gr.b, gr.b2);
  Check(gr.b, $15);

  gr.w := $5A7E;
  gr.w2 := $4F23;
  XorAssignWord(gr.w, gr.w2);
  Check(gr.w, $155D);

  gr.d := $5A7EFF44;
  gr.d2 := $4F23E768;
  XorAssignDWord(gr.d, gr.d2);
  Check(gr.d, $155D182C);

  gr.q := $5A7EFF4455AAFF00;
  gr.q2 := $4F23E7680FF05A78;
  XorAssignQWord(gr.q, gr.q2);
  Check(gr.q, $155D182C5A5AA578);
end;

procedure Test_Xor_RegVar_Const;
var
  b: Byte;
  w: Word;
  d: DWord;
  q: QWord;
begin
  b := $5A;
  XorAssignByte(b, $4F);
  Check(b, $15);

  w := $5A7E;
  XorAssignWord(w, $4F23);
  Check(w, $155D);

  d := $5A7EFF44;
  XorAssignDWord(d, $4F23E768);
  Check(d, $155D182C);

  q := $5A7EFF4455AAFF00;
  XorAssignQWord(q, $4F23E7680FF05A78);
  Check(q, $155D182C5A5AA578);

  q := $5A7EFF4455AAFF00;
  XorAssignQWord(q, $CF23E7680FF05A78);
  Check(q, $955D182C5A5AA578);
end;

procedure Test_Xor_RegVar_RegVar;
var
  b, b2: Byte;
  w, w2: Word;
  d, d2: DWord;
  q, q2: QWord;
begin
  b := $5A;
  b2 := $4F;
  XorAssignByte(b, b2);
  Check(b, $15);

  w := $5A7E;
  w2 := $4F23;
  XorAssignWord(w, w2);
  Check(w, $155D);

  d := $5A7EFF44;
  d2 := $4F23E768;
  XorAssignDWord(d, d2);
  Check(d, $155D182C);

  q := $5A7EFF4455AAFF00;
  q2 := $4F23E7680FF05A78;
  XorAssignQWord(q, q2);
  Check(q, $155D182C5A5AA578);
end;

begin
  Writeln('Testing And(Ref, Const)');
  Test_And_Ref_Const;
  Writeln('Testing And(Ref, Ref)');
  Test_And_Ref_Ref;
  Writeln('Testing And(RegVar, Const)');
  Test_And_RegVar_Const;
  Writeln('Testing And(RegVar, RegVar)');
  Test_And_RegVar_RegVar;

  Writeln('Testing Or(Ref, Const)');
  Test_Or_Ref_Const;
  Writeln('Testing Or(Ref, Ref)');
  Test_Or_Ref_Ref;
  Writeln('Testing Or(RegVar, Const)');
  Test_Or_RegVar_Const;
  Writeln('Testing Or(RegVar, RegVar)');
  Test_Or_RegVar_RegVar;

  Writeln('Testing Xor(Ref, Const)');
  Test_Xor_Ref_Const;
  Writeln('Testing Xor(Ref, Ref)');
  Test_Xor_Ref_Ref;
  Writeln('Testing Xor(RegVar, Const)');
  Test_Xor_RegVar_Const;
  Writeln('Testing Xor(RegVar, RegVar)');
  Test_Xor_RegVar_RegVar;

  Writeln('Ok!');
end.
