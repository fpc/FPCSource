program tnegnotassign1;

uses unegnotassign;

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
    b: Byte;
    w: Word;
    d: DWord;
    q: QWord;
  end;

procedure Test_RegVar;
var
  b: Byte;
  w: Word;
  d: DWord;
  q: QWord;
begin
  b := $5A;
  NotAssignByte(b);
  Check(b,$A5);

  w := $5A7E;
  NotAssignWord(w);
  Check(w, $A581);

  d := $5A7EFF44;
  NotAssignDWord(d);
  Check(d, $A58100BB);

  q := $5A7EFF4455AAFF00;
  NotAssignQWord(q);
  Check(q, $A58100BBAA5500FF);

  b := $5A;
  NegAssignByte(b);
  Check(b,$A6);

  w := $5A7E;
  NegAssignWord(w);
  Check(w, $A582);

  d := $5A7EFF44;
  NegAssignDWord(d);
  Check(d, $A58100BC);

  q := $5A7EFF4455AAFF00;
  NegAssignQWord(q);
  Check(q, $A58100BBAA550100);
end;

procedure Test_Ref;
begin
  gr.b := $5A;
  NotAssignByte(gr.b);
  Check(gr.b,$A5);

  gr.w := $5A7E;
  NotAssignWord(gr.w);
  Check(gr.w, $A581);

  gr.d := $5A7EFF44;
  NotAssignDWord(gr.d);
  Check(gr.d, $A58100BB);

  gr.q := $5A7EFF4455AAFF00;
  NotAssignQWord(gr.q);
  Check(gr.q, $A58100BBAA5500FF);

  gr.b := $5A;
  NegAssignByte(gr.b);
  Check(gr.b,$A6);

  gr.w := $5A7E;
  NegAssignWord(gr.w);
  Check(gr.w, $A582);

  gr.d := $5A7EFF44;
  NegAssignDWord(gr.d);
  Check(gr.d, $A58100BC);

  gr.q := $5A7EFF4455AAFF00;
  NegAssignQWord(gr.q);
  Check(gr.q, $A58100BBAA550100);
end;

begin
  Test_RegVar;
  Test_Ref;

  Writeln('Ok!');
end.
