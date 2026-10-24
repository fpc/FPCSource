{ %OPT=-gl -O- }
{ %TARGET=linux,freebsd,win64 }
program tw40471;

{$goto on}

const
  LineTargetBegin = 24;
  LineCall = 25;
  LineLabel = 27;

var
  RetAddr, LabelAddr: CodePointer;

procedure Callee;

begin
  RetAddr:=get_caller_addr(get_frame);
end;

procedure Target;
label
  l1;
begin
  Callee;
l1:
  LabelAddr:=@l1;
end;

procedure Check(aAddr: CodePointer; aReturnAddress: Boolean; const aExpFunc: shortstring; aExpLine: longint; const aMsg: shortstring);

var
  lFunc, lSource: shortstring;
  lLine: longint;

begin
  GetLineInfoReturnAddress:=aReturnAddress;
  if not GetLineInfo(codeptruint(aAddr), lFunc, lSource, lLine) then
    begin
      writeln(aMsg, ': GetLineInfo failed');
      halt(1);
    end;
  writeln(aMsg, ': func=', lFunc, ' line=', lLine);
  if (aExpLine<>0) and (lLine<>aExpLine) then
    begin
      writeln(aMsg, ': expected line ', aExpLine);
      halt(2);
    end;
  if (aExpFunc<>'') and (upcase(lFunc)<>aExpFunc) then
    begin
      writeln(aMsg, ': expected func ', aExpFunc);
      halt(3);
    end;
end;

var
  lFunc, lSource, lExp: shortstring;
  lLine: longint;

begin
  Target;
  if RetAddr<>LabelAddr then
    writeln('Note: return address differs from label address');

  Check(LabelAddr, False, 'TARGET', LineLabel, 'Exact label address');
  Check(@Target, False, 'TARGET', LineTargetBegin, 'Exact routine start');
  Check(RetAddr, True, 'TARGET', LineCall, 'Return address with GetLineInfoReturnAddress');

  GetLineInfoReturnAddress:=True;
  GetLineInfo(codeptruint(@Target), lFunc, lSource, lLine);
  writeln('Routine start with GetLineInfoReturnAddress: func=', lFunc, ' line=', lLine);
  if upcase(lFunc)='TARGET' then
    begin
      writeln('Function and line must come from the instruction before the routine');
      halt(4);
    end;

  GetLineInfoReturnAddress:=False;
  str(LineCall, lExp);
  lExp:=' line '+lExp+' ';
  writeln(DwarfBackTraceStr(RetAddr));
  if pos(lExp, DwarfBackTraceStr(RetAddr))=0 then
    begin
      writeln('Backtrace of return address must report the line of the call');
      halt(5);
    end;
  if pos('TARGET', upcase(DwarfBackTraceStr(RetAddr)))=0 then
    begin
      writeln('Backtrace of return address must report the calling routine');
      halt(6);
    end;
  writeln('ok');
end.
