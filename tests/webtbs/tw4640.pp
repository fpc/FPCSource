{ Source provided for Free Pascal Bug Report 4640 }
{ Submitted by "Tomas" on  2005-12-28 }
{ e-mail:  }
var
  T: text;
  W: word;
  N: integer;
begin
  DefaultTextLineBreakStyle := tlbsCRLF;
  Assign (T, 'readtest.txt');
  Rewrite (T);
  W := 123;
  Write (T, W);
  Close (T);
  Reset (T);
  N := -1;
  Read (T, N);
  WriteLn (N);
  if N <> 123 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, N); (* Funny thing - FPC works OK for integers, not for words. *)
  WriteLn (N);
  if N <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, N); (* Funny thing - FPC works OK for integers, not for words. *)
  WriteLn (N);
  if N <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Close (T);
  Reset (T);
  W := 65535;
  Read (T, W);
  WriteLn (W);
  if W <> 123 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, W); (* FPC issues RTE 106 here, TP/BP doesn't. *)
  WriteLn (W);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, W); (* FPC issues RTE 106 here, TP/BP doesn't. *)
  WriteLn (W);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Close (T);
  Rewrite (T);
  W := 123;
  WriteLn (T, W);
  Close (T);
  Reset (T);
  Read (T, W);
  WriteLn (W);
  if W <> 123 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, W); (* Another inconsistence - Read works, ReadLn doesn't. *)
  WriteLn (W);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Readln (T, W); (* Another inconsistence - Read works, ReadLn doesn't. *)
  WriteLn (W);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Close (T);
  Reset (T);
  Read (T, N);
  WriteLn (n);
  if n <> 123 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Read (T, N); (* Again, FPC issues RTE 106 here, TP/BP doesn't. *)
  WriteLn (N);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  ReadLn (T, N); (* Again, FPC issues RTE 106 here, TP/BP doesn't. *)
  WriteLn (N);
  if W <> 0 then
    begin
      WriteLn ('Wrong value!');
      Halt (255);
    end;
  Close (T);
  erase(t);
end.
