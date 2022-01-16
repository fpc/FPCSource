program tmoperator4;

{$MODE DELPHI}

type
  TR1 = record
  private
    class operator Initialize(var aR1: TR1);
    class operator Finalize(var aR1: TR1);
  public
    I: Integer;
  end;

  TR2 = record
  private
    class operator Initialize(var aR2: TR2);
    class operator Finalize(var aR2: TR2);
  public
    S: string;
  end;

{ TR1 }

class operator TR1.Initialize(var aR1: TR1);
begin
  WriteLn('TR1.Initialize');
  aR1.I := 1;
end;

class operator TR1.Finalize(var aR1: TR1);
begin
  if aR1.I <> 2 then
    Halt(1);
  WriteLn('TR1.Finalize');
end;

{ TR2 }

class operator TR2.Initialize(var aR2: TR2);
begin
  WriteLn('TR2.Initialize');
  aR2.S := 'A';
end;

class operator TR2.Finalize(var aR2: TR2);
begin
  if aR2.S <> 'B' then
    Halt(2);
  WriteLn('TR2.Finalize');
end;

{ TA }

type 
  TA = class
  public 
    F1: TR1;
  end;

  TB = class(TA)
  public
    F2: TR2;
  end;

var
  O: TB;
begin
  O := TB.Create;
  
  if O.F1.I <> 1 then
    Halt(3);
  if O.F2.S <> 'A' then
    Halt(4);
    
  O.F1.I := 2;
  O.F2.S := 'B'; 
  
  O.Free;
  
  WriteLn('end');
end. 