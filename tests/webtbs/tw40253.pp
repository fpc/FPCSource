program tw40253;

{$mode objfpc}{$h+}

uses
  SysUtils, fgl;

type
  TByteList = specialize TFPGList<Byte>;

// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    Writeln('check ',aCode,' failed: ',aMsg);
    Halt(aCode);
    end;
end;


// Fills a list up to MaxListSize items with Add and checks that one more Add fails
procedure TestGrowToMax;

var
  lList: TByteList;
  lRaised: Boolean;

begin
  lList:=TByteList.Create;
  try
    try
      lList.Capacity:=MaxListSize-2;
    except
      on EOutOfMemory do
        begin
        Writeln('not enough memory, skipped');
        exit;
        end;
    end;
    lList.Count:=MaxListSize-2;
    lList.Add(1);
    Check(1,lList.Count=MaxListSize-1,'Add beyond MaxListSize-2');
    lList.Add(2);
    Check(2,lList.Count=MaxListSize,'Add up to MaxListSize');
    Check(3,lList.Capacity=MaxListSize,'capacity clamped to MaxListSize');
    Check(4,(lList[MaxListSize-2]=1) and (lList[MaxListSize-1]=2),'added items');
    lRaised:=False;
    try
      lList.Add(3);
    except
      on EListError do
        lRaised:=True;
    end;
    Check(5,lRaised,'Add beyond MaxListSize raises EListError');
    Check(6,lList.Count=MaxListSize,'count unchanged after failed Add');
  finally
    lList.Free;
  end;
end;


// Checks that a capacity beyond MaxListSize is rejected
procedure TestCapacityLimit;

var
  lList: TByteList;
  lRaised: Boolean;

begin
  lList:=TByteList.Create;
  try
    lRaised:=False;
    try
      lList.Capacity:=MaxListSize+1;
    except
      on EListError do
        lRaised:=True;
    end;
    Check(10,lRaised,'Capacity beyond MaxListSize raises EListError');
    Check(11,lList.Capacity=0,'capacity unchanged after failed set');
  finally
    lList.Free;
  end;
end;


begin
  TestCapacityLimit;
{$ifdef CPU64}
  TestGrowToMax;
{$endif}
  Writeln('ok');
end.
