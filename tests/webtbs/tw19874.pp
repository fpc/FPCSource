{ -*- compile-command: "fpc -Sa test_fgl_first_last_set.pas" -*- }
{$mode objfpc}{$H+}

uses FGL;
type
  TInteger = class
    I: Integer;
    constructor Create(AI: Integer);
  end;

  TIntSimpleList = specialize TFPGList<Integer>;
  TIntObjectList = specialize TFPGObjectList<TInteger>;

constructor TInteger.Create(AI: Integer);
begin
  inherited Create;
  I := AI;
end;

var
  SL: TIntSimpleList;
  OL: TIntObjectList;
  Temp: TInteger;
begin
  SL := TIntSimpleList.Create;
  try
    try
      SL.First := 1;
      Assert(false, 'Assigning First on empty list should fail');
    except on E: EListError do ; end;
    try
      SL.Last := 1;
      Assert(false, 'Assigning Last on empty list should fail');
    except on E: EListError do ; end;
    SL.Add(1);
    SL.Add(2);
    SL.Add(3);
    Assert(SL.First = 1);
    Assert(SL.Last = 3);
    SL.First := 111;
    SL.Last := 333;
    Assert(SL.First = 111);
    Assert(SL.Last = 333);
    Assert(SL[0] = 111);
    Assert(SL[2] = 333);
  finally SL.Free end;

  OL := TIntObjectList.Create(true);
  try
    try
      Temp := TInteger.Create(1);
      OL.First := Temp;
      Assert(false, 'Assigning First on empty list should fail');
    except on E: EListError do Temp.Free; end;
    try
      Temp := TInteger.Create(1);
      OL.Last := TInteger.Create(1);
      Assert(false, 'Assigning Last on empty list should fail');
    except on E: EListError do Temp.Free; end;
    OL.Add(TInteger.Create(1));
    OL.Add(TInteger.Create(2));
    OL.Add(TInteger.Create(3));
    Assert(OL.First.I = 1);
    Assert(OL.Last.I = 3);
    OL.First := TInteger.Create(111);
    OL.Last := TInteger.Create(333);
    Assert(OL.First.I = 111);
    Assert(OL.Last.I = 333);
    Assert(OL[0].I = 111);
    Assert(OL[2].I = 333);
  finally OL.Free end;
end.
