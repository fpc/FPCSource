unit uw18121;
{$inline on}

interface 

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  { T1 }

  TPointerList = class
  private
    i: Pointer;
    procedure SetF(v: Pointer);
    function GetF: Pointer;
  end;

  { TPointerList2 }

  TPointerList2 = class(TPointerList)
  public
    procedure SetF(v: PInteger); inline;
    procedure WriteLn;
  end;

implementation

  procedure TPointerList.SetF(v: Pointer);
  begin
    i := v;
  end;

  function TPointerList.GetF: Pointer;
  begin
    Result := i;
  end;

  { TPointerList2 }

  procedure TPointerList2.SetF(v: PInteger); inline;
  begin
    inherited SetF(Pointer(v));
  end;

  procedure TPointerList2.WriteLn;
  var
    S: string;
  begin
    S := Format('%P', [i]);
    System.WriteLn(S);
  end;

end.
