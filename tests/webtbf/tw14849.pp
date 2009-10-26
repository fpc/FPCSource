{ %norun }
{ %fail }

unit tw14849;

{$mode objfpc}

interface
uses
  Classes, SysUtils; 

type
  TMarkerState=(leftActive,rightActive);

  TWorldPoint=record
    fX,fY:double;
  end;

  TCoolClass = class(TComponent)
  private
    fMarkerPos:array[TMarkerState] of TWorldPoint;
    { private declarations }

  public
    function LeftMarker :integer;
		function RightMarker:integer;
    { public declarations }
    { error: using function to index property }
    property xLPM:double read fMarkerPos[leftMarker].fX write fMarkerPos[leftmarker].fX;
  end; 

implementation

function TCoolClass.LeftMarker :integer;
begin
  Result:=0;
end;

function TCoolClass.RightMarker:integer;
begin
  Result:=1;
end;


 

end.

