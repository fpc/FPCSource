{ Source provided for Free Pascal Bug Report 3634 }
{ Submitted by "Thomas Schatzl" on  2005-02-06 }
{ e-mail:  }
{$calling oldfpccall}

type
  PMedia=^TMedia;
  TMedia=OBJECT
    constructor Init;
    destructor Done;
    FUNCTION GetNumFrame:word;virtual;
    FUNCTION GetCurFrame:word;virtual;
  END;

constructor TMedia.Init;
begin
end;

destructor TMedia.Done;
begin
end;


function TMedia.GetNumFrame : Word;
begin
  getnumframe:=4;
end;

function TMedia.GetCurFrame : Word;
begin
  getcurframe:=2;
end;


VAR    pos:longint;
	m : PMedia;
begin
	new(m, Init());
	pos := (m^.GetNumFrame)*298 div (m^.GetCurFrame);
	writeln(pos);
	if pos<>2*298 then
	  halt(1);
	dispose(m, Done);
end.
