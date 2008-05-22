{ %opt=-gl -O- }
{ %needlibrary }
{$goto on}

{ test lineinfo in libraries, to get always proper results, the optimizer
  must be turned off with -O- }

uses
  {$ifdef unix}dl,{$endif unix}sysutils;

{$ifdef darwin}
{$linklib tlib1a}
{$endif darwin}

procedure p(var a : pointer);external 'tlib1a' name 'p';

var
  s1,s2 : string;
  l : longint;
  a : pointer;
label
  w;
begin
w:
  { library }
  p(a);
  GetLineInfo(PtrUInt(a),s1,s2,l);
  s2:=ExtractFilename(s2);
  writeln({ 'Func: ',s1,'} 'Source: ',s2,' Line: ',l);
  { GetLineInfo of dwarf doesn't return the function name }
  if { (s1<>'P') or } (s2<>'tlib1a.pp') or (l<>9) then
    halt(1);

  { main program }
  GetLineInfo(PtrUInt(@w),s1,s2,l);
  s2:=ExtractFilename(s2);
  writeln({ 'Func: ',s1,'} 'Source: ',s2,' Line: ',l);
  { GetLineInfo of dwarf doesn't return the function name }
  if { (s1<>'P') or } (s2<>'tlib1b.pp') or (l<>23) then
    halt(1);

  writeln('ok');
end.
