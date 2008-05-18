{ %opt=-gl }
{ %needlibrary }
{$goto on}

{ test lineinfo in libraries }

{$ifdef unix}
uses
  dl;
{$endif unix}

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
  writeln({ 'Func: ',s1,'} 'Source: ',s2,' Line: ',l);
  { GetLineInfo of dwarf doesn't return the function name }
  if { (s1<>'P') or } (s2<>'tlib1a.pp') or (l<>9) then
    halt(1);

  { main program }
  GetLineInfo(PtrUInt(@w),s1,s2,l);
  writeln({ 'Func: ',s1,'} 'Source: ',s2,' Line: ',l);
  { GetLineInfo of dwarf doesn't return the function name }
  if { (s1<>'P') or } (s2<>'tlib1.pp') or (l<>20) then
    halt(1);
  
  writeln('ok');
end.
