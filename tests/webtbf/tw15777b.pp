{ %opt=-vw -Sew }
{ %fail }

{ has to fail because of the longint/single mixing with the procvars }

{$mode macpas}

program testunivprocparams;

type
	Int8 = -128..127;
	Int16 = integer;
	Int32 = longint;
	Rec32 = packed record f1, f2: Int16 end;

procedure calli32value( procedure pp( i: univ Int32; x: string); i: univ Int32; x: string);
begin
	pp( i, x)
end;
procedure calli32var( procedure pp( var i: univ Int32; x: string); i: univ Int32; x: string);
begin
	pp( i, x)
end;
procedure calli32const( procedure pp( const i: univ Int32; x: string); i: univ Int32; x: string);
begin
	pp( i, x)
end;

procedure psvalue( s: single; x: string);
begin
	writeln( s, ', ', x)
end;
procedure psvar( var s: single; x: string);
begin
	writeln( s, ', ', x)
end;
procedure psconst( const s: single; x: string);
begin
	writeln( s, ', ', x)
end;

procedure pdvalue( d: double; x: string);
begin
	writeln( d, ', ', x)
end;
procedure pdvar( var d: double; x: string);
begin
	writeln( d, ', ', x)
end;
procedure pdconst( const d: double; x: string);
begin
	writeln( d, ', ', x)
end;

procedure pi8value( i8: Int8; x: string);
begin
	writeln( i8, ', ', x)
end;
procedure pi8var( var i8: Int8; x: string);
begin
	writeln( i8, ', ', x)
end;
procedure pi8const( const i8: Int8; x: string);
begin
	writeln( i8, ', ', x)
end;

procedure pi16value( i16: Int16; x: string);
begin
	writeln( i16, ', ', x)
end;
procedure pi16var( var i16: Int16; x: string);
begin
	writeln( i16, ', ', x)
end;
procedure pi16const( const i16: Int16; x: string);
begin
	writeln( i16, ', ', x)
end;

procedure pi32value( i32: Int32; x: string);
begin
	writeln( i32, ', ', x)
end;
procedure pi32var( var i32: Int32; x: string);
begin
	writeln( i32, ', ', x)
end;
procedure pi32const( const i32: Int32; x: string);
begin
	writeln( i32, ', ', x)
end;

procedure variouscalli32;
var
	s: single;
	d: double;
	i8: Int8;
	i16: Int16;
	i32: Int32;
	r: Rec32;
begin
	s:=1.0;
	d:=1.0;
	i8:=1;
	i16:=2;
	r.f1:=3;
	r.f1:=4;
	i32:=5;

  calli32value( psvalue, s, 'psvalue');
  calli32var( psvar, s, 'psvar');
  calli32const( psconst, s, 'psconst');

end;

begin
	variouscalli32
end.
