{ %opt=-vw -Sew }

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

{ will crash on platforms that pass integers by register and
  floats by stack }
//  calli32value( psvalue, s, 'psvalue');
//  calli32var( psvar, s, 'psvar');
//  calli32const( psconst, s, 'psconst');

{ not allowed by fpc because sizeof(double) <> sizeof(longint) }
//	calli32value( pdvalue, d, 'pdvalue');
//  calli32var( pdvar, d, 'pdvar');
//	calli32const( pdconst, d, 'pdconst');

{ not allowed by fpc because size(shortint) <> sizeof(longint) }
//	calli32value( pi8value, i8, 'pi8value');
//  calli32var( pi8var, i8, 'pi8var');
//	calli32const( pi8const, i8, 'pi8const');

{ not allowed by fpc because sizeof(smallint) <> sizeof(longint) }
//	calli32value( pi16value, i16, 'pi16value');
//  calli32var( pi16var, i16, 'pi16var');
//	calli32const( pi16const, i16, 'pi16const');

	calli32value( pi32value, i32, 'pi32value');
  calli32var( pi32var, i32, 'pi32var');
	calli32const( pi32const, i32, 'pi32const');

end;

begin
	variouscalli32
end.
{

 Below is the output from CodeWarrior. FPC's output can be different in case
 sizes differ, and if floating point/integer types are mixed

 1.000e+0   , psvalue
 0.000e+0   , psvar
 1.000e+0   , psconst
 1.000e+9   , 
 3.227e-314 , pdvar
 1.000e+15  , Q
Q
Q

       1, pi8value
       0, pi8var
       1, pi8const
       1, pi16value
       0, pi16var
       1, pi16const
       1, pi32value
       1, pi32var
       1, pi32const


}
