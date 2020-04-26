{ %NORUN }
{$mode objfpc}
{$modeswitch advancedrecords}
{
  test integer constants in static array ranges
}
program tgenconst3;

type
	generic TList<T;const U:integer> = record
		const
			max = U;
		public
			m_list: array[0..max-1] of T;
	end;

var
	list: specialize TList<integer,128>;
begin
end.
