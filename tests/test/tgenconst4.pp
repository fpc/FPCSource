{ %NORUN }
{$mode objfpc}
{
  test constants in generic procedures
}
program tgenconst4;

generic procedure DoThis<T;const U:string>(msg: string = U);
begin
	writeln(msg, ' sizeof:',sizeof(t), ' default: ', U);
end;

begin
	specialize DoThis<integer,'genparam'>('hello world');
end.
