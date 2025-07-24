{ %fail }
generic procedure proc<T>(arg: T); external 'lib';

begin
  specialize proc<Integer>(1);
end.
