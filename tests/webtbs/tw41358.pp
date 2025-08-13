program Project1;
{$Mode objfpc}
type
  //generic x<const K> = class  // error semicolon expected, but > found
  generic x<const K; T> = class // so we give it the semicolon...
  public type
    Foo = array[0..K] of byte; // internal error
  end;

begin
end.
