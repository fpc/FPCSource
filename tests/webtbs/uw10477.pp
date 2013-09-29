unit uw10477;

interface

var
  MyVar: longint;

implementation

{$if sizeof(MyVar)<>4}
  {$Message FAIL 'Error'}
{$ifend}

end.

