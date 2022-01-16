program test;

// {$mode ObjFPC}
{$mode Delphi}
//{$mode DelphiUnicode}

const
  b1 = TypeInfo(String)=TypeInfo(RawByteString);
  b2 = TypeInfo(NativeInt)=TypeInfo(NativeInt);

begin
  if TypeInfo(String)=TypeInfo(RawByteString) then
    writeln('equal')
  else
    writeln('not equal');

  if TypeInfo(NativeInt)=TypeInfo(NativeInt) then
    writeln('equal')
  else
    writeln('not equal');
end.
