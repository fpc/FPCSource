{ %RESULT=227 }
{ Old file: tbs0150.pp }
{ Shows that the assert() macro is missing under Delphi OK 0.99.9 (PFV) }

{
 bug to show that there is no assert() macro and directive
}

var B : boolean;
    i : integer;

begin
  b:=true;
  i:=0;
  // First for assert messages should not give anything.
  // First two generate code, but are OK.
  // second two don't generate code ($C- !)
{$c+}
  assert (b);
  assert (I=0);
{$c-}
  assert (not(b));
  assert (i<>0);
{$c+}
  // This one should give the normal assert message.
  assert (not(b));
  // This one should give a custom assert message.
  // you must uncomment the previous one to see this one.
  assert (not(I=0),'Custom assert message');
end.
