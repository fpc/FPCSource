{ %target=win32,win64 }
program Win64HINSTANCE;

{$C+}

begin
  Assert(HINSTANCE <> 0, 'HINSTANCE is zero.');
end.