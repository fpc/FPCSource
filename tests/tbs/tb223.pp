{ Old file: tbs0256.pp }
{ problem with conditionnals in TP mode                OK 0.99.11 (PM) }

{$mode tp}

{$undef dummy }

{$ifdef dummy}
    procedure test;
      begin
         foreach({$ifndef TP}@{$endif}add_to_browserlog);
      end;
{$endif BrowserLog}

begin
end.
