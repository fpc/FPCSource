{$mode tp}
    
{$ifdef dummy}
    procedure test;
      begin
         foreach({$ifndef TP}@{$endif}add_to_browserlog);
      end;
{$endif BrowserLog}

begin
end.
