  uses  DOS;
  var
    error : boolean;

  procedure Expand(FileMask: string);
    var
      FileMaskOut : string;
    begin
      WriteLn;
      WriteLn('Expanding "',FileMask,'"');
      FileMaskOut := FExpand(FileMask);
      WriteLn('To become "',FileMaskOut,'"');
      if FileMask<>FileMaskOut then
       error:=true;
    end;
  begin
{$ifdef Win32}
    Expand('C:\Windows1\System');
    Expand('\\.\C\Windows1\System');
    Expand('C:\Windows1\System');
    if error then
     begin
       Writeln('ERROR!');
       Halt(1);
     end;
{$else}
   Writeln('Win32 only test');
{$endif}
  end.

