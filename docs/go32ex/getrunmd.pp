uses
        go32;

begin
        case (get_run_mode) of
                rm_unknown :
                        Writeln('Unknown environment found');
                rm_raw     :
                        Writeln('You are currently running in raw mode ',
                                '(without HIMEM)');
                rm_xms     :
                        Writeln('You are currently using HIMEM.SYS only');
                rm_vcpi    :
                        Writeln('VCPI server detected. You''re using HIMEM and ',
                                'EMM386');
                rm_dpmi    :
                        Writeln('DPMI detected. You''re using a DPMI host like ',
                                'a windows DOS box or CWSDPMI');
        end;
end.