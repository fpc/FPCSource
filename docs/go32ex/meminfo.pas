{ Shows how to obtain memory information via get_meminfo();

notice the checks if any of the returned information is invalid (-1)
}

uses
        go32;

var
        meminfo : tmeminfo;

begin
        get_meminfo(meminfo);
        if (int31error <> 0)  then begin
                Writeln('Error getting DPMI memory information... Halting');
                Writeln('DPMI error number : ', int31error);
        end else begin
                with meminfo do begin
                        Writeln('Largest available free block : ',
                                available_memory div 1024, ' kbytes');
                        if (available_pages <> -1) then
                                Writeln('Maximum available unlocked pages : ',
                                        available_pages);
                        if (available_lockable_pages <> -1) then
                                Writeln('Maximum lockable available pages : ',
                                        available_lockable_pages);
                        if (linear_space <> -1) then
                                Writeln('Linear address space size : ',
                                        linear_space*get_page_size div 1024, ' kbytes');
                        if (unlocked_pages <> -1) then
                                Writeln('Total number of unlocked pages : ',
                                        unlocked_pages);
                        if (available_physical_pages <> -1) then
                                Writeln('Total number of free pages : ',
                                        available_physical_pages);
                        if (total_physical_pages <> -1) then
                                Writeln('Total number of physical pages : ',
                                        total_physical_pages);
                        if (free_linear_space <> -1) then
                                Writeln('Free linear address space : ',
                                        free_linear_space*get_page_size div 1024,
                                        ' kbytes');
                        if (max_pages_in_paging_file <> -1) then
                                Writeln('Maximum size of paging file : ',
                                        max_pages_in_paging_file*get_page_size div 1024,
                                        ' kbytes');
                end;
        end;
end.