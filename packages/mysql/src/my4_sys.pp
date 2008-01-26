unit my4_sys;

  interface

    uses
      ctypes;

    type
      st_dynamic_array = record
        buffers : pchar;
        elements : cuint;
        max_elements : cuint;
        alloc_increment : cuint;
        size_of_element : cuint;
      end;

      tst_dynamic_array = st_dynamic_array;
      pst_dynamic_array = ^tst_dynamic_array;

  implementation

end.
