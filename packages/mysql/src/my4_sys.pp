{$IFNDEF FPC_DOTTEDUNITS}
unit my4_sys;
{$ENDIF FPC_DOTTEDUNITS}

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
    uses
      ctypes;
{$ENDIF FPC_DOTTEDUNITS}

    type
      st_dynamic_array = record
        buffers : PAnsiChar;
        elements : cuint;
        max_elements : cuint;
        alloc_increment : cuint;
        size_of_element : cuint;
      end;

      tst_dynamic_array = st_dynamic_array;
      pst_dynamic_array = ^tst_dynamic_array;

  implementation

end.
