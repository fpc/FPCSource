{$mode objfpc}

 type
  TDOS_FIELDNAMES = (
                      Dos_Signature, // ord = 0
                      Dos_OffsetToNewExecutable // ord = 1
                    );



const
  DosFieldLabelsB : array[TDOS_FIELDNAMES]
                        of pwidechar =
  (
    'DOS signature',
    'offset to new executable'
  );

  d : ppwidechar = @DosFieldLabelsB[Dos_OffsetToNewExecutable];

begin
  if d<>@DosFieldLabelsB[Dos_OffsetToNewExecutable] then
    halt(1);
end.
