unit uandorxorassign;

interface

const
   fpc_in_and_assign_x_y   = 86;
   fpc_in_or_assign_x_y    = 87;
   fpc_in_xor_assign_x_y   = 88;

procedure AndAssignByte(var X: Byte; Const Mask: Byte);[internproc:fpc_in_and_assign_x_y];
procedure AndAssignWord(var X: Word; Const Mask: Word);[internproc:fpc_in_and_assign_x_y];
procedure AndAssignDWord(var X: DWord; Const Mask: DWord);[internproc:fpc_in_and_assign_x_y];
procedure AndAssignQWord(var X: QWord; Const Mask: QWord);[internproc:fpc_in_and_assign_x_y];

procedure OrAssignByte(var X: Byte; Const Mask: Byte);[internproc:fpc_in_or_assign_x_y];
procedure OrAssignWord(var X: Word; Const Mask: Word);[internproc:fpc_in_or_assign_x_y];
procedure OrAssignDWord(var X: DWord; Const Mask: DWord);[internproc:fpc_in_or_assign_x_y];
procedure OrAssignQWord(var X: QWord; Const Mask: QWord);[internproc:fpc_in_or_assign_x_y];

procedure XorAssignByte(var X: Byte; Const Mask: Byte);[internproc:fpc_in_xor_assign_x_y];
procedure XorAssignWord(var X: Word; Const Mask: Word);[internproc:fpc_in_xor_assign_x_y];
procedure XorAssignDWord(var X: DWord; Const Mask: DWord);[internproc:fpc_in_xor_assign_x_y];
procedure XorAssignQWord(var X: QWord; Const Mask: QWord);[internproc:fpc_in_xor_assign_x_y];

implementation

end.
