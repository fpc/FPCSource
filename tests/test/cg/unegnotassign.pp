unit unegnotassign;

interface

const
   fpc_in_neg_assign_x      = 94;
   fpc_in_not_assign_x      = 95;

procedure NegAssignByte(var X: Byte);[internproc:fpc_in_neg_assign_x];
procedure NegAssignWord(var X: Word);[internproc:fpc_in_neg_assign_x];
procedure NegAssignDWord(var X: DWord);[internproc:fpc_in_neg_assign_x];
procedure NegAssignQWord(var X: QWord);[internproc:fpc_in_neg_assign_x];

procedure NotAssignByte(var X: Byte);[internproc:fpc_in_not_assign_x];
procedure NotAssignWord(var X: Word);[internproc:fpc_in_not_assign_x];
procedure NotAssignDWord(var X: DWord);[internproc:fpc_in_not_assign_x];
procedure NotAssignQWord(var X: QWord);[internproc:fpc_in_not_assign_x];

implementation

end.
