{$R+}

type
  size_t = Cardinal;

function CMSG_ALIGN(len: size_t): size_t;
begin
  CMSG_ALIGN := (len + SizeOf(size_t) - 1) and (not (SizeOf(size_t) - 1));
end;


begin
end.
