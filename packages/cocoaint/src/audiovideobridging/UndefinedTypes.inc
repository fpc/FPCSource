
// device_types.h
type
	io_object_t = ^OpaqueRecord; {IOObject}
	io_connect_t = io_object_t;
	
// IOTypes.h
type
	io_iterator_t = io_object_t;
	io_service_t = io_object_t;
	
// IOKitLib.h
type	
	IONotificationPortRef = ^OpaqueRecord; {IONotificationPort}

// vmtypes.h
type
	mach_vm_address_t = UInt64;
	mach_vm_offset_t = UInt64;
	mach_vm_size_t = UInt64;
