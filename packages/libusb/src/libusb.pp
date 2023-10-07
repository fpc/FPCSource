unit libusb;

{$IFDEF FPC}
{$mode objfpc}
{$PACKRECORDS C}
{$ENDIF}

interface
{$ifdef MSWINDOWS}
uses windows;
{$else}
uses ctypes,sockets,unixtype;
{$endif}




  {
   * Public libusb header file
   * Copyright © 2001 Johannes Erdfelt <johannes@erdfelt.com>
   * Copyright © 2007-2008 Daniel Drake <dsd@gentoo.org>
   * Copyright © 2012 Pete Batard <pete@akeo.ie>
   * Copyright © 2012 Nathan Hjelm <hjelmn@cs.unm.edu>
   * For more information, please visit: http://libusb.info
   *
   * This library is free software; you can redistribute it and/or
   * modify it under the terms of the GNU Lesser General Public
   * License as published by the Free Software Foundation; either
   * version 2.1 of the License, or (at your option) any later version.
   *
   * This library is distributed in the hope that it will be useful,
   * but WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   * Lesser General Public License for more details.
   *
   * You should have received a copy of the GNU Lesser General Public
   * License along with this library; if not, write to the Free Software
   * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
    }
{$ifndef LIBUSB_H}
{$define LIBUSB_H}  
{$ifdef MSWINDOWS}

{$ifdef WIN64}
  type
    ssize_t = int64;
{$else}
  type
    ssize_t = longint;
{$endif  WIN64}
    ptimeval = pointer;

{$else}
  type
    ssize_t = ptrint;
{$endif MSWINDOWS}


  type
    puint8_t = ^uint8_t;
    uint8_t = byte;

    uint16_t = word;

    uint32_t = dword;

{* \def LIBUSB_CALL
 * \ingroup misc
 * libusb's Windows calling convention.
 *
 * Under Windows, the selection of available compilers and configurations
 * means that, unlike other platforms, there is not <em>one true calling
 * convention</em> (calling convention: the manner in which parameters are
 * passed to functions in the generated assembly code).
 *
 * Matching the Windows API itself, libusb uses the WINAPI convention (which
 * translates to the <tt>stdcall</tt> convention) and guarantees that the
 * library is compiled in this way. The public header file also includes
 * appropriate annotations so that your own software will use the right
 * convention, even if another convention is being used by default within
 * your codebase.
 *
 * The one consideration that you must apply in your software is to mark
 * all functions which you use as libusb callbacks with this LIBUSB_CALL
 * annotation, so that they too get compiled for the correct calling
 * convention.
 *
 * On non-Windows operating systems, this macro is defined as nothing. This
 * means that you can apply it to your code without worrying about
 * cross-platform compatibility.
  }
{ LIBUSB_CALL must be defined on both definition and declaration of libusb
 * functions. You'd think that declaration would be enough, but cygwin will
 * complain about conflicting types unless both are marked this way.
 * The placement of this macro is important too; it must appear after the
 * return type, before the function name. See internal documentation for
 * API_EXPORTED.
  }
{$macro on}

{$ifdef MSWINDOWS}
 
const libusb1='libusb-1.0.dll';
{$define LIBUSB_CALL := WINAPI }
{$else}
const libusb1='libusb-1.0.so';
{$define LIBUSB_CALL := cdecl }
{$endif}

{* \def LIBUSB_API_VERSION
 * \ingroup misc
 * libusb's API version.
 *
 * Since version 1.0.13, to help with feature detection, libusb defines
 * a LIBUSB_API_VERSION macro that gets increased every time there is a
 * significant change to the API, such as the introduction of a new call,
 * the definition of a new macro/enum member, or any other element that
 * libusb applications may want to detect at compilation time.
 *
 * The macro is typically used in an application as follows:
 * \code
 * #if defined(LIBUSB_API_VERSION) && (LIBUSB_API_VERSION >= 0x01001234)
 * // Use one of the newer features from the libusb API
 * #endif
 * \endcode
 *
 * Internally, LIBUSB_API_VERSION is defined as follows:
 * (libusb major << 24) | (libusb minor << 16) | (16 bit incremental)
  }

    const
      LIBUSB_API_VERSION = $01000104;      
    { The following is kept for compatibility, but will be deprecated in the future  }
      LIBUSBX_API_VERSION = LIBUSB_API_VERSION;      
{ C++ extern C conditionnal removed }
{*
 * \ingroup misc
 * Convert a 16-bit value from host-endian to little-endian format. On
 * little endian systems, this function does nothing. On big endian systems,
 * the bytes are swapped.
 * \param x the host-endian value to convert
 * \returns the value in little-endian byte order
  }

function libusb_cpu_to_le16(const x:uint16_t):uint16_t;


    {* \def libusb_le16_to_cpu
     * \ingroup misc
     * Convert a 16-bit value from little-endian to host-endian format. On
     * little endian systems, this function does nothing. On big endian systems,
     * the bytes are swapped.
     * \param x the little-endian value to convert
     * \returns the value in host-endian byte order
      }

function libusb_le16_to_cpu(const x:uint16_t):uint16_t;

{ standard USB stuff  }
{* \ingroup desc
 * Device and/or Interface Class codes  }
type
  libusb_class_code = (
  {* In the context of a \ref libusb_device_descriptor "device descriptor",
    	 * this bDeviceClass value indicates that each interface specifies its
    	 * own class information and all interfaces operate independently.
    	  }
    LIBUSB_CLASS_PER_INTERFACE = 0,
    {* Audio class  }
    LIBUSB_CLASS_AUDIO = 1,
    {* Communications class  }
    LIBUSB_CLASS_COMM = 2,
    {* Human Interface Device class  }
    LIBUSB_CLASS_HID = 3,
    {* Physical  }
    LIBUSB_CLASS_PHYSICAL = 5,
    {* Printer class  }
    LIBUSB_CLASS_PRINTER = 7,
    {* Image class  } { legacy name from libusb-0.1 usb.h  }
    LIBUSB_CLASS_PTP = 6,LIBUSB_CLASS_IMAGE = 6,
    {* Mass storage class  }
    LIBUSB_CLASS_MASS_STORAGE = 8,
    {* Hub class  }
    LIBUSB_CLASS_HUB = 9,
    {* Data class  }
    LIBUSB_CLASS_DATA = 10,
    {* Smart Card  }
    LIBUSB_CLASS_SMART_CARD = $0b,
    {* Content Security  }
    LIBUSB_CLASS_CONTENT_SECURITY = $0d,
    {* Video  }
    LIBUSB_CLASS_VIDEO = $0e,
    {* Personal Healthcare  }
    LIBUSB_CLASS_PERSONAL_HEALTHCARE = $0f,
    {* Diagnostic Device  }
    LIBUSB_CLASS_DIAGNOSTIC_DEVICE = $dc,
    {* Wireless class  }
    LIBUSB_CLASS_WIRELESS = $e0,
    {* Application class  }
    LIBUSB_CLASS_APPLICATION = $fe,
    {* Class is vendor-specific  }
    LIBUSB_CLASS_VENDOR_SPEC = $ff);

    {* \ingroup desc
     * Descriptor types as defined by the USB specification.  }
      libusb_descriptor_type = (
        {* Device descriptor. See libusb_device_descriptor.  }
        LIBUSB_DT_DEVICE = $01,
        {* Configuration descriptor. See libusb_config_descriptor.  }
        LIBUSB_DT_CONFIG = $02,
        {* String descriptor  }
        LIBUSB_DT_STRING = $03,
        {* Interface descriptor. See libusb_interface_descriptor.  }
        LIBUSB_DT_INTERFACE = $04,
        {* Endpoint descriptor. See libusb_endpoint_descriptor.  }
        LIBUSB_DT_ENDPOINT = $05,
        {* BOS descriptor  }
        LIBUSB_DT_BOS = $0f,
        {* Device Capability descriptor  }
        LIBUSB_DT_DEVICE_CAPABILITY = $10,
        {* HID descriptor  }
        LIBUSB_DT_HID = $21,
        {* HID report descriptor  }
        LIBUSB_DT_REPORT = $22,
        {* Physical descriptor  }
        LIBUSB_DT_PHYSICAL = $23,
        {* Hub descriptor  }
        LIBUSB_DT_HUB = $29,
        {* SuperSpeed Hub descriptor  }
        LIBUSB_DT_SUPERSPEED_HUB = $2a,
        {* SuperSpeed Endpoint Companion descriptor  }
        LIBUSB_DT_SS_ENDPOINT_COMPANION = $30
        );

    { Descriptor sizes per descriptor type  }

    const
      LIBUSB_DT_DEVICE_SIZE = 18;      
      LIBUSB_DT_CONFIG_SIZE = 9;      
      LIBUSB_DT_INTERFACE_SIZE = 9;      
      LIBUSB_DT_ENDPOINT_SIZE = 7;      
    { Audio extension  }
      LIBUSB_DT_ENDPOINT_AUDIO_SIZE = 9;      
      LIBUSB_DT_HUB_NONVAR_SIZE = 7;      
      LIBUSB_DT_SS_ENDPOINT_COMPANION_SIZE = 6;      
      LIBUSB_DT_BOS_SIZE = 5;      
      LIBUSB_DT_DEVICE_CAPABILITY_SIZE = 3;      
    { BOS descriptor sizes  }
      LIBUSB_BT_USB_2_0_EXTENSION_SIZE = 7;      
      LIBUSB_BT_SS_USB_DEVICE_CAPABILITY_SIZE = 10;      
      LIBUSB_BT_CONTAINER_ID_SIZE = 20;      
    { We unwrap the BOS => define its max size  }

    { was #define dname def_expr }

    { was #define dname def_expr }

const LIBUSB_DT_BOS_MAX_SIZE =
              ((LIBUSB_DT_BOS_SIZE) +
               (LIBUSB_BT_USB_2_0_EXTENSION_SIZE) +
					     (LIBUSB_BT_SS_USB_DEVICE_CAPABILITY_SIZE) +
               (LIBUSB_BT_CONTAINER_ID_SIZE));

{ in bEndpointAddress  }
const
  LIBUSB_ENDPOINT_ADDRESS_MASK = $0f;
  LIBUSB_ENDPOINT_DIR_MASK = $80;

{* \ingroup desc
 * Endpoint direction. Values for bit 7 of the
 * \ref libusb_endpoint_descriptor::bEndpointAddress "endpoint address" scheme.
  }
type
  libusb_endpoint_direction = (
    {* In: device-to-host  }
    LIBUSB_ENDPOINT_IN = $80,
    {* Out: host-to-device  }
    LIBUSB_ENDPOINT_OUT = $00
    );

  { in bmAttributes  }

const
  LIBUSB_TRANSFER_TYPE_MASK = $03;

{* \ingroup desc
 * Endpoint transfer type. Values for bits 0:1 of the
 * \ref libusb_endpoint_descriptor::bmAttributes "endpoint attributes" field.
  }
type
  libusb_transfer_type = (
    {* Control endpoint  }
    LIBUSB_TRANSFER_TYPE_CONTROL = 0,
    {* Isochronous endpoint  }
    LIBUSB_TRANSFER_TYPE_ISOCHRONOUS = 1,
    {* Bulk endpoint  }
    LIBUSB_TRANSFER_TYPE_BULK = 2,
    {* Interrupt endpoint  }
    LIBUSB_TRANSFER_TYPE_INTERRUPT = 3,
    {* Stream endpoint  }
    LIBUSB_TRANSFER_TYPE_BULK_STREAM = 4
    );

  {* \ingroup misc
  * Standard requests, as defined in table 9-5 of the USB 3.0 specifications  }
  libusb_standard_request = (
    {* Request status of the specific recipient  }
    LIBUSB_REQUEST_GET_STATUS = $00,
    {* Clear or disable a specific feature  }
    LIBUSB_REQUEST_CLEAR_FEATURE = $01,
    { 0x02 is reserved  }
    {* Set or enable a specific feature  }
    LIBUSB_REQUEST_SET_FEATURE = $03,
    { 0x04 is reserved  }
    {* Set device address for all future accesses  }
    LIBUSB_REQUEST_SET_ADDRESS = $05,
    {* Get the specified descriptor  }
    LIBUSB_REQUEST_GET_DESCRIPTOR = $06,
    {* Used to update existing descriptors or add new descriptors  }
    LIBUSB_REQUEST_SET_DESCRIPTOR = $07,
    {* Get the current device configuration value  }
    LIBUSB_REQUEST_GET_CONFIGURATION = $08,
    {* Set device configuration  }
    LIBUSB_REQUEST_SET_CONFIGURATION = $09,
    {* Return the selected alternate setting for the specified interface  }
    LIBUSB_REQUEST_GET_INTERFACE = $0A,
    {* Select an alternate interface for the specified interface  }
    LIBUSB_REQUEST_SET_INTERFACE = $0B,
    {* Set then report an endpoint's synchronization frame  }
    LIBUSB_REQUEST_SYNCH_FRAME = $0C,
    {* Sets both the U1 and U2 Exit Latency  }
    LIBUSB_REQUEST_SET_SEL = $30,
    {* Delay from the time a host transmits a packet to the time it is
     * received by the device.  }
    LIBUSB_SET_ISOCH_DELAY = $31);

{* \ingroup misc
 * Request type bits of the
 * \ref libusb_control_setup::bmRequestType "bmRequestType" field in control
 * transfers.  }
  libusb_request_type = (
    {* Standard  }
    LIBUSB_REQUEST_TYPE_STANDARD = $00 shl 5,
    {* Class  }
    LIBUSB_REQUEST_TYPE_CLASS = $01 shl 5,
    {* Vendor  }
    LIBUSB_REQUEST_TYPE_VENDOR = $02 shl 5,
    {* Reserved  }
    LIBUSB_REQUEST_TYPE_RESERVED = $03 shl 5
    );

{* \ingroup misc
 * Recipient bits of the
 * \ref libusb_control_setup::bmRequestType "bmRequestType" field in control
 * transfers. Values 4 through 31 are reserved.  }
  libusb_request_recipient = (
    {* Device  }
    LIBUSB_RECIPIENT_DEVICE = $00,
    {* Interface  }
    LIBUSB_RECIPIENT_INTERFACE = $01,
    {* Endpoint  }
    LIBUSB_RECIPIENT_ENDPOINT = $02,
    {* Other  }
    LIBUSB_RECIPIENT_OTHER = $03
    );


const
  LIBUSB_ISO_SYNC_TYPE_MASK = $0C;
  {* \ingroup desc
   * Synchronization type for isochronous endpoints. Values for bits 2:3 of the
   * \ref libusb_endpoint_descriptor::bmAttributes "bmAttributes" field in
   * libusb_endpoint_descriptor.
    }
type
  libusb_iso_sync_type = (
    {* No synchronization  }
    LIBUSB_ISO_SYNC_TYPE_NONE = 0,
    {* Asynchronous  }
    LIBUSB_ISO_SYNC_TYPE_ASYNC = 1,
    {* Adaptive  }
    LIBUSB_ISO_SYNC_TYPE_ADAPTIVE = 2,
    {* Synchronous  }
    LIBUSB_ISO_SYNC_TYPE_SYNC = 3);


const
  LIBUSB_ISO_USAGE_TYPE_MASK = $30;
  {* \ingroup desc
   * Usage type for isochronous endpoints. Values for bits 4:5 of the
   * \ref libusb_endpoint_descriptor::bmAttributes "bmAttributes" field in
   * libusb_endpoint_descriptor.
    }
type
  libusb_iso_usage_type = (
    {* Data endpoint  }
    LIBUSB_ISO_USAGE_TYPE_DATA = 0,
    {* Feedback endpoint  }
    LIBUSB_ISO_USAGE_TYPE_FEEDBACK = 1,
    {* Implicit feedback Data endpoint  }
    LIBUSB_ISO_USAGE_TYPE_IMPLICIT = 2
    );

  {* \ingroup desc
   * A structure representing the standard USB device descriptor. This
   * descriptor is documented in section 9.6.1 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE LIBUSB_DT_DEVICE in this
  	 * context.  }
  {* USB specification release number in binary-coded decimal. A value of
  	 * 0x0200 indicates USB 2.0, 0x0110 indicates USB 1.1, etc.  }
  {* USB-IF class code for the device. See \ref libusb_class_code.  }
  {* USB-IF subclass code for the device, qualified by the bDeviceClass
  	 * value  }
  {* USB-IF protocol code for the device, qualified by the bDeviceClass and
  	 * bDeviceSubClass values  }
  {* Maximum packet size for endpoint 0  }
  {* USB-IF vendor ID  }
  {* USB-IF product ID  }
  {* Device release number in binary-coded decimal  }
  {* Index of string descriptor describing manufacturer  }
  {* Index of string descriptor describing product  }
  {* Index of string descriptor containing device serial number  }
  {* Number of possible configurations  }
    libusb_device_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bcdUSB : uint16_t;
        bDeviceClass : uint8_t;
        bDeviceSubClass : uint8_t;
        bDeviceProtocol : uint8_t;
        bMaxPacketSize0 : uint8_t;
        idVendor : uint16_t;
        idProduct : uint16_t;
        bcdDevice : uint16_t;
        iManufacturer : uint8_t;
        iProduct : uint8_t;
        iSerialNumber : uint8_t;
        bNumConfigurations : uint8_t;
      end;

  {* \ingroup desc
   * A structure representing the standard USB endpoint descriptor. This
   * descriptor is documented in section 9.6.6 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_ENDPOINT LIBUSB_DT_ENDPOINT in
  	 * this context.  }
  {* The address of the endpoint described by this descriptor. Bits 0:3 are
  	 * the endpoint number. Bits 4:6 are reserved. Bit 7 indicates direction,
  	 * see \ref libusb_endpoint_direction.
  	  }
  {* Attributes which apply to the endpoint when it is configured using
  	 * the bConfigurationValue. Bits 0:1 determine the transfer type and
  	 * correspond to \ref libusb_transfer_type. Bits 2:3 are only used for
  	 * isochronous endpoints and correspond to \ref libusb_iso_sync_type.
  	 * Bits 4:5 are also only used for isochronous endpoints and correspond to
  	 * \ref libusb_iso_usage_type. Bits 6:7 are reserved.
  	  }
  {* Maximum packet size this endpoint is capable of sending/receiving.  }
  {* Interval for polling endpoint for data transfers.  }
  {* For audio devices only: the rate at which synchronization feedback
  	 * is provided.  }
  {* For audio devices only: the address if the synch endpoint  }
  {* Extra descriptors. If libusb encounters unknown endpoint descriptors,
  	 * it will store them here, should you wish to parse them.  }
(* Const before type ignored *)
  {* Length of the extra descriptors, in bytes.  }
  	plibusb_endpoint_descriptor = ^libusb_endpoint_descriptor;
    libusb_endpoint_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bEndpointAddress : uint8_t;
        bmAttributes : uint8_t;
        wMaxPacketSize : uint16_t;
        bInterval : uint8_t;
        bRefresh : uint8_t;
        bSynchAddress : uint8_t;
        extra : ^byte;
        extra_length : longint;
      end;

  {* \ingroup desc
   * A structure representing the standard USB interface descriptor. This
   * descriptor is documented in section 9.6.5 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_INTERFACE LIBUSB_DT_INTERFACE
  	 * in this context.  }
  {* Number of this interface  }
  {* Value used to select this alternate setting for this interface  }
  {* Number of endpoints used by this interface (excluding the control
  	 * endpoint).  }
  {* USB-IF class code for this interface. See \ref libusb_class_code.  }
  {* USB-IF subclass code for this interface, qualified by the
  	 * bInterfaceClass value  }
  {* USB-IF protocol code for this interface, qualified by the
  	 * bInterfaceClass and bInterfaceSubClass values  }
  {* Index of string descriptor describing this interface  }
  {* Array of endpoint descriptors. This length of this array is determined
  	 * by the bNumEndpoints field.  }
(* Const before type ignored *)
  {* Extra descriptors. If libusb encounters unknown interface descriptors,
  	 * it will store them here, should you wish to parse them.  }
(* Const before type ignored *)
  {* Length of the extra descriptors, in bytes.  }
    libusb_interface_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bInterfaceNumber : uint8_t;
        bAlternateSetting : uint8_t;
        bNumEndpoints : uint8_t;
        bInterfaceClass : uint8_t;
        bInterfaceSubClass : uint8_t;
        bInterfaceProtocol : uint8_t;
        iInterface : uint8_t;
        endpoint : ^libusb_endpoint_descriptor;
        extra : ^byte;
        extra_length : longint;
      end;

  {* \ingroup desc
   * A collection of alternate settings for a particular USB interface.
    }
  {* Array of interface descriptors. The length of this array is determined
  	 * by the num_altsetting field.  }
(* Const before type ignored *)
  {* The number of alternate settings that belong to this interface  }
    libusb_interface = record
        altsetting : ^libusb_interface_descriptor;
        num_altsetting : longint;
      end;

  {* \ingroup desc
   * A structure representing the standard USB configuration descriptor. This
   * descriptor is documented in section 9.6.3 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_CONFIG LIBUSB_DT_CONFIG
  	 * in this context.  }
  {* Total length of data returned for this configuration  }
  {* Number of interfaces supported by this configuration  }
  {* Identifier value for this configuration  }
  {* Index of string descriptor describing this configuration  }
  {* Configuration characteristics  }
  {* Maximum power consumption of the USB device from this bus in this
  	 * configuration when the device is fully operation. Expressed in units
  	 * of 2 mA when the device is operating in high-speed mode and in units
  	 * of 8 mA when the device is operating in super-speed mode.  }
  {* Array of interfaces supported by this configuration. The length of
  	 * this array is determined by the bNumInterfaces field.  }
(* Const before type ignored *)
  {* Extra descriptors. If libusb encounters unknown configuration
  	 * descriptors, it will store them here, should you wish to parse them.  }
(* Const before type ignored *)
  {* Length of the extra descriptors, in bytes.  }
    plibusb_config_descriptor=^libusb_config_descriptor;
    libusb_config_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        wTotalLength : uint16_t;
        bNumInterfaces : uint8_t;
        bConfigurationValue : uint8_t;
        iConfiguration : uint8_t;
        bmAttributes : uint8_t;
        MaxPower : uint8_t;
        interface_ : ^libusb_interface;
        extra : ^byte;
        extra_length : longint;
      end;

  {* \ingroup desc
   * A structure representing the superspeed endpoint companion
   * descriptor. This descriptor is documented in section 9.6.7 of
   * the USB 3.0 specification. All multiple-byte fields are represented in
   * host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_SS_ENDPOINT_COMPANION in
  	 * this context.  }
  {* The maximum number of packets the endpoint can send or
  	 *  recieve as part of a burst.  }
  {* In bulk EP:	bits 4:0 represents the	maximum	number of
  	 *  streams the	EP supports. In	isochronous EP:	bits 1:0
  	 *  represents the Mult	- a zero based value that determines
  	 *  the	maximum	number of packets within a service interval   }
  {* The	total number of bytes this EP will transfer every
  	 *  service interval. valid only for periodic EPs.  }
    plibusb_ss_endpoint_companion_descriptor = ^libusb_ss_endpoint_companion_descriptor;
    libusb_ss_endpoint_companion_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bMaxBurst : uint8_t;
        bmAttributes : uint8_t;
        wBytesPerInterval : uint16_t;
      end;

  {* \ingroup desc
   * A generic representation of a BOS Device Capability descriptor. It is
   * advised to check bDevCapabilityType and call the matching
   * libusb_get_*_descriptor function to get a structure fully matching the type.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE_CAPABILITY
  	 * LIBUSB_DT_DEVICE_CAPABILITY in this context.  }
  {* Device Capability type  }
  {* Device Capability data (bLength - 3 bytes)  }

  type
    plibusb_bos_dev_capability_descriptor = ^libusb_bos_dev_capability_descriptor;
    libusb_bos_dev_capability_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bDevCapabilityType : uint8_t;
        dev_capability_data : array[0..0] of uint8_t;
      end;

  {* \ingroup desc
   * A structure representing the Binary Device Object Store (BOS) descriptor.
   * This descriptor is documented in section 9.6.2 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_BOS LIBUSB_DT_BOS
  	 * in this context.  }
  {* Length of this descriptor and all of its sub descriptors  }
  {* The number of separate device capability descriptors in
  	 * the BOS  }
  {* bNumDeviceCap Device Capability Descriptors  }

  type
    plibusb_bos_descriptor = ^libusb_bos_descriptor;
    libusb_bos_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        wTotalLength : uint16_t;
        bNumDeviceCaps : uint8_t;
        dev_capability : array[0..0] of ^libusb_bos_dev_capability_descriptor;
      end;

  {* \ingroup desc
   * A structure representing the USB 2.0 Extension descriptor
   * This descriptor is documented in section 9.6.2.1 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE_CAPABILITY
  	 * LIBUSB_DT_DEVICE_CAPABILITY in this context.  }
  {* Capability type. Will have value
  	 * \ref libusb_capability_type::LIBUSB_BT_USB_2_0_EXTENSION
  	 * LIBUSB_BT_USB_2_0_EXTENSION in this context.  }
  {* Bitmap encoding of supported device level features.
  	 * A value of one in a bit location indicates a feature is
  	 * supported; a value of zero indicates it is not supported.
  	 * See \ref libusb_usb_2_0_extension_attributes.  }
    plibusb_usb_2_0_extension_descriptor = ^libusb_usb_2_0_extension_descriptor;
    libusb_usb_2_0_extension_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bDevCapabilityType : uint8_t;
        bmAttributes : uint32_t;
      end;

  {* \ingroup desc
   * A structure representing the SuperSpeed USB Device Capability descriptor
   * This descriptor is documented in section 9.6.2.2 of the USB 3.0 specification.
   * All multiple-byte fields are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE_CAPABILITY
  	 * LIBUSB_DT_DEVICE_CAPABILITY in this context.  }
  {* Capability type. Will have value
  	 * \ref libusb_capability_type::LIBUSB_BT_SS_USB_DEVICE_CAPABILITY
  	 * LIBUSB_BT_SS_USB_DEVICE_CAPABILITY in this context.  }
  {* Bitmap encoding of supported device level features.
  	 * A value of one in a bit location indicates a feature is
  	 * supported; a value of zero indicates it is not supported.
  	 * See \ref libusb_ss_usb_device_capability_attributes.  }
  {* Bitmap encoding of the speed supported by this device when
  	 * operating in SuperSpeed mode. See \ref libusb_supported_speed.  }
  {* The lowest speed at which all the functionality supported
  	 * by the device is available to the user. For example if the
  	 * device supports all its functionality when connected at
  	 * full speed and above then it sets this value to 1.  }
  {* U1 Device Exit Latency.  }
  {* U2 Device Exit Latency.  }
  	plibusb_ss_usb_device_capability_descriptor = ^libusb_ss_usb_device_capability_descriptor;
    libusb_ss_usb_device_capability_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bDevCapabilityType : uint8_t;
        bmAttributes : uint8_t;
        wSpeedSupported : uint16_t;
        bFunctionalitySupport : uint8_t;
        bU1DevExitLat : uint8_t;
        bU2DevExitLat : uint16_t;
      end;

  {* \ingroup desc
   * A structure representing the Container ID descriptor.
   * This descriptor is documented in section 9.6.2.3 of the USB 3.0 specification.
   * All multiple-byte fields, except UUIDs, are represented in host-endian format.
    }
  {* Size of this descriptor (in bytes)  }
  {* Descriptor type. Will have value
  	 * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE_CAPABILITY
  	 * LIBUSB_DT_DEVICE_CAPABILITY in this context.  }
  {* Capability type. Will have value
  	 * \ref libusb_capability_type::LIBUSB_BT_CONTAINER_ID
  	 * LIBUSB_BT_CONTAINER_ID in this context.  }
  {* Reserved field  }
  {* 128 bit UUID  }
  	plibusb_container_id_descriptor = ^libusb_container_id_descriptor;
    libusb_container_id_descriptor = record
        bLength : uint8_t;
        bDescriptorType : uint8_t;
        bDevCapabilityType : uint8_t;
        bReserved : uint8_t;
        ContainerID : array[0..15] of uint8_t;
      end;

  {* \ingroup asyncio
   * Setup packet for control transfers.  }
  {* Request type. Bits 0:4 determine recipient, see
  	 * \ref libusb_request_recipient. Bits 5:6 determine type, see
  	 * \ref libusb_request_type. Bit 7 determines data transfer direction, see
  	 * \ref libusb_endpoint_direction.
  	  }
  {* Request. If the type bits of bmRequestType are equal to
  	 * \ref libusb_request_type::LIBUSB_REQUEST_TYPE_STANDARD
  	 * "LIBUSB_REQUEST_TYPE_STANDARD" then this field refers to
  	 * \ref libusb_standard_request. For other cases, use of this field is
  	 * application-specific.  }
  {* Value. Varies according to request  }
  {* Index. Varies according to request, typically used to pass an index
  	 * or offset  }
  {* Number of bytes to transfer  }
  	plibusb_control_setup = ^libusb_control_setup;
    libusb_control_setup = record
        bmRequestType : uint8_t;
        bRequest : uint8_t;
        wValue : uint16_t;
        wIndex : uint16_t;
        wLength : uint16_t;
      end;

    const LIBUSB_CONTROL_SETUP_SIZE = sizeof(libusb_control_setup);

type
    { libusb  }
      plibusb_context = ^libusb_context;
      libusb_context = record
          {undefined structure}
        end;

      plibusb_device=^libusb_device;
      pplibusb_device=^plibusb_device;
      libusb_device = record
          {undefined structure}
        end;

      plibusb_device_handle=^libusb_device_handle;
      libusb_device_handle = record
          {undefined structure}
        end;

    {* \ingroup lib
     * Structure providing the version of the libusb runtime
      }



(* Const before type ignored *)

(* Const before type ignored *)

(* Const before type ignored *)

(* Const before type ignored *)

(* Const before type ignored *)
       plibusb_version = ^libusb_version;
       libusb_version = record
          {* Library major version.  }
          major : uint16_t;
          {* Library minor version.  }
          minor : uint16_t;
          {* Library micro version.  }
          micro : uint16_t;
          {* Library nano version.  }
          nano : uint16_t;
          {* Library release candidate suffix string, e.g. "-rc4".  }
          rc : ^ansichar;
          {* For ABI compatibility only.  }
          describe : ^ansichar;
        end;

{* \ingroup lib
 * Structure representing a libusb session. The concept of individual libusb
 * sessions allows for your program to use two libraries (or dynamically
 * load two modules) which both independently use libusb. This will prevent
 * interference between the individual libusb users - for example
 * libusb_set_debug() will not affect the other user of the library, and
 * libusb_exit() will not destroy resources that the other user is still
 * using.
 *
 * Sessions are created by libusb_init() and destroyed through libusb_exit().
 * If your application is guaranteed to only ever include a single libusb
 * user (i.e. you), you do not have to worry about contexts: pass NULL in
 * every function call where a context is required. The default context
 * will be used.
 *
 * For more information, see \ref contexts.
  }
{* \ingroup dev
 * Structure representing a USB device detected on the system. This is an
 * opaque type for which you are only ever provided with a pointer, usually
 * originating from libusb_get_device_list().
 *
 * Certain operations can be performed on a device, but in order to do any
 * I/O you will have to first obtain a device handle using libusb_open().
 *
 * Devices are reference counted with libusb_ref_device() and
 * libusb_unref_device(), and are freed when the reference count reaches 0.
 * New devices presented by libusb_get_device_list() have a reference count of
 * 1, and libusb_free_device_list() can optionally decrease the reference count
 * on all devices in the list. libusb_open() adds another reference which is
 * later destroyed by libusb_close().
  }
{* \ingroup dev
 * Structure representing a handle on a USB device. This is an opaque type for
 * which you are only ever provided with a pointer, usually originating from
 * libusb_open().
 *
 * A device handle is used to perform I/O and other operations. When finished
 * with a device handle, you should call libusb_close().
  }
{* \ingroup dev
 * Speed codes. Indicates the speed at which the device is operating.
  }
  libusb_speed = (
    {* The OS doesn't report or know the device speed.  }
    LIBUSB_SPEED_UNKNOWN = 0,
    {* The device is operating at low speed (1.5MBit/s).  }
    LIBUSB_SPEED_LOW = 1,
    {* The device is operating at full speed (12MBit/s).  }
    LIBUSB_SPEED_FULL = 2,
    {* The device is operating at high speed (480MBit/s).  }
    LIBUSB_SPEED_HIGH = 3,
    {* The device is operating at super speed (5000MBit/s).  }
    LIBUSB_SPEED_SUPER = 4);

{* \ingroup dev
 * Supported speeds (wSpeedSupported) bitfield. Indicates what
 * speeds the device supports.
  }
  libusb_supported_speed = (
    {* Low speed operation supported (1.5MBit/s).  }
    LIBUSB_LOW_SPEED_OPERATION = 1,
    {* Full speed operation supported (12MBit/s).  }
    LIBUSB_FULL_SPEED_OPERATION = 2,
    {* High speed operation supported (480MBit/s).  }
    LIBUSB_HIGH_SPEED_OPERATION = 4,
    {* Superspeed operation supported (5000MBit/s).  }
    LIBUSB_SUPER_SPEED_OPERATION = 8
    );

{* \ingroup dev
 * Masks for the bits of the
 * \ref libusb_usb_2_0_extension_descriptor::bmAttributes "bmAttributes" field
 * of the USB 2.0 Extension descriptor.
  }
    {* Supports Link Power Management (LPM)  }
    libusb_usb_2_0_extension_attributes = (LIBUSB_BM_LPM_SUPPORT = 2);

{* \ingroup dev
 * Masks for the bits of the
 * \ref libusb_ss_usb_device_capability_descriptor::bmAttributes "bmAttributes" field
 * field of the SuperSpeed USB Device Capability descriptor.
  }
    {* Supports Latency Tolerance Messages (LTM)  }
    libusb_ss_usb_device_capability_attributes = (LIBUSB_BM_LTM_SUPPORT = 2);

{* \ingroup dev
 * USB capability types
  }
  libusb_bos_type = (
    {* Wireless USB device capability  }
    LIBUSB_BT_WIRELESS_USB_DEVICE_CAPABILITY = 1,
    {* USB 2.0 extensions  }
    LIBUSB_BT_USB_2_0_EXTENSION = 2,
    {* SuperSpeed USB device capability  }
    LIBUSB_BT_SS_USB_DEVICE_CAPABILITY = 3,
    {* Container ID type  }
    LIBUSB_BT_CONTAINER_ID = 4);

{* \ingroup misc
 * Error codes. Most libusb functions return 0 on success or one of these
 * codes on failure.
 * You can call libusb_error_name() to retrieve a string representation of an
 * error code or libusb_strerror() to get an end-user suitable description of
 * an error code.
  }
{ NB: Remember to update LIBUSB_ERROR_COUNT below as well as the
     message strings in strerror.c when adding new error codes here.  }
  libusb_error = (
    {* Success (no error)  }
    LIBUSB_SUCCESS = 0,
    {* Input/output error  }
    LIBUSB_ERROR_IO = -(1),
    {* Invalid parameter  }
    LIBUSB_ERROR_INVALID_PARAM = -(2),
    {* Access denied (insufficient permissions)  }
    LIBUSB_ERROR_ACCESS = -(3),
    {* No such device (it may have been disconnected)  }
    LIBUSB_ERROR_NO_DEVICE = -(4),
    {* Entity not found  }
    LIBUSB_ERROR_NOT_FOUND = -(5),
    {* Resource busy  }
    LIBUSB_ERROR_BUSY = -(6),
    {* Operation timed out  }
    LIBUSB_ERROR_TIMEOUT = -(7),
    {* Overflow  }
    LIBUSB_ERROR_OVERFLOW = -(8),
    {* Pipe error  }
    LIBUSB_ERROR_PIPE = -(9),
    {* System call interrupted (perhaps due to signal)  }
    LIBUSB_ERROR_INTERRUPTED = -(10),
    {* Insufficient memory  }
    LIBUSB_ERROR_NO_MEM = -(11),
    {* Operation not supported or unimplemented on this platform  }
    LIBUSB_ERROR_NOT_SUPPORTED = -(12),
    {* Other error  }
    LIBUSB_ERROR_OTHER = -(99)
    );

{ Total number of error codes in enum libusb_error  }
const
    LIBUSB_ERROR_COUNT = 14;

    {* \ingroup asyncio
     * Transfer status codes  }





type
  libusb_transfer_status = (
   {* Transfer completed without error. Note that this does not indicate
    	 * that the entire amount of requested data was transferred.  }
    LIBUSB_TRANSFER_COMPLETED,
    {* Transfer failed  }
    LIBUSB_TRANSFER_ERROR,
    {* Transfer timed out  }
    LIBUSB_TRANSFER_TIMED_OUT,
    {* Transfer was cancelled  }
    LIBUSB_TRANSFER_CANCELLED,
    {* For bulk/interrupt endpoints: halt condition detected (endpoint
    	 * stalled). For control endpoints: control request not supported.  }
    LIBUSB_TRANSFER_STALL,
    {* Device was disconnected  }
    LIBUSB_TRANSFER_NO_DEVICE,
    {* Device sent more data than requested  }
    LIBUSB_TRANSFER_OVERFLOW);

  { NB! Remember to update libusb_error_name()
  	   when adding new status codes here.  }


{* \ingroup asyncio
 * libusb_transfer.flags values  }
  libusb_transfer_flags = (
    {* Report short frames as errors  }
    LIBUSB_TRANSFER_SHORT_NOT_OK = 1 shl 0,
    {* Automatically free() transfer buffer during libusb_free_transfer()  }
    LIBUSB_TRANSFER_FREE_BUFFER = 1 shl 1,
    {* Automatically call libusb_free_transfer() after callback returns.
     * If this flag is set, it is illegal to call libusb_free_transfer()
     * from your transfer callback, as this will result in a double-free
     * when this flag is acted upon.  }
    LIBUSB_TRANSFER_FREE_TRANSFER = 1 shl 2,
    {* Terminate transfers that are a multiple of the endpoint's
     * wMaxPacketSize with an extra zero length packet. This is useful
     * when a device protocol mandates that each logical request is
     * terminated by an incomplete packet (i.e. the logical requests are
     * not separated by other means).
     *
     * This flag only affects host-to-device transfers to bulk and interrupt
     * endpoints. In other situations, it is ignored.
     *
     * This flag only affects transfers with a length that is a multiple of
     * the endpoint's wMaxPacketSize. On transfers of other lengths, this
     * flag has no effect. Therefore, if you are working with a device that
     * needs a ZLP whenever the end of the logical request falls on a packet
     * boundary, then it is sensible to set this flag on <em>every</em>
     * transfer (you do not have to worry about only setting it on transfers
     * that end on the boundary).
     *
     * This flag is currently only supported on Linux.
     * On other systems, libusb_submit_transfer() will return
     * LIBUSB_ERROR_NOT_SUPPORTED for every transfer where this flag is set.
     *
     * Available since libusb-1.0.9.
      }
    LIBUSB_TRANSFER_ADD_ZERO_PACKET = 1 shl 3
    );

{* \ingroup asyncio
 * Isochronous packet descriptor.  }
{* Length of data to request in this packet  }
{* Amount of data that was actually transferred  }
{* Status code for this packet  }
  libusb_iso_packet_descriptor = record
      length : dword;
      actual_length : dword;
      status : libusb_transfer_status;
    end;
  {* \ingroup asyncio
        * Asynchronous transfer callback function type. When submitting asynchronous
        * transfers, you pass a pointer to a callback function of this type via the
        * \ref libusb_transfer::callback "callback" member of the libusb_transfer
        * structure. libusb will call this function later, when the transfer has
        * completed or failed. See \ref asyncio for more information.
        * \param transfer The libusb_transfer struct the callback function is being
        * notified about.
         }
         plibusb_transfer=^libusb_transfer;

       libusb_transfer_cb_fn = procedure (transfer:plibusb_transfer);LIBUSB_CALL;

    {* \ingroup asyncio
     * The generic USB transfer structure. The user populates this structure and
     * then submits it in order to request a transfer. After the transfer has
     * completed, the library populates the transfer with the results and passes
     * it back to the user.
      }
    {* Handle of the device that this transfer will be submitted to  }
    {* A bitwise OR combination of \ref libusb_transfer_flags.  }
    {* Address of the endpoint where this transfer will be sent.  }
    {* Type of the endpoint from \ref libusb_transfer_type  }
    {* Timeout for this transfer in millseconds. A value of 0 indicates no
    	 * timeout.  }
    {* The status of the transfer. Read-only, and only for use within
    	 * transfer callback function.
    	 *
    	 * If this is an isochronous transfer, this field may read COMPLETED even
    	 * if there were errors in the frames. Use the
    	 * \ref libusb_iso_packet_descriptor::status "status" field in each packet
    	 * to determine if errors occurred.  }
    {* Length of the data buffer  }
    {* Actual length of data that was transferred. Read-only, and only for
    	 * use within transfer callback function. Not valid for isochronous
    	 * endpoint transfers.  }
    {* Callback function. This will be invoked when the transfer completes,
    	 * fails, or is cancelled.  }
    {* User context data to pass to the callback function.  }
    {* Data buffer  }
    {* Number of isochronous packets. Only used for I/O with isochronous
    	 * endpoints.  }
    {* Isochronous packet descriptors, for isochronous transfers only.  }

      libusb_transfer = record
          dev_handle : ^libusb_device_handle;
          flags : uint8_t;
          endpoint : byte;
          _type : byte;
          timeout : dword;
          status : libusb_transfer_status;
          length : longint;
          actual_length : longint;
          callback : libusb_transfer_cb_fn;
          user_data : pointer;
          buffer : ^byte;
          num_iso_packets : longint;
          iso_packet_desc : array[0..0] of libusb_iso_packet_descriptor;
        end;




{* \ingroup misc
 * Capabilities supported by an instance of libusb on the current running
 * platform. Test if the loaded library supports a given capability by calling
 * \ref libusb_has_capability().
  }
  libusb_capability = (
    {* The libusb_has_capability() API is available.  }
    LIBUSB_CAP_HAS_CAPABILITY = $0000,
    {* Hotplug support is available on this platform.  }
    LIBUSB_CAP_HAS_HOTPLUG = $0001,
    {* The library can access HID devices without requiring user intervention.
     * Note that before being able to actually access an HID device, you may
     * still have to call additional libusb functions such as
     * \ref libusb_detach_kernel_driver().  }
    LIBUSB_CAP_HAS_HID_ACCESS = $0100,
    {* The library supports detaching of the default USB driver, using
     * \ref libusb_detach_kernel_driver(), if one is set by the OS kernel  }
    LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER = $0101
    );

{* \ingroup lib
 *  Log message levels.
 *  - LIBUSB_LOG_LEVEL_NONE (0)    : no messages ever printed by the library (default)
 *  - LIBUSB_LOG_LEVEL_ERROR (1)   : error messages are printed to stderr
 *  - LIBUSB_LOG_LEVEL_WARNING (2) : warning and error messages are printed to stderr
 *  - LIBUSB_LOG_LEVEL_INFO (3)    : informational messages are printed to stdout, warning
 *    and error messages are printed to stderr
 *  - LIBUSB_LOG_LEVEL_DEBUG (4)   : debug and informational messages are printed to stdout,
 *    warnings and errors to stderr
  }
  libusb_log_level = (
    LIBUSB_LOG_LEVEL_NONE = 0,
    LIBUSB_LOG_LEVEL_ERROR,
    LIBUSB_LOG_LEVEL_WARNING,
    LIBUSB_LOG_LEVEL_INFO,
    LIBUSB_LOG_LEVEL_DEBUG);


function libusb_init(var ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;

procedure libusb_exit(ctx:plibusb_context);LIBUSB_CALL;external libusb1;
procedure libusb_set_debug(ctc:plibusb_context;level:integer);LIBUSB_CALL;external libusb1;

function libusb_get_version():libusb_version;LIBUSB_CALL;external libusb1;
function libusb_has_capability(capability:uint32_t):integer;LIBUSB_CALL;external libusb1;

function libusb_error_name(errcode:integer):pansichar;LIBUSB_CALL;external libusb1;

function libusb_setlocale(const locale:pansichar):integer;LIBUSB_CALL;external libusb1;

function libusb_strerror(errcode:libusb_error):pansichar;LIBUSB_CALL;external libusb1;
function libusb_get_device_list(ctx:plibusb_context;var list:pplibusb_device):ssize_t;LIBUSB_CALL;external libusb1;
procedure libusb_free_device_list(list:pplibusb_device;unref_devices:integer);LIBUSB_CALL;external libusb1;
function libusb_ref_device(dev:plibusb_device):plibusb_device;LIBUSB_CALL;external libusb1;
procedure libusb_unref_device(dev:plibusb_device);LIBUSB_CALL;external libusb1;
function libusb_get_configuration(dev:plibusb_device_handle;
	config:pinteger):integer;LIBUSB_CALL;external libusb1;
function libusb_get_device_descriptor(dev:plibusb_device;
	var desc:libusb_device_descriptor):integer;LIBUSB_CALL;external libusb1;
function libusb_get_active_config_descriptor(dev:plibusb_device;
	var config:plibusb_config_descriptor):integer;LIBUSB_CALL;external libusb1;
function libusb_get_config_descriptor(dev:plibusb_device;
	config_index:uint8_t ; var config:plibusb_config_descriptor):integer;LIBUSB_CALL;external libusb1;

function libusb_get_config_descriptor_by_value(dev:plibusb_device;
	bConfigurationValue:uint8_t;var config:plibusb_config_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_config_descriptor(config:plibusb_config_descriptor);LIBUSB_CALL;external libusb1;

function  libusb_get_ss_endpoint_companion_descriptor(
	ctx:plibusb_context;
	endpoint:plibusb_endpoint_descriptor;
	var ep_comp:plibusb_ss_endpoint_companion_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_ss_endpoint_companion_descriptor(
	ep_comp:plibusb_ss_endpoint_companion_descriptor);LIBUSB_CALL;external libusb1;

function libusb_get_bos_descriptor(
  handle:plibusb_device_handle;
	var bos:plibusb_bos_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_bos_descriptor(bos:plibusb_bos_descriptor);LIBUSB_CALL;external libusb1;

function libusb_get_usb_2_0_extension_descriptor(
	ctx:plibusb_context;
	dev_cap:plibusb_bos_dev_capability_descriptor;
	var usb_2_0_extension:plibusb_usb_2_0_extension_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_usb_2_0_extension_descriptor(
	usb_2_0_extension:plibusb_usb_2_0_extension_descriptor);LIBUSB_CALL;external libusb1;


function libusb_get_ss_usb_device_capability_descriptor(
	ctx:plibusb_context;
	dev_cap:plibusb_bos_dev_capability_descriptor;
	var ss_usb_device_cap:plibusb_ss_usb_device_capability_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_ss_usb_device_capability_descriptor(
	ss_usb_device_cap:plibusb_ss_usb_device_capability_descriptor);LIBUSB_CALL;external libusb1;

function libusb_get_container_id_descriptor(
  ctx:plibusb_context;
	dev_cap:plibusb_bos_dev_capability_descriptor;
	var container_id:plibusb_container_id_descriptor):integer;LIBUSB_CALL;external libusb1;

procedure libusb_free_container_id_descriptor(
	container_id:plibusb_container_id_descriptor);LIBUSB_CALL;external libusb1;

function libusb_get_bus_number(dev:plibusb_device):uint8_t;LIBUSB_CALL;external libusb1;
function libusb_get_port_number(dev:plibusb_device):uint8_t;LIBUSB_CALL;external libusb1;

function libusb_get_port_numbers(
  	dev:plibusb_device;
    port_numbers:puint8_t;
    port_numbers_len:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_get_port_path(
    ctx:plibusb_context;
    dev:plibusb_device;
    path:puint8_t;
    path_length:uint8_t):integer;LIBUSB_CALL;external libusb1;

function libusb_get_parent(dev:plibusb_device):plibusb_device;LIBUSB_CALL;external libusb1;

function libusb_get_device_address(dev:plibusb_device):uint8_t;LIBUSB_CALL;external libusb1;

function libusb_get_device_speed(dev:plibusb_device):integer;LIBUSB_CALL;external libusb1;

function libusb_get_max_packet_size(
    dev:plibusb_device;
		endpoint:uint8_t):integer;LIBUSB_CALL;external libusb1;

function libusb_get_max_iso_packet_size(
    dev:plibusb_device;
		endpoint:uint8_t):integer;LIBUSB_CALL;external libusb1;

function libusb_open(
    dev:plibusb_device;
    var handle:plibusb_device_handle):integer;LIBUSB_CALL;external libusb1;

procedure libusb_close(dev_handle:plibusb_device_handle);LIBUSB_CALL;external libusb1;

function libusb_get_device(dev_handle:plibusb_device_handle):plibusb_device;LIBUSB_CALL;external libusb1;

function libusb_set_configuration(
    dev:plibusb_device_handle;
		configuration:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_claim_interface(
    dev:plibusb_device_handle;
		interface_number:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_release_interface(
    dev:plibusb_device_handle;
		interface_number:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_open_device_with_vid_pid(
		ctx:plibusb_context;
    vendor_id:uint16_t;
    product_id:uint16_t):plibusb_device_handle;LIBUSB_CALL ;external libusb1;

function libusb_set_interface_alt_setting(
    dev:plibusb_device_handle;
		interface_number:integer;
    alternate_setting:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_clear_halt(
    dev:plibusb_device_handle;
		endpoint:uint8_t):integer;LIBUSB_CALL;external libusb1;

function libusb_reset_device(dev:plibusb_device_handle):integer;LIBUSB_CALL;external libusb1;

function libusb_alloc_streams(
    dev:plibusb_device_handle;
		num_streams:uint32_t;
    endpoints:puint8_t;
    num_endpoints:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_free_streams(
    dev:plibusb_device_handle;
		endpoints:puint8_t;
    num_endpoints:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_kernel_driver_active(
    dev:plibusb_device_handle;
		interface_number:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_detach_kernel_driver(
    dev:plibusb_device_handle;
		interface_number:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_attach_kernel_driver(
    dev:plibusb_device_handle;
		interface_number:integer):integer;LIBUSB_CALL;external libusb1;

function libusb_set_auto_detach_kernel_driver(
    dev:plibusb_device_handle;
		enable:integer):integer;LIBUSB_CALL;external libusb1;

{ async I/O  }
{* \ingroup asyncio
 * Get the data section of a control transfer. This convenience function is here
 * to remind you that the data does not start until 8 bytes into the actual
 * buffer, as the setup packet comes first.
 *
 * Calling this function only makes sense from a transfer callback function,
 * or situations where you have already allocated a suitably sized buffer at
 * transfer->buffer.
 *
 * \param transfer a transfer
 * \returns pointer to the first byte of the data section
  }

function  libusb_control_transfer_get_data(
	transfer:plibusb_transfer):puint8_t;inline;



{* \ingroup asyncio
 * Get the control setup packet of a control transfer. This convenience
 * function is here to remind you that the control setup occupies the first
 * 8 bytes of the transfer data buffer.
 *
 * Calling this function only makes sense from a transfer callback function,
 * or situations where you have already allocated a suitably sized buffer at
 * transfer->buffer.
 *
 * \param transfer a transfer
 * \returns a casted pointer to the start of the transfer data buffer
  }

function libusb_control_transfer_get_setup(
  transfer:plibusb_transfer):plibusb_control_setup;


{* \ingroup asyncio
 * Helper function to populate the setup packet (first 8 bytes of the data
 * buffer) for a control transfer. The wIndex, wValue and wLength values should
 * be given in host-endian byte order.
 *
 * \param buffer buffer to output the setup packet into
 * This pointer must be aligned to at least 2 bytes boundary.
 * \param bmRequestType see the
 * \ref libusb_control_setup::bmRequestType "bmRequestType" field of
 * \ref libusb_control_setup
 * \param bRequest see the
 * \ref libusb_control_setup::bRequest "bRequest" field of
 * \ref libusb_control_setup
 * \param wValue see the
 * \ref libusb_control_setup::wValue "wValue" field of
 * \ref libusb_control_setup
 * \param wIndex see the
 * \ref libusb_control_setup::wIndex "wIndex" field of
 * \ref libusb_control_setup
 * \param wLength see the
 * \ref libusb_control_setup::wLength "wLength" field of
 * \ref libusb_control_setup
  }

procedure libusb_fill_control_setup(
  buffer:puint8_t;
  bmRequestType:uint8_t;
  bRequest: uint8_t;
  wValue:uint16_t;
  wIndex:uint16_t;
  wLength:uint16_t);

function libusb_alloc_transfer(iso_packets:integer):plibusb_transfer;LIBUSB_CALL;external libusb1;
function libusb_submit_transfer(transfer:plibusb_transfer):integer;LIBUSB_CALL;external libusb1;
function libusb_cancel_transfer(transfer:plibusb_transfer):integer;LIBUSB_CALL;external libusb1;
procedure libusb_free_transfer(transfer:plibusb_transfer);LIBUSB_CALL;external libusb1;
procedure libusb_transfer_set_stream_id(
		transfer:plibusb_transfer;stream_id:uint32_t);LIBUSB_CALL;external libusb1;
function libusb_transfer_get_stream_id(
		transfer:plibusb_transfer):uint32_t;LIBUSB_CALL;external libusb1;


{* \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for a control transfer.
 *
 * If you pass a transfer buffer to this function, the first 8 bytes will
 * be interpreted as a control setup packet, and the wLength field will be
 * used to automatically populate the \ref libusb_transfer::length "length"
 * field of the transfer. Therefore the recommended approach is:
 * -# Allocate a suitably sized data buffer (including space for control setup)
 * -# Call libusb_fill_control_setup()
 * -# If this is a host-to-device transfer with a data stage, put the data
 *    in place after the setup packet
 * -# Call this function
 * -# Call libusb_submit_transfer()
 *
 * It is also legal to pass a NULL buffer to this function, in which case this
 * function will not attempt to populate the length field. Remember that you
 * must then populate the buffer and length fields later.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param buffer data buffer. If provided, this function will interpret the
 * first 8 bytes as a setup packet and infer the transfer length from that.
 * This pointer must be aligned to at least 2 bytes boundary.
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
  }

procedure libusb_fill_control_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  buffer:puint8_t;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);


{* \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for a bulk transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
  }

procedure libusb_fill_bulk_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);


{* \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for a bulk transfer using bulk streams.
 *
 * Since version 1.0.19, \ref LIBUSB_API_VERSION >= 0x01000103
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param stream_id bulk stream id for this transfer
 * \param buffer data buffer
 * \param length length of data buffer
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
  }

procedure libusb_fill_bulk_stream_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  stream_id:uint32_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);


{* \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for an interrupt transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
  }

procedure libusb_fill_interrupt_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);


{* \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for an isochronous transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param num_iso_packets the number of isochronous packets
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
  }

procedure libusb_fill_iso_transfer(transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  num_iso_packets:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);


{* \ingroup asyncio
 * Convenience function to set the length of all packets in an isochronous
 * transfer, based on the num_iso_packets field in the transfer structure.
 *
 * \param transfer a transfer
 * \param length the length to set in each isochronous packet descriptor
 * \see libusb_get_max_packet_size()
  }

procedure libusb_set_iso_packet_lengths(
  transfer:plibusb_transfer;
  length:cardinal);


{* \ingroup asyncio
 * Convenience function to locate the position of an isochronous packet
 * within the buffer of an isochronous transfer.
 *
 * This is a thorough function which loops through all preceding packets,
 * accumulating their lengths to find the position of the specified packet.
 * Typically you will assign equal lengths to each packet in the transfer,
 * and hence the above method is sub-optimal. You may wish to use
 * libusb_get_iso_packet_buffer_simple() instead.
 *
 * \param transfer a transfer
 * \param packet the packet to return the address of
 * \returns the base address of the packet buffer inside the transfer buffer,
 * or NULL if the packet does not exist.
 * \see libusb_get_iso_packet_buffer_simple()
  }

function libusb_get_iso_packet_buffer(
  	transfer:plibusb_transfer;
    packet:cardinal):puint8_t;


{* \ingroup asyncio
 * Convenience function to locate the position of an isochronous packet
 * within the buffer of an isochronous transfer, for transfers where each
 * packet is of identical size.
 *
 * This function relies on the assumption that every packet within the transfer
 * is of identical size to the first packet. Calculating the location of
 * the packet buffer is then just a simple calculation:
 * <tt>buffer + (packet_size * packet)</tt>
 *
 * Do not use this function on transfers other than those that have identical
 * packet lengths for each packet.
 *
 * \param transfer a transfer
 * \param packet the packet to return the address of
 * \returns the base address of the packet buffer inside the transfer buffer,
 * or NULL if the packet does not exist.
 * \see libusb_get_iso_packet_buffer()
  }

function libusb_get_iso_packet_buffer_simple(
  	transfer:plibusb_transfer;
    packet:cardinal):puint8_t;


{ sync I/O  }

function libusb_control_transfer(
    dev_handle:plibusb_device_handle;
		request_type:uint8_t;
    bRequest:uint8_t;
    wValue:uint16_t;
    wIndex:uint16_t;
		data:puint8_t;
    wLength:uint16_t;
    timeout:cardinal):integer;LIBUSB_CALL;external libusb1;

function libusb_bulk_transfer(
    dev_handle:plibusb_device_handle;
		endpoint:uint8_t;
    data:puint8_t;
    length:integer;
		var actual_length:integer;
    timeout:cardinal):integer;LIBUSB_CALL;external libusb1;

function libusb_interrupt_transfer(
    dev_handle:plibusb_device_handle;
		endpoint:uint8_t;
    data:puint8_t;
    length:integer;
		var actual_length:integer;
    timeout:cardinal):integer;LIBUSB_CALL;external libusb1;

{* \ingroup desc
 * Retrieve a descriptor from the default control pipe.
 * This is a convenience function which formulates the appropriate control
 * message to retrieve the descriptor.
 *
 * \param dev a device handle
 * \param desc_type the descriptor type, see \ref libusb_descriptor_type
 * \param desc_index the index of the descriptor to retrieve
 * \param data output buffer for descriptor
 * \param length size of data buffer
 * \returns number of bytes returned in data, or LIBUSB_ERROR code on failure
  }

function libusb_get_descriptor(
    dev:plibusb_device_handle;
  	desc_type:uint8_t;
    desc_index:uint8_t;
    data:puint8_t;
    length:integer):integer;


{* \ingroup desc
 * Retrieve a descriptor from a device.
 * This is a convenience function which formulates the appropriate control
 * message to retrieve the descriptor. The string returned is Unicode, as
 * detailed in the USB specifications.
 *
 * \param dev a device handle
 * \param desc_index the index of the descriptor to retrieve
 * \param langid the language ID for the string descriptor
 * \param data output buffer for descriptor
 * \param length size of data buffer
 * \returns number of bytes returned in data, or LIBUSB_ERROR code on failure
 * \see libusb_get_string_descriptor_ascii()
  }

function libusb_get_string_descriptor(
    dev:plibusb_device_handle;
  	desc_index:uint8_t;
    langid:uint16_t;
    data:puint8_t;
    length:integer):integer; { wrapper function }

function libusb_get_string_descriptor_ascii(
    dev:plibusb_device_handle;
		desc_index:uint8_t;
    data:puint8_t;
    length:integer):integer;LIBUSB_CALL;external libusb1;

(* polling and timeouts *)

function libusb_try_lock_events(ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;
procedure libusb_lock_events(ctx:plibusb_context);LIBUSB_CALL;external libusb1;
procedure libusb_unlock_events(ctx:plibusb_context);LIBUSB_CALL;external libusb1;
function libusb_event_handling_ok(ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;
function libusb_event_handler_active(ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;
procedure libusb_lock_event_waiters(ctx:plibusb_context);LIBUSB_CALL;external libusb1;
procedure libusb_unlock_event_waiters(ctx:plibusb_context);LIBUSB_CALL;external libusb1;
function libusb_wait_for_event(ctx:plibusb_context; tv:ptimeval):integer;LIBUSB_CALL;external libusb1;

function libusb_handle_events_timeout(ctx:plibusb_context;
	 tv:ptimeval):integer;LIBUSB_CALL;external libusb1;
function libusb_handle_events_timeout_completed(ctx:plibusb_context;
		tv:ptimeval;var completed:integer):integer;LIBUSB_CALL;external libusb1;
function libusb_handle_events(ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;
function libusb_handle_events_completed(ctx:plibusb_context; var completed:integer):integer;LIBUSB_CALL;external libusb1;
function libusb_handle_events_locked(ctx:plibusb_context;
		tv:ptimeval):integer;LIBUSB_CALL;external libusb1;
function libusb_pollfds_handle_timeouts(ctx:plibusb_context):integer;LIBUSB_CALL;external libusb1;
function libusb_get_next_timeout(ctx:plibusb_context;
		tv:ptimeval):integer;LIBUSB_CALL;external libusb1;


{* \ingroup poll
 * File descriptor for polling
  }
{* Numeric file descriptor  }
{* Event flags to poll for from <poll.h>. POLLIN indicates that you
   * should monitor this file descriptor for becoming ready to read from,
   * and POLLOUT indicates that you should monitor this file descriptor for
   * nonblocking write readiness.  }

type
  plibusb_pollfd = ^libusb_pollfd;
  libusb_pollfd = record
      fd : longint;
      events : smallint;
    end;

{* \ingroup poll
 * Callback function, invoked when a new file descriptor should be added
 * to the set of file descriptors monitored for events.
 * \param fd the new file descriptor
 * \param events events to monitor for, see \ref libusb_pollfd for a
 * description
 * \param user_data User data pointer specified in
 * libusb_set_pollfd_notifiers() call
 * \see libusb_set_pollfd_notifiers()
  }

type
  libusb_pollfd_added_cb = procedure(
      fd:integer;
      events:smallint;
  		user_data:pointer);LIBUSB_CALL;


{* \ingroup poll
 * Callback function, invoked when a file descriptor should be removed from
 * the set of file descriptors being monitored for events. After returning
 * from this callback, do not use that file descriptor again.
 * \param fd the file descriptor to stop monitoring
 * \param user_data User data pointer specified in
 * libusb_set_pollfd_notifiers() call
 * \see libusb_set_pollfd_notifiers()
  }

type
  libusb_pollfd_removed_cb = procedure(
      fd:integer;
      user_data:pointer);LIBUSB_CALL;

  libusb_get_pollfds = function(
			ctx:plibusb_context):plibusb_pollfd;LIBUSB_CALL;

  libusb_set_pollfd_notifiers = procedure (
      ctx:plibusb_context;
			added_cb:libusb_pollfd_added_cb;
      removed_cb:libusb_pollfd_removed_cb;
			user_data:pointer);LIBUSB_CALL;



{* \ingroup hotplug
 * Callback handle.
 *
 * Callbacks handles are generated by libusb_hotplug_register_callback()
 * and can be used to deregister callbacks. Callback handles are unique
 * per libusb_context and it is safe to call libusb_hotplug_deregister_callback()
 * on an already deregisted callback.
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * For more information, see \ref hotplug.
  }

  libusb_hotplug_callback_handle = longint;
{* \ingroup hotplug
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * Flags for hotplug events  }
{* Default value when not using any flags.  }
{* Arm the callback and fire it for all matching currently attached devices.  }

  libusb_hotplug_flag = (LIBUSB_HOTPLUG_NO_FLAGS = 0, LIBUSB_HOTPLUG_ENUMERATE = 1
    );
{* \ingroup hotplug
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * Hotplug events  }
{* A device has been plugged in and is ready to use  }
{* A device has left and is no longer available.
   * It is the user's responsibility to call libusb_close on any handle associated with a disconnected device.
   * It is safe to call libusb_get_device_descriptor on a device that has left  }

  libusb_hotplug_event = (LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED = $01,
    LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT = $02
    );
{* \ingroup hotplug
 * Wildcard matching for hotplug events  }

const
  LIBUSB_HOTPLUG_MATCH_ANY = -(1);

{* \ingroup hotplug
 * Hotplug callback function type. When requesting hotplug event notifications,
 * you pass a pointer to a callback function of this type.
 *
 * This callback may be called by an internal event thread and as such it is
 * recommended the callback do minimal processing before returning.
 *
 * libusb will call this function later, when a matching event had happened on
 * a matching device. See \ref hotplug for more information.
 *
 * It is safe to call either libusb_hotplug_register_callback() or
 * libusb_hotplug_deregister_callback() from within a callback function.
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * \param ctx            context of this notification
 * \param device         libusb_device this event occurred on
 * \param event          event that occurred
 * \param user_data      user data provided when this callback was registered
 * \returns bool whether this callback is finished processing events.
 *                       returning 1 will cause this callback to be deregistered
  }

type
  libusb_hotplug_callback_fn = function(
      	ctx:plibusb_context;
				device:plibusb_device;
				event:libusb_hotplug_event;
				user_data:pointer):integer;LIBUSB_CALL;

{* \ingroup hotplug
 * Register a hotplug callback function
 *
 * Register a callback with the libusb_context. The callback will fire
 * when a matching event occurs on a matching device. The callback is
 * armed until either it is deregistered with libusb_hotplug_deregister_callback()
 * or the supplied callback returns 1 to indicate it is finished processing events.
 *
 * If the \ref LIBUSB_HOTPLUG_ENUMERATE is passed the callback will be
 * called with a \ref LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED for all devices
 * already plugged into the machine. Note that libusb modifies its internal
 * device list from a separate thread, while calling hotplug callbacks from
 * libusb_handle_events(), so it is possible for a device to already be present
 * on, or removed from, its internal device list, while the hotplug callbacks
 * still need to be dispatched. This means that when using \ref
 * LIBUSB_HOTPLUG_ENUMERATE, your callback may be called twice for the arrival
 * of the same device, once from libusb_hotplug_register_callback() and once
 * from libusb_handle_events(); and/or your callback may be called for the
 * removal of a device for which an arrived call was never made.
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * \param[in] ctx context to register this callback with
 * \param[in] events bitwise or of events that will trigger this callback. See \ref
 *            libusb_hotplug_event
 * \param[in] flags hotplug callback flags. See \ref libusb_hotplug_flag
 * \param[in] vendor_id the vendor id to match or \ref LIBUSB_HOTPLUG_MATCH_ANY
 * \param[in] product_id the product id to match or \ref LIBUSB_HOTPLUG_MATCH_ANY
 * \param[in] dev_class the device class to match or \ref LIBUSB_HOTPLUG_MATCH_ANY
 * \param[in] cb_fn the function to be invoked on a matching event/device
 * \param[in] user_data user data to pass to the callback function
 * \param[out] handle pointer to store the handle of the allocated callback (can be NULL)
 * \returns LIBUSB_SUCCESS on success LIBUSB_ERROR code on failure
  }

	function libusb_hotplug_register_callback(
        ctx:plibusb_context;
  			events:libusb_hotplug_event;
  			flags:libusb_hotplug_flag;
  			vendor_id:integer;
        product_id:integer;
  			dev_class:integer;
  			cb_fn:libusb_hotplug_callback_fn;
  			user_data:pointer;
  			var handle:libusb_hotplug_callback_handle):integer;LIBUSB_CALL;external libusb1;


{* \ingroup hotplug
 * Deregisters a hotplug callback.
 *
 * Deregister a callback from a libusb_context. This function is safe to call from within
 * a hotplug callback.
 *
 * Since version 1.0.16, \ref LIBUSB_API_VERSION >= 0x01000102
 *
 * \param[in] ctx context this callback is registered with
 * \param[in] handle the handle of the callback to deregister
  }
  procedure libusb_hotplug_deregister_callback(
        ctx:plibusb_context;
  			handle:libusb_hotplug_callback_handle);LIBUSB_CALL;external libusb1;


{$endif}


implementation



function libusb_cpu_to_le16(const x:uint16_t):uint16_t;inline;
type
  w_union=record
  case byte of
  0:(b8:array[0..1] of uint8_t);
  1:(b16:uint16_t);
  end;
var
  _tmp:w_union;
begin
  _tmp.b8[1] := uint8_t (x shl 8);
  _tmp.b8[0] := uint8_t (x and $ff);
  Result:=_tmp.b16;
end;

function libusb_le16_to_cpu(const x:uint16_t):uint16_t;inline;
begin
  Result:=libusb_cpu_to_le16(x);
end;

function  libusb_control_transfer_get_data(
	transfer:plibusb_transfer):puint8_t;inline;
begin
	result := pointer(transfer^.buffer) + LIBUSB_CONTROL_SETUP_SIZE;
end;

function libusb_control_transfer_get_setup(
  transfer:plibusb_transfer):plibusb_control_setup;inline;
begin
  Result:= plibusb_control_setup(transfer^.buffer);
end;


procedure libusb_fill_control_setup(
  buffer:puint8_t;
  bmRequestType:uint8_t;
  bRequest: uint8_t;
  wValue:uint16_t;
  wIndex:uint16_t;
  wLength:uint16_t);inline;
var setup:plibusb_control_setup;
begin
  setup := plibusb_control_setup(buffer);
  setup^.bmRequestType := bmRequestType;
  setup^.bRequest := bRequest;
  setup^.wValue := libusb_cpu_to_le16(wValue);
  setup^.wIndex := libusb_cpu_to_le16(wIndex);
  setup^.wLength := libusb_cpu_to_le16(wLength);
end;

procedure libusb_fill_control_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  buffer:puint8_t;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);inline;
var
  setup:plibusb_control_setup;
begin
  setup := plibusb_control_setup(buffer);
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint := 0;
  transfer^._type := byte(LIBUSB_TRANSFER_TYPE_CONTROL);
  transfer^.timeout := timeout;
  transfer^.buffer := buffer;
  if (setup<>nil) then
  	transfer^.length := (LIBUSB_CONTROL_SETUP_SIZE
  		+ libusb_le16_to_cpu(setup^.wLength));
  transfer^.user_data := user_data;
  transfer^.callback := callback;
end;

procedure libusb_fill_bulk_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);inline;
begin
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint := endpoint;
  transfer^._type := byte(LIBUSB_TRANSFER_TYPE_BULK);
  transfer^.timeout := timeout;
  transfer^.buffer := buffer;
  transfer^.length := length;
  transfer^.user_data := user_data;
  transfer^.callback := callback;
end;

procedure libusb_fill_bulk_stream_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  stream_id:uint32_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);inline;
begin
  libusb_fill_bulk_transfer(transfer, dev_handle, endpoint, buffer,
  				length, callback, user_data, timeout);
  transfer^._type := byte(LIBUSB_TRANSFER_TYPE_BULK_STREAM);
  libusb_transfer_set_stream_id(transfer, stream_id);
end;

procedure libusb_fill_interrupt_transfer(
  transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);inline;
begin
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint := endpoint;
  transfer^._type := byte(LIBUSB_TRANSFER_TYPE_INTERRUPT);
  transfer^.timeout := timeout;
  transfer^.buffer := buffer;
  transfer^.length := length;
  transfer^.user_data := user_data;
  transfer^.callback := callback;
end;

procedure libusb_fill_iso_transfer(transfer:plibusb_transfer;
  dev_handle:plibusb_device_handle;
  endpoint:uint8_t;
  buffer:puint8_t;
  length:integer;
  num_iso_packets:integer;
  callback:libusb_transfer_cb_fn;
  user_data:pointer;
  timeout:cardinal);inline;
begin
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint := endpoint;
  transfer^._type := byte(LIBUSB_TRANSFER_TYPE_ISOCHRONOUS);
  transfer^.timeout := timeout;
  transfer^.buffer := buffer;
  transfer^.length := length;
  transfer^.num_iso_packets := num_iso_packets;
  transfer^.user_data := user_data;
  transfer^.callback := callback;
end;

procedure libusb_set_iso_packet_lengths(
  transfer:plibusb_transfer;
  length:cardinal);inline;
var
  i:integer;
begin
  for i := 0 to transfer^.num_iso_packets-1 do
  	transfer^.iso_packet_desc[i].length := length;
end;

function libusb_get_iso_packet_buffer(
  	transfer:plibusb_transfer;
    packet:cardinal):puint8_t;inline;
var
  i:integer;
  offset:size_t;
  _packet:integer;
begin
  (* oops..slight bug in the API. packet is an unsigned int, but we use
   * signed integers almost everywhere else. range-check and convert to
   * signed to avoid compiler warnings. FIXME for libusb-2. *)
  if (packet > MaxInt) then
  begin
    Result:=nil;
  	exit;;
  end;

  _packet := integer(packet);

  if (_packet >= transfer^.num_iso_packets) then
  begin
    Result:=nil;
  	exit;;
  end;

  for i := 0 to _packet-1 do
  	offset := offset + transfer^.iso_packet_desc[i].length;

  Result := pointer(transfer^.buffer) + offset;
end;


function libusb_get_iso_packet_buffer_simple(
  	transfer:plibusb_transfer;
    packet:cardinal):puint8_t;inline;
var
  _packet:integer;
begin
  (* oops..slight bug in the API. packet is an unsigned int, but we use
   * signed integers almost everywhere else. range-check and convert to
   * signed to avoid compiler warnings. FIXME for libusb-2. *)
  if (packet > MaxInt) then
  begin
    Result:=nil;
  	exit;;
  end;

  _packet := integer(packet);

  if (_packet >= transfer^.num_iso_packets) then
  begin
    Result:=nil;
  	exit;;
  end;

  Result := pointer(transfer^.buffer) + integer(transfer^.iso_packet_desc[0].length * _packet);
end;


function libusb_get_descriptor(
    dev:plibusb_device_handle;
  	desc_type:uint8_t;
    desc_index:uint8_t;
    data:puint8_t;
    length:integer):integer;
begin
  Result := libusb_control_transfer(dev, byte(LIBUSB_ENDPOINT_IN),
  	byte(LIBUSB_REQUEST_GET_DESCRIPTOR), uint16_t( (desc_type shl 8) or desc_index),
  	0, data, uint16_t(length), 1000);
end;


function libusb_get_string_descriptor(
    dev:plibusb_device_handle;
  	desc_index:uint8_t;
    langid:uint16_t;
    data:puint8_t;
    length:integer):integer;
begin
  result:= libusb_control_transfer(dev, byte(LIBUSB_ENDPOINT_IN),
  	byte(LIBUSB_REQUEST_GET_DESCRIPTOR), uint16_t((byte(LIBUSB_DT_STRING) shl 8) or desc_index),
  	langid, data, uint16_t (length), 1000);
end;


end.
