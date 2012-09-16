{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements XSD Constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit xsd_consts;

interface

const
  s_address                    : WideString = 'address';
  s_all                        : WideString = 'all';
  s_annotation                 : WideString = 'annotation';
  s_any                                     = 'any';
  s_anyAttribute                            = 'anyAttribute';
  s_anyURI                                  = 'anyURI';
  s_appinfo                    : WideString = 'appinfo';
  s_array                      : WideString = 'array';
  s_arrayType                  : WideString = 'arrayType';
  s_attribute                  : WideString = 'attribute';
  s_base                       : WideString = 'base';
  s_binding                    : WideString = 'binding';
  s_body                       : WideString = 'body';
  s_complexContent             : WideString = 'complexContent';
  s_complexType                : WideString = 'complexType';
  s_customAttributes           : WideString = 'customAttributes';
  s_definitions                             = 'definitions';
  s_default                                 = 'default';
  s_document                   : WideString = 'document';
  s_documentation                           = 'documentation';
  s_element                    : WideString = 'element';
  s_enumeration                : WideString = 'enumeration';
  s_extension                  : WideString = 'extension';
  s_guid                       : WideString = 'GUID';
  s_import                                  = 'import';
  s_input                      : WideString = 'input';
  s_item                       : WideString = 'item';
  s_literal                                 = 'literal';
  s_location                   : WideString = 'location';
  s_message                    : WideString = 'message';
  s_maxOccurs                  : WideString = 'maxOccurs';
  s_minOccurs                  : WideString = 'minOccurs';
  s_name                       : WideString = 'name';
  s_namespace                               = 'namespace';
  s_operation                               = 'operation';
  s_optional                   : WideString = 'optional';
  s_output                     : WideString = 'output';
  s_paramAccess                             = 'ParamAccess';
  s_part                       : WideString = 'part';
  s_port                       : WideString = 'port';
  s_portType                                = 'portType';
  s_processContents                         = 'processContents';
  s_prohibited                              = 'prohibited';

  s_ref                        : WideString = 'ref';
  s_required                   : WideString = 'required';
  s_restriction                : WideString = 'restriction';
  //s_return                     : WideString = 'return';
  s_rpc                                     = 'rpc';
  s_schema                     : WideString = 'schema';
  s_schemaLocation                          = 'schemaLocation';
  s_xs                         : WideString = 'http://www.w3.org/2001/XMLSchema';
  s_xs_short                                = 'xsd';
  s_sequence                   : WideString = 'sequence';
  s_service                    : WideString = 'service';
  s_simpleContent              : WideString = 'simpleContent';
  s_simpleType                 : WideString = 'simpleType';
  s_soap                       : WideString = 'http://schemas.xmlsoap.org/wsdl/soap/';
  s_soap_short_name                         = 'soap';
  s_soapAction                              = 'soapAction';
  s_soapEncodingNameSpace                   = 'http://schemas.xmlsoap.org/soap/encoding/';
  s_soapInputEncoding                       = 'Input_EncodingStyle';
  s_soapOutputEncoding                      = 'OutputEncodingStyle';
  s_soapStyle                               = 'style';
  s_soapTransport                           = 'http://schemas.xmlsoap.org/soap/http';
  s_style                                   = 'style';
  s_targetNamespace                         = 'targetNamespace';
  s_tns                                     = 'tns';
  s_transport                               = 'transport';
  s_type                                    = 'type';
  s_types                                   = 'types';
  s_unbounded                               = 'unbounded';
  s_use                                     = 'use';
  s_value                                   = 'value';
  s_wsdl                                    = 'http://schemas.xmlsoap.org/wsdl/';
  s_xmlns                                   = 'xmlns';

  
  s_SDO                           = 'sdo';
  s_SDO_base_namespace            = 'urn:sdo_base';
  s_SDO_collection                = 'sdo_collection';
  s_SDO_headerBlock               = 'sdo_headerBlock';
  s_SDO_headerBlockSimpleContent  = 'sdo_headerBlockSimpleContent';
  s_SDO_record                    = 'sdo_record';
  s_SDO_storeType                 = 'StoreType';
  s_SDO_typeHint                  = 'TypeHint';

  
implementation

end.

