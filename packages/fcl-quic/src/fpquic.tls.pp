{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Interfaces between the QUIC connection and a TLS 1.3 handshake implementation (RFC 9001 section 4).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.TLS;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, FpQuic.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, FpQuic.Consts;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Receiver of TLS handshake output; implemented by the QUIC connection.
  IQuicTLSEvents = interface
    // Handshake bytes to send in CRYPTO frames at aLevel.
    procedure TLSSendCrypto(aLevel: TQuicEncryptionLevel; const aData: TBytes);
    // New traffic secret for aLevel; aWrite is True for the sending direction.
    procedure TLSSecret(aLevel: TQuicEncryptionLevel; aWrite: Boolean; aSuite: TQuicCipherSuite;
      const aSecret: TBytes);
    // Transport parameters received from the peer.
    procedure TLSPeerTransportParams(const aParams: TBytes);
    // TLS alert to send; the connection closes with CRYPTO_ERROR + aAlert.
    procedure TLSAlert(aAlert: Byte);
    // 0-RTT traffic secret: the write direction on a client, the read direction on a server.
    procedure TLSEarlySecret(aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);
    // New session ticket received by a client, to pass to IQuicTLS.Resume of a later connection.
    procedure TLSSessionTicket(const aTicket: TBytes);
  end;

  // TLS 1.3 handshake driven by the QUIC connection; the creator of the implementing object owns it.
  IQuicTLS = interface
    // Starts the handshake with the local transport parameters; a client produces its ClientHello here.
    procedure Start(const aEvents: IQuicTLSEvents; const aTransportParams: TBytes);
    // Passes in-order CRYPTO data received at aLevel.
    procedure ProvideData(aLevel: TQuicEncryptionLevel; const aData: TBytes);
    // True once the handshake is complete.
    function IsComplete: Boolean;
    // Application protocol negotiated with ALPN, empty when none.
    function NegotiatedAlpn: AnsiString;
    // Client, before Start: resumes the session of aTicket. Returns True when 0-RTT data may be sent;
    // aPeerParams then receives the encoded transport parameters the server sent with the ticket.
    function Resume(const aTicket: TBytes; out aPeerParams: TBytes): Boolean;
    // True once the handshake completed with 0-RTT data accepted.
    function EarlyDataAccepted: Boolean;
  end;

implementation

end.
