%%<copyright>
%% <year>2007-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% Purpose: Record and constant defenitions for the SSL ciphers and
%% the SSL-cipher protocol see RFC 4346, RFC 3268
%%----------------------------------------------------------------------

-ifndef(ssl_cipher).
-define(ssl_cipher, true).

%%% SSL cipher protocol  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(CHANGE_CIPHER_SPEC_PROTO, 1).           % _PROTO to not clash with 
						% SSL record protocol

-record(change_cipher_spec, {
	  type = 1
	 }).

%%% SSL cipher suites %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -record(cipher_state, 
%% 	{
%% 	  suite,
%% 	  name,
%% 	  state
%% 	 }).

-record(cipher_state, {
	  iv,
	  key,
	  state
	 }).

%%% TLS_NULL_WITH_NULL_NULL is specified and is the initial state of a
%%% TLS connection during the first handshake on that channel, but
%%% must not be negotiated, as it provides no more protection than an
%%% unsecured connection.  

%% TLS_NULL_WITH_NULL_NULL = { 0x00,0x00 };
-define(TLS_NULL_WITH_NULL_NULL, <<?BYTE(16#00), ?BYTE(16#00)>>).

%%% The following CipherSuite definitions require that the server
%%% provide an RSA certificate that can be used for key exchange. The
%%% server may request either an RSA or a DSS signature-capable
%%% certificate in the certificate request message.

%%      TLS_RSA_WITH_NULL_MD5 = { 0x00,0x01 };
-define(TLS_RSA_WITH_NULL_MD5, <<?BYTE(16#00), ?BYTE(16#01)>>).

%%      TLS_RSA_WITH_NULL_SHA = { 0x00,0x02 };
-define(TLS_RSA_WITH_NULL_SHA, <<?BYTE(16#00), ?BYTE(16#02)>>).

%%      TLS_RSA_EXPORT_WITH_RC4_40_MD5 = { 0x00,0x03 };
-define(TLS_RSA_EXPORT_WITH_RC4_40_MD5, <<?BYTE(16#00), ?BYTE(16#03)>>).

%%      TLS_RSA_WITH_RC4_128_MD5 = { 0x00,0x04 };
-define(TLS_RSA_WITH_RC4_128_MD5, <<?BYTE(16#00), ?BYTE(16#04)>>).

%%      TLS_RSA_WITH_RC4_128_SHA = { 0x00,0x05 };
-define(TLS_RSA_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#05)>>).

%%      TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5 = { 0x00,0x06 };
-define(TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5, <<?BYTE(16#00), ?BYTE(16#06)>>).

%%      TLS_RSA_WITH_IDEA_CBC_SHA = { 0x00,0x07 };
-define(TLS_RSA_WITH_IDEA_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#07)>>).

%%      TLS_RSA_EXPORT_WITH_DES40_CBC_SHA = { 0x00,0x08 };
-define(TLS_RSA_EXPORT_WITH_DES40_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#08)>>).

%%      TLS_RSA_WITH_DES_CBC_SHA = { 0x00,0x09 };
-define(TLS_RSA_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#09)>>).

%%      TLS_RSA_WITH_3DES_EDE_CBC_SHA = { 0x00,0x0A };
-define(TLS_RSA_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#0A)>>).

%%% The following CipherSuite definitions are used for server-
%%% authenticated (and optionally client-authenticated)
%%% Diffie-Hellman. DH denotes cipher suites in which the server's
%%% certificate contains the Diffie-Hellman parameters signed by the
%%% certificate authority (CA). DHE denotes ephemeral Diffie-Hellman,
%%% where the Diffie-Hellman parameters are signed by a DSS or RSA
%%% certificate, which has been signed by the CA. The signing
%%% algorithm used is specified after the DH or DHE parameter. The
%%% server can request an RSA or DSS signature- capable certificate
%%% from the client for client authentication or it may request a
%%% Diffie-Hellman certificate. Any Diffie-Hellman certificate
%%% provided by the client must use the parameters (group and
%%% generator) described by the server.

%%      TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA   = { 0x00,0x0B }; 
-define(TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#0B)>>).

%%	TLS_DH_DSS_WITH_DES_CBC_SHA            = { 0x00,0x0C };
-define(TLS_DH_DSS_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#0C)>>).

%%	TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x0D };
-define(TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#0D)>>).

%%	TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA   = { 0x00,0x0E };
-define(TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#0E)>>).

%%	TLS_DH_RSA_WITH_DES_CBC_SHA            = { 0x00,0x0F };
-define(TLS_DH_RSA_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#0F)>>).

%%	TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x10 };
-define(TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#10)>>).

%%	TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA  = { 0x00,0x11 };
-define(TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#11)>>).

%%	TLS_DHE_DSS_WITH_DES_CBC_SHA           = { 0x00,0x12 };
-define(TLS_DHE_DSS_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#12)>>).

%%	TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x13 };
-define(TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#13)>>).

%%	TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA  = { 0x00,0x14 };
-define(TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#14)>>).

%%	TLS_DHE_RSA_WITH_DES_CBC_SHA           = { 0x00,0x15 };
-define(TLS_DHE_RSA_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#15)>>).

%%	TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x16 };
-define(TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#16)>>).

%%	TLS_DH_anon_EXPORT_WITH_RC4_40_MD5     = { 0x00,0x17 };
-define(TLS_DH_anon_EXPORT_WITH_RC4_40_MD5,  <<?BYTE(16#00), ?BYTE(16#17)>>).

%%	TLS_DH_anon_WITH_RC4_128_MD5           = { 0x00,0x18 };
-define(TLS_DH_anon_WITH_RC4_128_MD5,   <<?BYTE(16#00),?BYTE(16#18)>>).

%%	TLS_DH_anon_EXPORT_WITH_DES40_CBC_SHA  = { 0x00,0x19 };
-define(TLS_DH_anon_EXPORT_WITH_DES40_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#19)>>).

%%	TLS_DH_anon_WITH_DES_CBC_SHA           = { 0x00,0x1A };
-define(TLS_DH_anon_WITH_DES_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#1A)>>).

%%	TLS_DH_anon_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x1B };
-define(TLS_DH_anon_WITH_3DES_EDE_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#1B)>>).


%%% AES Cipher Suites RFC 3268

%%	TLS_RSA_WITH_AES_128_CBC_SHA      = { 0x00, 0x2F };
-define(TLS_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#2F)>>).   

%%	TLS_DH_DSS_WITH_AES_128_CBC_SHA   = { 0x00, 0x30 };
-define(TLS_DH_DSS_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#30)>>).   
   
%%	TLS_DH_RSA_WITH_AES_128_CBC_SHA   = { 0x00, 0x31 };
-define(TLS_DH_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#31)>>).      

%%	TLS_DHE_DSS_WITH_AES_128_CBC_SHA  = { 0x00, 0x32 };
-define(TLS_DHE_DSS_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#32)>>).     

%%	TLS_DHE_RSA_WITH_AES_128_CBC_SHA  = { 0x00, 0x33 };
-define(TLS_DHE_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#33)>>).   
   
%%	TLS_DH_anon_WITH_AES_128_CBC_SHA  = { 0x00, 0x34 };
-define(TLS_DH_anon_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#34)>>).   
   
%%	TLS_RSA_WITH_AES_256_CBC_SHA      = { 0x00, 0x35 };
-define(TLS_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#35)>>).   
   
%%	TLS_DH_DSS_WITH_AES_256_CBC_SHA   = { 0x00, 0x36 };
-define(TLS_DH_DSS_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#36)>>).   
   
%%	TLS_DH_RSA_WITH_AES_256_CBC_SHA   = { 0x00, 0x37 };
-define(TLS_DH_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#37)>>).   
   
%%	TLS_DHE_DSS_WITH_AES_256_CBC_SHA  = { 0x00, 0x38 };
-define(TLS_DHE_DSS_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#38)>>).   
   
%%	TLS_DHE_RSA_WITH_AES_256_CBC_SHA  = { 0x00, 0x39 };
-define(TLS_DHE_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#39)>>).   
   
%%	TLS_DH_anon_WITH_AES_256_CBC_SHA  = { 0x00, 0x3A };
-define(TLS_DH_anon_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#3A)>>).   

%%% Kerberos Cipher Suites 

%%      TLS_KRB5_WITH_DES_CBC_SHA            = { 0x00,0x1E };
-define(TLS_KRB5_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#1E)>>).
 
%%      TLS_KRB5_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x1F };
-define(TLS_KRB5_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#1F)>>).

%%      TLS_KRB5_WITH_RC4_128_SHA            = { 0x00,0x20 };
-define(TLS_KRB5_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#20)>>). 

%%      TLS_KRB5_WITH_IDEA_CBC_SHA           = { 0x00,0x21 };
-define(TLS_KRB5_WITH_IDEA_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#21)>>).
 
%%      TLS_KRB5_WITH_DES_CBC_MD5            = { 0x00,0x22 };
-define(TLS_KRB5_WITH_DES_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#22)>>). 

%%      TLS_KRB5_WITH_3DES_EDE_CBC_MD5       = { 0x00,0x23 };
-define(TLS_KRB5_WITH_3DES_EDE_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#23)>>). 

%%      TLS_KRB5_WITH_RC4_128_MD5            = { 0x00,0x24 };
-define(TLS_KRB5_WITH_RC4_128_MD5, <<?BYTE(16#00), ?BYTE(16#24)>>). 

%%      TLS_KRB5_WITH_IDEA_CBC_MD5           = { 0x00,0x25 };
-define(TLS_KRB5_WITH_IDEA_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#25)>>).

%%      TLS_KRB5_EXPORT_WITH_DES_CBC_40_SHA  = { 0x00,0x26 };
-define(TLS_KRB5_EXPORT_WITH_DES_CBC_40_SHA, <<?BYTE(16#00), ?BYTE(16#26)>>). 

%%      TLS_KRB5_EXPORT_WITH_RC2_CBC_40_SHA  = { 0x00,0x27 };
-define(TLS_KRB5_EXPORT_WITH_RC2_CBC_40_SHA, <<?BYTE(16#00), ?BYTE(16#27)>>). 

%%      TLS_KRB5_EXPORT_WITH_RC4_40_SHA      = { 0x00,0x28 };
-define(TLS_KRB5_EXPORT_WITH_RC4_40_SHA, <<?BYTE(16#00), ?BYTE(16#28)>>). 

%%      TLS_KRB5_EXPORT_WITH_DES_CBC_40_MD5  = { 0x00,0x29 };
-define(TLS_KRB5_EXPORT_WITH_DES_CBC_40_MD5, <<?BYTE(16#00), ?BYTE(16#29)>>). 

%%      TLS_KRB5_EXPORT_WITH_RC2_CBC_40_MD5  = { 0x00,0x2A };
-define(TLS_KRB5_EXPORT_WITH_RC2_CBC_40_MD5, <<?BYTE(16#00), ?BYTE(16#2A)>>). 

%%      TLS_KRB5_EXPORT_WITH_RC4_40_MD5      = { 0x00,0x2B };
-define(TLS_KRB5_EXPORT_WITH_RC4_40_MD5, <<?BYTE(16#00), ?BYTE(16#2B)>>).

%% Additional TLS ciphersuites from draft-ietf-tls-56-bit-ciphersuites-00.txt

-define(TLS_RSA_EXPORT1024_WITH_RC4_56_MD5, <<?BYTE(16#00), ?BYTE(16#60)>>).
-define(TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5, <<?BYTE(16#00), ?BYTE(16#61)>>).
-define(TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#62)>>).
-define(TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#63)>>).
-define(TLS_RSA_EXPORT1024_WITH_RC4_56_SHA, <<?BYTE(16#00), ?BYTE(16#64)>>).
-define(TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA, <<?BYTE(16#00), ?BYTE(16#65)>>).
-define(TLS_DHE_DSS_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#66)>>).

-endif. % -ifdef(ssl_cipher).
