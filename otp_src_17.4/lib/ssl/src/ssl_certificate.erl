%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2014 All Rights Reserved.
%%
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
%% %CopyrightEnd%
%%

%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling certificat verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%%----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-export([trusted_cert_and_path/4,
	 certificate_chain/3,
	 file_to_certificats/2,
	 validate_extension/3,
	 is_valid_extkey_usage/2,
	 is_valid_key_usage/2,
	 select_extension/2,
	 extensions_list/1,
	 public_key_type/1
	]).
 
%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec trusted_cert_and_path([der_cert()], db_handle(), certdb_ref(), fun()) ->
				   {der_cert() | unknown_ca, [der_cert()]}.
%%
%% Description: Extracts the root cert (if not presents tries to 
%% look it up, if not found {bad_cert, unknown_ca} will be added verification
%% errors. Returns {RootCert, Path, VerifyErrors}
%%--------------------------------------------------------------------
trusted_cert_and_path(CertChain, CertDbHandle, CertDbRef, PartialChainHandler) ->
    Path = [Cert | _] = lists:reverse(CertChain),
    OtpCert = public_key:pkix_decode_cert(Cert, otp),
    SignedAndIssuerID =
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{ok, IssuerId} = public_key:pkix_issuer_id(OtpCert, self),
		{self, IssuerId};
	    false ->
		other_issuer(OtpCert, CertDbHandle)
	end,
    
    case SignedAndIssuerID of
	{error, issuer_not_found} ->
	    %% The root CA was not sent and can not be found.
	    handle_incomplete_chain(Path, PartialChainHandler);
	{self, _} when length(Path) == 1 ->
	    {selfsigned_peer, Path};
	{_ ,{SerialNr, Issuer}} ->
	    case ssl_manager:lookup_trusted_cert(CertDbHandle, CertDbRef, SerialNr, Issuer) of
		{ok, Trusted} ->
		    %% Trusted must be selfsigned or it is an incomplete chain
		    handle_path(Trusted, Path, PartialChainHandler);
		_ ->
		    %% Root CA could not be verified
		    handle_incomplete_chain(Path, PartialChainHandler)
	    end
    end.

%%--------------------------------------------------------------------
-spec certificate_chain(undefined | binary(), db_handle(), certdb_ref()) ->
			  {error, no_cert} | {ok, [der_cert()]}.
%%
%% Description: Return the certificate chain to send to peer.
%%--------------------------------------------------------------------
certificate_chain(undefined, _, _) ->
    {error, no_cert};
certificate_chain(OwnCert, CertDbHandle, CertsDbRef) ->
    ErlCert = public_key:pkix_decode_cert(OwnCert, otp),
    certificate_chain(ErlCert, OwnCert, CertDbHandle, CertsDbRef, [OwnCert]).
%%--------------------------------------------------------------------
-spec file_to_certificats(binary(), term()) -> [der_cert()].
%%
%% Description: Return list of DER encoded certificates.
%%--------------------------------------------------------------------
file_to_certificats(File, DbHandle) ->
    {ok, List} = ssl_manager:cache_pem_file(File, DbHandle),
    [Bin || {'Certificate', Bin, not_encrypted} <- List].
%%--------------------------------------------------------------------
-spec validate_extension(term(), {extension, #'Extension'{}} | {bad_cert, atom()} | valid,
			 term()) -> {valid, term()} |
				    {fail, tuple()} |
				    {unknown, term()}.
%%
%% Description:  Validates ssl/tls specific extensions
%%--------------------------------------------------------------------
validate_extension(_,{extension, #'Extension'{extnID = ?'id-ce-extKeyUsage',
					      extnValue = KeyUse}}, Role) ->
    case is_valid_extkey_usage(KeyUse, Role) of
	true ->
	    {valid, Role};
	false ->
	    {fail, {bad_cert, invalid_ext_key_usage}}
    end;
validate_extension(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
validate_extension(_, {extension, _}, Role) ->
    {unknown, Role};
validate_extension(_, valid, Role) ->
    {valid, Role};
validate_extension(_, valid_peer, Role) ->
    {valid, Role}.

%%--------------------------------------------------------------------
-spec is_valid_key_usage(list(), term()) -> boolean().
%%
%% Description: Checks if Use is a valid key usage.
%%--------------------------------------------------------------------
is_valid_key_usage(KeyUse, Use) ->
    lists:member(Use, KeyUse).
 
%%--------------------------------------------------------------------
-spec select_extension(term(), list()) -> undefined | #'Extension'{}.
%%
%% Description: Selects the extension identified by Id if present in
%% a list of extensions.
%%--------------------------------------------------------------------
select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

%%--------------------------------------------------------------------
-spec extensions_list(asn1_NOVALUE | list()) -> list().
%%
%% Description: Handles that 
%%--------------------------------------------------------------------
extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.

%%--------------------------------------------------------------------
-spec public_key_type(term()) -> rsa | dsa | ec.
%%
%% Description:
%%--------------------------------------------------------------------
public_key_type(?'rsaEncryption') ->
    rsa;
public_key_type(?'id-dsa') ->
    dsa;
public_key_type(?'id-ecPublicKey') ->
    ec.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_chain(OtpCert, _Cert, CertDbHandle, CertsDbRef, Chain) ->
    IssuerAndSelfSigned = 
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{public_key:pkix_issuer_id(OtpCert, self), true};
	    false  ->
		{public_key:pkix_issuer_id(OtpCert, other), false}
	end,
    
    case IssuerAndSelfSigned of 
	{_, true = SelfSigned} ->
	    certificate_chain(CertDbHandle, CertsDbRef, Chain, ignore, ignore, SelfSigned);
	{{error, issuer_not_found}, SelfSigned} ->
	    case find_issuer(OtpCert, CertDbHandle) of
		{ok, {SerialNr, Issuer}} ->
		    certificate_chain(CertDbHandle, CertsDbRef, Chain,
				      SerialNr, Issuer, SelfSigned);
		_ ->
		    %% Guess the the issuer must be the root
		    %% certificate. The verification of the
		    %% cert chain will fail if guess is
		    %% incorrect.
		    {ok, lists:reverse(Chain)}
	    end;
	{{ok, {SerialNr, Issuer}}, SelfSigned} -> 
	    certificate_chain(CertDbHandle, CertsDbRef, Chain, SerialNr, Issuer, SelfSigned)
    end.
  
certificate_chain(_,_, Chain, _SerialNr, _Issuer, true) ->
    {ok, lists:reverse(Chain)};

certificate_chain(CertDbHandle, CertsDbRef, Chain, SerialNr, Issuer, _SelfSigned) ->
    case ssl_manager:lookup_trusted_cert(CertDbHandle, CertsDbRef,
						SerialNr, Issuer) of
	{ok, {IssuerCert, ErlCert}} ->
	    ErlCert = public_key:pkix_decode_cert(IssuerCert, otp),
	    certificate_chain(ErlCert, IssuerCert, 
			      CertDbHandle, CertsDbRef, [IssuerCert | Chain]);
	_ ->
	    %% The trusted cert may be obmitted from the chain as the
	    %% counter part needs to have it anyway to be able to
	    %% verify it.
	    {ok, lists:reverse(Chain)}		      
    end.

find_issuer(OtpCert, CertDbHandle) ->
    IsIssuerFun =
	fun({_Key, {_Der, #'OTPCertificate'{} = ErlCertCandidate}}, Acc) ->
		case public_key:pkix_is_issuer(OtpCert, ErlCertCandidate) of
		    true ->
			case verify_cert_signer(OtpCert, ErlCertCandidate#'OTPCertificate'.tbsCertificate) of
			    true ->
				throw(public_key:pkix_issuer_id(ErlCertCandidate, self));
			    false ->
				Acc
			end;
		    false ->
			Acc
		end;
	   (_, Acc) ->
		Acc
	end,

    try ssl_pkix_db:foldl(IsIssuerFun, issuer_not_found, CertDbHandle) of
	issuer_not_found ->
	    {error, issuer_not_found}
    catch 
	{ok, _IssuerId} = Return ->
	    Return
    end.

is_valid_extkey_usage(KeyUse, client) ->
    %% Client wants to verify server
    is_valid_key_usage(KeyUse,?'id-kp-serverAuth');
is_valid_extkey_usage(KeyUse, server) ->
    %% Server wants to verify client
    is_valid_key_usage(KeyUse, ?'id-kp-clientAuth').

verify_cert_signer(OtpCert, SignerTBSCert) -> 
    PublicKey = public_key(SignerTBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo),
    public_key:pkix_verify(public_key:pkix_encode('OTPCertificate', OtpCert, otp),  PublicKey).

public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
									parameters = Params},
				      subjectPublicKey = Point}) ->
    {Point, Params};
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'rsaEncryption'}, 
				      subjectPublicKey = Key}) ->
    Key;
public_key(#'OTPSubjectPublicKeyInfo'{algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-dsa',
									parameters = {params, Params}},
				      subjectPublicKey = Key}) ->
    {Key, Params}.

other_issuer(OtpCert, CertDbHandle) ->
    case public_key:pkix_issuer_id(OtpCert, other) of
	{ok, IssuerId} ->
	    {other, IssuerId};
	{error, issuer_not_found} ->
	    case find_issuer(OtpCert, CertDbHandle) of
		{ok, IssuerId} ->
		    {other, IssuerId};
		Other ->
		    Other
	    end
    end.

handle_path({BinCert, OTPCert}, Path, PartialChainHandler) ->
    case public_key:pkix_is_self_signed(OTPCert) of
	true ->
	    {BinCert, Path};
	false ->
	   handle_incomplete_chain(Path, PartialChainHandler)
    end.

handle_incomplete_chain(Chain, Fun) ->
    case catch Fun(Chain) of
	{trusted_ca, DerCert} ->
	    new_trusteded_chain(DerCert, Chain);
	unknown_ca = Error ->
	    {Error, Chain};
	_  ->
	    {unknown_ca, Chain}
    end.

new_trusteded_chain(DerCert, [DerCert | Chain]) ->
    {DerCert, Chain};
new_trusteded_chain(DerCert, [_ | Rest]) ->
    new_trusteded_chain(DerCert, Rest);
new_trusteded_chain(_, []) ->
    unknown_ca.