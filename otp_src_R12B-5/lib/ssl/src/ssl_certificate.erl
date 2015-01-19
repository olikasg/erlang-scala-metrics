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
%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling certificat verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%%----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_debug.hrl").

-export([trusted_cert_and_path/3, 
	 certificate_chain/2, 
	 file_to_certificats/1]).
 
%%====================================================================
%% Internal application API
%%====================================================================

trusted_cert_and_path(CertChain, CertDbRef, Verify) ->
    [Cert | RestPath] = lists:reverse(CertChain),
    {ok, OtpCert} = public_key:pkix_decode_cert(Cert, otp),
    IssuerAnPath = 
	case public_key:pkix_is_self_signed(OtpCert) of
	    true ->
		{ok, IssuerId} = public_key:pkix_issuer_id(OtpCert, self),
		{IssuerId, RestPath};
	    false  ->
		case  public_key:pkix_issuer_id(OtpCert, other) of
		    {ok, IssuerId} ->
			{IssuerId, [Cert | RestPath]};
		    {error, issuer_not_found} ->
			case find_issuer(OtpCert, no_candidate) of
			    {ok, IssuerId} ->
				{IssuerId, RestPath};
			    Other ->
				{Other, RestPath}
			end
		end
	end,
    
    case IssuerAnPath of
	{{error, issuer_not_found}, _ } ->
	    %% The root CA was not sent and can not be found, we have
	    %% to fail.
	    throw(?ALERT_REC(?FATAL, ?UNKNOWN_CA));
	{{SerialNr, Issuer}, Path} ->
	    case ssl_certificate_db:lookup_trusted_cert(CertDbRef, 
							SerialNr, Issuer) of
		{ok, {BinCert,_}} ->
		    {BinCert, Path, []};
		_ ->
		    %% In this case only fail Verify = true
		    not_valid(?ALERT_REC(?FATAL, ?UNKNOWN_CA),
			     Verify,  {Cert, RestPath})
	    end
    end.

certificate_chain(OwnCert, CertsDbRef) ->
    {ok, ErlCert} = public_key:pkix_decode_cert(OwnCert, otp),
    certificate_chain(ErlCert, OwnCert, CertsDbRef, [OwnCert]).


file_to_certificats(File) ->
    {ok, List} = ssl_manager:cache_pem_file(File),
    [Bin || {cert, Bin, not_encrypted} <- List].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_chain(ErlCert, _Cert, CertsDbRef, Chain) ->    
    IssuerAndSelfSigned = 
	case public_key:pkix_is_self_signed(ErlCert) of
	    true ->
		{public_key:pkix_issuer_id(ErlCert, self), true};
	    false  ->
		{public_key:pkix_issuer_id(ErlCert, other), false}
	end,
    
    case IssuerAndSelfSigned of 
	{{error, issuer_not_found}, _} ->
	    {error, issuer_not_found};
	{{ok, {SerialNr, Issuer}}, SelfSigned} -> 
	    certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, SelfSigned)
    end.
  
certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, SelfSigned) ->
    case ssl_certificate_db:lookup_trusted_cert(CertsDbRef, 
						SerialNr, Issuer) of
	{ok, _IssuerCert} when SelfSigned ->
	    {ok, lists:reverse(Chain)};
	{ok, {IssuerCert, ErlCert}} ->
	    {ok, ErlCert} = public_key:pkix_decode_cert(IssuerCert, otp),
	    certificate_chain(ErlCert, IssuerCert, 
			      CertsDbRef, [IssuerCert | Chain]);
	_ ->
	    {error, {issuer_not_found, {SerialNr, Issuer}}}		      
    end.

find_issuer(OtpCert, PrevCandidateKey) ->
    case ssl_certificate_db:issuer_candidate(PrevCandidateKey) of
 	no_more_candidates ->
 	    {error, issuer_not_found};
 	{Key, BinCandidate} ->
	    case public_key:pkix_is_issuer(OtpCert, BinCandidate) of
		true ->
		    public_key:pkix_issuer_id(BinCandidate, self);
		false ->
		    find_issuer(OtpCert, Key)
	    end
    end.

not_valid(Alert, true, _) ->
    throw(Alert);
not_valid(_, false, {ErlCert, Path}) ->
    {ErlCert, Path, [{bad_cert, unknown_ca}]}.
