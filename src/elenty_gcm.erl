%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-module(elenty_gcm).

-compile(export_all).

do_decode(<<"query">>,
	  <<"https://fcm.googleapis.com/fcm">>, El, Opts) ->
    decode_elenty_gcm(<<"https://fcm.googleapis.com/fcm">>,
		      Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"query">>, <<"https://fcm.googleapis.com/fcm">>}].

pp(_, _) -> no.

records() -> [].

decode_elenty_gcm(__TopXMLNS, __Opts,
		  {xmlel, <<"query">>, _attrs, _els}) ->
    true.

encode_elenty_gcm(true, __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"https://fcm.googleapis.com/fcm">>,
				    [], __TopXMLNS),
    _els = [],
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"query">>, _attrs, _els}.
