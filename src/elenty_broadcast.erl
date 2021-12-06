%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-module(elenty_broadcast).

-compile(export_all).

do_decode(<<"query">>, <<"elenty:broadcastmessage">>,
	  El, Opts) ->
    decode_elenty_broadcast(<<"elenty:broadcastmessage">>,
			    Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"query">>, <<"elenty:broadcastmessage">>}].

pp(_, _) -> no.

records() -> [].

decode_elenty_broadcast(__TopXMLNS, __Opts,
			{xmlel, <<"query">>, _attrs, _els}) ->
    true.

encode_elenty_broadcast(true, __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"elenty:broadcastmessage">>,
				    [], __TopXMLNS),
    _els = [],
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"query">>, _attrs, _els}.
