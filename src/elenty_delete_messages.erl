-module(elenty_delete_messages).

-compile(export_all).

do_decode(<<"query">>, <<"elenty:deletemessages">>,
	  El, Opts) ->
    decode_elenty_delete_messages(<<"elenty:deletemessages">>,
			    Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"query">>, <<"elenty:deletemessages">>}].

pp(_, _) -> no.

records() -> [].

decode_elenty_delete_messages(__TopXMLNS, __Opts,
			{xmlel, <<"query">>, _attrs, _els}) ->
    true.

encode_elenty_delete_messages(true, __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"elenty:deletemessages">>,
				    [], __TopXMLNS),
    _els = [],
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"query">>, _attrs, _els}.
