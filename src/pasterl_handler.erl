-module(pasterl_handler).

-export([init/3, allowed_methods/2, content_types_provided/2, resource_exists/2,
	content_types_accepted/2, handle_req/2, paste_text/2, paste_html/2]).

init(_Transport, _Req, []) ->
	pasterl_database:init(),
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, paste_text},
		{{<<"text">>, <<"html">>,  []}, paste_html}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"multipart">>, <<"form-data">>, '*'}, handle_req}], Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(id, Req) of
		{undefined, Req2} ->
			{true, Req2, index};
		{Id, Req2} ->
			case pasterl_database:exists(Id) of
				true ->
					{true, Req2, Id};
				false ->
					{false, Req2, Id}
			end
	end.

handle_req(Req, State) ->
	{headers, _, Req2} = cowboy_req:multipart_data(Req),
	{body, Data, Req3} = cowboy_req:multipart_data(Req2),

	Id        = pasterl_database:insert(Data),
	{Host, _} = cowboy_req:header(<<"host">>, Req3),
	Body      = <<"http://", Host/binary, "/", Id/binary, "\n">>,
	Req4      = cowboy_req:set_resp_body(Body,  Req3),

	{<<"/", Id/binary>>, Req4, State}.

paste_text(Req, index) ->
	{Host, _} = cowboy_req:header(<<"host">>, Req),
	Body = <<"Usage: <comand> | curl -F \"paste=<-\" http://", Host/binary, "\n">>,
	{Body, Req, index};
paste_text(Req, Id) ->
	{Paste} = pasterl_database:select(Id),
	{Paste, Req, Id}.

paste_html(Req, index) ->
	{Host, _} = cowboy_req:header(<<"host">>, Req),
	Body = <<"<html><body><pre>Usage: &lt;comand&gt; | curl -F \"paste=&lt;-\" http://", Host/binary, "</pre></body></html>">>,
	{Body, Req, index};
paste_html(Req, Id) ->
	{Paste} = pasterl_database:select(Id),
	EPaste  = escape_html_chars(Paste),
	Resp = <<"<html><body><pre>", EPaste/binary, "</pre></body></html>">>,
	{Resp, Req, Paste}.

escape_html_chars(Bin) ->
	<<<<(escape_html_char(B))/binary>> || <<B>> <= Bin >>.
escape_html_char($<) ->
	<<"&lt;">>;
escape_html_char($>) ->
	<<"&gt;">>;
escape_html_char($&) ->
	<<"&amp;">>;
escape_html_char(C) ->
	<<C>>.
