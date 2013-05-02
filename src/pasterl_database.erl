-module(pasterl_database).
-export([init/0, insert/1, select/1, exists/1]).
-include_lib("stdlib/include/qlc.hrl").

-record(paste, {index, content}).

init() ->
	random:seed(erlang:now()),

	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(paste,
		[{disc_copies, [node()]},
		 {attributes, record_info(fields, paste)}]),
	mnesia:wait_for_tables([paste], infinity).

insert(Content) ->
	Id  = rand_id(),
	Fun = fun() ->
		mnesia:write(#paste {
			index   = Id,
			content = Content
		})
	end,
	mnesia:activity(transaction, Fun),
	Id.

select(Index) ->
	Fun = fun() ->
		case mnesia:read({paste, Index}) of
			[Row] ->
				{Row#paste.content};
			[] ->
				undefined
		end
	end,
	mnesia:activity(transaction, Fun).

exists(Index) ->
	case select(Index) of
		undefined ->
			false;
		_Else ->
			true
	end.

rand_id() ->
	Chars = lists:seq(48, 57) ++ lists:seq(65, 90) ++ lists:seq(97, 122),
	Id = random_string(8, Chars),
	list_to_binary(Id).

random_string(Length, AllowedChars) ->
	Fun = fun(_, Acc) -> [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc end,
	lists:foldl(Fun, [], lists:seq(1, Length)).

