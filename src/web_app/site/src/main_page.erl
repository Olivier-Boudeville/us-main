%% -*- mode: nitrogen -*-

-module(main_page).

-include_lib("nitrogen_core/include/wf.hrl").

-compile(export_all).


main() ->
	case wf:role( users ) of

		true ->
			%wf:console_log_fmt( "Access granted to user '~ts' of roles ~w.",
			%                    [ wf:user(), wf:roles() ] ),
			#template{ file="./site/templates/bare.html" };

		false ->
			%wf:console_log_fmt( "Access denied to user '~ts' of roles ~w.",
			%                    [ wf:user(), wf:roles() ] ),
			wf:redirect_to_login( "/index" )

   end.



title() ->
	"US-Main Server".


body() ->
	#container_12{
		body=[
			#grid_8{ alpha=true, prefix=2, suffix=2, omega=true,
					 body=inner_body() }
		]
	}.



inner_body() ->
	[
		#h1{ text=title() },
		#p{},
		#panel{ style="margin: 50px;", body=[
			#br{},
			#button{ id=logout_button, text="Log out", postback=log_out } ] }
	].



event( log_out ) ->

	% Clears all user, roles, session state, and page state:
	wf:logout(),

	wf:redirect_to_login( "/index" );


event( Other ) ->
	wf:console_log( "Unexpected event: ~p.", [ Other ] ).
