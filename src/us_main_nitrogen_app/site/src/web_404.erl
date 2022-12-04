%% -*- mode: nitrogen -*-

-module(web_404).

-include_lib("nitrogen_core/include/wf.hrl").

-compile(export_all).


main() ->
	#template{ file="./site/templates/bare.html"}.


title() ->
	"Page not found.".


body() ->
	[
	 #container_12{
		body = [
			#grid_8{ alpha=true, prefix=2, suffix=2, omega=true,
					 body=inner_body() }
			   ]
	   } ].


inner_body() ->
	[
	  #h1{ text="Requested page not found" },
	  #p{},
	  "I could not find the document you wanted, sorry.\n",
	  #br{},
	  "        --- The US-Main server.",
	  #br{},

	  #panel{ class=effects_target, body=[
		#button { text="Go back to main page", postback=continue } ] }
	].


event( continue ) ->
	wf:redirect( "/main_page" );

event( Other ) ->
	wf:console_log_fmt( "Unexpected event: ~p.", [ Other ] ).
