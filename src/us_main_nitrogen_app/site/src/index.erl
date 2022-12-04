%% -*- mode: nitrogen -*-

% @doc This is both the entry point and the login page of the US-Main web
% application.
%
-module(index).

-include_lib("nitrogen_core/include/wf.hrl").

-compile(export_all).

-define( min_password_length, 8 ).


main() ->
	#template{ file="./site/templates/bare.html" }.


title() ->
	"Welcome to the US-Main Server".


body() ->
	#container_12{
		body = [
			#grid_16{ alpha=true, prefix=2, suffix=2, omega=true,
					 body=inner_body() }
		]
	}.


inner_body() ->
	wf:console_log( "Entering index page." ),

	wf:wire( authenticate_button, login_text_box, #validate{ validators=[
		#is_required{ text="Required" },
		#is_email { text="Not a valid email address." }
																	 ] } ),

	wf:wire( authenticate_button, password_text_box, #validate{ validators=[
		#is_required{ text="Required" },
		#custom{ text="Hint: enter a correct password.",
				 function=fun check_password/2 }
																	 ] } ),
	CharCount = 30,

	[
	  "<div style=\"text-align:center\">",
	  #h1{ text=title() },
	  "\n"
	  "        Please authenticate in order to access this server.\n"
	  "        ",
	  #label{ text="Your login is your email address:" },
	  #textbox{ id=login_text_box, size=CharCount, next=password_text_box },
	  #p{},
	  #label{ text="Password:" },
	  #password{ id=password_text_box, size=CharCount,
				 next=authenticate_button },
	  #p{},

	  % Not relevant: disabled=true
	  #button{ id=authenticate_button, text="Authenticate",
			   postback=check_auth,
			   enter_clicks=[ login_text_box, password_text_box ] },

	  #flash{},

	  % To wait for delayed auth:
	  #spinner{},

	  % Points to the current URL, to switch to mobile for example:
	  #qr{ size=200 },

	  "</div>" ].



check_password( _Tag, PasswordValue ) ->

	%wf:console_log_fmt( "Checking password '~p' (tag: ~p).",
	%                    [ PasswordValue, Tag ] ),

	length( PasswordValue ) > ?min_password_length.



event( check_auth ) ->

	wf:console_log( "Checking auth." ),

	%wf:replace( authenticate_button, #panel{
	%   body="Authentication in progress",
	%   actions = #effect{effect = highlight} } ),

	SubmittedLogin = wf:q( login_text_box ),
	SubmittedPassword = wf:q( password_text_box ),

	wf:console_log_fmt( "Connection attempt with login '~ts' and "
		"password '~ts'.", [ SubmittedLogin, SubmittedPassword ] ),

	case get_password_for( SubmittedLogin ) of

		undefined ->
			wf:console_log_fmt( "Access denied to '~ts' "
				"(invalid login).", [ SubmittedLogin ] ),
			auth_failed();

		SubmittedPassword ->

			wf:user( SubmittedLogin ),
			Role = users,

			wf:console_log_fmt( "Access granted to '~ts', "
				"whose role has been set to '~ts'.", [ SubmittedLogin, Role ] ),

			wf:role( Role, _IsInRole=true ),

			wf:redirect_from_login("/main_page");

		_OtherPassword ->
			wf:console_log_fmt( "Access denied to '~ts' "
				"(invalid password).", [ SubmittedLogin ] ),
			auth_failed()

	end;


event( Other ) ->
	wf:console_log_fmt( "Unexpected event: ~p.", [ Other ] ).


auth_failed() ->

	% Already a form of defense, as preventing any brute-force attacker
	% to close too quickly their socket:
	%
	timer:sleep( 1000 ),
	wf:flash( "Authentication denied." ).
	%wf:replace( authenticate_button, #panel{
	%	body="Authentication denied",
	%	actions=#effect{ effect=highlight } } )



% @doc Returns the expected password for the specified login.
-spec get_password_for( text_utils:ustring() ) ->
			basic_utils:maybe( text_utils:ustring() ).
get_password_for( _Login="foo@bar.com" ) ->
	"wrgjI4[Z7yjvz{Cei{";

get_password_for( _Login ) ->
	undefined.
