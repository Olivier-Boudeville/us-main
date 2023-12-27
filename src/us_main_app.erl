% Copyright (C) 2021-2024 Olivier Boudeville
%
% This file belongs to the US-Main project, a part of the Universal Server
% framework.
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.
%
% You should have received a copy of the GNU Affero General Public License along
% with this program. If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, June 16, 2021.


% @doc The <b>main entry point</b> of the US-Main (Universal Server) active OTP
% application.
%
% Typically triggered:
%
%  - through OTP/rebar3, by ebin/us_main.app (as obtained from
%  conf/us_main.app.src; see start/2)
%
%  - directly, with the help of Myriad's otp_utils (see exec/0)
%
-module(us_main_app).

-behaviour(application).


-export([ exec/0, start/2, stop/1 ]).


% Shorthands:
-type application_name() :: otp_utils:application_name().


% Implementation notes:
%
% We define two starting procedures here:
%
%  - the direct, native one (yet OTP-compliant for prerequisites), through our
%  Ceylan-Myriad make system
%
%  - the OTP release-based one (generally based on rebar3)


% Calls to io:format/{1,2} shall not be replaced (for example by trace_bridge
% ones), in order to better diagnose problems with dependencies (typically
% should Myriad not be found).


% Silencing:
-export([ start_application/1 ]).



% @doc Runs US-Main, directly (e.g. as 'make us_main_exec') rather than as an
% OTP release.
%
-spec exec() -> void().
exec() ->

	% Expecting Myriad to be already available in this branch:
	trace_bridge:info( "Starting the US-Main application natively "
					   "(e.g. not as an OTP release)." ),

	cond_utils:if_defined( us_main_debug_execution,
		trace_bridge:debug_fmt( "Initially, the ~ts",
								[ code_utils:get_code_path_as_string() ] ) ),

	% Not in an OTP context here, yet we need OTP applications (e.g. jsx) to be
	% available (e.g. w.r.t. their .app and BEAMs being found, their starting to
	% be done, etc.); we just not want US-Main to be launched the same way:

	% Base build root directory from which prerequisite applications may be
	% found:
	%
	BuildRootDir = "..",

	% For all (direct and indirect) OTP prerequisites: updating ebin paths so
	% that the corresponding *.app and BEAM files are found, checking that
	% applications are compiled and preparing their starting; no application
	% blacklisting done here.
	%
	% Includes the us_main application itself:

	OrderedAppNames =
		otp_utils:prepare_for_execution( _ThisApp=us_main, BuildRootDir ),

	otp_utils:start_applications( OrderedAppNames, _RestartType=temporary ),

	trace_bridge:debug( "US-Main started (as native)." ).



% @doc Called when US-Main itself is started as an OTP release (as opposed to
% natively, "manually", see exec/0).
%
% The setup and dependency management shall have been done already by the OTP
% release system. So here no ebin path to set or prerequisite applications to
% start for applications listed in US-Main's .app file, we focus only on the
% applications not listed whereas possibly useful in this context and on us_main
% itself.
%
% Note that it may easier/more reliable to add these applications directly in
% the OTP release / rebar configuration.
%
start( StartType, StartArgs ) ->

	% Myriad may be already available in this branch as well, though:
	io:format( "Starting the us_main application "
		"(start type: ~p, arguments: ~p)...~n", [ StartType, StartArgs ] ),

	% To debug any dependency-related 'undef' problem, or to ensure
	% concurrently-emitted messages can be seen (otherwise many outputs may be
	% lost):
	%
	%io:format( "Current code path:~n~p~n", [ code:get_path() ] ),
	%io:format( "undef interpretation: ~ts~n",
	%           [ code_utils:interpret_undef_exception( M, F, A ) ] ),
	%
	%timer:sleep( 2000 ),

	%basic_utils:display( "Prerequisites started; loaded applications:~n~p~n",
	%                     [ application:loaded_applications() ] ),

	% See http://erlang.org/doc/design_principles/applications.html:
	us_main_sup:start_link( as_otp_release ).



% @doc Stops the US-Main application.
stop( _State ) ->
	trace_bridge:info( "Stopping the us_main application." ),

	% In native context, explicit stopping should be done, with the same
	% blacklisting.

	ok.



% Internal functions:


% @doc Starts required applications (not used currently).
-spec start_application( application_name() ) -> void().
start_application( App ) ->
	otp_utils:start_application( App ).
