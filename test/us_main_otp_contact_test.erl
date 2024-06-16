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
% Creation date: Saturday, August 21, 2021.

-module(us_main_otp_contact_test).

-moduledoc """
Module in charge of the testing of the **US-Main contact services**>.
""".


-include("us_main_defines.hrl").

-include_lib("traces/include/traces_for_tests.hrl").



-doc "Actual test.".
test_us_main_contact_management( OrderedAppNames ) ->

	test_facilities:display( "Testing the contact-related services; for that "
							 "starting the US-Main OTP active application." ),

	% We did not trap EXIT messages, as we wanted this test to crash (thanks to
	% the links below) in case of problem (and not to receive an EXIT message
	% bound not to be read, as it happened when no US configuration file was
	% found).
	%
	% However such tests may crash even when stopping (normally) applications,
	% as apparently an OTP application has its child processes terminated with
	% reason 'shutdown' (not 'normal').
	%
	% So now this test process traps EXIT messages, and ensures that none
	% besides {'EXIT',P,shutdown}, P being the PID of a US-Common process, is
	% received (actually for US-Common no such message is received, unlike for
	% the WOOPER counterpart test case).
	%
	false = erlang:process_flag( trap_exit, true ),

	% We now link to the US-Main contact manager.

	% No ?test_start/?test_stop here, as we start/stop Traces through
	% OTP-related operations.
	%
	% If in batch mode (not in a release, hence no sys.config read here, so only
	% the --batch command-line option matters here), the trace aggregator will
	% record that a trace supervisor is wanted later (iff renamed), otherwise
	% (not in batch mode), no trace supervisor is wanted at all.
	%
	otp_utils:start_applications( OrderedAppNames ),

	ContactDirectoryPid = class_USContactDirectory:get_contact_directory(),

	% The top-level user process may not be aware that an OTP application fails
	% (e.g. because its main process crashed), which is a problem for a test. So
	% here we link explicitly this test process to the US-Main contact manager,
	% to have a chance of detecting issues:
	%
	erlang:link( ContactDirectoryPid ),


	% 5 seconds:
	WaitDurationMs = 5*1000,

	test_facilities:display( "Waiting for ~ts. Not much can be done in the "
		"meantime except perhaps looking at the corresponding trace file "
		"(in src/traces_via_otp.traces).",
		[ time_utils:duration_to_string( WaitDurationMs ) ] ),

	timer:sleep( WaitDurationMs ),

	test_facilities:display( "Waiting over." ),


	?test_info( "Successful test (not fully ended yet) of the US-Main OTP "
				"application." ),

	% Including US-Main:
	?test_info( "Stopping all user applications." ),
	otp_utils:stop_user_applications( OrderedAppNames ),

	% Not able to use Traces anymore:
	trace_utils:debug_fmt( "Waiting for the termination of the US-Main "
						   "contact directory (~w).", [ ContactDirectoryPid ] ),

	receive

		{'EXIT', ContactDirectoryPid, normal } ->
			ok

	end,

	% None expected to be left:
	basic_utils:check_no_pending_message(),

	test_facilities:display(
		"Successful end of test of the US-Main OTP application." ).



-doc """
Runs the tests.

Note that the {us_main, us_common, mobile, seaplus, traces, wooper, myriad}.app
files will have to be found and used for this test to succeed: US-Main,
US-Common, Mobile, Seaplus, Traces, WOOPER and Myriad must be already available
as prerequisite, fully-built OTP applications.
""".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which sibling prerequisite applications may be
	% found:
	%
	BuildRootDir = "..",

	OrderedAppNames = otp_utils:prepare_for_execution( _ThisApp=us_main,
													   BuildRootDir ),

	trace_bridge:info_fmt( "Resulting applications to start, in order: ~w.",
						   [ OrderedAppNames ] ),

	test_us_main_contact_management( OrderedAppNames ),

	test_facilities:stop().
