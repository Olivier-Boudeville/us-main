% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Tuesday, December 31, 2024.

-module(us_main_controller_app).

-moduledoc """
Actual US-Main client-side **controlling logic**, as a (Myriad) application.

Typically called through the `us_main/priv/bin/control-us-main.sh` script.

Designed to control a US-Main instance typically from any remote host able to
connect to the VM hosting that instance.
""".


-export([ exec/0 ]).


% For us_main_home_automation_server_registration_name:
-include("us_main_defines.hrl").


-doc "Runs this controller app.".
-spec exec() -> no_return().
exec() ->

	% No app_start here, hence we need the following (see
	% traces_for_apps:app_start/2 for a detailed explanation):
	%
	erlang:process_flag( trap_exit, false ),

	{ ActualTargetNodeName, CfgTable, ArgTable } =
        us_client:setup( _ServerPrefix=us_main ),

    UMLookupInfo = us_client:get_config_server_info( CfgTable ),

	{ AllArgs, FinalArgTable } =
		cmd_line_utils:extract_optionless_command_arguments( ArgTable ),

	list_table:is_empty( FinalArgTable ) orelse
		throw( { unexpected_arguments,
				 list_table:enumerate( FinalArgTable ) } ),

	% The US-Main configuration server is expected to run on the target node:
    MainSrvPid = naming_utils:get_registered_pid_from( UMLookupInfo,
                                                       ActualTargetNodeName ),

    BinTokens = text_utils:strings_to_binaries( AllArgs ),

	app_facilities:display( "Will interact with the US-Main server obtained "
        "based on a ~ts: ~w, so that it executes action from tokens '~p'.",
        [ naming_utils:lookup_info_to_string( UMLookupInfo ), MainSrvPid,
          BinTokens ] ),


    % Now actions are managed on the US-Main side, notably as they may originate
    % from various means (e.g. SMS).

    MainSrvPid ! { performActionFromTokens, [ BinTokens ], self() },

    receive

        { wooper_result, { action_outcome, Outcome } } ->
            trace_utils:info_fmt( "Outcome of triggered action: ~p",
                                  [ Outcome ] )

    end,

	%timer:sleep( 2000 ),

	us_client:teardown().
