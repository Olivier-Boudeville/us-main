% Copyright (C) 2021-2022 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Wednesday, June 9, 2021.


% @doc Reads and interprets specified text file containing <b>sensor output
% data</b> (possibly gathered from another host, to test/improve the sensor
% coverage of US-Main).
%
% Such a file is typically obtained thanks to:
% $ sensors --no-adapter -j > test_sensor_output.txt
%
-module(read_sensor_file_test).


% Test target:
-include_lib("traces/include/traces_for_tests.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the sensor-related services." ),

	SensorDataFilename = "test_sensor_output.txt",

	case file_utils:is_existing_file_or_link( SensorDataFilename ) of

		true ->

			SensorManagerPid =
				class_USSensorManager:new_link( SensorDataFilename ),

			% Hence processed just after the completion of the constructor:
			wooper:delete_synchronously_instance( SensorManagerPid );

		false ->
			test_facilities:display_fmt( "Warning: no '~ts' file available, "
				"hence no testing of the corresponding, arbitrary sensor "
				"output data.", [ SensorDataFilename ] )

	end,

	?test_stop.
