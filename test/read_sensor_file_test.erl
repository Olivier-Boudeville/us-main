% Copyright (C) 2021-2021 Olivier Boudeville
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


% @doc Reads specified text file containing sensor output data.
%
% Such a file is typically obtained thanks to:
%      $ sensors --no-adapter -j > my_sensor_output.txt
%
-module(read_sensor_file_test).


% Test target:
-include_lib("traces/include/traces_for_tests.hrl").



% Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the sensor-related services." ),

	SensorManagerPid = class_USSensorManager:new_link( "my_sensor_output.txt" ),

	wooper:delete_synchronously_instance( SensorManagerPid ),

	?test_stop.
