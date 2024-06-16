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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Saturday, August 21, 2021.

-module(read_contact_file_test).

-moduledoc """
Reads and interprets specified ETF text file containing **contact information**.
""".


% Test target:
-include_lib("traces/include/traces_for_tests.hrl").


-doc "Runs the tests.".
-spec run() -> no_return().
run() ->

	?test_start,

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the contact directory services." ),

	ContactFilename = "test_contact_directory.etf",

	case file_utils:is_existing_file_or_link( ContactFilename ) of

		true ->

			ContactDirPid =
				class_USContactDirectory:new_link( ContactFilename ),

			% Hence processed just after the completion of the constructor:
			wooper:delete_synchronously_instance( ContactDirPid );

		false ->
			test_facilities:display_fmt( "Warning: no '~ts' file available, "
				"hence no testing of the corresponding, arbitrary contact "
				"information.", [ ContactFilename ] )

	end,

	?test_stop.
