% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Sunday, July 19, 2020.


% Common US-Main defines.


-define( us_main_sensor_server_registration_name, us_main_sensor_server ).


% Local, as one instance of such server per host may run:
-define( us_main_sensor_server_registration_scope, local_only ).


% Same from the upper US-Common level:
-include_lib("us_common/include/us_common_defines.hrl").
