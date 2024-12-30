% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Thursday, December 22, 2022.


% User-specified setting regarding an instance of presence simulation:
-record( presence_simulation_setting, {

	% The specific EURID (if any) to be used to identify the source of the
	% emitted (Enocean) telegrams to simulate a presence (typically in order to
	% switch a target light on/off); expected to have been already learnt by the
	% target actuator(s).
	%
	% If left to 'undefined', the base identifier of the Enocean gateway will be
	% used.
	%
	source_eurid :: option( oceanic:eurid_string() ),


	% The EURID of the target actuator (typically a smart plug controlling a
	% lamp):
	%
	% (possibly a broadcast address; the default if not set)
	%
	target_eurid :: option( oceanic:eurid_string() ),


	% A chronologically-ordered intra-day (from midnight to midnight) time
	% slots, or a constant policy, during which a presence shall be simulated.
	%
	presence_program :: class_USHomeAutomationServer:presence_program(),


	% Tells whether lighting shall be switched off during a presence slot when
	% the light of day should be available (provided that the position of the
	% server is known):
	%
	smart_lighting = 'true' :: boolean() } ).
