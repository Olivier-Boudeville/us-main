% Description of the US-Main OTP active application, typically used by rebar3.

% Note: if this file is named us_main.app, it is a *generated* file, whose real
% source is conf/us_main.app.src, from which _build/lib/us_main/ebin/us_main.app
% is obtained and copied to ebin/us_main.app; finally src/us_main.app.src is a
% mere symlink to this last file, so we have:
%
% ./conf/us_main.app.src [only real source]
% ./_build/lib/us_main/ebin/us_main.app
% ./ebin/us_main.app
% ./src/us_main.app.src -> ../ebin/us_main.app
%
% For more information see the Ceylan-Myriad 'create-app-file' make target and
% its associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, us_main,
 [{description, "US-Main, the OTP active application corresponding to the Universal Server (see http://us-main.esperide.org)"},
  {vsn, "0.0.19"},
  {registered, [us_main]},

 % Active application:
  %
  % (no specific relevant startup argument to specify here)
  %
  {mod, {us_main_app, []}},

  % Regarding:
  %  - US-Common, see http://us.esperide.org/us.html#otp
  %  - Traces, see http://traces.esperide.org/traces.html#otp
  %  - WOOPER, see http://wooper.esperide.org/wooper.html#otp
  %  - Myriad, see http://myriad.esperide.org/myriad.html#otp
  %

  % myriad is a dependency of wooper, which is itself a dependency of traces,
  % dependency of us_common (dependency of this us_main); as such they may not
  % be listed here, however we stay conservative;
  %
  {applications, [kernel, stdlib, sasl, myriad, wooper, traces, us_common, seaplus, mobile]},
  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [class_USCommunicationGateway, class_USContactDirectory, class_USHomeAutomationServer, class_USMainConfigServer, class_USSensorManager, us_main_app, us_main_monitor_app, us_main_stop_app, us_main_sup]},

  {licenses, ["US-Main is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  {links, [ {"Official website", "http://us-main.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-main"} ]}

  %{exclude_files, []}

 ]}.
