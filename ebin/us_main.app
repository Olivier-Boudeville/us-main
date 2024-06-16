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
  {modules, [accept_ack_protocol, accept_callback_h, accept_callback_missing_h, acceptor_SUITE, action, action_add_class, action_add_option, action_alert, action_animate, action_api, action_appear, action_before_postback, action_buttonize, action_clear_validation, action_click, action_comet, action_confirm, action_console_log, action_continue, action_disable, action_disable_selection, action_effect, action_enable, action_event, action_fade, action_function, action_hide, action_if_value, action_jquery_effect, action_js_fun, action_make_readonly, action_make_writable, action_open_window, action_redirect, action_remove_class, action_remove_option, action_script, action_set, action_set_cookie, action_set_multiple, action_show, action_slide_down, action_slide_up, action_toggle, action_toggle_mobile_panel, action_toggle_option, action_update, action_validate, action_validation_error, action_wire, active_echo_protocol, asterisk_h, cache_handler, cf, cf_term, charset_in_content_types_provided_h, charset_in_content_types_provided_implicit_h, charset_in_content_types_provided_implicit_no_callback_h, charsets_provided_empty_h, charsets_provided_h, check_tcp_options, chunked_hello_world_app, chunked_hello_world_sup, class_USCommunicationGateway, class_USContactDirectory, class_USHomeAutomationServer, class_USMainConfigServer, class_USSensorManager, compress_h, compress_response_app, compress_response_sup, compress_SUITE, config_handler, content_types_accepted_h, content_types_provided_h, cookie_app, cookie_sup, cow_base64url, cowboy, cowboy_app, cowboy_bstr, cowboy_children, cowboy_clear, cowboy_clock, cowboy_compress_h, cowboy_constraints, cowboy_ct_hook, cowboy_handler, cowboy_http, cowboy_http2, cowboy_loop, cowboy_metrics_h, cowboy_middleware, cowboy_req, cowboy_request_server, cowboy_rest, cowboy_router, cowboy_simple_bridge, cowboy_simple_bridge_anchor, cowboy_simple_bridge_sup, cowboy_static, cowboy_stream, cowboy_stream_h, cowboy_sub_protocol, cowboy_sup, cowboy_tls, cowboy_tracer_h, cowboy_websocket, cow_cookie, cow_date, cow_hpack, cow_http, cow_http2, cow_http2_machine, cow_http_hd, cow_http_te, cow_iolists, cow_mimetypes, cow_multipart, cow_qs, cow_spdy, cow_sse, cow_uri, cow_ws, crash_h, crash_handler, crash_protocol, custom_req_fields_h, debug_crash_handler, default_cache_handler, default_config_handler, default_crash_handler, default_h, default_identity_handler, default_log_handler, default_postback_handler, default_query_handler, default_role_handler, default_security_handler, default_state_handler, delay_hello_h, delete_resource_h, directory_h, directory_lister, dynamic_route_handler, ec_assoc_list, ec_cmd_log, ec_cnv, ec_compile, ec_date, ec_dict, ec_dictionary, ec_file, ec_gb_trees, ec_git_vsn, echo_get_app, echo_get_sup, echo_h, echo_post_app, echo_post_sup, echo_protocol, echo_protocol, ec_lists, ec_orddict, ec_plists, ec_plists_tests, ec_rbdict, ec_semver, ec_semver_parser, ec_talk, ec_vsn, element, element_article, element_aside, element_bind, element_br, element_button, element_checkbox, element_date_dropdown, element_datepicker_textbox, element_delay_body, element_draggable, element_dropdown, element_droppable, element_em, element_email_link, element_fieldset, element_file, element_flash, element_function, element_google_chart, element_gravatar, element_grid, element_h, element_hidden, element_hr, element_html5_footer, element_html5_header, element_i, element_iframe, element_image, element_inplace, element_inplace_textarea, element_inplace_textbox, element_label, element_lightbox, element_link, element_list, element_listitem, element_literal, element_main, element_mark, element_mermaid, element_mobile_collapsible, element_mobile_collapsible_set, element_mobile_grid, element_mobile_grid_block, element_mobile_list, element_mobile_list_divider, element_mobile_listitem, element_mobile_panel, element_mobile_toggle, element_nav, element_p, element_panel, element_password, element_placeholder, element_pre, element_progress_bar, element_qr, element_radio, element_radiogroup, element_range, element_recaptcha, element_restful_form, element_restful_reset, element_restful_submit, element_restful_upload, element_section, element_singlerow, element_sortblock, element_sortitem, element_span, element_sparkline, element_spinner, element_strong, element_sync_panel, element_table, element_tablecell, element_tableheader, element_tablerow, element_template, element_textarea, element_textbox, element_textbox_autocomplete, element_time, element_upload, element_value, element_video, element_wizard, element_youtube, embedded_sup, erlmarkdown, eventsource_app, eventsource_h, eventsource_sup, examples_SUITE, expires_h, file_not_found_page, file_server_app, file_server_sup, fs, fs_app, fs_event_bridge, fsevents, fs_server, fs_sup, generate_etag_h, gproc_registry_handler, h2spec_SUITE, hello_h, hello_world_app, hello_world_sup, http2_SUITE, http_basic_auth_security_handler, http_SUITE, ibuild, identity_handler, if_range_h, index, inets_simple_bridge, inets_simple_bridge_anchor, inets_simple_bridge_sup, inotifywait, inotifywait_win32, kqueue, last_modified_h, localtime, localtime_dst, log_handler, long_polling_h, long_polling_sys_h, loop_handler_abort_h, loop_handler_body_h, loop_handler_SUITE, loop_handler_timeout_h, main_page, markdown_converter, markdown_middleware_app, markdown_middleware_sup, metrics_SUITE, misc_SUITE, mobile, mochiweb_simple_bridge, mochiweb_simple_bridge_anchor, mochiweb_simple_bridge_sup, multipart_h, named_route_handler, nitro_cache, nitro_cache_app, nitro_cache_expirer, nitro_cache_mutex, nitro_cache_SUITE, nitro_cache_sup, nitrogen, nitrogen_app, nitrogen_dev, nitrogen_main_handler, nitrogen_rest, nitrogen_smart_extensions, nitrogen_sup, nitro_mochijson2, nitro_mochinum, notify_and_wait_protocol, nprocreg, nprocreg_app, nprocreg_registry_handler, nprocreg_sup, page, passthrough_route_handler, plain_handler_SUITE, plugin_page, postback_handler, process_registry_handler, provide_callback_missing_h, provide_range_callback_h, proxy_header_SUITE, proxy_header_SUITE, proxy_protocol, qdate, qdate_app, qdate_srv, qdate_sup, query_handler, ranch, ranch_acceptor, ranch_acceptors_sup, ranch_app, ranch_conns_sup, ranch_crc32c, ranch_ct_hook, ranch_listener_sup, ranch_protocol, ranch_proxy_header, ranch_server, ranch_ssl, ranch_sup, ranch_tcp, ranch_transport, range_satisfiable_h, ranges_provided_auto_h, ranges_provided_h, rate_limited_h, rekt, remove_conn_and_wait_protocol, req_SUITE, resp_h, resp_iolist_body_h, rest_basic_auth_app, rest_basic_auth_sup, rest_handler_SUITE, rest_hello_h, rest_hello_world_app, rest_hello_world_sup, rest_pastebin_app, rest_pastebin_sup, reverse_protocol, rfc6585_SUITE, rfc7230_SUITE, rfc7231_SUITE, rfc7538_SUITE, rfc7540_SUITE, rfc8297_SUITE, rfc8441_SUITE, role_handler, route_handler, sb_file_upload_handler, sb_uploaded_file, sbw, security_handler, security_SUITE, sendfile_SUITE, send_message_h, session_handler, set_options_h, shutdown_SUITE, simple_bridge, simple_bridge_app, simple_bridge_handler, simple_bridge_handler_sample, simple_bridge_multipart, simple_bridge_multipart_SUITE, simple_bridge_SUITE, simple_bridge_test_handler, simple_bridge_util, simple_bridge_websocket, simple_session_handler, ssl_hello_world_app, ssl_hello_world_sup, ssl_upgrade_protocol, stacktrace_compat_SUITE, stacktrace_transform, state_handler, static_handler_SUITE, stop_handler_h, stream_handler_h, stream_handler_SUITE, supervisor_separate, switch_handler_h, switch_protocol_flush_h, sync, sync_notify, sync_options, sync_scanner, sync_utils, sys_SUITE, tcp_echo_app, tcp_echo_sup, tcp_reverse_app, tcp_reverse_sup, test, test_module, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, toppage_h, tracer_SUITE, transport_capabilities_protocol, trap_exit_protocol, upload_app, upload_h, upload_sup, us_main_app, us_main_monitor_app, us_main_stop_app, us_main_sup, validator_confirm_password, validator_confirm_same, validator_custom, validator_is_email, validator_is_integer, validator_is_number, validator_is_required, validator_js_custom, validator_max_length, validator_min_length, web_404, webmachine_simple_bridge, webmachine_simple_bridge_anchor, webmachine_simple_bridge_static, webmachine_simple_bridge_sup, websocket_app, websocket_sup, wf, wf_action_queue, wf_context, wf_convert, wf_cookies, wf_core, wf_event, wf_handler, wf_pandoc, wf_pickle, wf_render_actions, wf_render_elements, wf_security_policy, wf_tags, wf_test_srv, wf_utils, wf_validation, ws_active_commands_h, ws_autobahn_SUITE, ws_deflate_commands_h, ws_deflate_opts_h, ws_dont_validate_utf8_h, ws_echo, ws_echo_timer, ws_h, ws_handle_commands_h, ws_handler_SUITE, ws_info_commands_h, ws_init_commands_h, ws_init_h, ws_init_shutdown, ws_max_frame_size, ws_send_many, ws_set_options_commands_h, ws_shutdown_reason_commands_h, ws_subprotocol, ws_SUITE, ws_terminate_h, ws_timeout_cancel, ws_timeout_hibernate, yaws_simple_bridge, yaws_simple_bridge_anchor, yaws_simple_bridge_sup]},

  {licenses, ["US-Main is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  {links, [ {"Official website", "http://us-main.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-main"} ]}

  %{exclude_files, []}

 ]}.
