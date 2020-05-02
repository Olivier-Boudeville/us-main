.. _Top:


.. title:: Welcome to the Universal Server documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Would appear too late, can only be an be used only in preamble:
.. comment :raw-latex:`\usepackage{graphicx}`
.. comment As a result, in this document at least a '.. figure:: XXXX' must
.. exist, otherwise: 'Undefined control sequence \includegraphics.'.


:raw-html:`<a name="universal-server_top"></a>`

:raw-html:`<div class="banner"><p><em>Universal Server documentation</em> <a href="http://us.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/us-main/index.html">browse mirror</a> <a href="universal-server-technical-manual-english.pdf">get PDF</a> <a href="#universal-server_top">go to top</a> <a href="#universal-server_bottom">go to bottom</a> <a href="mailto:about(dash)universal-server(at)esperide(dot)com?subject=[Universal%20Server]%20Remark">email us</a></p></div>`



:raw-html:`<center><img src="us-main-title.png" width="70%"></img></center>`
:raw-latex:`\includegraphics[scale=1.6]{us-main-title.png}`



============================================
Technical Manual of the ``Universal Server``
============================================


:Organisation: Copyright (C) 2019-2020 Olivier Boudeville
:Contact: about (dash) universal-server (at) esperide (dot) com
:Creation date: Saturday, May 2, 2020
:Lastly updated: Saturday, May 2, 2020
:Status: Work in progress
:Version: 0.0.1
:Dedication: Users and maintainers of the ``Universal Server``.
:Abstract:

	The `Universal Server <http://us-main.esperide.org/>`_, part of the umbrella project `of the same name <https://github.com/Olivier-Boudeville/Universal-Server>`_, is a multi-service daemon in charge of the automation (monitoring, scheduling and performing) of various computer-based tasks, such as the proper management of the server itself or, in the future, of house automation.

	We present here a short overview of these services, to introduce them to newcomers.

	The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/us-main>`_, which are intensely commented and generally straightforward.


.. meta::
   :keywords: Universal Server


:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 3


:raw-latex:`\pagebreak`

--------
Overview
--------

We present here a short overview of these services, to introduce them to newcomers.

The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/us-main>`_, which are intensely commented and generally straightforward.


-----------
Layer Stack
-----------

From the highest level to the lowest, as summarised `here <https://github.com/Olivier-Boudeville/us-main>`_, a software stack involving the Universal Server usually is like:

- the *Universal Server* services themselves (i.e. this `us-main <http://us.esperide.org/>`_ layer)
- [optional] the *Universal Webserver*, i.e. `US-Web <http://us-web.esperide.org/>`_ (for web interaction)
- `US-Common <http://us-common.esperide.org/>`_ (for US base facilities)
- `Ceylan-Traces <http://traces.esperide.org>`_ (for advanced runtime traces)
- `Ceylan-WOOPER <http://wooper.esperide.org>`_ (for OOP)
- `Ceylan-Myriad <http://myriad.esperide.org>`_ (as an Erlang toolbox)
- `Erlang <http://erlang.org>`_ (for the compiler and runtime)
- `GNU/Linux <https://en.wikipedia.org/wiki/Linux>`_

The shorthand for ``Universal Server`` is ``us``.

:raw-latex:`\pagebreak`


.. _`free software`:


-------
Licence
-------

The ``Universal Server`` is licensed by its author (Olivier Boudeville) under the `GNU Affero General Public License <https://www.gnu.org/licenses/agpl-3.0.en.html>`_ as published by the Free Software Foundation, either version 3 of this license, or (at your option) any later version.

This allows the use of the Universal Server code in a wide a variety of software projects, while still maintaining copyleft on this code, ensuring improvements are shared.

We hope indeed that enhancements will be back-contributed (ex: thanks to merge requests), so that everyone will be able to benefit from them.



---------------------------------
Current Stable Version & Download
---------------------------------

As mentioned, the single, the single mandatory prerequisite of the `Universal Server <https://github.com/Olivier-Boudeville/Universal Server>`_ is `US-Common <http://us-common.esperide.org/>`_, which relies on `Ceylan-Traces <https://github.com/Olivier-Boudeville/Ceylan-Traces>`_, which implies in turn `Ceylan-WOOPER <https://github.com/Olivier-Boudeville/Ceylan-WOOPER>`_, then `Ceylan-Myriad <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ and `Erlang <http://erlang.org>`_, version 22.1 or more recent [#]_.

.. [#] Note that, in the Ceylan-Myriad repository, we have a script to streamline the installation of Erlang, see `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_; use ``install-erlang.sh --help`` for guidance.

We recommend securing Erlang thanks to a manual installation, and, for all Erlang-related software, to rely on rebar3.

One wanting to be able to operate on the source code of these dependencies may define appropriate symbolic links in a ``_checkouts`` directory created at the root of ``us-main``, these links pointing to relevant GIT clones.




Using Cutting-Edge GIT
======================

This is the installation method that we use and recommend; the Universal Server ``master`` branch is meant to stick to the latest stable version: we try to ensure that this main line always stays functional (sorry for the pun). Evolutions are to take place in feature branches and to be merged only when ready.

Once Erlang, Cowboy and possibly Awstats are available, it should be just a matter of executing:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad myriad
 $ cd myriad && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-WOOPER wooper
 $ cd wooper && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Traces traces
 $ cd traces && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/us-common
 $ cd us-common && make all

 $ git clone https://github.com/Olivier-Boudeville/us-main
 $ cd us-main && make all



Running a corresponding test just then boils down to:

.. code:: bash

 $ make debug


.. Should LogMX be installed and available in the PATH, the test may simply become:

.. .. code:: bash

..  $ make class_USScheduler_run


:raw-html:`<a name="otp"></a>`

.. _`otp-build`:

Using OTP-Related Build/Runtime Conventions
===========================================

As discussed in these sections of `Myriad <http://myriad.esperide.org/myriad.html#otp>`_, `WOOPER <http://wooper.esperide.org/index.html#otp>`_, `Traces <http://traces.esperide.org/index.html#otp>`_ and `US-Common <http://us-common.esperide.org/index.html#otp>`_, we added the (optional) possibility of generating a Universal Server *OTP application* out of the build tree, ready to result directly in an *(OTP) release*. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and `hex <https://hex.pm/>`_.

Then we benefit from a standalone, complete Universal Server.

As for Myriad, WOOPER, Traces and US-Common, most versions of the Universal Server are also published as `Hex packages <https://hex.pm/packages/us_main>`_.

For more details, one may have a look at:

- `rebar.config.template <https://github.com/Olivier-Boudeville/us-main/blob/master/conf/rebar.config.template>`_, the general rebar configuration file used when generating the Universal Server OTP application and release (implying the automatic management of Myriad and WOOPER)
- `rebar-for-hex.config.template <https://github.com/Olivier-Boudeville/us-main/blob/master/conf/rebar-for-hex.config.template>`_, to generate a corresponding Hex package for Universal Server (whose structure and conventions is quite different from the previous OTP elements)


-------
Support
-------

Bugs, questions, remarks, patches, requests for enhancements, etc. are to be reported to the `project interface <https://github.com/Olivier-Boudeville/us-main>`_ (typically `issues <https://github.com/Olivier-Boudeville/us-main/issues>`_) or directly at the email address mentioned at the beginning of this document.




-------------
Please React!
-------------

If you have information more detailed or more recent than those presented in this document, if you noticed errors, neglects or points insufficiently discussed, drop us a line! (for that, follow the Support_ guidelines).



-----------
Ending Word
-----------

Have fun with the Universal Server!

.. comment Mostly added to ensure there is at least one figure directive,
.. otherwise the LateX graphic support will not be included:

.. figure:: us-main-title.png
   :alt: Universal Server logo
   :width: 100 %
   :align: center

:raw-html:`<a name="us-main_bottom"></a>`
