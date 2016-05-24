Webmachine-Example

This is an example for a simple REST API build with Erlang and Webmachine. Its creation is discussed in: http://en.wikiversity.org/wiki/Web_Development_with_Webmachine_for_Erlang

Also I took an example from here: https://github.com/figield/webmachine-example
Thanks jups23

Project Skeleton for the mybets app.

You should find in this directory:

- README.md : this file
- Makefile : simple make commands
- rebar : the Rebar build tool for Erlang applications
- rebar.config : configuration for Rebar
- start.sh : simple startup script for running mybets
-- /ebin
---  /mybets.app : the Erlang app specification
-- /src
---  /mybets_app.erl : base module for the Erlang application
---  /mybets_sup.erl : OTP supervisor for the application
---  /mybets_resource.erl : a simple example Webmachine resource
- /priv
--  /dispatch.conf : the Webmachine URL-dispatching table
--  /www : a convenient place to put your static web content

You probably want to do one of a couple of things at this point:

0. Build the skeleton application:
   $ make
   - or:
   $ ./rebar compile

1. Start up the skeleton application:
   $ ./start.sh

2. Change the basic application:
   edit src/mybets_resource.erl

3. Add some new resources:
   edit src/YOUR_NEW_RESOURCE.erl
   edit priv/dispatch.conf
