SCOAP3 TopicHub
=================================

This is a web application to allow users to discover, subscribe to,
and obtain automatic delivery of, aggregations of article content from the SCOAP3 repository.

[![Build Status](https://travis-ci.org/MITLibraries/scoap3hub.svg?branch=master)](https://travis-ci.org/MITLibraries/scoap3hub) [![Coverage Status](https://coveralls.io/repos/MITLibraries/scoap3hub/badge.svg?branch=master)](https://coveralls.io/r/MITLibraries/scoap3hub?branch=master)

Authentication
=====
Authentication uses OAuth2 and has been confirmed working with both an MIT
OAuth server and Google. Examples for each are currently in the authentication.conf file.
Additional providers will likely work as well. You can override all authentication related
settings via Environment Variables.

Setting Environment Variables for `AUTH_ID` and `AUTH_SECRET` with values from your
OAuth provider will be required. `AUTH_CALLBACK_URL` should be whatever domain you are
running this app on plus `/_oauth-callback`. You will need to configure that on your
OAuth provider site as well. For development `http://localhost:9000/_oauth-callback`
(or similar) should also be set with the provider.
