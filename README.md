TopicHub
=================================
This web application allows users to discover, subscribe to, and obtain automatic delivery of
aggregations of article content repositories.

[![Build Status](https://travis-ci.org/MITLibraries/topichub.svg?branch=master)](https://travis-ci.org/MITLibraries/topichub) [![Coverage Status](https://coveralls.io/repos/MITLibraries/topichub/badge.svg?branch=master)](https://coveralls.io/r/MITLibraries/topichub?branch=master)

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

**A full list of settings and some examples can been seen in app/conf/authentication.conf.**

Authorization
=====
Explicit roles `sysadmin` and `analyst` can manually be assigned in the `hub_user` table. All other
roles are implicit so don't need to be defined.

Branding
=====
Three Environment Variables control the branding experience.
1. `SITE_NAME`: controls the overall branded sitename and defaults to "TopicHub"
2. `SITE_DESCRIPTION`: allows for a description of the site to be configured for the home page.
Html is allowed.
3. `SITE_BASE_URL`: This is used in generated emails to provide links back to the site.
Use the protocol and FQDN for your site. Defaults to "http://example.com" so if you get emails
with that in them you'll want to check this value.

**Developers should access those values using the HubUtils wrapper methods for consistency.**
