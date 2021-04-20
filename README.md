# `zoho_auth`

Maintains zoho crm authentication

## Build

    $ rebar3 compile

## Usage

Make sure configuration along these lines is setup in your `sys.config`:

```
[ { zoho_auth
  , [ {endpoint, <<"https://accounts.zoho.com/oath/v2/token">>}
    , {client_id, <<"zoho client id">>}
    , {client_secret, <<"zoho client secret">>}
    , {refresh_token, <<"zoho refresh token">>}
    ]
  }
].
```

With appropriate values for `client_id`, `client_secret` and `refresh_token`.
