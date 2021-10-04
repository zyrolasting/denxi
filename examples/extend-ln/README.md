This launcher behaves like `ln -s` in terms of HTTPS URLs.  It
bypasses Denxi's package management subsystem entirely, and issues
links to individual data files using `denxi/state`.

Useful for data that you intend to verify yourself. Unsafe for
security-critical automations.

e.g. `./lnx https://www.iana.org/assignments/media-types/media-types.txt`

**Exercise**: Add integrity checking support. Hint: `raco docs current-chfs`

**Exercise**: Add signature checking support.

If you add signature checking, the launcher will only use data you
trust assuming that you trust all system binaries and any relevant
public keys.
