Xiden comes bundled with an OpenSSL derivative so that everyone who
installs it has access to the same CHF and cipher implementations.

In the event the library fails to load, of if you simply do not trust
it, you can define your own implementations for cryptographic
operations.

`openssl-subprocess-backend.rkt` is a launcher that ignores the
bundled library in favor of the host OpenSSL binary.
