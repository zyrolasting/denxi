To understand this example, you need a working understanding of
cryptographic hash functions (CHFs).

By default, Denxi always verifies if data produces a specific digest.
You need to trust the relevant CHF for Denxi to trust the data enough
to perform an integrity check in the first place. The launcher uses a
prototype CHF to enable integrity checking for toy examples.
