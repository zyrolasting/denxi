#lang scribble/manual

@require[@for-label[racket/base xiden/signature/snake-oil]]

@title{Signature Prototyping}

@defmodule[xiden/signature/snake-oil]

@deftogether[(
@defthing[snake-oil-public-key bytes?]
@defthing[snake-oil-private-key bytes?]
@defthing[snake-oil-private-key-password bytes?]
)]{
An intentionally-leaked RSA keypair, with a password for the private
key. The private key is encrypted using AES-128 (CBC).

Each key is PEM-encoded.

Use @bold{only} for prototyping signature creation and
verification. Distrust for all other purposes.
}
