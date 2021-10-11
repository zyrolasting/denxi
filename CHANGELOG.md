# Changelog

All notable changes to this project will be documented in this file.

The format of this file is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
This project follows [these versioning rules](https://sagegerard.com/edition-revision-versioning.html).


## Unreleased

- Rename to Denxi
- Change default branch from `master` to `default`
- Allow use of custom cryptographic backends to unblock users when the
  bundled library doesn't load.
- Merge non-guide and non-reference documentation into examples that
  double as functional tests.
- Add draft functional test API
- Switch to launcher-centric workflow for users
- Add default CHF that is subject to change
- Support `file://` URLs in `http-source`
- Make catalogs more useful: Request packages using a base URL and/or
  highly-abbreviated queries
- Add setting for trusting server certificates outside of the OS trust store.
- Refactor to allow dynamically-computed inputs


## [draft:beta] - 2021-03-17

- Can now extract from any archive format using a plugin
- Can now extract from both `.tar.gz` and `.tgz`, not just the latter.
- Rewrite project to support transactions and content addressable package outputs
- Add `tags`, `home-page`, `racket-versions`, and `description` fields to package definition
- Switch to `denxi/pkgdef` and `#lang denxi` as canonical languages.
- Add `mkint` command
- Help authors make input expressions
- Redirected downloads with no limit now work properly
- Distinguish between abstract and concrete inputs
- Add missing flags to CLI
- Add cycle detection
- Make revision number conversions more explicit
- Loosen requirement for well-formed package queries
- `make-digest` expands user paths before opening files.
- Add `fetch` command
- Simplify `mkinput` command
- Support source-level caching
- Allow integrity and signature information to use sources
- Require trust in cryptographic hash functions
- Allow signature verification via plugin
- Merge rcfile functionality into plugin


## [draft:alpha] - 2020-08-02

- Define a namespace-specific string format for packages
- Use SHA-384 for subresource integrity checking
- Verify digital signatures (No restrictions on asymmetric crypto yet. Just bring your own keys).
- Capture digests, installed packages, and `denxi` configuration for collaborators.
- Create a new package from a template
- Start a sandboxed REPL inside of a package
- Show a diff between a capture and a workspace
- Reproduce a workspace from a capture
    - Apply stored configuration
    - Run install commands
- Create a direct link to a package
- Bundle a package for use on a server
- Print informative output on demand
- Basic operations
    - Install a package (as a link) from a directory
    - Install a package from a service
    - Uninstall a package
- Serve package artifacts
    - Run server
    - GET file
    - GET directory listing
    - GET file from URN's NSS


[Unreleased]: https://github.com/zyrolasting/denxi/compare/beta...HEAD
[draft:alpha]: https://github.com/zyrolasting/denxi/releases/tag/alpha
[draft:beta]: https://github.com/zyrolasting/denxi/releases/tag/beta
