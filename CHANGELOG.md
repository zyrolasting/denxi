# Changelog

All notable changes to this project will be documented in this file.

The format of this file is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
This project follows [these versioning rules](https://sagegerard.com/edition-revision-versioning.html).

## [Unreleased] - 2020-08-02
* Switch to `setup/infotab` and `#lang info` as canonical configuration language.

## [draft:alpha] - 2020-08-02

- Define a namespace-specific string format for packages
- Use SHA-384 for subresource integrity checking
- Verify digital signatures (No restrictions on asymmetric crypto yet. Just bring your own keys).
- Capture digests, installed packages, and `zcpkg` configuration for collaborators.
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


[Unreleased]: https://github.com/zyrolasting/zcpkg/compare/alpha...HEAD
[draft:alpha]: https://github.com/zyrolasting/zcpkg/releases/tag/alpha