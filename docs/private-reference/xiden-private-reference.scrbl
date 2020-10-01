#lang scribble/manual

@require["../shared.rkt"]

@title{@|project-name|: Private API Reference}

This document covers modules in the @tt{xiden} collection that are technically
visible, but meant for internal use. Since one can always read the code for the
most precise information, this document might not be comprehensive.  Rather
than clutter the code with documentation generation logic, this document
focuses on design justifications to better frame knowledge of the code for
contributors, and my particularly forgetful self.

If you were looking for the public API reference, see @other-doc['(lib
"xiden/docs/public-reference/xiden-public-reference.scrbl")]. If you want a
high-level overview of what @project-name is, see @other-doc['(lib
"xiden/docs/guide/xiden-guide.scrbl")].

@table-of-contents[]

@include-section{string.scrbl}
