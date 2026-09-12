# Contributing Guidelines

## Purpose

This document defines contribution guidelines and project-wide policies for the repository, including the policy for licensing headers in source files.

## License Policy

This project uses the MIT License. Maintain a single canonical `LICENSE` file at the repository root containing the full text of the MIT license.

Source files should not duplicate the full license text. Instead, each source file should include a short header that references the canonical `LICENSE` file and the copyright owner. This keeps files concise while preserving legal attribution.

### Recommended short header (F#)

Place this header at the top of each `.fs` file (and analogous short headers for other file types):

```fsharp
(*
Copyright (c) 2026+ bookofproofs
See LICENSE in the project root for license terms.
*)
```

For files that already contain the full license block (historical files), it is acceptable to leave those unchanged. New files and file updates should use the short header format.

## Why this approach

- Single source of truth: keeps the full license text in one place making updates straightforward.
- Reduces noise in individual source files, improving readability.
- Many tools (license scanners, package managers) expect a top-level `LICENSE` file for automated detection.
- Maintains legal attribution via the short header while reducing duplication.

## Contact

If you need to change the license text or adopt a different header policy, open an issue or a PR and document the reasons in the PR description.

## How to contribute?
It is important to coordinate the project FPL Interpreter among the team.
* In the beginning, get in touch with the team via [Discussions](https://github.com/bookofproofs/fpl/discussions). 
* Please propose the work items you would like to focus on in the Discussions section. Please also describe your anticipated solution, and be sufficiently specific. 
* Next, agree with the team upon the work items you will get assigned to cover.
* Implement the work items.
* If necessary, create unit tests related to the new code. 
* Test your repository against your new and the existing unit tests.
* Before creating a pull request, verify if the number of failed unit test got greater than the number you got before you implemented the change. Ideally, no unit tests should fail before creating the pull request. 
* Create a pull request.

## Conventions for your pull-requests
* Never push your repository directly into the master branch.
* Instead, create pull requests after pushing repositories using the following naming conventions:
* ```bugfix/<descriptive_repository_name>```
* ```feature/<descriptive_repository_name>```
* ```refactoring/<descriptive_repository_name>```

# Release management
There are separate CHANGES.md files with release notes for the [grammar](https://github.com/bookofproofs/fpl/blob/master/grammar/CHANGES.md), the [interpreter](https://github.com/bookofproofs/fpl/blob/master/poc/CHANGES.md), the [ide](https://github.com/bookofproofs/fpl/blob/master/ide/CHANGES.md), and the [example theories](https://github.com/bookofproofs/fpl/blob/master/poc/theories/CHANGES.md). The version numbers follow the semantic versioning MAJOR.MINOR.PATCH convention. Please change the version number in CHANGES.md and describe your specific changes and amendments to the code base accordingly before pushing your repository for a change request. 








