# How to contribute?
It is important to coordinate the project FPL Interpreter among the team.
* In the beginning, get in touch with the team via [Discussions](https://github.com/bookofproofs/fpl/discussions). 
* Please propose the work items you would like to focus on in the Discussions section. Please also describe your anticipated solution, and be sufficiently specific. 
* Next, agree with the team upon the work items you will get assigned to cover.
* Implement the work items.
* If necessary, create unit tests related to the new code. 
* Test your repository against your new and the existing unit tests.
* Before creating a pull request, verify if the number of failed unit test got greater than the number you got before you implemented the change. Ideally, no unit tests should fail before creating the pull request. 
* Create a pull request.

# Conventions for your pull-requests
* Never push your repository directly into the master branch.
* Instead, create pull requests after pushing repositories using the following naming conventions:
* ```bugfix/<descriptive_repository_name>```
* ```feature/<descriptive_repository_name>```
* ```refactoring/<descriptive_repository_name>```

# Release management
There are separate CHANGES.md files with release notes for the [grammar](https://github.com/bookofproofs/fpl/blob/master/grammar/CHANGES.md), the [interpreter](https://github.com/bookofproofs/fpl/blob/master/poc/CHANGES.md), the [ide](https://github.com/bookofproofs/fpl/blob/master/ide/CHANGES.md), and the [example theories](https://github.com/bookofproofs/fpl/blob/master/poc/theories/CHANGES.md). The version numbers follow the semantic versioning MAJOR.MINOR.PATCH convention. Please change the version number in CHANGES.md and describe your specific changes and amendments to the code base accordingly before pushing your repository for a change request. 








