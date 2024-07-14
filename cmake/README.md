CMake Modules
=============

How to embed the modules into a project
---------------------------------------
Just put these files in your `cmake` directory or, if you already use `git`, the easiest way is to use the [`git-subtree`](https://github.com/git/git/blob/master/contrib/subtree/git-subtree.txt) command.

You could use [`git-remote`](https://git-scm.com/docs/git-remote) to add this repo to your set of tracked repositories, and then use a short name in all next `git-subtree` commands, e.g. `cmake-modules` instead of the full URL to this repo.
```
git remote add cmake-modules git@github.com:tsayukov/cmake-modules.git
```
Use the next command to initial checkout. `--prefix=` points to `cmake` that is a default directory where CMake modules live in. `--squash` prevents merging the entire history from this repo, produces only a single commit with all the differences.
```
git subtree --squash --prefix=cmake add cmake-modules main
```
To get updates:
```
git subtree --squash --prefix=cmake pull cmake-modules main
```

License
-------
These CMake modules are distributed under the [MIT License](./LICENSE), but other projects that use this modules can have its own license.

Extra materials
---------------
1. [CMake Reference Documentation](https://cmake.org/cmake/help/latest/)
2. [“Effective CMake”](https://youtu.be/bsXLMQ6WgIk) by Daniel Pfeifer on C++Now 2017
3. [“Deep CMake for Library Authors”](https://youtu.be/m0DwB4OvDXk) by Craig Scott on CppCon 2019
4. “Modern CMake for C++: Discover a better approach to building, testing, and packaging your software” by Rafał Świdziński, 2022
5. Some features were inspired by [this repository](https://github.com/friendlyanon/cmake-init-header-only)
