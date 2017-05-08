# Haskell FUSE API

Filesystem in Userspace ("FUSE") makes it possible to implement a filesystem as a userspace program.

This library is the Haskell binding to this library.

This is a fork of this binding https://github.com/m15k/hfuse to the 2nd version of libfuse

Right now API is a bit unstable (because it is changing to better reflect how 3rd version of libfuse if used).

## License

[BSD 3-Clause](./LICENSE)

## Information

- Programs using HFuse should be compiled with -threaded.
- This now works for base 4.6+

Following was not tested with libfuse3 (tho works in project which i forked):
- Added build options support for FreeBSD (contribution by https://github.com/pesco)
- MacFUSE should also work (https://github.com/mwotton/hfuse)
- [OSXFuse](https://osxfuse.github.io/) also works (https://github.com/edyu/hfuse)

## Installation

**Installation for development**

Can be installed via Github [repo]

```
git clone git://github.com/fyrchik/hfuse3
cd hfuse3
stack build [--flag hfuse3:examples]
```

## Examples

[HelloFS](./examples/HelloFS.hs) is as basic as you get.  Haskell version of the canonical [example](http://fuse.sourceforge.net/helloworld.html) from the FUSE project.  Once compiled here is how you run HelloFS.

```
$ mkdir ~/fuseTest
$ ./HelloFS ~/fuseTest --name=hello_filename --contents="This will be inside"
```

This creates a file in the *fuseTest* directory.  Now to test the application.

```
$ cat ~/fuseTest/hello_filename
This will be inside
```

To unmount issue the following command:

```
$ fusermount -u ~/fuseTest
```

## Other Samples

If you lack for inspiration the FUSE [Wiki](http://sourceforge.net/p/fuse/wiki/FileSystems/) have amassed quite the list of links to downstream projects.

## Contributions

Help is always welcome.  Pull requests are appreciated.

If you run into any problems or bugs, please report the issue on [Github]
